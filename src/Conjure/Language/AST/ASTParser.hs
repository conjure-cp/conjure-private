{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Conjure.Language.AST.ASTParser
  ( parseProgram,
    ParserError,
    runASTParser,
    parseExpression,
    parseDomain,
    parseTopLevels,
    example,
    exampleFile, -- For debugging
  )
where

import Conjure.Language.AST.Helpers
import Conjure.Language.AST.Reformer (HighLevelTree (..), flattenSeq)
import Conjure.Language.AST.Syntax
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Language.Lexemes
import Conjure.Language.Lexer
import Conjure.Prelude hiding (many, some)
import Control.Monad.Combinators.Expr
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Text.Megaparsec

newtype ParserError = ParserError Doc
  deriving (Show)

runASTParser :: (HighLevelTree a) => Parser a -> ETokenStream -> Either ParserError a
runASTParser p str =
  case runParser p "parser" str of
    Left peb -> Left $ ParserError . pretty $ errorBundlePretty peb
    Right res -> Right res

parseProgram :: Parser ProgramTree
parseProgram =
  do
    langV <- optional parseLangVersion
    (tl, ending) <- manyTill_ parseTopLevel pEnding
    return $ ProgramTree langV tl ending
    <?> "Program"

parseLangVersion :: Parser LangVersionNode
parseLangVersion = do
  lLang <- need L_language
  lLName <- parseIdentifier
  nums <- parseSequence L_Dot (StrictToken [] <$> intLiteral)
  return $ LangVersionNode lLang lLName nums

parseTopLevels :: Parser [StatementNode]
parseTopLevels = manyTill parseTopLevel pEnding

parseTopLevel :: Parser StatementNode
parseTopLevel =
  do
    parseDeclaration
    <|> parseBranching
    <|> parseSuchThat
    <|> parseWhere
    <|> parseObjective
    <|> parseHeuristic
    <|> UnexpectedToken
    <$> makeUnexpected

parseHeuristic :: Parser StatementNode
parseHeuristic = do
  lHeuristic <- need L_heuristic
  expr <- parseExpression
  return $ HeuristicStatement lHeuristic expr

parseBranching :: Parser StatementNode
parseBranching = do
  lBranching <- need L_branching
  lOn <- want L_on
  branchSts <- squareBracketList (commaList parseExpression)
  return $ BranchingStatement $ BranchingStatementNode lBranching lOn branchSts

parseSuchThat :: Parser StatementNode
parseSuchThat = do
  lSuch <- need L_such
  lThat <- want L_that
  exprs <- commaList1 parseExpression
  return $ SuchThatStatement $ SuchThatStatementNode lSuch lThat exprs

parseWhere :: Parser StatementNode
parseWhere = do
  lWhere <- need L_where
  exprs <- commaList1 parseExpression
  return $ WhereStatement $ WhereStatementNode lWhere exprs

parseObjective :: Parser StatementNode
parseObjective = do
  ObjectiveStatement <$> parseObjectiveStatement

parseDeclaration :: Parser StatementNode
parseDeclaration =
  DeclarationStatement
    <$> choice
      [ declaration LettingStatement L_letting parseLetting,
        declaration GivenStatement L_given parseGiven,
        declaration FindStatement L_find parseFind
      ]
  where
    declaration :: (Null a, Show a) => (SToken -> Sequence a -> b) -> Lexeme -> Parser a -> Parser b
    declaration c t p = do
      l <- need t
      seq <- option (Seq []) (commaList1 p)
      return $ c l seq

parseLetting :: Parser LettingStatementNode
parseLetting = try $ do
  names <- commaList1 parseIdentifier
  lBe <- want L_be
  guard $ not (isMissing names && isMissing lBe)
  let start = LettingStatementNode names lBe
  start
    <$> choice
      [ finishDomain,
        try finishAnon,
        try finishEnum,
        LettingExpr <$> parseExpression
      ]
  where
    finishDomain = do
      lDomain <- need L_domain
      domain <- parseDomain
      return $ LettingDomain lDomain domain
    finishAnon = try $ do
      lNew <- want L_new
      lType <- want L_type
      lOf <- want L_of
      lSize <- want L_size
      guard (not $ all isMissing [lOf, lSize])
      expr <- parseExpression
      return $ LettingUnnamed lNew lType lOf lSize expr
    finishEnum = do
      lNew <- want L_new
      lType <- want L_type
      lEnum <- want L_enum
      guard (not $ all isMissing [lNew, lType, lEnum])
      members <- curlyBracketList $ commaList parseIdentifier
      return $ LettingEnum lNew lType lEnum members

parseGiven :: Parser GivenStatementNode
parseGiven = do
  names <- commaList1 parseIdentifier
  choice
    [ finishEnum (GivenEnumNode names),
      finishDomain (GivenStatementNode names)
    ]
  where
    finishEnum start = do
      lNew <- want L_new
      lType <- want L_type
      lEnum <- want L_enum
      guard (not $ all isMissing [lNew, lType, lEnum])
      return $ start lNew lType lEnum
    finishDomain start = do
      lColon <- want L_Colon -- want here so that parse cannot fail
      domain <- parseDomain
      return $ start lColon domain

parseFind :: Parser FindStatementNode
parseFind =
  do
    names <- commaList1 parseIdentifier
    lColon <- want L_Colon
    domain <- parseDomain
    return $ FindStatementNode names lColon domain
    <?> "Find Statement"

parseObjectiveStatement :: Parser ObjectiveStatementNode
parseObjectiveStatement =
  do
    s <- eSymbol L_minimising <|> eSymbol L_maximising
    e <- parseExpression
    return $ case s of
      (ETok {lexeme = L_minimising}) -> ObjectiveMin (StrictToken [] s) e
      _ -> ObjectiveMax (StrictToken [] s) e
    <?> "Objective Statement"

pEnding :: Parser SToken
pEnding = do
  t <- lookAhead anySingle
  case t of
    ETok {lexeme = L_EOF} -> return $ StrictToken [] t
    _ -> empty

---------------------------------------

---------------------------------------

parseExpression :: Parser ExpressionNode
parseExpression = try $ do
  parseOperator
    <|> parseAtomicExpression
    <|> (MissingExpressionNode <$> makeMissing (L_Missing MissingExpression))

parseExpressionStrict :: Parser ExpressionNode -- can fail
parseExpressionStrict = try $ do
  expr <- parseExpression
  case expr of
    MissingExpressionNode _ -> empty
    _ -> return expr

parseAtomicExpression :: Parser ExpressionNode
parseAtomicExpression = do
  try
    $ choice
      [ parseSpecialCase,
        parseFunction, -- has to be first because true is overloaded
        Literal <$> parseLiteral,
        parseAttributeAsConstraint,
        IdentifierNode <$> parseIdentifierStrict,
        MetaVarExpr <$> parseMetaVar,
        ParenExpression <$> parseParenExpression parensPair,
        AbsExpression <$> parseAbsExpression,
        QuantificationExpr <$> parseQuantificationStatement,
        DomainExpression <$> parseDomainExpression,
        MissingExpressionNode <$> makeMissing (L_Missing MissingExpression)
      ]

parseDomainExpression :: Parser DomainExpressionNode
parseDomainExpression = try $ do
  lTick <- needWeak L_BackTick
  domain <- parseDomain
  case domain of
    MissingDomainNode _ -> empty
    _ -> pure ()
  rTick <- want L_BackTick
  return $ DomainExpressionNode lTick domain rTick

-- [a,b,c : int (1..2)]
-- [a,b,c : int (1..4) | x < 3,letting x be int]

parseMatrixBasedExpression :: Parser LiteralNode
parseMatrixBasedExpression = do
  openB <- needWeak L_OpenBracket
  exprs <- commaList parseExpression
  range <- optional pOverDomain
  comprehension <- optional pComp
  closeB <- want L_CloseBracket
  let es = exprs
  return $ MatrixLiteral $ MatrixLiteralNode openB es range comprehension closeB
  where
    pOverDomain = OverDomainNode <$> needWeak L_SemiColon <*> parseDomain
    pComp = do
      bar <- need L_Bar
      body <- commaList parseComprehensionCondition
      return $ ComprehensionNode bar body

-- TODO look into adding enviorment to the parser to configure forgiveness
parseAbsExpression :: Parser ParenExpressionNode
parseAbsExpression = try $ do
  lParen <- needWeak L_Bar
  expr <- parseExpression
  rParen <- needWeak L_Bar
  return $ ParenExpressionNode lParen expr rParen

parseParenExpression :: (Lexeme, Lexeme) -> Parser ParenExpressionNode
parseParenExpression (open, close) = try $ do
  lParen <- needWeak open
  body <- parseExpression
  notFollowedBy $ need L_Comma
  rParen <- want close
  return $ ParenExpressionNode lParen body rParen

parseLiteral :: Parser LiteralNode
parseLiteral =
  choice
    [ parseIntLiteral,
      parseBoolLiteral,
      parseMatrixBasedExpression,
      parseTupleLiteral,
      parseShortTupleLiteral,
      parseRecordLiteral,
      parseVariantLiteral,
      parseSetLiteral,
      parseMSetLiteral,
      parseFunctionLiteral,
      parseSequenceLiteral,
      parsePermutationLiteral,
      parseRelationLiteral,
      parsePartitionLiteral
    ]

parseShortTupleLiteral :: Parser LiteralNode
parseShortTupleLiteral = try $ do
  lOpen <- needWeak L_OpenParen
  exprs <- commaList parseExpression
  let Seq xs = exprs
  guard (length xs > 1)
  lClose <- want L_CloseParen
  return $ TupleLiteralNodeShort $ ShortTuple (ListNode lOpen exprs lClose)

parseIntLiteral :: Parser LiteralNode
parseIntLiteral = do
  lit <- intLiteral
  maybe_tag <- optional $ do
    _ <- want L_Colon
    identifier
  return $ IntLiteral (StrictToken [] lit) maybe_tag

parseBoolLiteral :: Parser LiteralNode
parseBoolLiteral = BoolLiteral <$> (need L_true <|> need L_false)

parseTupleLiteral :: Parser LiteralNode
parseTupleLiteral = do
  lTuple <- need L_tuple
  members <- parenList $ commaList parseExpression
  return $ TupleLiteralNode $ LongTuple lTuple members

parseRecordLiteral :: Parser LiteralNode
parseRecordLiteral = do
  lRecord <- need L_record
  members <- curlyBracketList (commaList parseRecordMember)
  return $ RecordLiteral lRecord members

parseVariantLiteral :: Parser LiteralNode
parseVariantLiteral = do
  lVariant <- need L_variant
  members <- curlyBracketList (commaList parseRecordMember)
  return $ VariantLiteral lVariant members

parseRecordMember :: Parser RecordMemberNode
parseRecordMember = do
  name <- parseIdentifier
  lEqual <- want L_Eq
  val <- parseExpression
  return $ RecordMemberNode name lEqual val

parseSetLiteral :: Parser LiteralNode
parseSetLiteral = do
  -- cant just recycle list as it does not require first char
  lOpen <- needWeak L_OpenCurly
  members <- commaList parseExpression
  lClose <- want L_CloseCurly
  return $ SetLiteral (ListNode lOpen members lClose)

parseMSetLiteral :: Parser LiteralNode
parseMSetLiteral = do
  lMSet <- need L_mset
  members <- parenList (commaList parseExpression)
  return $ MSetLiteral lMSet members

parseFunctionLiteral :: Parser LiteralNode
parseFunctionLiteral = do
  lFunc <- need L_function
  args <- parenList (commaList parseArrowPair)
  return $ FunctionLiteral lFunc args

parseArrowPair :: Parser ArrowPairNode
parseArrowPair = try $ do
  lhs <- parseExpression
  arrow <- want L_LongArrow
  rhs <- parseExpression
  return $ ArrowPairNode lhs arrow rhs

parseSequenceLiteral :: Parser LiteralNode
parseSequenceLiteral = do
  lSeq <- need L_sequence
  members <- parenList (commaList parseExpression)
  return $ SequenceLiteral lSeq members

parsePermutationLiteral :: Parser LiteralNode
parsePermutationLiteral = do
  lPer <- need L_permutation
  members <- parenList (commaList parsePermutationElem)
  return $ PermutationLiteral lPer members

parseRelationLiteral :: Parser LiteralNode
parseRelationLiteral = do
  lRel <- need L_relation
  members <- parenList (commaList parseRelationMember)
  return $ RelationLiteral lRel members

parseRelationMember :: Parser RelationElemNode
parseRelationMember = try $ do
  f <- optional $ need L_tuple
  members <- parenList $ commaList parseExpression
  case f of
    Just lTup -> return $ RelationElemNodeLabeled $ LongTuple lTup members
    Nothing -> case members of
      ListNode l c r | (isMissing l || isMissing r) && isMissing c -> empty
      _ -> return $ RelationElemNodeShort $ ShortTuple members

parsePartitionLiteral :: Parser LiteralNode
parsePartitionLiteral = do
  lPartition <- need L_partition
  members <- parenList (commaList parsePartitionElem)
  return $ PartitionLiteral lPartition members

parsePermutationElem :: Parser PermutationElemNode
parsePermutationElem = try $ do
  lOpen <- needWeak L_OpenParen
  exprs <- commaList parseExpression
  let Seq xs = exprs
  guard (length xs >= 2)
  lClose <- want L_CloseParen
  return $ PermutationElemNode $ ListNode lOpen exprs lClose

parsePartitionElem :: Parser PartitionElemNode
parsePartitionElem = PartitionElemNode <$> parseList L_OpenCurly L_CloseCurly (commaList parseExpression)

parseQuantificationStatement :: Parser QuantificationExpressionNode
parseQuantificationStatement = do
  lType <- choice $ map need quantifiers
  terms <- commaList parseAbstractPattern
  over <- parseQuantificationOver
  qGuard <- optional $ do
    lComma <- need L_Comma
    expr <- parseExpression
    return $ QuanticationGuard lComma expr
  lDot <- want L_Dot
  expr <- parseExpression
  return $ QuantificationExpressionNode lType terms over qGuard lDot expr
  where
    parseQuantificationOver :: Parser QuantificationOverNode
    parseQuantificationOver =
      choice
        [ QuantifiedMemberOfNode <$> need L_in <*> parseExpression,
          QuantifiedSubsetOfNode <$> need L_subsetEq <*> parseExpression,
          QuantifiedDomainNode <$> (OverDomainNode <$> want L_Colon <*> parseDomain)
        ]

parseAbstractPattern :: Parser AbstractPatternNode
parseAbstractPattern = do
  choice
    [ parseAbstractId,
      parseAbstractMetaVar,
      parseAbstractPatternTuple,
      parseAbstractPatternMatrix,
      parseAbstractPatternSet
    ]
  where
    parseAbstractId :: Parser AbstractPatternNode
    parseAbstractId = AbstractIdentifier <$> parseIdentifierStrict
    parseAbstractMetaVar :: Parser AbstractPatternNode
    parseAbstractMetaVar = AbstractMetaVar <$> parseMetaVar
    parseAbstractPatternTuple :: Parser AbstractPatternNode
    parseAbstractPatternTuple = do
      lTuple <- optional $ needWeak L_tuple
      openB <- (if null lTuple then needWeak else want) L_OpenParen
      es <- commaList parseAbstractPattern
      closeB <- want L_CloseParen
      return $ AbstractPatternTuple lTuple (ListNode openB es closeB)
    parseAbstractPatternMatrix :: Parser AbstractPatternNode
    parseAbstractPatternMatrix = do
      openB <- needWeak L_OpenBracket
      es <- commaList parseAbstractPattern
      closeB <- want L_CloseBracket
      return $ AbstractPatternMatrix (ListNode openB es closeB)
    parseAbstractPatternSet :: Parser AbstractPatternNode
    parseAbstractPatternSet = do
      openB <- needWeak L_OpenCurly
      es <- commaList parseAbstractPattern
      closeB <- want L_CloseCurly
      return $ AbstractPatternSet (ListNode openB es closeB)

parseComprehensionCondition :: Parser ComprehensionBodyNode
parseComprehensionCondition = do
  letting <|> generator <|> condition
  where
    letting = do
      lLetting <- need L_letting
      v <- parseAbstractPattern
      lBe <- want L_be
      expr <- parseExpression
      return $ CompBodyLettingNode lLetting v lBe expr
    generator = try $ do
      pats <- commaList parseAbstractPattern
      choice
        [ try $ do
            lColon <- need L_Colon
            domain <- parseDomain
            return $ CompBodyDomain pats lColon domain,
          try $ do
            lArrow <- need L_LeftArrow
            expr <- parseExpression
            return $ CompBodyGenExpr pats lArrow expr
        ]

    condition = CompBodyCondition <$> parseExpressionStrict

parseOperator :: Parser ExpressionNode
parseOperator = try (makeExprParser parseAtomicExpressionAndFixes operatorTable <?> "Expression")

parseFunction :: Parser ExpressionNode
parseFunction = try $ do
  name <- choice $ map need functionals
  let ol = isOverloaded name
  let parenP = if ol then parenListStrict else parenList
  args <- parenP $ commaList parseExpression
  guard $ not ol || argsHasNoLeadingTrivia args
  return $ FunctionalApplicationNode name args
  where
    isOverloaded (StrictToken _ ETok {lexeme = lex}) = lex `elem` overloadedFunctionals
    argsHasNoLeadingTrivia (ListNode (RealToken (StrictToken [] ETok {trivia = []})) _ _) = True
    argsHasNoLeadingTrivia _ = False

parseAttributeAsConstraint :: Parser ExpressionNode
parseAttributeAsConstraint = do
  name <- parseAttributeLexeme
  args <- parenList $ commaList parseExpression
  return $ AttributeAsConstriant name args

parsePostfixOp :: Parser (ExpressionNode -> ExpressionNode)
parsePostfixOp = do
  op <-
    try
      $ choice
        [ indexed,
          factorial,
          application,
          explicitDomain
        ]
  return $ \e -> OperatorExpressionNode $ PostfixOpNode e op
  where
    indexed = do
      lBracket <- need L_OpenBracket
      indexer <- commaList parseRange
      rBracket <- want L_CloseBracket
      return $ IndexedNode $ ListNode (RealToken lBracket) indexer rBracket
    factorial = OpFactorial <$> need L_ExclamationMark
    application = do
      lBracket <- need L_OpenParen
      args <- commaList parseExpression
      rBracket <- want L_CloseParen
      return $ ApplicationNode $ ListNode (RealToken lBracket) args rBracket
    explicitDomain = try $ do
      lColon <- need L_Colon
      lTickl <- need L_BackTick
      dom <- parseDomain
      lTickr <- want L_BackTick
      return $ ExplicitDomain lColon lTickl dom lTickr

-- TODO treat funcitonals differently or actually don't but why

operatorTable :: [[Operator Parser ExpressionNode]]
operatorTable =
  let operatorsGrouped =
        operators
          |> sortBy (\(_, a) (_, b) -> compare a b)
          |> groupBy (\(_, a) (_, b) -> a == b)
          |> reverse
   in postfixOps
        : [ [ case descr of
                BinaryOp op FLeft -> InfixL $ exprBinary <$> need op
                BinaryOp op FNone -> InfixN $ exprBinary <$> need op
                BinaryOp op FRight -> InfixR $ exprBinary <$> need op
                UnaryPrefix op -> prefixOps op
              | -- UnaryPrefix L_ExclamationMark -> Prefix $ prefixBinary--foldr1 (.) <$> some parseUnaryNot
                -- UnaryPrefix l                 -> bug ("Unknown UnaryPrefix" <+> pretty (show l))
                (descr, _) <- operatorsInGroup
            ]
            | operatorsInGroup <- operatorsGrouped
          ]

parseAtomicExpressionAndFixes :: Parser ExpressionNode
parseAtomicExpressionAndFixes = try $ do
  let prefixes = do
        fs <- some parsePrefixes
        return $ foldr1 (.) fs
      postfixes = do
        fs <- some parsePostfixOp
        return $ foldr1 (.) (reverse fs)
      withPrefix x = do f <- option id prefixes; i <- x; return $ f i
      withPostfix x = do
        i <- x
        -- guard $ not $ isMissing i ;
        mf <- optional postfixes
        return $ fromMaybe id mf i
  withPrefix (withPostfix parseAtomicExpression) <?> "expression"

parsePrefixes :: Parser (ExpressionNode -> ExpressionNode)
parsePrefixes = choice [parseUnary L_Minus, parseUnary L_ExclamationMark]
  where
    parseUnary l = (\e -> OperatorExpressionNode . PrefixOpNode e) <$> need l

exprBinary :: SToken -> ExpressionNode -> ExpressionNode -> ExpressionNode
exprBinary t l = OperatorExpressionNode . BinaryOpNode l t

prefixOps :: Lexeme -> Operator Parser ExpressionNode
prefixOps l = Prefix $ foldr1 (.) <$> some (try opBuilder)
  where
    opBuilder :: Parser (ExpressionNode -> ExpressionNode)
    opBuilder = do
      t <- need l
      return (OperatorExpressionNode . PrefixOpNode t)

postfixOps :: [Operator Parser ExpressionNode]
postfixOps =
  [ Postfix $ foldr1 (.) . reverse <$> some parsePostfixOp
  ]

-- DOMAINS
parseDomain :: Parser DomainNode
parseDomain =
  do
    choice
      [ BoolDomainNode <$> need L_bool,
        parseIntDomain,
        MetaVarDomain <$> parseMetaVar,
        parseTuple,
        parseRecord,
        parseVariant,
        parseMatrix,
        parseSet,
        parseMSet,
        parseFunctionDomain,
        parseSequenceDomain,
        parsePermutationDomain,
        parseRelation,
        parsePartition,
        parseEnumDomain,
        parseShortTuple
      ]
      <?> "Domain"
    <|> parseMissingDomain
    <?> "missingDomain"

parseSpecialCase :: Parser ExpressionNode
parseSpecialCase = do
  SpecialCase <$> choice [parseWithDecls]
  where
    parseWithDecls = try
      $ do
        p1 <- need L_OpenCurly
        exp1 <- parseExpression
        lAt <- need L_At
        (decsl, p2) <- manyTill_ parseTopLevel (need L_CloseCurly)
        return $ ExprWithDecls p1 exp1 lAt decsl p2

parseIntDomain :: Parser DomainNode
parseIntDomain = do
  lInt <- need L_int
  maybe_tag <- optional $ do
    _ <- want L_Colon
    identifier
  ranges <- optional $ parenListStrict $ commaList parseRange
  return $ RangedIntDomainNode lInt ranges maybe_tag

parseTuple :: Parser DomainNode
parseTuple = do
  lTuple <- need L_tuple
  members <- parenList $ commaList parseDomain
  return $ TupleDomainNode lTuple members

parseShortTuple :: Parser DomainNode
parseShortTuple = do
  openB <- need L_OpenParen
  lst <- commaList parseDomain
  closeB <- want L_CloseParen
  return $ case lst of
    Seq [SeqElem d Nothing] -> ParenDomainNode openB d closeB
    Seq _ -> ShortTupleDomainNode $ ListNode (RealToken openB) lst closeB

parseRecord :: Parser DomainNode
parseRecord = do
  lRecord <- need L_record
  members <- curlyBracketList $ commaList parseNameDomain
  return $ RecordDomainNode lRecord members

parseVariant :: Parser DomainNode
parseVariant = do
  lVariant <- need L_variant
  members <- curlyBracketList $ commaList parseNameDomain
  return $ VariantDomainNode lVariant members

parseMatrix :: Parser DomainNode
parseMatrix = do
  lMatrix <- need L_matrix
  lIndexed <- want L_indexed
  lBy <- want L_by
  let indexByNode = case (lIndexed, lBy) of
        (MissingToken _, MissingToken _) -> Nothing
        _ -> Just (IndexedByNode lIndexed lBy)
  members <- squareBracketList $ commaList parseDomain
  lOf <- want L_of
  domain <- parseDomain
  return $ MatrixDomainNode lMatrix indexByNode members lOf domain

parseSet :: Parser DomainNode
parseSet = do
  lSet <- need L_set
  attributes <- optional parseAttributes
  lOf <- want L_of
  domain <- parseDomain
  return $ SetDomainNode lSet attributes lOf domain

parseMSet :: Parser DomainNode
parseMSet = do
  lMSet <- need L_mset
  attributes <- optional parseAttributes
  lOf <- want L_of
  domain <- parseDomain
  return $ MSetDomainNode lMSet attributes lOf domain

parseFunctionDomain :: Parser DomainNode
parseFunctionDomain = do
  lFunction <- need L_function
  attributes <- optional parseAttributes
  fromDom <- parseDomain
  arrow <- want L_LongArrow
  toDom <- parseDomain
  return $ FunctionDomainNode lFunction attributes fromDom arrow toDom

--   where
--     parseFunctionAttributes :: Parser (ListNode AttributeNode)
--     parseFunctionAttributes = try $ do
--         openB <- want L_OpenParen
--         lst <- commaList1 parseAttribute
--         closeB <- want L_CloseParen
--         return $ ListNode openB lst closeB

parseSequenceDomain :: Parser DomainNode
parseSequenceDomain = do
  lSequence <- need L_sequence
  attributes <- optional parseAttributes
  lOf <- want L_of
  domain <- parseDomain
  return $ SequenceDomainNode lSequence attributes lOf domain

parsePermutationDomain :: Parser DomainNode
parsePermutationDomain = do
  lPermutation <- need L_permutation
  attributes <- optional parseAttributes
  lOf <- want L_of
  domain <- parseDomain
  return $ PermutationDomainNode lPermutation attributes lOf domain

parseRelation :: Parser DomainNode
parseRelation = do
  lRelation <- need L_relation
  attributes <- optional parseAttributes
  lOf <- want L_of
  domains <- parenList $ parseSequence L_Times parseDomain
  return $ RelationDomainNode lRelation attributes lOf domains

parsePartition :: Parser DomainNode
parsePartition = do
  lPartition <- need L_partition
  attributes <- optional $ try parseAttributes
  lFrom <- want L_from
  domain <- parseDomain
  return $ PartitionDomainNode lPartition attributes lFrom domain

parseEnumDomain :: Parser DomainNode
parseEnumDomain = do
  name <- parseIdentifierStrict
  brackets <- optional $ parenListStrict (commaList parseRange)
  return $ RangedEnumNode name brackets

-- (RangedEnumNode name <$> try (parenList (commaList parseRange)))
--     <|> return (EnumDomainNode name)

-- Util
parseNameDomain :: Parser NamedDomainNode
parseNameDomain = do
  name <- parseIdentifier
  lColon <- want L_Colon
  domain <- parseDomain
  let definedDomain = case (lColon, domain) of
        (a, b) | isMissing a && isMissing b -> Nothing
        (a, b) -> Just (a, b)
  return $ NameDomainNode name definedDomain

parseRange :: Parser RangeNode
parseRange = ranged <|> singleR
  where
    ranged = try $ do
      lExpr <- optional $ try parseExpressionStrict
      dots <- need L_DoubleDot
      rExpr <- optional parseExpressionStrict
      case (lExpr, rExpr) of
        (Nothing, Nothing) -> return $ OpenRangeNode dots
        (Just l, Nothing) -> return $ RightUnboundedRangeNode l dots
        (Nothing, Just r) -> return $ LeftUnboundedRangeNode dots r
        (Just l, Just r) -> return $ BoundedRangeNode l dots r
    singleR = SingleRangeNode <$> parseExpressionStrict

parseAttributes :: Parser (ListNode AttributeNode)
parseAttributes = try $ do
  attrs <- parenList (commaList parseAttribute)
  case attrs of
    ListNode _ (Seq xs) _ | not (validInterior xs) -> empty
    _ -> return attrs
  where
    validInterior :: [SeqElem AttributeNode] -> Bool
    validInterior members =
      not
        $ null
          [ x
            | (SeqElem (NamedAttributeNode x _) _) <- members,
              isNonIdentifier x
          ]
    isNonIdentifier :: SToken -> Bool
    isNonIdentifier (StrictToken _ ETok {lexeme = (LIdentifier _)}) = False
    isNonIdentifier _ = True

parseAttribute :: Parser AttributeNode
parseAttribute = do
  name <- parseAttributeLexeme <|> StrictToken [] <$> identifier
  expr <- optional parseExpressionStrict
  return $ NamedAttributeNode name expr

parseMissingDomain :: Parser DomainNode
parseMissingDomain =
  do
    m <- makeMissing (L_Missing MissingDomain)
    return $ MissingDomainNode m
    <?> "Anything"

---------------------------------------
---EXAMPLES AND TESTING            ----
---------------------------------------
example :: String -> IO ()
example s = do
  let str = s
  let txt = T.pack str
  let lexed = runParser eLex "lexer" txt
  case lexed of
    Left peb -> putStrLn "Lexer error:" >> putStrLn (errorBundlePretty peb)
    Right ets -> do
      putStrLn $ "Lexed " ++ show (length ets) ++ " symbols"
      print $ take 100 ets
      putStrLn "reformed"
      -- putTextLn $ reformList ets
      let stream = ETokenStream txt ets
      case runParser parseProgram "parser" stream of
        Left peb -> putStrLn "Parser error: " >> putStrLn (errorBundlePretty peb)
        Right pt -> do
          print $ show pt
          putStrLn "Reforming"
          print $ reformList (flattenSeq pt) == L.fromStrict txt

          putStrLn "Pretty:"
          let pp = renderAST 80 pt
          putStrLn $ T.unpack pp

-- let flat = flatten pt
-- putStrLn $ show $ flat
-- putTextLn $ reformList $ flat

exampleFile :: String -> IO ()
exampleFile p = do
  path <- readFileIfExists p
  case path of
    Nothing -> putStrLn "NO such file"
    Just s -> example s
  return ()
