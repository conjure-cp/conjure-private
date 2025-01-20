{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

-- This module is where the syntax tree is mapped to the model. This is also the
-- stage at which all errrors are reported.
-- This has three main parts
-- Syntax checking:
--      When it comes to missing tokens these should usually be handled by the
--      low level token validation functions, however in some special cases
--      where the tokens are manipulated manually the checks need to be added
-- Type checking:
--      Type check operators and build up the symbol table.
-- Metadata additions:
--      this includeds things like marking tokens for documentation, as well as
--      setting up structural regions such as quantigied expressions

module Conjure.Language.Validator where

import Conjure.Bug (bug)
import Conjure.Language.AST.Reformer (HighLevelTree, flattenSeq, makeTree)
import Conjure.Language.AST.Syntax as S
import Conjure.Language.Attributes
import Conjure.Language.CategoryOf (Category (CatConstant, CatDecision, CatParameter))
import Conjure.Language.Definition hiding (Typed)
import Conjure.Language.Domain
import Conjure.Language.Domain.AddAttributes (allSupportedAttributes)
import Conjure.Language.Expression qualified as D
  ( Expression (Typed),
  )
import Conjure.Language.Expression.Op
  ( Op (..),
    OpAttributeAsConstraint (OpAttributeAsConstraint),
    OpIndexing (OpIndexing),
    OpPowerSet (..),
    OpRelationProj (OpRelationProj),
    OpSlicing (..),
    OpType (..),
    mkBinOp,
    mkOp,
  )
import Conjure.Language.Lexemes
import Conjure.Language.Lexer (ETok (ETok, lexeme), sourcePos0, sourcePosAfter, tokenSourcePos, tokenStartOffset, totalLength, trueLength)
import Conjure.Language.Pretty
import Conjure.Language.Type
import Conjure.Prelude
import Control.Monad (mapAndUnzipM)
import Control.Monad.Writer.Strict (Writer)
import Data.List (splitAt)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Sequence (Seq (..), ViewR (..), viewr)
import Data.Set qualified as S
import Data.Text (append, pack, unpack)
import Data.Text qualified as T
import Text.Megaparsec (SourcePos, mkPos, unPos)
import Text.Megaparsec.Pos (SourcePos (..))

data TagType
  = TtType
  | TtNumber
  | TtBool
  | TtDomain
  | TtEnum
  | TtEnumMember
  | TtRecord
  | TtRecordMember
  | TtUserFunction
  | TtFunction
  | TtAttribute
  | TtAAC
  | TtVariable
  | TtKeyword
  | TtQuantifier
  | TtSubKeyword
  | TtOperator
  | TtLocal
  | TtOther Text
  deriving (Show)

data TaggedToken
  = TaggedToken TagType ETok
  deriving (Show)

class WithRegion a where
  getRegion :: (WithRegion a) => a -> DiagnosticRegion

instance WithRegion (DiagnosticRegion, a) where
  getRegion (r, _) = r

instance WithRegion DiagnosticRegion where
  getRegion = id

instance WithRegion LToken where
  getRegion = symbolRegion

instance WithRegion SToken where
  getRegion = symbolRegion

type RegionTagged a = (DiagnosticRegion, a)

unregion :: RegionTagged a -> a
unregion (_, a) = a

data Typed a = Typed Type a
  deriving (Show)

instance Functor Typed where
  fmap f (Typed k a) = Typed k (f a)

simple :: Type -> Kind
simple = Kind $ ValueType CatConstant

withCat :: Category -> Type -> Kind
withCat = Kind . ValueType

data Kind = Kind Class Type
  deriving (Show, Eq, Ord)

instance Pretty Kind where
  -- pretty (Kind MemberType t) = "Member of " <> pretty t
  pretty (Kind DomainType t) = "domain `" <> pretty t <> "`"
  pretty (Kind (ValueType _) t) = pretty t

data Class = DomainType | ValueType Category
  deriving (Show, Eq, Ord)

instance Pretty Class where
  pretty c = case c of
    DomainType -> "Domain"
    ValueType _ -> "Value"

untype :: Typed a -> a
untype (Typed _ a) = a

typeOf_ :: Typed a -> Type
typeOf_ (Typed t _) = t

untypeAs :: Type -> Typed a -> ValidatorS a
untypeAs r ((Typed t a)) =
  if let ?typeCheckerMode = StronglyTyped in typeUnify r t
    then return a
    else contextTypeError (TypeError r t) >> return a

type TypeCheck a = Typed a -> ValidatorS ()

exactly :: Type -> TypeCheck a
exactly t r = void $ untypeAs t r

typeAs :: Type -> Maybe a -> Maybe (Typed a)
typeAs t (Just a) = Just $ Typed t a
typeAs _ Nothing = Nothing

(?=>) :: ValidatorS (Typed a) -> TypeCheck a -> ValidatorS a
v ?=> t = v >>= (\a -> t a >> return (untype a))

castAny :: Validator a -> Validator (Typed a)
castAny a = typeAs TypeAny <$> a

tInt :: Type
tInt = TypeInt TagInt

typeSplit :: Typed a -> (Type, a)
typeSplit (Typed t v) = (t, v)

getTypeList :: [Typed a] -> [(Type, a)]
getTypeList = map typeSplit

data ErrorType
  = TokenError LToken
  | SyntaxError Text
  | WithReplacements ErrorType [Text]
  | SemanticError Text
  | CustomError Text
  | SkippedTokens
  | MissingArgsError Int Int
  | UnexpectedArg
  | TypeError Type Type -- Expected, got
  | ComplexTypeError Text Type -- Expected, got
  | CategoryError Category Text
  | KindError Class Class
  | InternalError -- Used to explicitly tag invalid pattern matches
  | InternalErrorS Text -- Used for giving detail to bug messages
  deriving (Show, Eq, Ord)

data WarningType
  = UnclassifiedWarning Text
  | AmbiguousTypeWarning
  deriving (Show, Eq, Ord)

data InfoType = UnclassifiedInfo Text deriving (Show, Eq, Ord)

data Diagnostic = Error ErrorType | Warning WarningType | Info InfoType
  deriving (Show, Eq, Ord)

data ValidatorDiagnostic = ValidatorDiagnostic DiagnosticRegion Diagnostic
  deriving (Show)

isError :: ValidatorDiagnostic -> Bool
isError (ValidatorDiagnostic _ (Error _)) = True
isError _ = False

data RegionType
  = Definition Text Kind
  | LiteralDecl Kind
  | Ref Text Kind DiagnosticRegion
  | Structural StructuralType
  | Documentation DocType Text
  deriving (Show)

data DocType = OperatorD | FunctionD | KeywordD | TypeD | AttributeD
  deriving (Show)

data StructuralType
  = SSuchThat
  | SGiven
  | SFind
  | SLetting
  | SWhere
  | SBranching
  | SGoal Text
  | SEnum Text
  | SQuantification Text Kind
  | SComprehension Kind
  | SGuard
  | SGen
  | SBody
  deriving (Show)

data RegionInfo = RegionInfo
  { rRegion :: DiagnosticRegion,
    rSubRegion :: Maybe DiagnosticRegion,
    rRegionType :: RegionType,
    rChildren :: [RegionInfo],
    rTable :: SymbolTable
  }
  deriving (Show)

mkDeclaration :: DiagnosticRegion -> Text -> Kind -> RegionInfo
mkDeclaration r n t = RegionInfo r (Just r) (Definition n t) [] M.empty

mkLiteral :: DiagnosticRegion -> Text -> Typed a -> RegionInfo
mkLiteral r _ (Typed t _) = RegionInfo r (Just r) (LiteralDecl (simple t)) [] M.empty

putReference :: DiagnosticRegion -> Text -> Kind -> DiagnosticRegion -> ValidatorS ()
putReference r n t ref = addRegion (RegionInfo r Nothing (Ref n t ref) [] M.empty)

holdDeclarations :: ValidatorS a -> ValidatorS (a, [RegionInfo])
holdDeclarations f = do
  prev <- gets regionInfo
  modify (\s -> s {regionInfo = []})
  res <- f
  decls <- gets regionInfo
  modify (\s -> s {regionInfo = prev})
  return (res, decls)

wrapRegion :: (HighLevelTree a, HighLevelTree b) => a -> b -> StructuralType -> ValidatorS n -> ValidatorS n
wrapRegion regMain regSel = wrapRegion' (symbolRegion regMain) (symbolRegion regSel)

wrapRegion' :: DiagnosticRegion -> DiagnosticRegion -> StructuralType -> ValidatorS n -> ValidatorS n
wrapRegion' regMain regSel ty f = do
  (res, ds) <- holdDeclarations f
  let rMain = regMain
  let rSel = Just regSel
  st <- gets symbolTable
  let new = RegionInfo rMain rSel (Structural ty) ds st
  unless (null ds) $ addRegion new
  return res

-- injectRegion :: DiagnosticRegion -> DiagnosticRegion -> ()

putDocs :: (HighLevelTree a) => DocType -> Text -> a -> ValidatorS ()
putDocs t nm r = addRegion $ RegionInfo {rRegion = symbolRegion r, rSubRegion = Nothing, rRegionType = Documentation t nm, rChildren = [], rTable = M.empty}

-- Infix symbol validation and tagging
isA :: SToken -> TagType -> ValidatorS ()
isA = flagSToken

isA' :: LToken -> TagType -> ValidatorS ()
isA' a b = validateSymbol a >> flagToken a b

are :: [LToken] -> TagType -> ValidatorS ()
are a b = mapM_ (`isA'` b) a

flagToken :: LToken -> TagType -> ValidatorS ()
flagToken (RealToken s) c = flagSToken s c
flagToken _ _ = return ()

flagSToken :: SToken -> TagType -> ValidatorS ()
flagSToken (StrictToken _ t) c = modify (\x@ValidatorState {symbolCategories = sc} -> x {symbolCategories = M.insert t (TaggedToken c t) sc})

tagWithType :: NameNode -> Kind -> ValidatorS ()
tagWithType (NameNode (NameNodeS lt)) (Kind (ValueType _) ty) = flagSToken lt $ case ty of
  TypeEnum _ -> TtEnum
  TypeInt (TagEnum _) -> TtEnumMember
  TypeInt (TagUnnamed _) -> TtEnumMember
  TypeUnnamed _ -> TtEnum
  TypeVariant _ -> TtRecord
  TypeVariantMember {} -> TtRecordMember
  TypeRecord _ -> TtRecord
  TypeRecordMember {} -> TtRecordMember
  TypeFunction _ _ -> TtFunction
  TypeSequence _ -> TtFunction
  TypeRelation _ -> TtFunction
  _ -> TtVariable
tagWithType (NameNode (NameNodeS lt)) (Kind DomainType (TypeEnum {})) = flagSToken lt TtEnum
tagWithType (NameNode (NameNodeS lt)) (Kind DomainType (TypeRecord {})) = flagSToken lt TtRecord
tagWithType (NameNode (NameNodeS lt)) (Kind DomainType (TypeVariant {})) = flagSToken lt TtRecord
tagWithType (NameNode (NameNodeS lt)) (Kind DomainType _) = flagSToken lt TtDomain
tagWithType _ _ = pure ()

data ValidatorState = ValidatorState
  { typeChecking :: Bool,
    regionInfo :: [RegionInfo],
    symbolTable :: SymbolTable,
    symbolCategories :: Map ETok TaggedToken,
    currentContext :: DiagnosticRegion,
    filePath :: Maybe Text,
    categoryLimit :: (Category, Text) -- Category,Context (e.g domain)
  }
  deriving (Show)

-- instance Default ValidatorState where
--     def = ValidatorState {
--         typeChecking = True,
--         regionInfo=[],
--         symbolCategories=M.empty,
--         symbolTable=M.empty
--         }

initialState :: (HighLevelTree a) => a -> Maybe Text -> ValidatorState
initialState r path =
  ValidatorState
    { typeChecking = True,
      regionInfo = [],
      symbolCategories = M.empty,
      symbolTable = M.empty,
      currentContext = symbolRegion r,
      filePath = path,
      categoryLimit = (CatDecision, "root")
    }

type SymbolTable = (Map Text SymbolTableValue)

type SymbolTableValue = (DiagnosticRegion, Bool, Kind)

-- instance Show SymbolTableValue where
--     show (SType t) = show $ pretty t
--     show (SDomain d) = show $ pretty d
newtype ValidatorT r w a = ValidatorT (StateT r (Writer [w]) a)
  deriving (Monad, Applicative, Functor, MonadState r, MonadWriter [w])

-- synonym wrapped in maybe to allow errors to propagate
type Validator a = ValidatorT ValidatorState ValidatorDiagnostic (Maybe a)

-- Non maybe version used in outward facing applications/ lists
type ValidatorS a = ValidatorT ValidatorState ValidatorDiagnostic a

-- addEnumDefns ::  [Text] -> SymbolTable -> SymbolTable
-- addEnumDefns names (SymbolTable enums) = SymbolTable $ enums ++  map (\m -> (m,"Enum")) names
-- instance  MonadFail (ValidatorT ValidatorState ValidatorDiagnostic a) where
--     fail = return . fallback . T.pack

modifySymbolTable :: (SymbolTable -> SymbolTable) -> ValidatorS ()
modifySymbolTable f = modify (\x -> x {symbolTable = f . symbolTable $ x})

getSymbol :: Text -> ValidatorS (Maybe SymbolTableValue)
getSymbol n = M.lookup n <$> getSymbolTable

putSymbol :: (Name, SymbolTableValue) -> ValidatorS Bool
putSymbol (Name name, t) = do
  x <- getSymbol name
  modifySymbolTable (M.insert name t)
  case x of
    Nothing -> return False
    Just _ -> return True
putSymbol _ = return False -- skip types for meta and machine vars

addRegion :: RegionInfo -> ValidatorS ()
addRegion r = modify (\x -> x {regionInfo = r : regionInfo x})

makeEnumDomain :: Name -> Maybe [Range Expression] -> Domain () Expression
makeEnumDomain n es = DomainEnum n es Nothing

makeUnnamedDomain :: Name -> Domain () ()
makeUnnamedDomain n = DomainUnnamed n ()

getSymbolTable :: ValidatorS SymbolTable
getSymbolTable = symbolTable <$> get

getContext :: ValidatorS DiagnosticRegion
getContext = currentContext <$> get

setContext :: DiagnosticRegion -> ValidatorS ()
setContext r = modify (\p -> p {currentContext = r})

setContextFrom :: (HighLevelTree a) => a -> ValidatorS ()
setContextFrom a = setContext $ symbolRegion a

-- strict :: Validator a -> ValidatorS a
-- strict a = do res <- a; return res

deState :: ((a, r), n) -> (a, n, r)
deState ((a, r), n) = (a, n, r)

runValidator :: ValidatorT r w a -> r -> (a, [w], r)
runValidator (ValidatorT r) d = deState $ runWriter (runStateT r d)

isSyntacticallyValid :: (HighLevelTree a) => (a -> ValidatorS b) -> a -> Bool
isSyntacticallyValid v s = case runValidator (v s) (initialState s Nothing) {typeChecking = False} of
  (_, vds, _) -> not $ any isError vds

todoTypeAny :: Maybe a -> Maybe (Typed a)
todoTypeAny = typeAs TypeAny

setCategoryLimit :: (Category, Text) -> ValidatorS a -> ValidatorS a
setCategoryLimit c f = do
  tmp <- gets categoryLimit
  modify (\s -> s {categoryLimit = c})
  res <- f
  modify (\s -> s {categoryLimit = tmp})
  return res

checkCategory :: Kind -> ValidatorS ()
checkCategory (Kind (ValueType category) _) = do
  (refCat, context) <- gets categoryLimit
  unless (refCat >= category) $ contextTypeError $ CategoryError category context
checkCategory (Kind DomainType _) = return ()

validateModel :: ProgramTree -> ValidatorS Model
validateModel model = do
  langVersion <- validateLanguageVersion $ langVersionInfo model
  sts <- validateProgramTree (statements model)
  return $ Model (fromMaybe def langVersion) sts def

validateProgramTree :: [StatementNode] -> ValidatorS [Statement]
validateProgramTree sts = do
  q <- validateArray validateStatement sts
  return $ concat q

validateLanguageVersion :: Maybe LangVersionNode -> Validator LanguageVersion
validateLanguageVersion Nothing = return $ pure $ LanguageVersion "Essence" [1, 3]
validateLanguageVersion (Just lv@(LangVersionNode l1 n v)) = do
  setContextFrom lv
  l1 `isA` TtKeyword
  name <- validateIdentifier n
  checkLanguageName name
  nums <- catMaybes <$> validateSequence_ getNum v
  return
    . pure
    $ LanguageVersion
      (Name name)
      (if null nums then [1, 3] else nums)
  where
    getNum :: SToken -> Validator Int
    getNum c = do
      c' <- validateSToken c
      case c' of
        (LIntLiteral x) -> return . pure $ fromInteger x
        _ -> invalid $ c <!> InternalError
    checkLanguageName nm
      | T.toLower nm == "essence" = pure ()
      | T.toLower nm == "essence'" = do
          raiseError (symbolRegion lv /!\ UnclassifiedWarning "Essence prime file detected, type checking is off")
          modify (\s -> s {typeChecking = False})
      | otherwise = raiseError $ symbolRegion n <!> SyntaxError "Not a valid language name"

validateStatement :: StatementNode -> ValidatorS [Statement]
validateStatement (DeclarationStatement dsn) = validateDeclarationStatement dsn
validateStatement (BranchingStatement bsn) = validateBranchingStatement bsn
validateStatement (SuchThatStatement stsn) = validateSuchThatStatement stsn
validateStatement (WhereStatement wsn) = validateWhereStatement wsn
validateStatement (ObjectiveStatement osn) = validateObjectiveStatement osn
validateStatement (HeuristicStatement lt exp) = validateHeuristicStatement lt exp
validateStatement (UnexpectedToken lt) = [] <$ invalid (lt <!> TokenError lt) -- TODO address as part of skip token refactor

validateHeuristicStatement :: SToken -> ExpressionNode -> ValidatorS [Statement]
validateHeuristicStatement lt exp = do
  let validHeuristics = ["static", "sdf", "conflict", "srf", "ldf", "wdeg", "domoverwdeg"]
  lt `isA` TtKeyword
  h <- case exp of
    IdentifierNode nn@(NameNodeS (StrictToken _ (ETok {lexeme = (LIdentifier nm)}))) -> do
      if nm `elem` validHeuristics
        then return $ pure [SearchHeuristic (Name nm)]
        else invalid $ symbolRegion nn <!> SemanticError (T.concat ["Invalid heuristic ", nm, " Expected one of: ", pack $ show validHeuristics])
    _ -> invalid $ symbolRegion exp <!> SemanticError "Only identifiers are allowed as heuristics"
  return $ fromMaybe [] h

tCondition :: TypeCheck a
tCondition (Typed TypeAny _) = pure ()
tCondition (Typed TypeBool _) = pure ()
tCondition (Typed (TypeMatrix _ TypeBool) _) = pure ()
tCondition (Typed (TypeList TypeBool) _) = pure ()
tCondition t = contextTypeError $ ComplexTypeError "Bool or [Bool]" $ typeOf_ t

validateWhereStatement :: WhereStatementNode -> ValidatorS [Statement]
validateWhereStatement w@(WhereStatementNode l1 exprs) = wrapRegion w w SWhere $ do
  l1 `isA` TtKeyword
  ws <- Where <$> validateSequence_ (\x -> do setContextFrom x; validateExpression x ?=> tCondition) exprs
  return [ws]

validateObjectiveStatement :: ObjectiveStatementNode -> ValidatorS [Statement]
validateObjectiveStatement o@(ObjectiveMin lt en) = wrapRegion o o (SGoal "Minimising") $ do
  lt `isA` TtKeyword
  exp <- validateExpression en
  return [Objective Minimising $ untype exp]
validateObjectiveStatement o@(ObjectiveMax lt en) = wrapRegion o o (SGoal "Maximising") $ do
  lt `isA` TtKeyword
  exp <- validateExpression en
  return [Objective Maximising $ untype exp]

validateSuchThatStatement :: SuchThatStatementNode -> ValidatorS [Statement]
validateSuchThatStatement s@(SuchThatStatementNode l1 l2 exprs) = wrapRegion s s SSuchThat $ do
  l1 `isA` TtKeyword
  l2 `isA'` TtKeyword
  putDocs KeywordD "such_that" (makeTree l1 `mappend` makeTree l2)
  exprs' <- validateSequence validateExpression exprs
  bools <- mapM (\(a, b) -> do setContext a; return b ?=> tCondition) exprs'
  let bool_exprs = bools
  return [SuchThat bool_exprs]

validateBranchingStatement :: BranchingStatementNode -> ValidatorS [Statement]
validateBranchingStatement b@(BranchingStatementNode l1 l2 sts) = wrapRegion b b SBranching $ do
  l1 `isA` TtKeyword
  l2 `isA'` TtKeyword
  putDocs KeywordD "branching_on" (makeTree l1 `mappend` makeTree l2)
  branchings <- catMaybes <$> validateList_ (f2n validateBranchingParts) sts
  return [SearchOrder branchings]
  where
    validateBranchingParts :: ExpressionNode -> ValidatorS SearchOrder
    validateBranchingParts (IdentifierNode nn) = do
      n <- tagNameAs TtVariable nn
      return $ BranchingOn n
    validateBranchingParts exp = do
      x <- validateExpression exp ?=> exactly TypeAny
      return $ Cut x

validateDeclarationStatement :: DeclarationStatementNode -> ValidatorS [Statement]
validateDeclarationStatement stmt = do
  stmt' <- case stmt of
    FindStatement l1 fs -> l1 `isA` TtKeyword >> putDocs KeywordD "find" l1 >> validateStatementSeq SFind validateFind fs
    GivenStatement l1 gs -> l1 `isA` TtKeyword >> putDocs KeywordD "given" l1 >> validateStatementSeq SGiven validateGiven gs
    LettingStatement l1 ls -> l1 `isA` TtKeyword >> putDocs KeywordD "letting" l1 >> validateStatementSeq SLetting validateLetting ls
  return $ Declaration <$> stmt'
  where
    validateStatementSeq s v l = wrapRegion stmt stmt s $ do
      decls <- validateSequence_ v l
      when (null decls) $ raiseError (symbolRegion stmt <!> SemanticError "Declaration without any members")
      return $ concat decls

validateGiven :: GivenStatementNode -> ValidatorS [Declaration]
validateGiven (GivenStatementNode idents l1 domain) =
  do
    checkSymbols [l1] -- Colon
    names <- validateSequence (validateNameAs TtVariable) idents
    (dType, dom) <- typeSplit <$> validateDomain domain
    let memberType = getDomainMembers dType
    let declarations = [mkDeclaration r n (withCat CatParameter memberType) | (r, Name n) <- names]
    mapM_ addRegion declarations
    mapM_ (\(r, x) -> putSymbol (x, (r, False, withCat CatParameter memberType))) names
    return $ [FindOrGiven Given nm dom | (_, nm) <- names]
validateGiven (GivenEnumNode se l1 l2 l3) =
  do
    [l1, l2, l3] `are` TtKeyword -- new Type enum
    putDocs KeywordD "new_type_enum" [l1, l2, l3]
    names <- validateSequence (validateNameAs TtEnum) se
    let eType = Kind DomainType . TypeEnum
    mapM_ (\(r, x) -> putSymbol (x, (r, True, eType x))) names
    return $ [GivenDomainDefnEnum n | (_, n) <- names]

validateFind :: FindStatementNode -> ValidatorS [Declaration]
validateFind (FindStatementNode names colon domain) = do
  checkSymbols [colon] -- colon
  names' <- validateSequence (validateNameAs TtVariable) names
  (dType, dom) <- typeSplit <$> validateDomain domain
  let memberType = getDomainMembers dType
  mapM_ (\(r, x) -> putSymbol (x, (r, False, withCat CatDecision memberType))) names'
  mapM_ addRegion [mkDeclaration r n (withCat CatDecision memberType) | (r, Name n) <- names']
  return $ [FindOrGiven Find nm dom | (_, nm) <- names']

validateLetting :: LettingStatementNode -> ValidatorS [Declaration]
-- Letting [names] be
validateLetting (LettingStatementNode names l1 assign) = do
  l1 `isA'` TtKeyword -- be
  validateLettingAssignment names assign

validateLettingAssignment :: Sequence NameNode -> LettingAssignmentNode -> ValidatorS [Declaration]
validateLettingAssignment names (LettingExpr en) = do
  expr <- validateExpression en
  setContextFrom en
  names' <- validateSequence (validateNameAs TtVariable) names
  let (t, e) = typeSplit expr
  let declarations = [mkDeclaration r n (simple t) | (r, Name n) <- names']
  mapM_ addRegion declarations
  mapM_ (\(r, x) -> putSymbol (x, (r, False, simple t))) names'
  return $ [Letting n e | (_, n) <- names']
validateLettingAssignment names (LettingDomain lt dn) = do
  lt `isA` TtSubKeyword
  putDocs KeywordD "letting_domain" [lt]
  (tDomain, domain) <- typeSplit <$> validateDomain dn
  names' <- validateSequence (validateNameAs TtDomain) names
  let declarations = [mkDeclaration r n (Kind DomainType tDomain) | (r, Name n) <- names']
  mapM_ addRegion declarations
  mapM_ (\(r, x) -> putSymbol (x, (r, False, Kind DomainType tDomain))) names'
  return $ [Letting n (Domain domain) | (_, n) <- names']
validateLettingAssignment names (LettingEnum l1 l2 l3 enames) = do
  [l1, l2, l3] `are` TtKeyword
  putDocs KeywordD "new_type_enum" [l1, l2, l3]
  names' <- validateSequence (validateNameAs TtEnum) names
  memberNames <- validateList (validateNameAs TtEnumMember) enames
  let members = map snd memberNames
  -- let (members,memberDecls) = unzip . map (\(r,n)->(n,\t->mkDeclaration r n (Kind ValueType (TypeEnum t)))) $ memberNames
  sequence_
    [ wrapRegion' (catRegions [(r, ()), (symbolRegion enames, ())]) r (SEnum n) $ do
        let nameMap = zip memberNames ([1 ..] :: [Int])
        let dType = Kind DomainType $ TypeEnum name
        let tVal = TypeInt $ TagEnum n

        putReference r n dType r
        void $ putSymbol (Name n, (r, True, dType))
        mapM_
          ( \((r', x), _) -> do
              let n' = case x of Name nm -> nm; _ -> ""
              addRegion $ mkDeclaration r' n' (simple $ TypeInt (TagEnum n))
              putSymbol (x, (r, False, simple tVal))
          )
          nameMap
      | (r, name@(Name n)) <- names'
    ]
  return $ [LettingDomainDefnEnum n members | (_, n) <- names']
validateLettingAssignment names (LettingUnnamed l1 l2 l3 l4 szExp) = do
  [l1, l2, l3, l4] `are` TtKeyword -- TODO keywords
  putDocs KeywordD "letting_unnamed" [l1, l2, l3, l4]
  names' <- validateSequence (validateNameAs TtEnum) names
  size <- do
    setContextFrom szExp
    validateExpression szExp ?=> exactly tInt
  let d = Kind DomainType . TypeUnnamed
  mapM_ addRegion [mkDeclaration r n (d $ Name n) | (r, Name n) <- names']
  mapM_ (\(r, x) -> putSymbol (x, (r, False, d x))) names'
  return $ [LettingDomainDefnUnnamed n size | (_, n) <- names']

invalid :: ValidatorDiagnostic -> Validator a
invalid err = do
  raiseError err
  return Nothing

validateSToken :: SToken -> ValidatorS Lexeme
validateSToken (StrictToken ss t) = do
  checkSymbols (map SkippedToken ss)
  return $ lexeme t

validateSymbol :: LToken -> Validator Lexeme
validateSymbol s =
  case s of
    RealToken st -> pure <$> validateSToken st
    _ -> invalid $ ValidatorDiagnostic (getRegion s) $ Error $ TokenError s

-- [MissingTokenError ]
getValueType :: Kind -> ValidatorS Type
getValueType (Kind (ValueType _) t) = pure t
getValueType (Kind k _) = do
  contextTypeError $ KindError (ValueType CatConstant) k
  return TypeAny

getDomainType :: Kind -> ValidatorS Type
getDomainType (Kind DomainType t) = pure t
getDomainType (Kind k _) = do
  contextTypeError $ KindError DomainType k
  return TypeAny

type TypedDomain = Typed (Domain () Expression)

type DomainValidator = Validator TypedDomain

validateDomainWithRepr :: DomainNode -> ValidatorS (Typed (Domain HasRepresentation Expression))
validateDomainWithRepr dom = do
  (t, dom') <- typeSplit <$> validateDomain dom
  return . Typed t $ changeRepr NoRepresentation dom'

validateDomain :: DomainNode -> ValidatorS TypedDomain
validateDomain dm = setCategoryLimit (CatParameter, "Domain") $ case dm of
  ParenDomainNode _ dom rt -> do checkSymbols [rt]; validateDomain dom
  MetaVarDomain lt -> do mv <- validateMetaVar lt; return . Typed TypeAny $ DomainMetaVar mv
  BoolDomainNode lt -> lt `isA` TtType >> (return . Typed TypeBool) DomainBool
  RangedIntDomainNode l1 rs maybe_tag -> do
    l1 `isA` TtType
    validateRangedInt maybe_tag rs
  RangedEnumNode nn ranges -> validateEnumRange nn ranges
  ShortTupleDomainNode lst -> validateTupleDomain lst
  TupleDomainNode l1 doms -> do
    l1 `isA` TtType
    putDocs TypeD "tuple" l1
    validateTupleDomain doms
  RecordDomainNode l1 ndom -> do
    l1 `isA` TtType
    putDocs TypeD "record" l1
    validateRecordDomain ndom
  VariantDomainNode l1 ndom -> do
    l1 `isA` TtType
    putDocs TypeD "variant" l1
    validateVariantDomain ndom
  MatrixDomainNode l1 m_ib idoms l2 dom -> do
    l1 `isA` TtType
    putDocs TypeD "matrix" l1
    l2 `isA'` TtSubKeyword
    validateIndexedByNode m_ib
    validateMatrixDomain idoms dom
  SetDomainNode l1 attrs l2 dom -> do
    l1 `isA` TtType
    putDocs TypeD "set" l1
    l2 `isA'` TtSubKeyword
    validateSetDomain attrs dom
  MSetDomainNode l1 attrs l2 dom -> do
    l1 `isA` TtType
    putDocs TypeD "mset" l1
    l2 `isA'` TtSubKeyword
    validateMSetDomain attrs dom
  FunctionDomainNode l1 attrs dom1 l2 dom2 -> do
    l1 `isA` TtType
    putDocs TypeD "function" l1
    l2 `isA'` TtOperator
    validateFunctionDomain attrs dom1 dom2
  SequenceDomainNode l1 attrs l2 dom -> do
    l1 `isA` TtType
    putDocs TypeD "sequence" l1
    l2 `isA'` TtSubKeyword
    validateSequenceDomain attrs dom
  PermutationDomainNode l1 attrs l2 dom -> do
    l1 `isA` TtType
    putDocs TypeD "permutation" l1
    l2 `isA'` TtSubKeyword
    validatePermutationDomain attrs dom
  RelationDomainNode l1 attrs l2 doms -> do
    l1 `isA` TtType
    putDocs TypeD "relation" l1
    l2 `isA'` TtSubKeyword
    validateRelationDomain attrs doms
  PartitionDomainNode l1 attrs l2 dom -> do
    l1 `isA` TtType
    putDocs TypeD "partition" l1
    l2 `isA'` TtSubKeyword
    validatePartitionDomain attrs dom
  MissingDomainNode lt -> do raiseError $ lt <!> TokenError lt; return $ fallback "Missing Domain"
  where
    validateRangedInt :: Maybe ETok -> Maybe (ListNode RangeNode) -> ValidatorS TypedDomain
    validateRangedInt maybe_tag (Just (ListNode _ (Seq [SeqElem a _]) _)) = do
      d <- case a of
        SingleRangeNode en -> do
          (t, e) <- typeSplit <$> validateExpression en
          case t of
            TypeInt _ -> return $ DomainInt (mkIntTag maybe_tag) [RangeSingle e]
            TypeMatrix _ _ -> return $ DomainIntE (mkIntTag maybe_tag) e
            TypeList _ -> return $ DomainIntE (mkIntTag maybe_tag) e
            TypeSet _ -> return $ DomainIntE (mkIntTag maybe_tag) e
            _ -> DomainIntE (mkIntTag maybe_tag) e <$ raiseTypeError (symbolRegion en <!> ComplexTypeError "Set/List of int or Int" t)
        _ -> do
          r <- validateRange tInt a
          return $ DomainInt (mkIntTag maybe_tag) [r]
      return $ Typed tInt d
    validateRangedInt maybe_tag (Just ranges) = do
      ranges' <- catMaybes <$> validateList_ (f2n (validateRange tInt)) ranges
      return . Typed tInt $ DomainInt (mkIntTag maybe_tag) ranges'
    validateRangedInt maybe_tag Nothing = return . Typed tInt $ DomainInt (mkIntTag maybe_tag) []
    validateEnumRange :: NameNodeS -> Maybe (ListNode RangeNode) -> ValidatorS TypedDomain
    validateEnumRange name@(NameNodeS n) ranges = do
      flagSToken n TtEnum
      setContextFrom name
      name' <- validateIdentifierS name
      _ <- resolveReference (symbolRegion name, Name name')
      a <- getSymbol name'
      case a of
        Just (_, True, t) -> do
          t' <- getDomainType t
          rs <- case ranges of
            Just rs -> pure . catMaybes <$> validateList_ (f2n $ validateRange (getDomainMembers t')) rs
            Nothing -> return Nothing
          return $ Typed t' $ DomainEnum (Name name') rs Nothing
        Just (_, False, t) -> do
          t' <- getDomainType t
          case ranges of
            Nothing -> return . Typed t' $ DomainReference (Name name') Nothing
            Just rs -> do
              void $ validateList_ (f2n (validateRange TypeAny)) rs
              raiseError (symbolRegion name <!> SemanticError "range not supported on non enum ranges")
              return . Typed t' $ DomainReference (Name name') Nothing
        Nothing -> return $ fallback "unknown symbol"

    validateTupleDomain :: ListNode DomainNode -> ValidatorS TypedDomain
    validateTupleDomain doms = do
      (ts, ds) <- unzip . map typeSplit <$> validateList_ validateDomain doms
      return $ Typed (TypeTuple ts) (DomainTuple ds)
    validateRecordDomain :: ListNode NamedDomainNode -> ValidatorS TypedDomain
    validateRecordDomain namedDoms = do
      lst <- validateList (f2n validateNamedDomainInVariant) namedDoms
      let lst' = mapMaybe (\(r, m) -> (\x -> (r, x)) <$> m) lst
      let (ts, ds) = unzip $ map (\(r, (x, typeSplit -> (t, d))) -> ((x, t), (r, (x, d)))) lst'
      -- push members
      let t n = Kind (ValueType CatConstant) $ TypeRecordMember n ts
      mapM_ (\(r, (a, _)) -> putSymbol (a, (r, False, t a))) ds
      return $ Typed (TypeRecord ts) (DomainRecord (unregion <$> ds))
    validateVariantDomain :: ListNode NamedDomainNode -> ValidatorS TypedDomain
    validateVariantDomain namedDoms = do
      lst <- validateList (f2n validateNamedDomainInVariant) namedDoms
      let lst' = mapMaybe (\(r, m) -> (\x -> (r, x)) <$> m) lst
      let (ts, ds) = unzip $ map (\(r, (x, typeSplit -> (t, d))) -> ((x, t), (r, (x, d)))) lst'
      -- push members
      let t n = Kind (ValueType CatConstant) $ TypeVariantMember n ts
      mapM_ (\(r, (a, _)) -> putSymbol (a, (r, False, t a))) ds
      return $ Typed (TypeVariant ts) (DomainVariant (unregion <$> ds))
    validateMatrixDomain :: ListNode DomainNode -> DomainNode -> ValidatorS TypedDomain
    validateMatrixDomain indexes dom = do
      idoms <- validateList_ validateDomain indexes
      dom' <- validateDomain dom
      return $ foldr acc dom' idoms
      where
        acc :: TypedDomain -> TypedDomain -> TypedDomain
        acc (Typed t d) (Typed t' d') = Typed (TypeMatrix t t') (DomainMatrix d d')

    validateSetDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> ValidatorS TypedDomain
    validateSetDomain attrs dom = do
      let repr = ()
      attrs' <- case attrs of
        Just a -> validateSetAttributes a
        Nothing -> return def
      (t, dom') <- typeSplit <$> validateDomain dom
      return . Typed (TypeSet t) $ DomainSet repr attrs' dom'

    validateMSetDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> ValidatorS TypedDomain
    validateMSetDomain attrs dom = do
      let repr = ()
      attrs' <- case attrs of
        Just a -> validateMSetAttributes a
        Nothing -> return def
      (t, dom') <- typeSplit <$> validateDomain dom
      return . Typed (TypeMSet t) $ DomainMSet repr attrs' dom'
    validateFunctionDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> DomainNode -> ValidatorS TypedDomain
    validateFunctionDomain attrs dom1 dom2 = do
      let _repr = Just () -- placeholder if this gets implemented in future
      attrs' <- case attrs of
        Just a -> validateFuncAttributes a
        Nothing -> return def
      (t1, d1) <- typeSplit <$> validateDomain dom1
      (t2, d2) <- typeSplit <$> validateDomain dom2
      let dType = Typed $ TypeFunction t1 t2
      return . dType $ DomainFunction () attrs' d1 d2

    -- attrs <- validateAttributes
    validateSequenceDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> ValidatorS TypedDomain
    validateSequenceDomain attrs dom = do
      let repr = ()
      attrs' <- case attrs of
        Just a -> validateSeqAttributes a
        Nothing -> return def
      (t, dom') <- typeSplit <$> validateDomain dom
      return . Typed (TypeSequence t) $ DomainSequence repr attrs' dom'

    validatePermutationDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> ValidatorS TypedDomain
    validatePermutationDomain attrs dom = do
      let repr = ()
      attrs' <- case attrs of
        Just a -> validatePermutationAttributes a
        Nothing -> return (PermutationAttr SizeAttr_None)
      (t, dom') <- typeSplit <$> validateDomain dom
      return . Typed (TypePermutation t) $ DomainPermutation repr attrs' dom'

    validateRelationDomain :: Maybe (ListNode AttributeNode) -> ListNode DomainNode -> ValidatorS TypedDomain
    validateRelationDomain attrs doms = do
      let repr = ()
      attrs' <- case attrs of
        Just a -> validateRelationAttributes a
        Nothing -> return def

      (ts, doms') <- unzip . map typeSplit <$> validateList_ validateDomain doms
      return . Typed (TypeRelation ts) $ DomainRelation repr attrs' doms'

    validatePartitionDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> ValidatorS TypedDomain
    validatePartitionDomain attrs dom = do
      let repr = ()
      attrs' <- case attrs of
        Just a -> validatePartitionAttributes a
        Nothing -> return def
      (t, dom') <- typeSplit <$> validateDomain dom
      return . Typed (TypePartition t) $ DomainPartition repr attrs' dom'

mkIntTag :: Maybe ETok -> IntTag
mkIntTag Nothing = TagInt
mkIntTag (Just t) =
  case lexeme t of
    LIdentifier i -> TaggedInt i
    _ -> bug $ "mkIntTag" <+> pretty (show t)

validateIndexedByNode :: Maybe IndexedByNode -> ValidatorS ()
validateIndexedByNode Nothing = return ()
validateIndexedByNode (Just (IndexedByNode a b)) = [a, b] `are` TtSubKeyword

todo :: Text -> Validator a
todo s = invalid $ ValidatorDiagnostic global $ Error $ InternalErrorS (append "Not Implemented: " s)

validateSizeAttributes :: [(Lexeme, Maybe Expression)] -> ValidatorS (SizeAttr Expression)
validateSizeAttributes attrs = do
  let sizeAttrs = [L_size, L_minSize, L_maxSize]
  let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
  case filtered of
    [] -> return SizeAttr_None
    [(L_size, Just a)] -> return $ SizeAttr_Size a
    [(L_minSize, Just a)] -> return (SizeAttr_MinSize a)
    [(L_maxSize, Just a)] -> return (SizeAttr_MaxSize a)
    [(L_minSize, Just a), (L_maxSize, Just b)] -> return (SizeAttr_MinMaxSize a b)
    as -> return . def <* contextError $ SemanticError $ pack $ "Incompatible attributes size:" ++ show as

validatePermAttributes :: [(Lexeme, Maybe Expression)] -> ValidatorS (SizeAttr Expression)
validatePermAttributes attrs = do
  let permAttrNames = [L_numMoved, L_minNumMoved, L_maxNumMoved]
  let filtered = sort $ filter (\x -> fst x `elem` permAttrNames) attrs
  case filtered of
    [] -> return SizeAttr_None
    [(L_numMoved, Just a)] -> return $ SizeAttr_Size a
    [(L_minNumMoved, Just a)] -> return (SizeAttr_MinSize a)
    [(L_maxNumMoved, Just a)] -> return (SizeAttr_MaxSize a)
    [(L_minNumMoved, Just a), (L_maxNumMoved, Just b)] -> return (SizeAttr_MinMaxSize a b)
    as -> return . def <* contextError $ SemanticError $ pack $ "Incompatible attributes numMoved:" ++ show as

validatePartSizeAttributes :: [(Lexeme, Maybe Expression)] -> ValidatorS (SizeAttr Expression)
validatePartSizeAttributes attrs = do
  let sizeAttrs = [L_partSize, L_minPartSize, L_maxPartSize]
  let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
  case filtered of
    [] -> return SizeAttr_None
    [(L_partSize, Just a)] -> return (SizeAttr_Size a)
    [(L_minPartSize, Just a)] -> return (SizeAttr_MinSize a)
    [(L_maxPartSize, Just a)] -> return (SizeAttr_MaxSize a)
    [(L_minPartSize, Just a), (L_maxPartSize, Just b)] -> return (SizeAttr_MinMaxSize a b)
    as -> return . def <* contextError $ SemanticError $ pack $ "Incompatible attributes partitionSize :" ++ show as

validateNumPartAttributes :: [(Lexeme, Maybe Expression)] -> ValidatorS (SizeAttr Expression)
validateNumPartAttributes attrs = do
  let sizeAttrs = [L_numParts, L_maxNumParts, L_minNumParts]
  let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
  case filtered of
    [] -> return SizeAttr_None
    [(L_numParts, Just a)] -> return (SizeAttr_Size a)
    [(L_minNumParts, Just a)] -> return (SizeAttr_MinSize a)
    [(L_maxNumParts, Just a)] -> return (SizeAttr_MaxSize a)
    [(L_minNumParts, Just a), (L_maxNumParts, Just b)] -> return (SizeAttr_MinMaxSize a b)
    as -> return . def <* contextError $ SemanticError $ pack $ "Incompatible attributes partitionSize :" ++ show as

validateJectivityAttributes :: [(Lexeme, Maybe Expression)] -> ValidatorS JectivityAttr
validateJectivityAttributes attrs = do
  let sizeAttrs = [L_injective, L_surjective, L_bijective]
  let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
  case filtered of
    [] -> return JectivityAttr_None
    [(L_injective, _)] -> return JectivityAttr_Injective
    [(L_surjective, _)] -> return JectivityAttr_Surjective
    [(L_bijective, _)] -> return JectivityAttr_Bijective
    [(L_injective, _), (L_surjective, _)] -> do
      contextInfo $ UnclassifiedInfo "Inj and Sur can be combined to bijective"
      return JectivityAttr_Bijective
    as -> do
      void . contextError $ SemanticError $ pack $ "Incompatible attributes jectivity" ++ show as
      return def

validateSetAttributes :: ListNode AttributeNode -> ValidatorS (SetAttr Expression)
validateSetAttributes atts = do
  setContextFrom atts
  attrs <- catMaybes <$> validateList_ (validateAttributeNode setValidAttrs) atts
  size <- validateSizeAttributes attrs
  return $ SetAttr size

validateMSetAttributes :: ListNode AttributeNode -> ValidatorS (MSetAttr Expression)
validateMSetAttributes atts = do
  setContextFrom atts
  attrs <- catMaybes <$> validateList_ (validateAttributeNode msetValidAttrs) atts
  size <- validateSizeAttributes attrs
  occurs <- validateOccursAttrs attrs
  return $ MSetAttr size occurs
  where
    validateOccursAttrs attrs = do
      let sizeAttrs = [L_minOccur, L_maxOccur]
      let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
      case filtered of
        [] -> return OccurAttr_None
        [(L_minOccur, Just a)] -> return (OccurAttr_MinOccur a)
        [(L_maxOccur, Just a)] -> return (OccurAttr_MaxOccur a)
        [(L_minOccur, Just a), (L_maxOccur, Just b)] -> return (OccurAttr_MinMaxOccur a b)
        as -> do void . contextError $ SemanticError $ pack $ "Bad args to occurs" ++ show as; return def

validateFuncAttributes :: ListNode AttributeNode -> ValidatorS (FunctionAttr Expression)
validateFuncAttributes atts = do
  attrs <- catMaybes <$> validateList_ (validateAttributeNode funAttrs) atts
  size <- validateSizeAttributes attrs
  let parts = if L_total `elem` map fst attrs then PartialityAttr_Total else PartialityAttr_Partial
  jectivity <- validateJectivityAttributes attrs
  return (FunctionAttr size parts jectivity)

validateSeqAttributes :: ListNode AttributeNode -> ValidatorS (SequenceAttr Expression)
validateSeqAttributes atts = do
  attrs <- catMaybes <$> validateList_ (validateAttributeNode seqAttrs) atts
  size <- validateSizeAttributes attrs
  jectivity <- validateJectivityAttributes attrs
  return $ SequenceAttr size jectivity

validatePermutationAttributes :: ListNode AttributeNode -> ValidatorS (PermutationAttr Expression)
validatePermutationAttributes atts = do
  attrs <- catMaybes <$> validateList_ (validateAttributeNode permValidAttrs) atts
  size <- validatePermAttributes attrs
  return $ PermutationAttr size

validateRelationAttributes :: ListNode AttributeNode -> ValidatorS (RelationAttr Expression)
validateRelationAttributes atts = do
  setContextFrom atts
  attrs <- catMaybes <$> validateList_ (validateAttributeNode relAttrs) atts
  size <- validateSizeAttributes attrs
  others <- catMaybes <$> validateArray validateBinaryRel (filter (\x -> fst x `elem` map fst binRelAttrs) attrs)
  return $ RelationAttr size (BinaryRelationAttrs $ S.fromList others)
  where
    validateBinaryRel :: (Lexeme, Maybe Expression) -> Validator BinaryRelationAttr
    validateBinaryRel (l, _) = case lexemeToBinRel l of
      Just b -> return . pure $ b
      Nothing -> contextError $ InternalErrorS $ pack $ "Not found (bin rel) " ++ show l

validatePartitionAttributes :: ListNode AttributeNode -> ValidatorS (PartitionAttr Expression)
validatePartitionAttributes atts = do
  attrs <- catMaybes <$> validateList_ (validateAttributeNode partitionAttrs) atts
  -- guard size attrs and complete as this is default
  size <- validateNumPartAttributes attrs
  partSize <- validatePartSizeAttributes attrs
  let regular = L_regular `elem` map fst attrs
  return $ PartitionAttr size partSize regular

validateAttributeNode :: Map Lexeme Bool -> AttributeNode -> Validator (Lexeme, Maybe Expression)
validateAttributeNode vs (NamedAttributeNode t Nothing) = do
  flagSToken t TtAttribute
  name <- validateSToken t
  putDocs AttributeD (T.pack $ show name) t
  case M.lookup name vs of
    Nothing -> invalid $ t <!> CustomError "Not a valid attribute in this context"
    Just True -> invalid $ t <!> CustomError "Argument required"
    Just False -> return . pure $ (name, Nothing)
validateAttributeNode vs (NamedAttributeNode t (Just e)) = do
  flagSToken t TtAttribute
  setContextFrom e
  expr <- validateExpression e ?=> exactly tInt
  name <- validateSToken t
  putDocs AttributeD (T.pack $ show name) t
  case M.lookup name vs of
    Nothing -> invalid $ t <!> CustomError "Not a valid attribute in this context"
    Just False -> invalid $ t <!> SemanticError "attribute %name% does not take an argument"
    Just True -> return . pure $ (\x -> (name, Just x)) expr

validateNamedDomainInVariant :: NamedDomainNode -> ValidatorS (Name, TypedDomain)
validateNamedDomainInVariant (NameDomainNode name m_dom) = do
  name' <- validateNameAs TtRecordMember name
  domain' <- case m_dom of
    Nothing -> return . Typed tInt $ DomainInt TagInt [RangeSingle 0]
    Just (l, d) -> do l `isA'` TtOperator; validateDomain d
  return (name', domain')

validateNamedDomainInRecord :: NamedDomainNode -> ValidatorS (Name, TypedDomain)
validateNamedDomainInRecord (NameDomainNode name m_dom) = do
  name' <- validateNameAs TtRecordMember name
  domain' <- case m_dom of
    Just (l, d) -> l `isA'` TtOperator >> validateDomain d
    Nothing -> do
      raiseError $ symbolRegion name <!> SemanticError "Dataless not allowed in record"
      return (fallback "Dataless RecordMemeber")
  return (name', domain')

validateRange :: Type -> RangeNode -> ValidatorS (Range Expression)
validateRange t range = case range of
  SingleRangeNode en -> do ex <- validateExpression en ?=> exactly t; return $ RangeSingle ex
  OpenRangeNode dots -> do dots `isA` TtOther "Ellips"; return RangeOpen
  RightUnboundedRangeNode e1 dots -> do dots `isA` TtOther "Ellips"; ex <- validateExpression e1 ?=> exactly t; return $ RangeLowerBounded ex
  LeftUnboundedRangeNode dots e1 -> do dots `isA` TtOther "Ellips"; ex <- validateExpression e1 ?=> exactly t; return $ RangeUpperBounded ex
  BoundedRangeNode e1 dots e2 -> do
    dots `isA` TtOther "Ellips"
    e1' <- validateExpression e1 ?=> exactly t
    e2' <- validateExpression e2 ?=> exactly t
    return $ RangeBounded e1' e2'

validateArrowPair :: ArrowPairNode -> Validator (RegionTagged (Typed Expression), RegionTagged (Typed Expression))
validateArrowPair (ArrowPairNode e1 s e2) = do
  s `isA'` TtOperator
  e1' <- validateExpression e1
  e2' <- validateExpression e2
  return . pure $ (\a b -> ((symbolRegion e1, a), (symbolRegion e2, b))) e1' e2'

validateExpression :: ExpressionNode -> ValidatorS (Typed Expression)
validateExpression expr = do
  setContextFrom expr
  res <- case expr of
    Literal ln -> validateLiteral ln
    IdentifierNode nn -> validateIdentifierExpr nn
    MetaVarExpr tok -> do
      x <- validateMetaVar tok
      return $ Typed TypeAny $ ExpressionMetaVar x
    QuantificationExpr qen -> validateQuantificationExpression qen
    OperatorExpressionNode oen -> validateOperatorExpression oen
    DomainExpression dex -> validateDomainExpression dex
    ParenExpression (ParenExpressionNode l1 exp l2) -> checkSymbols [l1, l2] >> validateExpression exp
    AbsExpression (ParenExpressionNode l1 exp l2) -> do
      [l1, l2] `are` TtOperator
      (kExp, exp') <- validateFlexibleExpression exp
      typeCheckAbs kExp
      return . Typed tInt $ mkOp TwoBarOp [exp']
    FunctionalApplicationNode lt ln -> validateFunctionApplication lt ln
    AttributeAsConstriant lt exprs -> validateAttributeAsConstraint lt exprs
    SpecialCase scn -> validateSpecialCase scn
    MissingExpressionNode lt -> do raiseError (lt <!> TokenError lt); return (fallback "Missing expression")
  setContextFrom expr
  return res
  where
    typeCheckAbs :: Kind -> ValidatorS ()
    typeCheckAbs (Kind DomainType _) = pure ()
    typeCheckAbs (Kind ValueType {} t) = case t of
      TypeAny -> return ()
      TypeInt _ -> return ()
      TypeList {} -> return ()
      TypeSet {} -> return ()
      TypeMSet {} -> return ()
      TypeFunction {} -> return ()
      TypeSequence {} -> return ()
      TypePermutation {} -> return ()
      TypeRelation {} -> return ()
      TypePartition {} -> return ()
      _ -> contextTypeError $ ComplexTypeError "integer or collection" t

validateFlexibleExpression :: ExpressionNode -> ValidatorS (Kind, Expression)
validateFlexibleExpression (IdentifierNode name) = do
  n <- validateIdentifierS name
  setContextFrom name
  t <- resolveReference (symbolRegion name, Name n)
  tagWithType (NameNode name) t
  return (t, Reference (Name n) Nothing)
validateFlexibleExpression (DomainExpression den) = do
  (dType, d) <- typeSplit <$> validateDomainExpression den
  return (Kind DomainType dType, d)
validateFlexibleExpression en = do
  (t, expr) <- typeSplit <$> validateExpression en
  return (simple t, expr)

validateAttributeAsConstraint :: SToken -> ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateAttributeAsConstraint l1 exprs = do
  es <- map untype <$> validateList_ validateExpression exprs
  do
    flagSToken l1 TtAAC
    lx <- validateSToken l1
    let n = lookup (Name (lexemeText lx)) allSupportedAttributes
    r <- case (n, es) of
      (Just 1, [e, v]) -> return . pure . Typed TypeBool $ aacBuilder e lx (Just v)
      (Just 1, _) -> invalid $ l1 <!> SemanticError (pack $ "Expected 2 args to " ++ show lx ++ "got" ++ show (length es))
      (Just 0, [e]) -> return . pure . Typed TypeBool $ aacBuilder e lx Nothing
      (Just 0, _) -> invalid $ l1 <!> SemanticError (pack $ "Expected 1 arg to " ++ show lx ++ "got" ++ show (length es))
      (_, _) -> invalid $ l1 <!> InternalErrorS "Bad AAC"
    return $ fromMaybe (fallback "bad AAC") r
  where
    aacBuilder e lx y = Op $ MkOpAttributeAsConstraint $ OpAttributeAsConstraint e (fromString (lexemeFace lx)) y

validateSpecialCase :: SpecialCaseNode -> ValidatorS (Typed Expression)
validateSpecialCase (ExprWithDecls l1 ex l2 sts l3) = do
  mapM_ validateSToken [l1, l2, l3]
  scoped $ do
    conds <- validateProgramTree sts
    let decls =
          [ Declaration (FindOrGiven LocalFind nm dom)
            | Declaration (FindOrGiven Find nm dom) <- conds
          ]
    let cons =
          concat
            [ xs
              | SuchThat xs <- conds
            ]
    let locals =
          if null decls
            then DefinednessConstraints cons
            else AuxiliaryVars (decls ++ [SuchThat cons])
    (t, expr) <- typeSplit <$> validateExpression ex
    return . Typed t $ WithLocals expr locals

translateQnName :: Lexeme -> OpType
translateQnName qnName = case qnName of
  L_ForAll -> FunctionOp L_fAnd
  L_Exists -> FunctionOp L_fOr
  L_Sum -> FunctionOp L_Sum
  L_Product -> FunctionOp L_Product
  _ -> FunctionOp qnName

validateQuantificationExpression :: QuantificationExpressionNode -> ValidatorS (Typed Expression)
validateQuantificationExpression q@(QuantificationExpressionNode name pats over m_guard dot expr) =
  do
    setContextFrom q
    dot `isA'` TtKeyword
    scoped $ do
      flagSToken name TtQuantifier
      name' <- validateSToken name
      (over', genDec) <- holdDeclarations $ wrapRegion pats pats SGen $ validateQuantificationOver pats over
      (g', gDec) <- case m_guard of
        Nothing -> return ([], [])
        Just qg ->
          holdDeclarations
            $ wrapRegion qg qg SGuard
            $ validateQuantificationGuard m_guard
      setContextFrom expr
      let (iType, rType) = case name' of
            L_ForAll -> (tCondition, TypeBool)
            L_Exists -> (tCondition, TypeBool)
            L_Sum -> (exactly tInt, tInt)
            L_Product -> (exactly tInt, tInt)
            _ -> bug $ pretty ("Unkown quantifier " ++ show name')
      (body, bDecl) <-
        holdDeclarations
          $ wrapRegion expr expr SBody
          $ validateExpression expr
          ?=> iType
      let qBody = Comprehension body (over' ++ g')
      let result = Typed rType (mkOp (translateQnName name') [qBody])
      putDocs KeywordD (T.pack $ show name') name
      wrapRegion q q (SQuantification (lexemeText name') (simple rType)) (mapM_ addRegion (gDec ++ genDec ++ bDecl))
      return result
  where
    validateQuantificationGuard :: Maybe QuanticationGuard -> ValidatorS [GeneratorOrCondition]
    validateQuantificationGuard Nothing = return []
    validateQuantificationGuard (Just (QuanticationGuard l1 exp)) = do
      l1 `isA` TtOther "Comma"
      expr' <- validateExpression exp ?=> exactly TypeBool
      return [Condition expr']
    validateQuantificationOver :: Sequence AbstractPatternNode -> QuantificationOverNode -> ValidatorS [GeneratorOrCondition]
    validateQuantificationOver lpats (QuantifiedSubsetOfNode lt en) = do
      lt `isA` TtKeyword
      putDocs KeywordD "powerset_projection" lt
      ps <- sequenceElems lpats
      exp <- validateExpression en
      let (t, e) = typeSplit exp
      void $ unifyTypesFailing (TypeSet TypeAny) (symbolRegion en, exp)
      let pt = t
      apats <- unifyPatterns pt ps
      return [Generator $ GenInExpr pat (Op $ MkOpPowerSet $ OpPowerSet e) | pat <- apats]
    -- x in exp
    validateQuantificationOver lpats (QuantifiedMemberOfNode lt en) = do
      lt `isA` TtKeyword
      ps <- sequenceElems lpats
      exp <- validateExpression en
      let (t, e) = typeSplit exp
      pt <- projectionType (symbolRegion en) t
      apats <- unifyPatterns pt ps
      return [Generator $ GenInExpr pat e | pat <- apats]
    -- x : domain
    validateQuantificationOver lpats (QuantifiedDomainNode (OverDomainNode l1 dom)) = do
      l1 `isA'` TtOther "Colon in comprehension"
      ps <- sequenceElems lpats
      (dType, dom') <- typeSplit <$> validateDomain dom
      pt <- projectionTypeDomain (symbolRegion dom) dType
      apats <- unifyPatterns pt ps
      return [Generator $ GenDomainNoRepr pat dom' | pat <- apats]

validateMetaVar :: SToken -> ValidatorS String
validateMetaVar tok = do
  lx <- validateSToken tok
  case lx of
    LMetaVar s -> return $ unpack s
    _ -> bug $ "Bad MetaVar" <+> pretty (show lx)

validateDomainExpression :: DomainExpressionNode -> ValidatorS (Typed Expression)
validateDomainExpression (DomainExpressionNode l1 dom l2) = do
  [l1, l2] `are` TtOther "Backtick"
  (tdom, dom') <- typeSplit <$> validateDomain dom
  return . Typed tdom $ Domain dom'

validateFunctionApplication :: SToken -> ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateFunctionApplication name args = do
  args' <- validateList validateFlexibleExpression args
  flagSToken name TtFunction
  name' <- validateSToken name
  putDocs FunctionD (lexemeText name') name
  setContextFrom args
  validateFuncOp name' args'

validateIdentifierExpr :: NameNodeS -> ValidatorS (Typed Expression)
validateIdentifierExpr name = do
  n <- validateIdentifierS name
  setContextFrom name
  t <- resolveReference (symbolRegion name, Name n)
  checkCategory t
  tagWithType (NameNode name) t
  t' <- getValueType t
  return . Typed t' $ Reference (Name n) Nothing

validateOperatorExpression :: OperatorExpressionNode -> ValidatorS (Typed Expression)
validateOperatorExpression (PrefixOpNode lt expr) = do
  flagSToken lt TtOperator
  op <- validateSToken lt
  setContextFrom expr
  let refT = case op of
        L_Minus -> tInt
        L_ExclamationMark -> TypeBool
        _ -> bug . pretty $ "Unknown prefix op " ++ show op
  putDocs OperatorD (T.pack $ "pre_" ++ show op) lt
  expr' <- validateExpression expr ?=> exactly refT
  return . Typed refT $ mkOp (PrefixOp op) [expr']
-- lookup symbol
validateOperatorExpression (BinaryOpNode lexp op rexp) = do
  (lType, lExpr) <- validateFlexibleExpression lexp
  (rType, rExpr) <- validateFlexibleExpression rexp
  flagSToken op TtOperator
  op' <- validateSToken op

  let resultValidator = binOpType op'
  resultType <- resultValidator (symbolRegion lexp, lType) (symbolRegion rexp, rType)
  addRegion
    ( RegionInfo
        { rRegion = symbolRegion op,
          rSubRegion = Nothing,
          rRegionType = Documentation OperatorD (T.pack $ show op'),
          rChildren = [],
          rTable = M.empty
        }
    )
  return . Typed resultType $ mkBinOp (pack $ lexemeFace op') lExpr rExpr
validateOperatorExpression (PostfixOpNode expr pon) = do
  postFixOp <- validatePostfixOp pon
  postFixOp expr

validatePostfixOp :: PostfixOpNode -> ValidatorS (ExpressionNode -> ValidatorS (Typed Expression))
validatePostfixOp (OpFactorial lt) = do
  lt `isA` TtOperator
  putDocs OperatorD "post_factorial" lt
  setContextFrom lt
  return $ \exp -> do
    v <- validateExpression exp ?=> exactly tInt
    return $ Typed tInt $ mkOp FactorialOp [v]
validatePostfixOp (ApplicationNode args) = return $ \exp -> do
  let reg = symbolRegion exp
  (t, e) <- typeSplit <$> validateExpression exp
  case t of
    TypeFunction _ _ -> do
      args' <- validateList (validateExpression >=> \(Typed t' e') -> return (simple t', e')) args
      validateFuncOp L_image ((reg, (simple t, e)) : args')
    TypePermutation _ -> do
      args' <- validateList (validateExpression >=> \(Typed t' e') -> return (simple t', e')) args
      validateFuncOp L_image ((reg, (simple t, e)) : args')
    TypeSequence _ -> do
      args' <- validateList (validateExpression >=> \(Typed t' e') -> return (simple t', e')) args
      validateFuncOp L_image ((reg, (simple t, e)) : args')
    _ -> do
      as <- catMaybes <$> listElems args
      args' <- mapM validateProjectionArgs as
      let ys = args' -- [if underscore == v then Nothing else Just (r,Typed t v)| x@(r,(Kind ValueType t,v)) <- args']
      iType <- case t of
        TypeRelation ts -> checkProjectionArgs ts ys
        _ -> do
          raiseTypeError $ symbolRegion exp <!> ComplexTypeError "function, permutation or relation" t
          let ts = map (maybe TypeAny (typeOf_ . snd)) ys
          return $ TypeRelation ts
      let op = Op $ MkOpRelationProj $ OpRelationProj e (map (untype . snd <$>) ys)
      let resType = if any null ys then iType else TypeBool
      return . Typed resType $ op
  where
    validateProjectionArgs :: ExpressionNode -> ValidatorS (Maybe (RegionTagged (Typed Expression)))
    validateProjectionArgs (IdentifierNode (NameNodeS ((StrictToken _ (lexeme -> l))))) | l == LIdentifier "_" = return Nothing
    validateProjectionArgs e = validateExpression e >>= \x -> return . pure $ (symbolRegion e, x)

    checkProjectionArgs :: [Type] -> [Maybe (RegionTagged (Typed Expression))] -> ValidatorS Type
    checkProjectionArgs ref bind = do
      unless (length ref == length bind)
        $ raiseError
        $ symbolRegion args
        <!> SemanticError "Member size mismatch for relation"
      let pairs = zip ref bind
      let (free, bound) = partition (null . snd) pairs
      mapM_
        ( \(t, x) -> case x of
            Nothing -> pure ()
            Just v -> void $ unifyTypes t v
        )
        bound
      let freeTypes = map fst free
      return $ if null freeTypes then TypeBool else TypeRelation freeTypes
validatePostfixOp (IndexedNode ln) = do
  res <- catMaybes <$> listElems ln
  return $ \exp -> do
    e <- validateExpression exp
    foldM validateIndexingOrSlicing e res
validatePostfixOp (ExplicitDomain l1 l2 dom l3) = do
  l1 `isA` TtOther "Colon in expr"
  l2 `isA` TtOther "BackTick"
  l3 `isA'` TtOther "BackTick"
  (getDomainMembers -> t, _) <- typeSplit <$> validateDomain dom
  return $ \exp -> do
    e <- validateExpression exp ?=> exactly t
    return . Typed t $ D.Typed e t

validateIndexingOrSlicing :: Typed Expression -> RangeNode -> ValidatorS (Typed Expression)
validateIndexingOrSlicing (Typed t exp) (SingleRangeNode r) = do
  setContextFrom r
  (vType, e) <- case t of
    TypeRecord ts -> validateRecordMemberIndex ts r
    TypeVariant ts -> validateRecordMemberIndex ts r
    _ -> do
      t' <- getIndexingType t
      e <- validateExpression r ?=> exactly t'
      setContextFrom r
      vType <- getIndexedType t (Typed t' e)
      return (vType, e)
  return . Typed vType $ Op $ MkOpIndexing (OpIndexing exp e)
validateIndexingOrSlicing exp range = do
  let (mType, m) = typeSplit exp
  setContextFrom range
  sType <- getSlicingType mType
  r' <- validateRange sType range
  let (i, j) = case r' of
        RangeOpen -> (Nothing, Nothing)
        RangeLowerBounded ex -> (Just ex, Nothing)
        RangeUpperBounded ex -> (Nothing, Just ex)
        RangeBounded exl exr -> (Just exl, Just exr)
        RangeSingle ex -> (Just ex, Just ex) -- This never gets hit in a well formed program
  return $ Typed mType $ Op $ MkOpSlicing (OpSlicing m i j)

validateRecordMemberIndex :: [(Name, Type)] -> ExpressionNode -> ValidatorS (Type, Expression)
validateRecordMemberIndex ns (IdentifierNode nn) = do
  n <- tagNameAs TtRecordMember nn
  let t = lookup n ns
  ty <- case t of
    Just ty -> return ty
    Nothing -> do
      raiseError
        $ symbolRegion nn
        <!> WithReplacements
          (SemanticError "Expected member of record/variant ")
          [x | (Name x, _) <- ns]
      return TypeAny
  return (ty, Reference n Nothing)
validateRecordMemberIndex ns (MissingExpressionNode nn) = do
  raiseError
    $ symbolRegion nn
    <!> WithReplacements
      (SemanticError "Expected member of record/variant ")
      [x | (Name x, _) <- ns]
  return (TypeAny, fallback "bad Index")
validateRecordMemberIndex ns en = do
  g <- validateExpression en
  let msg =
        T.concat
          [ "Expected one of ",
            T.intercalate "," [x | (Name x, _) <- ns],
            " "
          ]
  raiseTypeError $ symbolRegion en <!> ComplexTypeError msg (typeOf_ g)
  return (TypeAny, untype g)

getSlicingType :: Type -> ValidatorS Type
getSlicingType TypeAny = return TypeAny
getSlicingType (TypeMatrix i _) = return i
getSlicingType (TypeSequence _) = return tInt
getSlicingType t = do
  contextTypeError (CustomError . pack $ "Type " ++ show (pretty t) ++ " does not support slicing")
  return TypeAny

getIndexingType :: Type -> ValidatorS Type
getIndexingType TypeAny = return TypeAny
getIndexingType (TypeMatrix i _) = return $ getDomainMembers i
getIndexingType (TypeSequence _) = return tInt
getIndexingType (TypeList _) = return tInt
getIndexingType (TypeTuple _) = return tInt
getIndexingType t = do
  contextTypeError (CustomError . pack $ "Type " ++ show (pretty t) ++ " does not support indexing")
  return TypeAny

getIndexedType :: Type -> Typed Expression -> ValidatorS Type
getIndexedType (TypeMatrix _ ms) _ = return ms
getIndexedType (TypeSequence t) _ = return t
getIndexedType (TypeTuple ts) ex = case intOut "Index" (untype ex) of
  Left _ -> do
    contextTypeError (CustomError "Non constant value indexing tuple")
    return TypeAny
  Right v | v <= 0 || v > toInteger (length ts) -> do
    contextTypeError . CustomError . pack $ "Tuple index " ++ show v ++ " out of bounds"
    return TypeAny
  Right v -> return $ ts `at` (fromInteger v - 1)
getIndexedType (TypeRecord _) (Typed _ _) = bug "Index type called on record, should be handled by special case"
getIndexedType (TypeVariant _) _ = bug "Index type called on variant, should be handled by special case"
getIndexedType _ _ = return TypeAny

validateLiteral :: LiteralNode -> ValidatorS (Typed Expression)
validateLiteral litNode = case litNode of
  IntLiteral lt maybe_tag -> validateIntLiteral lt maybe_tag >>= \x -> return $ Typed tInt $ Constant x
  BoolLiteral lt -> validateBoolLiteral lt >>= \x -> return $ Typed TypeBool $ Constant x
  MatrixLiteral mln -> validateMatrixLiteral mln
  TupleLiteralNode (LongTuple lt xs) -> do
    lt `isA` TtType
    validateLiteral (TupleLiteralNodeShort (ShortTuple xs))
  TupleLiteralNodeShort (ShortTuple xs) -> do
    es <- validateExprList_ xs
    makeTupleLiteral es
  RecordLiteral lt ln -> lt `isA` TtType >> validateRecordLiteral ln
  VariantLiteral lt ln -> lt `isA` TtType >> validateVariantLiteral ln
  SetLiteral ls -> validateSetLiteral ls
  MSetLiteral lt ls -> lt `isA` TtType >> validateMSetLiteral ls
  FunctionLiteral lt ln -> lt `isA` TtType >> validateFunctionLiteral ln
  SequenceLiteral lt ln -> lt `isA` TtType >> validateSequenceLiteral ln
  PermutationLiteral lt ln -> lt `isA` TtType >> validatePermutationLiteral ln
  RelationLiteral lt ln -> lt `isA` TtType >> validateRelationLiteral ln
  PartitionLiteral lt ln -> lt `isA` TtType >> validatePartitionLiteral ln

validateSequenceLiteral :: ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateSequenceLiteral x = do
  (t, ss) <- typeSplit <$> (sameType =<< validateExprList x)
  let lType = TypeSequence t
  return . Typed lType $ mkAbstractLiteral $ AbsLitSequence ss

validatePermutationLiteral :: ListNode PermutationElemNode -> ValidatorS (Typed Expression)
validatePermutationLiteral x = do
  members <- validateList validatePermutationElem x
  (t, xs) <- typeSplit <$> sameType members
  let eType = TypePermutation t
  return $ Typed eType (mkAbstractLiteral $ AbsLitPermutation xs)
  where
    validatePermutationElem :: PermutationElemNode -> ValidatorS (Typed [Expression])
    validatePermutationElem (PermutationElemNode exprs) = do
      xs <- validateExprList exprs
      sameType xs

validateRelationLiteral :: ListNode RelationElemNode -> ValidatorS (Typed Expression)
validateRelationLiteral ln = do
  ms <- validateList validateRelationMember ln
  (t, xs) <- typeSplit <$> sameType ms
  setContextFrom ln
  return . Typed t $ mkAbstractLiteral $ AbsLitRelation xs
  where
    validateRelationMember :: RelationElemNode -> ValidatorS (Typed [Expression])
    validateRelationMember x = case x of
      RelationElemNodeLabeled (LongTuple lt xs) -> lt `isA` TtType >> validateRelationMember (RelationElemNodeShort $ ShortTuple xs)
      RelationElemNodeShort (ShortTuple xs) -> do
        es <- validateExprList_ xs
        let (ts, vs) = unzip $ typeSplit <$> es
        return $ Typed (TypeRelation ts) vs

validatePartitionLiteral :: ListNode PartitionElemNode -> ValidatorS (Typed Expression)
validatePartitionLiteral ln = do
  members <- validateList validatePartitionElem ln
  (t, xs) <- typeSplit <$> sameType members
  let eType = TypePartition t
  return $ Typed eType (mkAbstractLiteral $ AbsLitPartition xs)
  where
    validatePartitionElem :: PartitionElemNode -> ValidatorS (Typed [Expression])
    validatePartitionElem (PartitionElemNode exprs) = do
      xs <- validateExprList exprs
      sameType xs

validateRecordLiteral :: ListNode RecordMemberNode -> ValidatorS (Typed Expression)
validateRecordLiteral ln = do
  members <- catMaybes <$> listElems ln
  case members of
    [] -> return $ Typed (TypeRecord []) (mk []) -- REVIEW: should empty records be allowed?
    xs -> do
      (ns, unzip . map typeSplit -> (ts, es)) <- mapAndUnzipM validateRecordMember xs
      let t = TypeRecord $ zip ns ts
      return $ Typed t $ mk (zip ns es)
  where
    mk = mkAbstractLiteral . AbsLitRecord

validateVariantLiteral :: ListNode RecordMemberNode -> ValidatorS (Typed Expression)
validateVariantLiteral ln = do
  members <- catMaybes <$> validateList_ (f2n validateRecordMember) ln
  res <- case members of
    [] -> invalid $ symbolRegion ln <!> SemanticError "Variants must contain exactly one member"
    [(n, Typed t v)] -> return . pure . Typed (TypeVariant [(n, t)]) $ AbstractLiteral $ AbsLitVariant Nothing n v
    _ : _ -> invalid $ symbolRegion ln <!> SyntaxError "Variants must contain exactly one member" -- tag subsequent members as unexpected
  return $ fromMaybe (fallback "bad variant") res

validateRecordMember :: RecordMemberNode -> ValidatorS (Name, Typed Expression)
validateRecordMember (RecordMemberNode name lEq expr) = do
  lEq `isA'` TtKeyword
  name' <- validateName name
  expr' <- validateExpression expr
  return (name', expr')

validateFunctionLiteral :: ListNode ArrowPairNode -> ValidatorS (Typed Expression)
validateFunctionLiteral ln = do
  pairs <- catMaybes <$> validateList_ validateArrowPair ln
  let (pl, pr) = unzip pairs
  (lhType, ls) <- typeSplit <$> sameType pl
  (rhType, rs) <- typeSplit <$> sameType pr
  let fType = TypeFunction lhType rhType
  return . Typed fType $ mkAbstractLiteral $ AbsLitFunction $ zip ls rs

validateSetLiteral :: ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateSetLiteral ls = do
  xs <- validateList validateExpression ls
  (t, es) <- typeSplit <$> sameType xs
  return . Typed (TypeSet t) $ mkAbstractLiteral $ AbsLitSet es

validateMSetLiteral :: ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateMSetLiteral ls = do
  xs <- validateList validateExpression ls
  (t, es) <- typeSplit <$> sameType xs
  let eType = TypeMSet t
  let result = mkAbstractLiteral $ AbsLitMSet es
  return $ Typed eType result

validateMatrixLiteral :: MatrixLiteralNode -> ValidatorS (Typed Expression)
-- Matrix proper
validateMatrixLiteral (MatrixLiteralNode l1 se m_dom Nothing l2) = do
  [l1, l2] `are` TtOther "SquareBrackets"
  matElems <- validateSequence validateExpression se
  (t, es) <- typeSplit <$> sameType matElems
  let defaultDomain :: TypedDomain = Typed tInt (mkDomainIntB 1 (fromInt $ genericLength matElems))
  dom <- fromMaybe defaultDomain <$> validateOverDomain m_dom
  let lit = AbsLitMatrix (untype dom) es
  return $ Typed (TypeMatrix tInt t) $ mkAbstractLiteral lit
  where
    validateOverDomain :: Maybe OverDomainNode -> Validator TypedDomain
    validateOverDomain Nothing = return Nothing
    validateOverDomain (Just (OverDomainNode l3 dom)) = do l3 `isA'` TtOther "Semicolon in matrix"; pure <$> validateDomain dom

-- Matrix as comprehension
validateMatrixLiteral m@(MatrixLiteralNode l1 se m_dom (Just comp) l2) = do
  [l1, l2] `are` TtOther "SquareBrackets"
  case m_dom of
    Nothing -> return ()
    Just p@(OverDomainNode l3 dom) -> do
      l3 `isA'` TtOther "Semicolon in matrix"
      void $ validateDomain dom
      raiseError $ symbolRegion p <!> SemanticError "Index domains are not supported in comprehensions"
  scoped
    $ do
      -- check gens and put locals into scope
      (gens, dGens) <- holdDeclarations $ validateComprehension comp
      -- now validate expression(s)
      (es, dBody) <-
        holdDeclarations
          $ wrapRegion se se SBody
          $ validateSequence validateExpression se
      r <- case es of
        [] -> return $ fallback "missing" <$ raiseError $ symbolRegion se <!> SemanticError "MissingExpression"
        ((_, x) : xs) -> flagExtraExpressions xs >> return x
      let bodyType = typeOf_ r
      wrapRegion m se (SComprehension (simple $ TypeList bodyType)) (mapM_ addRegion (dGens ++ dBody))
      return . Typed (TypeList bodyType) $ Comprehension (untype r) gens
  where
    flagExtraExpressions :: [RegionTagged a] -> ValidatorS ()
    flagExtraExpressions [] = pure ()
    flagExtraExpressions xs = raiseError $ catRegions xs <!> SemanticError "Comprehensension may have only one expression before |"

validateComprehension :: ComprehensionNode -> ValidatorS [GeneratorOrCondition]
validateComprehension (ComprehensionNode l1 body) = do
  l1 `isA` TtKeyword
  concat <$> validateSequence_ validateComprehensionBody body

validateComprehensionBody :: ComprehensionBodyNode -> ValidatorS [GeneratorOrCondition]
-- guard
validateComprehensionBody (CompBodyCondition en) = wrapRegion en en SGuard $ do
  e <- validateExpression en ?=> exactly TypeBool
  return [Condition e]
-- x in dom
validateComprehensionBody c@(CompBodyDomain apn l1 dom) = wrapRegion c apn SGen $ do
  l1 `isA` TtKeyword
  putDocs KeywordD "expr_in_domain_projection" l1
  (td, domain) <- typeSplit <$> validateDomain dom
  td' <- projectionTypeDomain (symbolRegion dom) td
  pats <- validateSequence_ (flip unifyPattern td' . Just) apn
  return $ [Generator (GenDomainNoRepr pat domain) | pat <- pats]

-- x <- expr
validateComprehensionBody c@(CompBodyGenExpr apn lt en) = wrapRegion c apn SGen $ do
  lt `isA` TtKeyword
  putDocs KeywordD "expr_projection" lt
  e <- validateExpression en
  let (t, exp) = typeSplit e
  t' <- projectionType (symbolRegion en) t
  pats <- validateSequence_ (flip unifyPattern t' . Just) apn
  return $ [Generator (GenInExpr pat exp) | pat <- pats]
-- letting x be
validateComprehensionBody c@(CompBodyLettingNode l1 nn l2 en) = wrapRegion c nn SLetting $ do
  l1 `isA` TtKeyword
  l2 `isA'` TtKeyword
  (t, expr) <- typeSplit <$> validateExpression en
  pat <- unifyPattern (Just nn) t
  return [ComprehensionLetting pat expr]

projectionType :: DiagnosticRegion -> Type -> ValidatorS Type
projectionType r t = case t of
  TypeAny -> return TypeAny
  TypeTuple _ -> return t
  TypeMatrix _ ty -> return ty
  TypeList ty -> return ty
  TypeSet ty -> return ty
  TypeMSet ty -> return ty
  TypeSequence ty -> return $ TypeTuple [tInt, ty]
  TypePermutation ty -> return $ TypeTuple [ty, ty]
  TypeRelation ts -> return $ TypeTuple ts
  TypePartition ty -> return $ TypeSet ty
  TypeFunction fr to -> return $ TypeTuple [fr, to]
  _ -> raiseTypeError (r <!> SemanticError (pack $ "Expression of type " ++ show (pretty t) ++ " cannot be projected in a comprehension")) >> return TypeAny

projectionTypeDomain :: DiagnosticRegion -> Type -> ValidatorS Type
projectionTypeDomain _ t = case t of -- TODO check and do properly
  TypeAny -> return TypeAny
  TypeEnum (Name n) -> return $ TypeInt $ TagEnum n
  TypeUnnamed (Name n) -> return $ TypeInt $ TagUnnamed n
  _ -> return t

-- _ -> (raiseTypeError $ r <!> SemanticError  (pack $ "Domain of type " ++ (show $pretty t) ++ " cannot be projected in a comprehension")) >> return TypeAny

mkAbstractLiteral :: AbstractLiteral Expression -> Expression
mkAbstractLiteral x = case e2c (AbstractLiteral x) of
  Nothing -> AbstractLiteral x
  Just c -> Constant c

enforceConstraint :: Maybe Bool -> String -> ValidatorS ()
enforceConstraint p msg = case p of
  Just True -> return ()
  _ -> void (contextError (CustomError $ pack msg))

checkSymbols :: [LToken] -> ValidatorS ()
checkSymbols = mapM_ validateSymbol

-- Raise a non structural error (i.e type error)
raiseError :: ValidatorDiagnostic -> ValidatorS ()
raiseError e = tell [e]

raiseTypeError :: ValidatorDiagnostic -> ValidatorS ()
raiseTypeError e = do
  tc <- gets typeChecking
  when tc $ raiseError e

makeTupleLiteral :: [Typed Expression] -> ValidatorS (Typed Expression)
makeTupleLiteral members = do
  let memberTypes = unzip $ map typeSplit members
  let eType = TypeTuple (fst memberTypes)
  return . Typed eType . mkAbstractLiteral . AbsLitTuple $ snd memberTypes

validateIntLiteral :: SToken -> Maybe ETok -> ValidatorS Constant
validateIntLiteral t maybe_tag = do
  flagSToken t TtNumber
  l <- validateSToken t
  let tag = case maybe_tag of
        Just ta -> case lexeme ta of
          LIdentifier ta_i -> TaggedInt ta_i
          _ -> TagInt
        Nothing -> TagInt
  case l of
    (LIntLiteral x) -> return $ ConstantInt tag x
    _ -> error "Bad int literal"

validateBoolLiteral :: SToken -> ValidatorS Constant
validateBoolLiteral t = do
  flagSToken t TtBool
  l <- validateSToken t
  case l of
    L_true -> return $ ConstantBool True
    L_false -> return $ ConstantBool False
    _ -> error "Bad bool literal"

validateNameList :: Sequence NameNode -> ValidatorS [RegionTagged Name]
validateNameList = validateSequence validateName

validateNameList_ :: Sequence NameNode -> ValidatorS [Name]
validateNameList_ = validateSequence_ validateName

validateIdentifierS :: NameNodeS -> ValidatorS Text
validateIdentifierS (NameNodeS iden) = do
  q <- validateSToken iden
  case q of
    (LIdentifier x) -> checkName x >> return x
    _ -> bug $ "Name wasn't a name:" <+> pretty (show q)
  where
    checkName :: Text -> Validator Text
    checkName "" = invalid $ iden <!> SemanticError "Empty names not allowed"
    checkName "\"\"" = invalid $ iden <!> SemanticError "Empty names not allowed"
    checkName x = return . pure $ x

validateIdentifier :: NameNode -> ValidatorS Text
validateIdentifier (NameNode nns) = validateIdentifierS nns
validateIdentifier (MissingNameNode iden) = do
  _ <- validateSymbol iden
  return " <Missing name>"

validateName :: NameNode -> ValidatorS Name
validateName name = do
  n <- validateIdentifier name
  return (Name n)

validateNameAs :: TagType -> NameNode -> ValidatorS Name
validateNameAs f (NameNode n) = tagNameAs f n
validateNameAs _ name = validateName name

tagNameAs :: TagType -> NameNodeS -> ValidatorS Name
tagNameAs f nn@(NameNodeS n) = flagSToken n f >> Name <$> validateIdentifier (NameNode nn)

listToSeq :: ListNode a -> ValidatorS (Sequence a)
listToSeq (ListNode l1 s l2) = checkSymbols [l1, l2] >> return s

-- visit a sequence, return a list of elements, nothing if missing
sequenceElems :: (HighLevelTree a) => Sequence a -> ValidatorS [Maybe a]
sequenceElems (Seq els) = mapM (validateSequenceElem_ validateIdentity) els

listElems :: (HighLevelTree a) => ListNode a -> ValidatorS [Maybe a]
listElems = sequenceElems <=< listToSeq

validateIdentity :: a -> Validator a
validateIdentity = return . pure

validateArray :: (a -> ValidatorS b) -> [a] -> ValidatorS [b]
validateArray = mapM

validateList :: (HighLevelTree a, Fallback b) => (a -> ValidatorS b) -> ListNode a -> ValidatorS [RegionTagged b]
validateList validator (ListNode st seq end) = do
  _ <- validateSymbol st
  _ <- validateSymbol end
  validateSequence validator seq

validateList_ :: (HighLevelTree a, Fallback b) => (a -> ValidatorS b) -> ListNode a -> ValidatorS [b]
validateList_ validator (ListNode st seq end) = do
  _ <- validateSymbol st
  _ <- validateSymbol end
  validateSequence_ validator seq

-- mapPrefixToOp :: Lexeme -> Text
-- mapPrefixToOp x = case x of
--     L_Minus -> "negate"
--     L_ExclamationMark -> "not"
--     _ -> pack $ lexemeFace x

validateSequence :: (HighLevelTree a, Fallback b) => (a -> ValidatorS b) -> Sequence a -> ValidatorS [RegionTagged b]
validateSequence f (Seq vals) = validateArray (validateSequenceElem f) vals

validateSequence_ :: (HighLevelTree a, Fallback b) => (a -> ValidatorS b) -> Sequence a -> ValidatorS [b]
validateSequence_ f s = do
  q <- validateSequence f s
  return . map snd $ q

validateSequenceElem :: (HighLevelTree a, Fallback b) => (a -> ValidatorS b) -> SeqElem a -> ValidatorS (RegionTagged b)
validateSequenceElem f (SeqElem i s) = do
  case s of
    Nothing -> pure ()
    Just lt -> void $ validateSymbol lt
  v <- f i
  return (symbolRegion i, v)
validateSequenceElem _ (MissingSeqElem plc sepr) = do
  checkSymbols [sepr]
  raiseError $ symbolRegion plc <!> TokenError plc
  return (symbolRegion plc, fallback "Missing elem")

validateSequenceElem_ :: (HighLevelTree a, Fallback b) => (a -> ValidatorS b) -> SeqElem a -> ValidatorS b
validateSequenceElem_ f (SeqElem i s) = do
  case s of
    Nothing -> pure ()
    Just lt -> void $ validateSymbol lt
  f i
validateSequenceElem_ _ (MissingSeqElem plc sepr) = do
  checkSymbols [sepr]
  raiseError $ symbolRegion plc <!> TokenError plc
  return $ fallback "Missing Elem"

validateExprList :: ListNode ExpressionNode -> ValidatorS [RegionTagged (Typed Expression)]
validateExprList = validateList validateExpression

validateExprList_ :: ListNode ExpressionNode -> ValidatorS [Typed Expression]
validateExprList_ = validateList_ validateExpression

offsetPositionBy :: Int -> SourcePos -> SourcePos
offsetPositionBy amt sp@(SourcePos _ _ (unPos -> r)) = sp {sourceColumn = mkPos (amt + r)}

data DiagnosticRegion = DiagnosticRegion
  { drSourcePos :: SourcePos,
    drEndPos :: SourcePos,
    drOffset :: Int,
    drLength :: Int
  }
  deriving (Show, Eq, Ord)

global :: DiagnosticRegion
global = DiagnosticRegion sourcePos0 sourcePos0 0 0

-- getTokenRegion :: LToken -> DiagnosticRegion
-- getTokenRegion a =  do
--         let h =case a of
--               RealToken et -> et
--               MissingToken et -> et
--               SkippedToken et -> et
--         let start = tokenSourcePos h
--         let offset = tokenStart h
--         let tLength =case a of
--               RealToken _ -> trueLength h
--               MissingToken _ -> 1
--               SkippedToken _ -> trueLength h
--         DiagnosticRegion start (offsetPositionBy tLength start) offset tLength

symbolRegion :: (HighLevelTree a) => a -> DiagnosticRegion
symbolRegion a = case range of
  (h :<| rst) -> do
    let end = case viewr rst of
          EmptyR -> h
          _ :> et -> et
    let start = tokenSourcePos h
    let offset = tokenStartOffset h
    let tLength = sum (totalLength <$> rst) + trueLength h
    let en = sourcePosAfter end
    DiagnosticRegion start en offset tLength
  _ -> global
  where
    range :: Seq ETok = flattenSeq a

(<!>) :: (WithRegion a) => a -> ErrorType -> ValidatorDiagnostic
t <!> e = ValidatorDiagnostic (getRegion t) $ Error e

(/!\) :: (WithRegion a) => a -> WarningType -> ValidatorDiagnostic
t /!\ e = ValidatorDiagnostic (getRegion t) $ Warning e

(<?>) :: (WithRegion a) => a -> InfoType -> ValidatorDiagnostic
t <?> e = ValidatorDiagnostic (getRegion t) $ Info e

-- (<?!>) :: WithRegion a  => Maybe a -> ErrorType -> ValidatorDiagnostic
-- Nothing <?!> e =  ValidatorDiagnostic global $ Error e
-- Just t <?!> e =  t <!> e

contextError :: ErrorType -> Validator a
contextError e = do
  q <- getContext
  invalid $ ValidatorDiagnostic q $ Error e

contextTypeError :: ErrorType -> ValidatorS ()
contextTypeError e = do
  q <- getContext
  tc <- gets typeChecking
  when tc $ raiseError $ ValidatorDiagnostic q $ Error e

contextInfo :: InfoType -> ValidatorS ()
contextInfo e = do
  q <- getContext
  tell [ValidatorDiagnostic q $ Info e]
  return ()

-- getType :: (Pretty a ,TypeOf a) => a -> ValidatorS Type
-- getType a = do
--         tc <- gets typeChecking
--         (if tc then (do
--            let t = let ?typeCheckerMode = StronglyTyped  in typeOf a
--            case t of
--                Left err -> do
--                    void $ contextError (CustomError . pack $ "type err in :" ++ show (pretty a) ++ "err:" ++ show err)
--                    return  TypeAny
--                Right ty -> return ty) else return TypeAny)

-- assertType :: (Pretty a,TypeOf a) => Typed a -> Type -> Text -> ValidatorS ()
-- assertType v ref msg = do
--     let Typed t _ = v
--     tc <- gets typeChecking
--     unless (not tc || t == ref) $ void . contextError $ CustomError msg

resolveReference :: RegionTagged Name -> ValidatorS Kind
resolveReference (r, Name n) | n /= "" = do
  c <- getSymbol n
  case c of
    Nothing -> raiseTypeError (r <!> CustomError (T.concat ["Symbol not found \"", n, "\""])) >> return (simple TypeAny)
    Just (reg, _, t) -> do
      putReference r n t reg
      -- addRegion (RegionInfo {rRegion=r,rText=n, rType=Just t, rDeclaration=Ref reg})
      return t
resolveReference _ = return $ simple TypeAny

sameType :: [RegionTagged (Typed a)] -> ValidatorS (Typed [a])
sameType [] = return $ Typed TypeAny []
sameType xs@(x : _) = do
  let ?typeCheckerMode = StronglyTyped
  let t = mostDefined $ map (typeOf_ . snd) xs
  let t' = mostDefined [t, typeOf_ $ snd x] -- Do this again to set type to first elem if possible
  xs' <- mapM (unifyTypes t') xs
  return $ Typed t' xs'

typesUnifyS :: [Type] -> Bool
typesUnifyS = let ?typeCheckerMode = StronglyTyped in typesUnify

mostDefinedS :: [Type] -> Type
mostDefinedS [] = TypeAny
mostDefinedS [x] = x
mostDefinedS (x : xs) =
  let ?typeCheckerMode = StronglyTyped
   in case mostDefined (xs ++ [x]) of
        TypeAny -> x
        t -> t

unifyTypes :: Type -> RegionTagged (Typed a) -> ValidatorS a
unifyTypes _ (r, Typed TypeAny a) = raiseError (r /!\ AmbiguousTypeWarning) >> return a
unifyTypes t (r, Typed t' a) = do
  let ?typeCheckerMode = StronglyTyped
  if typesUnify [t', t] then pure () else raiseTypeError $ r <!> TypeError t t'
  return a

unifyTypesFailing :: Type -> RegionTagged (Typed a) -> Validator a
unifyTypesFailing _ (r, Typed TypeAny a) = raiseError (r /!\ AmbiguousTypeWarning) >> return (Just a)
unifyTypesFailing t (r, Typed t' a) = do
  tc <- gets typeChecking
  let ?typeCheckerMode = StronglyTyped
  if not tc || typesUnify [t', t] then return $ Just a else invalid (r <!> TypeError t t')

scoped :: ValidatorS a -> ValidatorS a
scoped m = do
  st <- gets symbolTable
  res <- m
  modifySymbolTable $ const st
  return res

unifyPatterns :: Type -> [Maybe AbstractPatternNode] -> ValidatorS [AbstractPattern]
unifyPatterns t = mapM (`unifyPattern` t)

unifyPattern :: Maybe AbstractPatternNode -> Type -> ValidatorS AbstractPattern
unifyPattern (Just (AbstractIdentifier nn)) t = do
  nm <- tagNameAs TtLocal nn
  let n = case nm of
        Name txt -> txt
        NameMetaVar s -> T.pack s
        _ -> bug "Bad name "
  void $ putSymbol (Name n, (symbolRegion nn, False, simple t))
  unless (n == "_") $ addRegion $ mkDeclaration (symbolRegion nn) n (simple t)
  -- addRegion (RegionInfo (symbolRegion nn) (Just $ simple t) n Definition)
  return $ Single $ Name n
unifyPattern (Just (AbstractMetaVar lt)) _ = do
  s <- validateMetaVar lt
  return $ AbstractPatternMetaVar s
unifyPattern (Just (AbstractPatternTuple m_lt ln)) t = do
  sps <- listToSeq ln
  ps <- sequenceElems sps
  case m_lt of
    Nothing -> void $ return ()
    Just lt -> lt `isA'` TtType
  memberTypes <- case t of
    TypeAny -> return $ replicate (length ps) TypeAny
    TypeTuple ts -> do
      let dif = length ts - length ps
      unless (dif <= 0) $ raiseError $ symbolRegion ln <!> (CustomError . T.pack $ "Missing " ++ show dif ++ " fields from projection tuple, if you dont want the value, use '_'")
      return ts
    _ -> replicate (length ps) TypeAny <$ raiseTypeError (symbolRegion ln <!> CustomError (T.concat ["Could not project ", prettyT t, " onto tuple pattern"]))

  let (paired, unpaired) = splitAt (length memberTypes) ps
  let q = zip paired memberTypes
  aps <- mapM (uncurry unifyPattern) q
  mapM_ (\x -> raiseError $ symbolRegion x <!> CustomError "Extraneous tuple field from projection") (catMaybes unpaired)
  return $ AbsPatTuple aps
unifyPattern (Just (AbstractPatternMatrix ln)) t = do
  sps <- listToSeq ln
  ps <- sequenceElems sps
  memberTypes <- case t of
    TypeAny -> return $ repeat TypeAny
    TypeList ty -> return $ repeat ty
    TypeMatrix _ ty -> return $ repeat ty
    _ -> repeat TypeAny <$ raiseTypeError (symbolRegion ln <!> CustomError (T.concat ["Could not project ", prettyT t, " onto list pattern"]))

  let q = zip ps memberTypes
  aps <- mapM (uncurry unifyPattern) q
  return $ AbsPatMatrix aps
unifyPattern (Just (AbstractPatternSet ln)) t = do
  sps <- listToSeq ln
  ps <- sequenceElems sps
  memberTypes <- case t of
    TypeAny -> return $ repeat TypeAny
    TypeSet ty -> return $ repeat ty
    TypeMSet ty -> return $ repeat ty
    _ -> repeat TypeAny <$ raiseTypeError (symbolRegion ln <!> CustomError (T.concat ["Could not project ", prettyT t, " onto set pattern"]))
  let q = zip ps memberTypes
  aps <- mapM (uncurry unifyPattern) q
  return $ AbsPatSet aps
unifyPattern Nothing _ = return . Single $ fallback "No Pattern"

catRegions :: [RegionTagged a] -> DiagnosticRegion
catRegions [] = global
catRegions xs =
  DiagnosticRegion
    { drSourcePos = drSourcePos . fst $ head xs,
      drEndPos = drEndPos . fst $ last xs,
      drOffset = drOffset . fst $ head xs,
      drLength = sum $ map (drLength . fst) xs
    }

getMemberTypes :: Type -> ValidatorS [Type]
getMemberTypes t = case t of
  TypeAny -> return $ repeat TypeAny
  --   TypeUnnamed na ->
  TypeTuple tys -> return tys
  _ -> return $ repeat TypeAny

-- unifyAbstractPatternOverExpression :: AbstractPatternNode -> Expression -> Validator (Name,Type)
-- unifyAbstractPatternOverExpression pat exp = do
--     t <- typeOf exp

--     empty

getDomainMembers :: Type -> Type
getDomainMembers t = case t of
  TypeAny -> t
  TypeBool -> t
  TypeInt it -> case it of
    TaggedInt {} -> t
    TagInt -> t
    TagEnum _ -> error "Domain should use typeEnum instead"
    TagUnnamed _ -> error "Domain should use typeUnamed instead"
  TypeEnum n -> TypeInt . TagEnum $ case n of
    Name txt -> txt
    MachineName {} -> error "This should never be user specified"
    NameMetaVar s -> pack s
  TypeUnnamed n -> TypeInt . TagUnnamed $ case n of
    Name txt -> txt
    MachineName {} -> error "This should never be user specified"
    NameMetaVar s -> pack s
  TypeTuple tys -> TypeTuple $ map getDomainMembers tys
  TypeRecord tys -> TypeRecord $ map (second getDomainMembers) tys
  TypeVariant tys -> TypeVariant $ map (second getDomainMembers) tys
  TypeList ty -> TypeList $ getDomainMembers ty
  TypeMatrix ty ty' -> TypeMatrix ty $ getDomainMembers ty'
  TypeSet ty -> TypeSet $ getDomainMembers ty
  TypeMSet ty -> TypeMSet $ getDomainMembers ty
  TypeFunction ty ty' -> TypeFunction (getDomainMembers ty) (getDomainMembers ty')
  TypeSequence ty -> TypeSequence $ getDomainMembers ty
  TypePermutation ty -> TypePermutation $ getDomainMembers ty
  TypeRelation tys -> TypeRelation $ map getDomainMembers tys
  TypePartition ty -> TypePartition $ getDomainMembers ty
  _ -> bug $ "Unknown domain type" <+> pretty t

getDomainFromValue :: Type -> Type
getDomainFromValue t = case t of
  TypeAny -> t
  TypeBool -> t
  TypeInt it -> case it of
    TaggedInt {} -> t
    TagInt -> t
    TagEnum n -> TypeEnum (Name n)
    TagUnnamed n -> TypeUnnamed (Name n)
  TypeEnum _ -> t
  TypeUnnamed _ -> t
  TypeTuple tys -> TypeTuple $ map getDomainFromValue tys
  TypeRecord tys -> TypeRecord $ map (second getDomainFromValue) tys
  TypeVariant tys -> TypeVariant $ map (second getDomainFromValue) tys
  TypeList ty -> TypeList $ getDomainFromValue ty
  TypeMatrix ty ty' -> TypeMatrix ty $ getDomainFromValue ty'
  TypeSet ty -> TypeSet $ getDomainFromValue ty
  TypeMSet ty -> TypeMSet $ getDomainFromValue ty
  TypeFunction ty ty' -> TypeFunction (getDomainFromValue ty) (getDomainFromValue ty')
  TypeSequence ty -> TypeSequence $ getDomainFromValue ty
  TypePermutation ty -> TypePermutation $ getDomainFromValue ty
  TypeRelation tys -> TypeRelation $ map getDomainFromValue tys
  TypePartition ty -> TypePartition $ getDomainFromValue ty
  _ -> bug $ "Unknown member type" <+> pretty t

data DomainTyped a = DomainTyped DomainType a

data DomainSize = Unkown | Infinite | Sized Int

data DomainType
  = DomainTypeBool
  | DomainTypeInt DomainSize
  | DomainTypeTuple [DomainType]

f2n :: (a -> ValidatorS b) -> a -> Validator b
f2n f a = do
  q <- f a
  return $ Just q

class Fallback a where
  fallback :: Text -> a

instance Fallback (Domain () Expression) where
  fallback reason = DomainAny reason TypeAny

instance Fallback Expression where
  fallback reason = Reference (Name reason) Nothing

instance (Fallback a) => Fallback (Typed a) where
  fallback = Typed TypeAny . fallback

instance (Fallback a) => Fallback (Kind, a) where
  fallback msg = (Kind (ValueType CatConstant) TypeAny, fallback msg)

instance Fallback (Maybe a) where
  fallback = const Nothing

instance Fallback Name where
  fallback = Name

instance Fallback [a] where
  fallback :: Text -> [a]
  fallback = const []

instance Fallback AbstractPattern where
  fallback = Single . fallback

type FuncOpDec = Int

funcOpBuilder :: Lexeme -> [Arg] -> ValidatorS (Typed Expression)
funcOpBuilder l = functionOps l (mkOp $ FunctionOp l)

-- functionOps l@L_fAnd = (validateArgList [isLogicalContainer],const TypeBool)
functionOps :: Lexeme -> ([Expression] -> Expression) -> [Arg] -> ValidatorS (Typed Expression)
functionOps l = case l of
  L_fAnd -> unFuncV isLogicalContainer (pure . const TypeBool)
  L_fOr -> unFuncV isLogicalContainer (pure . const TypeBool)
  L_fXor -> unFuncV isLogicalContainer (pure . const TypeBool)
  L_Sum -> unFuncV sumArgs (pure . const tInt)
  L_Product -> unFunc (valueOnly sumArgs) (pure . const tInt)
  L_true -> unFuncV anyType (pure . const TypeBool)
  L_toInt -> unFuncV (only TypeBool) (pure . const tInt)
  L_makeTable -> unFuncV (only TypeBool) (pure . const TypeBool)
  L_table -> biFuncV tableArgs (const2 TypeBool)
  L_gcc -> triFunc (each3 $ valueOnly listInt) (const3 TypeBool)
  L_atleast -> triFunc (each3 $ valueOnly listInt) (const3 TypeBool)
  L_atmost -> triFunc (each3 $ valueOnly listInt) (const3 TypeBool)
  L_defined -> unFuncV funcSeq (fmap TypeSet . funcDomain)
  L_range -> unFuncV funcSeq (fmap TypeSet . funcRange)
  L_restrict -> biFunc restrictArgs restrictTypes
  L_allDiff -> unFuncV listOrMatrix (const $ pure TypeBool)
  L_alldifferent_except -> biFuncV (indep listOrMatrix enumerable) (const2 TypeBool)
  L_catchUndef -> biFuncV unifies (\a b -> pure $ mostDefinedS $ catMaybes [a, b])
  L_quickPermutationOrder -> biFunc quickPermutationOrderArgs quickPermutationOrderTypes
  L_dontCare -> unFunc anyType (const $ pure TypeBool)
  L_toSet -> unFuncV toSetArgs typeToSet
  L_toMSet -> unFuncV toMSetArgs typeToMSet
  L_toRelation -> unFuncV func typeToRelation
  L_max -> unFunc minMaxArgs minMaxType
  L_min -> unFunc minMaxArgs minMaxType
  L_image -> biFuncV imageArgs (const . funcRange)
  L_transform -> biFuncV transformArgs (const id)
  L_imageSet -> biFuncV imSetArgs (\a -> const $ TypeSet <$> funcRange a)
  L_compose -> biFuncV composeArgs (const . funcRange)
  L_preImage -> biFuncV preImageArgs (\a -> const $ TypeSet <$> funcDomain a)
  L_inverse -> biFuncV inverseArgs (const2 TypeBool)
  L_freq -> biFuncV freqArgs (const2 tInt)
  L_hist -> unFuncV histArgs histType
  L_parts -> unFuncV part partsType
  L_together -> biFuncV setPartArgs (const2 TypeBool)
  L_apart -> biFuncV setPartArgs (const2 TypeBool)
  L_party -> biFuncV partyArgs partyType
  L_permInverse -> unFuncV permInverseArgs id
  L_participants -> unFuncV part partInner
  L_active -> biFuncV activeArgs (const2 TypeBool)
  L_pred -> unFuncV enumerable enumerableType
  L_succ -> unFuncV enumerable enumerableType
  L_factorial -> unFuncV (only tInt) (const $ pure tInt)
  L_powerSet -> unFuncV set powerSetType
  L_concatenate -> unFuncV concatArgs concatType
  L_flatten -> \b a -> case a of
    [] -> unFuncV unaryFlattenArgs (flattenType Nothing) b a
    [_] -> unFuncV unaryFlattenArgs (flattenType Nothing) b a
    _ -> biFunc (valueOnly2 binaryFlattenArgs) (\v t -> flattenType (getNum v) (typeOnly t)) b a
  _ -> bug $ pretty $ "Unkown functional operator " ++ show l
  where
    valueOnly :: (SArg -> Validator a) -> Arg -> Validator a
    valueOnly f (r, (k, e)) = do
      t <- getValueType k
      f (r, Typed t e)

    valueOnly2 :: (SArg -> SArg -> Validator a) -> Arg -> Arg -> Validator a
    valueOnly2 f (r1, (k1, e1)) (r2, (k2, e2)) = do
      t1 <- getValueType k1
      t2 <- getValueType k2
      f (r1, Typed t1 e1) (r2, Typed t2 e2)

    typeOnly :: Maybe (Kind, Expression) -> Maybe Type
    typeOnly (Just (Kind ValueType {} t, _)) = Just t
    typeOnly _ = Nothing

    unFuncV ::
      (SArg -> Validator a0) ->
      (Maybe Type -> Maybe Type) ->
      ([Expression] -> Expression) ->
      [Arg] ->
      ValidatorS (Typed Expression)
    unFuncV a t = unFunc (valueOnly a) (t . typeOnly)

    biFuncV :: (SArg -> SArg -> Validator ()) -> (Maybe Type -> Maybe Type -> Maybe Type) -> ([Expression] -> Expression) -> [Arg] -> ValidatorS (Typed Expression)
    biFuncV a t = biFunc (valueOnly2 a) (\t1 t2 -> t (typeOnly t1) (typeOnly t2))

    valid = return $ pure ()

    const2 = const . const . pure

    const3 = const . const . const . pure

    getNum :: Maybe (Kind, Expression) -> Maybe Int
    getNum (Just (_, x)) = case intOut "" x of
      Nothing -> Nothing
      Just n -> pure $ fromInteger n
    getNum _ = Nothing

    each3 f a b c = f a >> f b >> f c

    anyType = const . return $ Just ()

    indep :: (SArg -> Validator ()) -> (SArg -> Validator ()) -> (SArg -> SArg -> Validator ())
    indep f1 f2 a b = do
      v1 <- f1 a
      v2 <- f2 b
      if not . null $ catMaybes [v1, v2] then return $ pure () else return Nothing

    binaryFlattenArgs :: SArg -> SArg -> Validator ()
    binaryFlattenArgs (r1, d) b = do
      off <- case intOut "" (untype d) of
        Just (fromInteger -> a :: Integer) | a > 0 -> return $ Just a
        _ -> invalid $ r1 <!> CustomError "1st arg must be a constant positive int"
      let ref = map (const TypeList) [0 .. fromMaybe 1 off]
      let ref' = foldr id TypeAny ref
      r <- unifyTypesFailing ref' b
      return $ if null off || null r then Nothing else Just ()

    unaryFlattenArgs :: SArg -> Validator ()
    unaryFlattenArgs (_, typeOf_ -> (TypeMatrix _ _)) = valid
    unaryFlattenArgs (_, typeOf_ -> (TypeList _)) = valid
    unaryFlattenArgs (_, typeOf_ -> TypeAny) = valid
    unaryFlattenArgs (r, typeOf_ -> t) = invalid $ r <!> ComplexTypeError "List or Matrix " t

    concatType :: Maybe Type -> Maybe Type
    concatType (Just (TypeMatrix _ (TypeList t))) = Just $ TypeList t
    concatType (Just (TypeMatrix _ (TypeMatrix _ t))) = Just $ TypeList t
    concatType (Just (TypeList (TypeList t))) = Just $ TypeList t
    concatType (Just (TypeList (TypeMatrix _ t))) = Just $ TypeList t
    concatType _ = Just $ TypeList TypeAny
    concatArgs :: SArg -> Validator ()
    concatArgs s@(r, _) = binaryFlattenArgs (r, Typed tInt $ Constant $ ConstantInt TagInt 1) s

    tableArgs :: SArg -> SArg -> Validator ()
    tableArgs (r1, typeOf_ -> t1) (r2, typeOf_ -> t2) = do
      a <- case t1 of
        t | isValidInner t -> valid
        _ -> invalid $ r1 <!> ComplexTypeError "Matrix of Int/Enum" t1
      b <- case t2 of
        TypeAny -> valid
        TypeList t | isValidInner t -> valid
        TypeMatrix _ t | isValidInner t -> valid
        _ -> invalid $ r2 <!> ComplexTypeError "Matrix of Matrix of Int/Enum" t2

      return $ if null a || null b then Nothing else Just ()
      where
        isValidInner t = case t of
          TypeAny -> True
          TypeList TypeInt {} -> True
          TypeList TypeAny -> True
          TypeMatrix _ TypeInt {} -> True
          TypeMatrix _ TypeAny -> True
          _ -> False

    toMSetArgs :: SArg -> Validator ()
    toMSetArgs (r, typeOf_ -> a) = case a of
      TypeAny -> return $ pure ()
      TypeList _ -> return $ pure ()
      TypeMatrix {} -> return $ pure ()
      TypeMSet {} -> return $ pure ()
      TypeSet {} -> return $ pure ()
      TypeFunction {} -> return $ pure ()
      TypeRelation {} -> return $ pure ()
      _ -> invalid $ r <!> ComplexTypeError "Matrix ,list,function,relation,mset,set " a

    toSetArgs :: SArg -> Validator ()
    toSetArgs (r, typeOf_ -> a) = case a of
      TypeAny -> return $ pure ()
      TypeList _ -> return $ pure ()
      TypeMatrix {} -> return $ pure ()
      TypeMSet {} -> return $ pure ()
      TypeFunction {} -> return $ pure ()
      TypeRelation {} -> return $ pure ()
      _ -> invalid $ r <!> ComplexTypeError "Matrix ,list,function,relation,mset " a

    listOrMatrix :: SArg -> Validator ()
    listOrMatrix (r, typeOf_ -> a) = case a of
      TypeAny -> return $ pure ()
      TypeList _ -> return $ pure ()
      TypeMatrix {} -> return $ pure ()
      _ -> invalid $ r <!> ComplexTypeError "Matrix or list" a

    freqArgs :: SArg -> SArg -> Validator ()
    freqArgs (r1, a) (r2, b) = do
      let tb = typeOf_ b
      let (rt, ti) = case typeOf_ a of
            TypeMatrix idx ms -> (TypeMatrix idx md, md) where md = mostDefinedS [tb, ms]
            TypeMSet ms -> (TypeMSet md, md) where md = mostDefinedS [tb, ms]
            _ -> (TypeMatrix tInt tb, tb)
      a' <- unifyTypesFailing rt (r1, a)
      b' <- unifyTypesFailing ti (r2, b)
      return $ if null a' || null b' then Nothing else Just ()

    unifies :: SArg -> SArg -> Validator ()
    unifies a b = do
      let md = mostDefinedS $ map (typeOf_ . unregion) [a, b]
      a' <- unifyTypesFailing md a
      b' <- unifyTypesFailing md b
      return $ if null a' || null b' then Nothing else Just ()

    func :: SArg -> Validator ()
    func (_, Typed (TypeFunction _ _) _) = valid
    func (_, Typed TypeAny _) = valid
    func (r, Typed t _) = invalid $ r <!> TypeError (TypeFunction TypeAny TypeAny) t

    set :: SArg -> Validator Type
    set (_, Typed (TypeSet t) _) = return $ pure t
    set (_, Typed TypeAny _) = return $ pure TypeAny
    set (r, Typed t _) = invalid $ r <!> TypeError (TypeSet TypeAny) t

    powerSetType (Just ((TypeSet i))) = Just $ TypeSet (TypeSet i)
    powerSetType _ = Just $ TypeSet $ TypeSet TypeAny

    only t (r, typeOf_ -> t') = do
      setContext r
      if t' == TypeAny || t == t'
        then return $ Just t
        else invalid $ r <!> TypeError t t'

    listInt (r, typeOf_ -> t') = case t' of
      TypeAny -> return $ Just t'
      TypeList TypeInt {} -> return $ Just t'
      TypeMatrix _ TypeInt {} -> return $ Just t'
      _ -> invalid $ r <!> ComplexTypeError "Matrix or list of int or enum" t'

    partInner :: Maybe Type -> Maybe Type
    partInner (Just (TypePartition a)) = Just $ TypeSet a
    partInner _ = Just $ TypeSet TypeAny

    restrictArgs :: Arg -> Arg -> Validator ()
    restrictArgs (r1, (k, _)) (r2, (kd, _)) = do
      setContext r1
      funcType <- getValueType k
      setContext r2
      domType <- getDomainType kd
      unifyTypesFailing (TypeFunction domType TypeAny) (r1, Typed funcType ())

    restrictTypes :: Maybe (Kind, Expression) -> Maybe (Kind, Expression) -> Maybe Type
    restrictTypes (fmap fst -> kv) (fmap fst -> kd) = Just $ TypeFunction from to
      where
        dType = case kd of
          Just (Kind DomainType t) -> getDomainMembers t
          _ -> TypeAny
        from = case kv of
          Just (Kind ValueType {} (TypeFunction fr _)) | typesUnifyS [dType, fr] -> mostDefinedS [dType, fr]
          Just (Kind ValueType {} (TypeFunction fr _)) -> fr
          _ -> mostDefinedS [TypeAny, dType]
        to = case kv of
          Just (Kind ValueType {} (TypeFunction _ to')) -> to'
          _ -> TypeAny

    -- TODO: validate
    quickPermutationOrderArgs :: Arg -> Arg -> Validator ()
    quickPermutationOrderArgs _ _ = return (pure ())

    -- TODO
    permInverseArgs :: SArg -> Validator ()
    permInverseArgs _ = return (pure ())

    -- TODO
    quickPermutationOrderTypes :: Maybe (Kind, Expression) -> Maybe (Kind, Expression) -> Maybe Type
    quickPermutationOrderTypes _ _ = Just TypeBool

    imSetArgs :: SArg -> SArg -> Validator ()
    imSetArgs (r1, a) (r2, b) = do
      let t = case (typeOf_ a, typeOf_ b) of
            (TypeFunction i _, tb) -> mostDefinedS [i, tb]
            (TypeSequence _, _) -> tInt
            (_, tb) -> tb
      a' <- unifyTypesFailing (TypeFunction t TypeAny) (r1, a)
      b' <- unifyTypesFailing t (r2, b)
      return $ if null a' || null b' then Nothing else Just ()

    preImageArgs :: SArg -> SArg -> Validator ()
    preImageArgs _ _ = return $ Just ()
    -- preImageArgs (r1, a) (r2, b) = do
    --   let t = case (typeOf_ a, typeOf_ b) of
    --         (TypeFunction _ i, tb) -> mostDefinedS [i, tb]
    --         (TypeSequence i, _) -> i
    --         (_, tb) -> tb
    --   a' <- unifyTypesFailing (TypeFunction TypeAny t) (r1, a)
    --   b' <- unifyTypesFailing t (r2, b)
    --   return $ if null a' || null b' then Nothing else Just ()

    partyArgs :: SArg -> SArg -> Validator ()
    partyArgs (r1, a) (r2, b) = do
      let t = case (typeOf_ a, typeOf_ b) of
            (ta, TypePartition tb) -> mostDefinedS [ta, tb]
            (ta, _) -> ta
      a' <- unifyTypesFailing t (r1, a)
      b' <- unifyTypesFailing (TypePartition t) (r2, b)
      return $ if null a' || null b' then Nothing else Just ()

    inverseArgs :: SArg -> SArg -> Validator ()
    inverseArgs (_r1, a) (_r2, b) =
      case (typeOf_ a, typeOf_ b) of
        (TypeFunction {}, TypeFunction {}) -> return (Just ())
        (TypeFunction {}, _) -> return (Just ())
        (_, TypeFunction {}) -> return (Just ())
        (TypePermutation {}, TypePermutation {}) -> return (Just ())
        (TypePermutation {}, _) -> return (Just ())
        (_, TypePermutation {}) -> return (Just ())
        _ -> return Nothing

    setPartArgs :: SArg -> SArg -> Validator ()
    setPartArgs (r1, a) (r2, b) = do
      let t = case (typeOf_ a, typeOf_ b) of
            (TypeSet st, TypePartition pt) -> mostDefinedS [st, pt]
            (TypeSet st, _) -> st
            (_, TypePartition ts) -> ts
            _ -> TypeAny
      a' <- unifyTypesFailing (TypeSet t) (r1, a)
      b' <- unifyTypesFailing (TypePartition t) (r2, b)
      return $ if null a' || null b' then Nothing else Just ()

    partyType :: Maybe Type -> Maybe Type -> Maybe Type
    partyType a b = do
      let at' = fromMaybe TypeAny a
      let bt = case b of
            Just (TypePartition t) -> t
            _ -> TypeAny
      return $ TypeSet $ mostDefinedS [at', bt]

    partsType :: Maybe Type -> Maybe Type
    partsType (Just (TypePartition a)) = Just $ TypeSet $ TypeSet a
    partsType (Just TypeAny) = Just $ TypeSet $ TypeSet TypeAny
    partsType _ = Nothing

    minMaxArgs :: Arg -> Validator ()
    minMaxArgs (r, (Kind DomainType t, _)) =
      case t of
        TypeInt TagInt -> valid
        TypeInt (TagEnum _) -> valid
        TypeEnum {} -> valid
        TypeAny -> valid
        _ -> invalid $ r <!> ComplexTypeError "Domain of int-like or matrix of int-like" t
    minMaxArgs (r, (k, _)) = do
      t <- getValueType k
      inner <- case t of
        TypeList tyInner -> return tyInner
        TypeMatrix _ tyInner -> return tyInner
        TypeSet tyInner -> return tyInner
        TypeMSet tyInner -> return tyInner
        TypeAny -> return TypeAny
        _ -> TypeAny <$ invalid (r <!> ComplexTypeError "Domain of int-like or matrix of int-like" t)
      case inner of
        TypeInt TagInt -> valid
        TypeInt (TagEnum _) -> valid
        TypeEnum {} -> valid
        TypeAny -> valid
        _ -> invalid $ r <!> ComplexTypeError "Domain of int-like or matrix of int-like" t

    minMaxType :: Maybe (Kind, a) -> Maybe Type
    minMaxType (Just (Kind DomainType t@(TypeInt {}), _)) = Just t
    minMaxType (Just (Kind DomainType (TypeEnum (Name nm)), _)) = Just . TypeInt $ TagEnum nm
    minMaxType (Just (Kind ValueType {} (TypeMatrix _ a), _)) = minMaxType (Just (Kind DomainType a, ()))
    minMaxType (Just (Kind ValueType {} (TypeList a), _)) = minMaxType (Just (Kind DomainType a, ()))
    minMaxType (Just (Kind ValueType {} (TypeSet a), _)) = minMaxType (Just (Kind DomainType a, ()))
    minMaxType (Just (Kind ValueType {} (TypeMSet a), _)) = minMaxType (Just (Kind DomainType a, ()))
    minMaxType _ = Just TypeAny

    transformArgs :: SArg -> SArg -> Validator ()
    transformArgs _ _ = return $ pure ()
    activeArgs :: SArg -> SArg -> Validator ()
    activeArgs (_, typeOf_ -> TypeAny) _ = valid
    activeArgs (r, typeOf_ -> t@(TypeVariant _)) (r2, typeOf_ -> b) = checkRVMember (r, t) (r2, b)
    activeArgs (r, typeOf_ -> t) _ = invalid $ r <!> ComplexTypeError "Variant " t

    typeToSet :: Maybe Type -> Maybe Type
    typeToSet (Just t) = Just . TypeSet $ fromMaybe TypeAny (tMembers t)
    typeToSet _ = Just $ TypeSet TypeAny

    typeToMSet :: Maybe Type -> Maybe Type
    typeToMSet (Just t) = Just . TypeMSet $ fromMaybe TypeAny (tMembers t)
    typeToMSet _ = Just $ TypeMSet TypeAny

    typeToRelation :: Maybe Type -> Maybe Type
    typeToRelation (Just (TypeFunction i j)) = Just $ TypeRelation [i, j]
    typeToRelation (Just TypeAny) = Just $ TypeRelation [TypeAny, TypeAny]
    typeToRelation _ = Nothing
    tMembers t = case t of
      TypeAny -> Just TypeAny
      TypeList ty -> Just ty
      TypeMatrix _ i -> Just i
      TypeSet ty -> Just ty
      TypeMSet ty -> Just ty
      TypeFunction i j -> Just $ TypeTuple [i, j]
      TypeRelation tys -> Just $ TypeTuple tys
      _ -> Nothing

    imageArgs :: SArg -> SArg -> Validator ()
    imageArgs (r1, typeOf_ -> t1) r2 = do
      from <- case t1 of
        TypeAny -> return $ Just TypeAny
        TypeFunction a _ -> return $ Just a
        TypeSequence _ -> return $ Just tInt
        TypePermutation a -> return $ Just a
        _ -> Nothing <$ raiseTypeError (r1 <!> ComplexTypeError "function, sequence or permutation" t1)
      case from of
        Just f -> unifyTypes f r2 >> return (pure ())
        Nothing -> return Nothing

    -- TODO: validate
    composeArgs :: SArg -> SArg -> Validator ()
    composeArgs _ _ = return (pure ())

    sumArgs :: SArg -> Validator ()
    sumArgs (r, typeOf_ -> t') = do
      t <- case t' of
        TypeAny -> return TypeAny
        TypeList t -> return t
        TypeMatrix _ t -> return t
        TypeSet t -> return t
        TypeMSet t -> return t
        _ -> TypeAny <$ raiseTypeError (r <!> ComplexTypeError "Matrix or Set" t')
      case t of
        TypeAny -> return $ pure ()
        TypeInt TagInt -> return $ pure ()
        _ -> Nothing <$ raiseTypeError (r <!> ComplexTypeError "Integer elements" t)

    funcSeq :: SArg -> Validator ()
    funcSeq (r, typeOf_ -> t') = case t' of
      TypeAny -> return $ pure ()
      TypeSequence _ -> return $ pure ()
      TypePermutation _ -> return $ pure ()
      TypeFunction _ _ -> return $ pure ()
      _ -> invalid $ r <!> ComplexTypeError "function, sequence or permutation" t'

    funcDomain :: Maybe Type -> Maybe Type
    funcDomain (Just (TypeFunction a _)) = Just a
    funcDomain (Just (TypeSequence _)) = Just tInt
    funcDomain _ = Just TypeAny

    funcRange :: Maybe Type -> Maybe Type
    funcRange (Just (TypeFunction _ b)) = Just b
    funcRange (Just ((TypeSequence b))) = Just b
    funcRange _ = Just TypeAny

    part :: SArg -> Validator ()
    part (r, typeOf_ -> t) = case t of
      TypeAny -> valid
      TypePartition _ -> return $ pure ()
      _ -> invalid $ r <!> TypeError (TypePartition TypeAny) t

    histArgs :: SArg -> Validator ()
    histArgs (r, typeOf_ -> a) = case a of
      TypeMSet _ -> return $ pure ()
      TypeList _ -> return $ pure ()
      TypeMatrix _ _ -> return $ pure ()
      TypeAny -> return $ pure ()
      _ -> invalid $ r <!> ComplexTypeError "Matrix, List or MSet" a

    histType :: Maybe Type -> Maybe Type
    histType (Just ((TypeMSet a))) = Just $ TypeMatrix tInt $ TypeTuple [a, tInt]
    histType (Just ((TypeMatrix _ a))) = Just $ TypeMatrix tInt $ TypeTuple [a, tInt]
    histType (Just ((TypeList a))) = Just $ TypeMatrix tInt $ TypeTuple [a, tInt]
    histType _ = Just $ TypeMatrix tInt $ TypeTuple [TypeAny, tInt]

    enumerable :: SArg -> Validator ()
    enumerable (r, typeOf_ -> t) = case t of
      TypeAny -> return $ pure ()
      TypeInt TagUnnamed {} -> invalid $ r <!> CustomError "Anonymous enums are not explictly enumerable"
      TypeInt _ -> return $ pure ()
      TypeEnum {} -> return $ pure ()
      TypeBool -> return $ pure ()
      _ -> invalid $ r <!> ComplexTypeError "int enum or bool" t

    enumerableType :: Maybe Type -> Maybe Type
    enumerableType (Just t@(TypeInt TagInt)) = Just t
    enumerableType (Just t@(TypeInt (TagEnum _))) = Just t
    enumerableType (Just t@(TypeEnum {})) = Just t
    enumerableType _ = Nothing

flattenType :: Maybe Int -> Maybe Type -> Maybe Type
flattenType (Just n) (Just a) | n < 0 = Just $ TypeList a
flattenType (Just n) (Just (TypeList m)) = flattenType (Just (n - 1)) (Just m)
flattenType (Just n) (Just (TypeMatrix _ m)) = flattenType (Just (n - 1)) (Just m)
flattenType Nothing (Just (TypeMatrix _ m)) = flattenType Nothing (Just m)
flattenType Nothing (Just (TypeList m)) = flattenType Nothing (Just m)
flattenType Nothing (Just t) = Just $ TypeList t
flattenType _ _ = Just $ TypeList TypeAny

validateFuncOp :: Lexeme -> [RegionTagged (Kind, Expression)] -> ValidatorS (Typed Expression)
validateFuncOp l args = do
  let b = funcOpBuilder l
  b args

-- case argCheck of
--   Nothing -> return $ Typed  (r []) $ fallback "arg fail"
--   Just tys -> return $ Typed (r tys)(b $ map untype tys)

isOfType :: Type -> RegionTagged (Typed Expression) -> ValidatorS Bool
isOfType t (r, v) = setContext r >> return v ?=> exactly t >> return (typesUnifyS [t, typeOf_ v])

isLogicalContainer :: RegionTagged (Typed Expression) -> Validator ()
isLogicalContainer (r, Typed t _) = case t of
  TypeAny -> return $ pure ()
  TypeList TypeAny -> return $ pure ()
  TypeList TypeBool -> return $ pure ()
  TypeMatrix _ TypeAny -> return $ pure ()
  TypeMatrix _ TypeBool -> return $ pure ()
  TypeSet TypeAny -> return $ pure ()
  TypeMSet TypeBool -> return $ pure ()
  _ -> invalid $ r <!> ComplexTypeError "Collection of boolean" t

-- validateArgList :: [RegionTagged (Typed Expression) -> ValidatorS Bool] -> [RegionTagged (Typed Expression)] -> Validator [Typed Expression]
-- validateArgList ps args | length args < length ps = do invalid $ args <!> MissingArgsError (length ps)
-- validateArgList ps args = do
--     let ps' = ps ++ repeat argOverflow
--     xs <- zipWithM id ps' args
--     return (if and xs then  Just $ map unregion  args else Nothing)

-- argOverflow :: RegionTagged a -> ValidatorS Bool
-- argOverflow (region,_) = do
--     setContext region
--     void . contextError $ CustomError "Extra Args"
--     return False
type SArg = RegionTagged (Typed Expression)

type Arg = RegionTagged (Kind, Expression)

unFunc ::
  (Arg -> Validator a) -> -- Arg validator
  (Maybe (Kind, Expression) -> Maybe Type) -> -- typeEvaluator
  ([Expression] -> Expression) -> -- mkOp or similar
  [Arg] ->
  ValidatorS (Typed Expression)
unFunc argVal t f args = do
  (v, ts) <- case args of
    [] -> tooFewArgs 1 0 >> return (Nothing, Nothing)
    [x] -> do
      r <- argVal x
      tc <- gets typeChecking
      let result = case r of
            Nothing | tc -> Nothing
            _ -> Just [(snd . unregion) x]
      return (result, Just $ unregion x)
    (x : rs) -> do
      tooManyArgs rs
      r <- argVal x
      let result = case r of
            Nothing -> Nothing
            Just _ -> Just [(snd . unregion) x]
      return (result, Just $ unregion x)
  let res = maybe (fallback "Arg Fail Unfunc") f v
  return $ Typed (fromMaybe TypeAny $ t ts) res

biFunc :: (Arg -> Arg -> Validator a) -> (Maybe (Kind, Expression) -> Maybe (Kind, Expression) -> Maybe Type) -> ([Expression] -> Expression) -> [Arg] -> ValidatorS (Typed Expression)
biFunc argVal t f args = do
  (v, ts) <- case args of
    [] -> tooFewArgs 2 0 >> return (Nothing, (Nothing, Nothing))
    [x] -> tooFewArgs 2 1 >> return (Nothing, (Just $ unregion x, Nothing))
    [x, y] -> do
      r <- argVal x y
      tc <- gets typeChecking
      let result = case r of
            Nothing | tc -> Nothing
            _ -> Just $ map (snd . unregion) [x, y]
      return (result, (Just (unregion x), Just (unregion y)))
    (x : y : rs) -> do
      tooManyArgs rs
      r <- argVal x y
      let result = case r of
            Nothing -> Nothing
            Just _ -> Just $ map (snd . unregion) [x, y]
      return (result, (Just (unregion x), Just (unregion y)))
  let res = maybe (fallback "Arg Fail BiFunct") f v
  return $ Typed (fromMaybe TypeAny $ uncurry t ts) res

triFunc :: (Arg -> Arg -> Arg -> Validator a) -> (Maybe (Kind, Expression) -> Maybe (Kind, Expression) -> Maybe (Kind, Expression) -> Maybe Type) -> ([Expression] -> Expression) -> [Arg] -> ValidatorS (Typed Expression)
triFunc argVal t f args = do
  (v, ts) <- case args of
    [] -> tooFewArgs 3 0 >> return (Nothing, (Nothing, Nothing, Nothing))
    [x] -> tooFewArgs 3 1 >> return (Nothing, (Just $ unregion x, Nothing, Nothing))
    [x, y] -> tooFewArgs 3 2 >> return (Nothing, (Just $ unregion x, Just $ unregion y, Nothing))
    [x, y, z] -> do
      r <- argVal x y z
      tc <- gets typeChecking
      let result = case r of
            Nothing | tc -> Nothing
            _ -> Just $ map (snd . unregion) [x, y, z]
      return (result, (Just (unregion x), Just (unregion y), Just (unregion z)))
    (x : y : z : rs) -> do
      tooManyArgs rs
      r <- argVal x y z
      let result = case r of
            Nothing -> Nothing
            Just _ -> Just $ map (snd . unregion) [x, y, z]
      return (result, (Just (unregion x), Just (unregion y), Just (unregion z)))
  let res = maybe (fallback "Arg Fail Tri") f v
  return $ Typed (fromMaybe TypeAny $ uncurry3 t ts) res
  where
    uncurry3 fn (a, b, c) = fn a b c -- todo export from prelude

tooFewArgs :: Int -> Int -> ValidatorS ()
tooFewArgs n i = void . contextError $ MissingArgsError n i

tooManyArgs :: [RegionTagged a] -> ValidatorS ()
tooManyArgs = mapM_ (\x -> raiseError $ x <!> UnexpectedArg)

checkRVMember :: RegionTagged Type -> RegionTagged Type -> Validator ()
checkRVMember (_, TypeRecord ts) (_, TypeRecordMember nm _) | not . null $ lookup nm ts = return $ pure ()
checkRVMember (_, TypeRecord ts) (r2, r) = do
  raiseTypeError $ r2 <!> TypeError (TypeRecordMember (Name "") ts) r
  return Nothing
checkRVMember (_, TypeVariant ts) (_, TypeVariantMember nm _) | not . null $ lookup nm ts = return $ pure ()
checkRVMember (_, TypeVariant ts) (r2, r) = do
  raiseTypeError $ r2 <!> TypeError (TypeVariantMember (Name "") ts) r
  return Nothing
checkRVMember (r1, t) _ = do
  raiseTypeError $ r1 <!> TypeError (TypeVariant []) t
  return Nothing

type OpValidator = RegionTagged Kind -> RegionTagged Kind -> ValidatorS Type

type OpTyper = Kind -> Kind -> Type

sameToSameV :: (Type -> ValidatorS Type) -> (Type -> Type -> Type) -> OpValidator
sameToSameV tc t (rl, kl) (rr, kr) = do
  setContext rl
  lt <- getValueType kl
  lt' <- tc lt
  setContext rr
  rt <- getValueType kr
  rt' <- tc rt
  let md = mostDefinedS [lt, rt]
  _ <- unifyTypesFailing md (rl, Typed lt' ())
  _ <- unifyTypesFailing md (rr, Typed rt' ())
  return $ t lt' rt'

binOpType :: Lexeme -> OpValidator
binOpType l = case l of
  L_Plus -> sameToSameV number same
  L_Minus -> sameToSameV minusArgs same
  L_Times -> sameToSameV number same
  L_Div -> sameToSameV number same
  L_Mod -> sameToSameV number same
  L_Pow -> sameToSameV number same
  L_Eq -> sameToSameV pure cBool
  L_Neq -> sameToSameV pure cBool
  L_Lt -> sameToSameV orderable cBool
  L_Leq -> sameToSameV orderable cBool
  L_Gt -> sameToSameV orderable cBool
  L_Geq -> sameToSameV orderable cBool
  L_in -> checkIn
  L_And -> sameToSameV bools cBool
  L_Or -> sameToSameV bools cBool
  L_Imply -> sameToSameV bools cBool
  L_Iff -> sameToSameV bools cBool -- b b b
  L_subset -> sameToSameV setLike cBool -- set mset func rel
  L_subsetEq -> sameToSameV setLike cBool
  -- \^^^
  L_supset -> sameToSameV setLike cBool
  -- \^^^^
  L_supsetEq -> sameToSameV setLike cBool
  -- \^^

  L_subsequence -> sameToSameV justSequence cBool -- seq - seq -bool
  L_substring -> sameToSameV justSequence cBool -- seq - seq -bool
  L_intersect -> sameToSameV setLike same
  L_union -> sameToSameV setLike same
  L_LexLt -> sameToSameV pure cBool
  L_LexLeq -> sameToSameV pure cBool
  L_LexGt -> sameToSameV pure cBool
  L_LexGeq -> sameToSameV pure cBool
  L_DotLt -> sameToSameV pure cBool
  L_DotLeq -> sameToSameV pure cBool
  L_DotGt -> sameToSameV pure cBool
  L_DotGeq -> sameToSameV pure cBool
  L_TildeLt -> sameToSameV pure cBool
  L_TildeLeq -> sameToSameV pure cBool
  L_TildeGt -> sameToSameV pure cBool
  L_TildeGeq -> sameToSameV pure cBool
  _ -> bug $ "Unkown operator" <+> pretty (show l)
  where
    cBool = const . const TypeBool
    same a b = mostDefinedS [a, b]
    checkIn :: OpValidator
    checkIn (r1, kl) (r2, kr) = do
      setContext r1
      lt <- getValueType kl
      setContext r2
      rt <- getValueType kr
      case innerTypeOf rt of
        Just t -> unifyTypes t (r1, Typed lt ())
        Nothing -> unless (rt == TypeAny) $ raiseTypeError (r2 <!> ComplexTypeError (T.pack . show $ "Container of " <+> pretty lt) rt)
      return TypeBool

    number :: Type -> ValidatorS Type
    number t = case t of
      TypeInt TagInt -> return t
      TypeInt TagEnum {} -> return t
      TypeAny -> return t
      _ -> TypeAny <$ contextTypeError (ComplexTypeError "Number or Enum" t)
    minusArgs t = case t of
      TypeInt TagInt -> return t
      TypeSet _ -> return t
      TypeMSet _ -> return t
      TypeRelation _ -> return t
      TypeFunction _ _ -> return t
      _ -> TypeAny <$ contextTypeError (ComplexTypeError "Number / set/ mset / relation / function" t)
    orderable t = case t of
      TypeInt TagInt -> return t
      TypeInt TagEnum {} -> return t
      TypeBool -> return t
      TypeAny -> return t
      _ -> TypeAny <$ contextTypeError (ComplexTypeError "Number, Enum or Bool" t)
    justSequence t = case t of
      TypeAny -> return t
      TypeSequence _ -> return t
      _ -> TypeAny <$ contextTypeError (TypeError (TypeSequence TypeAny) t)
    bools t = case t of
      TypeAny -> return t
      TypeBool -> return t
      _ -> TypeAny <$ contextTypeError (TypeError TypeBool t)
    setLike t = case t of
      TypeAny -> return t
      TypeMSet _ -> return t
      TypeSet _ -> return t
      TypeFunction _ _ -> return t
      TypeRelation _ -> return t
      _ -> TypeAny <$ contextTypeError (ComplexTypeError "Set MSet funcition or relation" t)