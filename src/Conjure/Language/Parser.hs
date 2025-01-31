{-# LANGUAGE RecordWildCards #-}

module Conjure.Language.Parser
  ( runLexerAndParser,
    lexAndParse,
    parseIO,
    parseModel,
    parseTopLevels,
    parseExpr,
    parseDomain,
    parseDomainWithRepr,
    prettyPrintWithChecks,
    Pipeline,
    PipelineError (..),
    runPipeline,
  )
where

import Conjure.Language.AST.ASTParser (ParserError, parseProgram, runASTParser)
import Conjure.Language.AST.ASTParser qualified as P
import Conjure.Language.AST.Helpers qualified as P
import Conjure.Language.AST.Reformer (HighLevelTree (..), flatten)
import Conjure.Language.AST.Syntax (DomainNode, ProgramTree)
import Conjure.Language.AST.Syntax qualified as S
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Lexer (ETokenStream, LexerError)
import Conjure.Language.Lexer qualified as L
import Conjure.Language.Pretty
import Conjure.Language.Type (Type (..))
import Conjure.Language.Validator ((?=>))
import Conjure.Language.Validator qualified as V
import Conjure.Prelude
import Conjure.UI.ErrorDisplay (showDiagnosticsForConsole)
import Data.Text qualified as T
import Data.Void (Void)
import Prettyprinter qualified as Pr
import Text.Megaparsec (Parsec)

type Pipeline a b = ((Parsec Void ETokenStream) a, a -> V.ValidatorS b, Bool)

data PipelineError = LexErr LexerError | ParserError ParserError | ValidatorError Doc
  deriving (Show)

instance Pretty PipelineError where
  pretty (ValidatorError d) = d
  pretty e = pretty $ show e

lexAndParse :: (HighLevelTree a) => P.Parser a -> Text -> Either PipelineError a
lexAndParse parse t = do
  lr <- either (Left . LexErr) Right $ L.runLexer t Nothing
  either (Left . ParserError) Right $ runASTParser parse lr

runPipeline :: (HighLevelTree a) => Pipeline a b -> (Maybe FilePath, Text) -> Either PipelineError b
runPipeline (parse, val, tc) (fp, txt) = do
  lexResult <- either (Left . LexErr) Right $ L.runLexer txt fp
  parseResult <- either (Left . ParserError) Right $ runASTParser parse lexResult
  let fileNameText = T.pack <$> fp
  let x = V.runValidator (val parseResult) (V.initialState parseResult fileNameText) {V.typeChecking = tc}
  case x of
    (m, ds, _) | not $ any V.isError ds -> Right m
    (_, ves, _) -> Left $ ValidatorError $ pretty (showDiagnosticsForConsole ves fp txt)

parseModel :: Pipeline ProgramTree Model
parseModel = (parseProgram, V.validateModel, True)

parseIO :: (MonadFailDoc m, HighLevelTree i) => Pipeline i a -> String -> m a
parseIO p s = do
  case runPipeline p $ (Just "IO", T.pack s) of
    Left err -> failDoc $ pretty $ show err
    Right x -> return x

-- --------------------------------------------------------------------------------
-- -- Actual parsers --------------------------------------------------------------
-- --------------------------------------------------------------------------------

parseTopLevels :: Pipeline [S.StatementNode] [Statement]
parseTopLevels = (P.parseTopLevels, V.validateProgramTree, False)

parseDomain :: Pipeline DomainNode (Domain () Expression)
parseDomain = (P.parseDomain, fmap V.untype . V.validateDomain, True)

parseDomainWithRepr :: Pipeline DomainNode (Domain HasRepresentation Expression)
parseDomainWithRepr = (P.parseDomain, fmap V.untype . V.validateDomainWithRepr, True)

parseExpr :: Pipeline S.ExpressionNode Expression
parseExpr = (P.parseExpression, \x -> V.validateExpression x ?=> V.exactly TypeAny, True)

runLexerAndParser :: (HighLevelTree n) => Pipeline n a -> String -> T.Text -> Either Doc a
runLexerAndParser p file inp = case runPipeline p (Just file, inp) of
  Left pe -> Left $ pretty pe
  Right a -> Right a

prettyPrintWithChecks :: (MonadFailDoc m) => Text -> m (Pr.Doc ann)
prettyPrintWithChecks src = do
  v <- case lexAndParse parseProgram src of
    Left pe -> failDoc $ pretty $ show pe
    Right pt -> return pt
  return $ (if V.isSyntacticallyValid V.validateModel v then Pr.pretty else partialPretty) v

partialPretty :: ProgramTree -> Pr.Doc ann
partialPretty (S.ProgramTree lv sns lt) =
  Pr.vcat
    [ langVer,
      Pr.vcat $ map pTopLevel sns,
      Pr.pretty lt
    ]
  where
    langVer = case lv of
      Nothing -> "language Essence 1.3"
      Just _ -> if V.isSyntacticallyValid V.validateLanguageVersion lv then Pr.pretty lv else fallback lv
    fallback :: (HighLevelTree a) => a -> Pr.Doc ann
    fallback v = Pr.pretty $ L.reformList $ flatten v
    pTopLevel st = if V.isSyntacticallyValid V.validateStatement st then Pr.pretty st else fallback st
