module Language.E.Parser.EssenceFile
    ( parseSpec
    , parseExpr
    , parseDomain
    , parseRuleRepr
    , parseRuleRefn
    , parseTopLevels
    , lexAndParse, lexAndParseIO
    , inCompleteFile
    ) where

import Conjure.Prelude
import Language.E.Parser.Imports
import Language.E.Parser.EssenceFile.Domain ( parseDomain )
import Language.E.Parser.EssenceFile.Expr ( parseExpr )
import Language.E.Parser.EssenceFile.Declaration ( parseTopLevels )

import Language.E.Definition
-- import Language.E.Helpers
import Language.E.Lexer ( Lexeme(..) )

import Text.Parsec.Combinator ( optionMaybe, sepBy1 )

import qualified Data.Text as T


_testParsePrint :: T.Text -> IO ()
_testParsePrint = _testParsePrint' (inCompleteFile parseExpr)

lexAndParseIO :: Parser a -> T.Text -> IO a
lexAndParseIO p t = do
    case lexAndParse p t of
        Left  e -> error  $ show e
        Right x -> return x

lexAndParse :: Parser a -> T.Text -> Either Doc a
lexAndParse p t = runLexerAndParser p "" t

parseSpec :: Parser Spec
parseSpec = inCompleteFile $ do
    let
        pLanguage :: Parser LanguageVersion
        pLanguage = do
            l  <- lexeme L_language *> identifierText
            is <- sepBy1 integer dot
            return (LanguageVersion (Name l) (map fromInteger is))
    l  <- pLanguage
    xs <- many parseTopLevels
    return $ Spec l $ listAsStatement $ concat xs

parseRuleRefn :: T.Text -> Parser [RuleRefn]
parseRuleRefn t = inCompleteFile $ do
    level <- optionMaybe (brackets (fromInteger <$> integer))
    let
        one = do
            pattern   <- parseExpr
            templates <- some (lexeme L_SquigglyArrow >> parseExpr)
            locals    <- concat <$> many parseTopLevels
            return RuleRefn { ruleRefnName = Name t
                            , ruleRefnLevel = level
                            , ruleRefnPattern = pattern
                            , ruleRefnTemplates = templates
                            , ruleRefnLocals = locals
                            }
    some one

parseRuleReprCase :: Parser RuleReprCase
parseRuleReprCase = do
    lexeme L_CaseSeparator
    dom    <- parseDomain
    mcons  <- optionMaybe (lexeme L_SquigglyArrow >> parseExpr)
    locals <- concat <$> many parseTopLevels
    return (RuleReprCase dom mcons locals)


parseRuleRepr :: T.Text -> Parser RuleRepr
parseRuleRepr t = inCompleteFile $ do
    let arr i = lexeme L_SquigglyArrow >> i
    nmRepr <- arr identifierText
    domOut <- arr parseDomain
    mcons  <- optionMaybe $ arr parseExpr
    locals <- concat <$> many parseTopLevels
    cases  <- some parseRuleReprCase
    return ( RuleRepr (Name t)
             (Name nmRepr)
             domOut
             mcons
             locals
             cases )

