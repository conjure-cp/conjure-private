{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Conjure.Language.Lexer
    ( Lexeme(..)
    , LexemePos(..)
    , textToLexeme,
    ETok(..),
    Offsets(..),
    Reformable(..),
    prettySplitComments,
    eLex,
    reformList,
    tokenSourcePos,
    sourcePosAfter,
    totalLength,
    trueLength,
    trueStart,
    tokenOffset,
    tokenStartOffset,
    sourcePos0,
    nullBefore,
    LexerError(..),
    runLexer,
    ETokenStream(..)
    , lexemeText
    ) where

import Conjure.Language.Lexemes
import Conjure.Prelude hiding (many, some,Text)
import Data.Char (isAlpha, isAlphaNum)
import Data.Void

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Data.List (splitAt)
import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as L
import Prelude (read)
import qualified Prettyprinter as Pr
import Conjure.Prelude hiding (some,many)


import Text.Megaparsec.Stream ()


data LexemePos = LexemePos
                    Lexeme          -- the lexeme
                    SourcePos       -- source position, the beginning of this lexeme
                    SourcePos       -- source position, just after this lexeme, including whitespace after the lexeme
    deriving (Show,Eq, Ord)

sourcePos0 :: SourcePos
sourcePos0 = SourcePos "" (mkPos  1) (mkPos 1)

class Reformable a where
    reform :: a -> L.Text

instance Reformable ETok where
    reform e | totalLength e == 0 = ""
    reform (ETok{capture=cap,trivia=triv}) = L.append  (L.concat $ map showTrivia triv) (L.fromStrict cap)
        where
            showTrivia :: Trivia -> L.Text
            showTrivia x = case x of
              WhiteSpace txt    -> L.fromStrict txt
              LineComment txt   -> L.fromStrict txt
              BlockComment txt  -> L.fromStrict txt

reformList :: (Traversable t ,Reformable a) => t a -> L.Text
reformList =  L.concat  . map reform . toList

emojis :: [Char]
emojis =
    concat
        [ ['\x1f600' .. '\x1F64F']
        , ['\x1f300' .. '\x1f5ff']
        , ['\x1f680' .. '\x1f999']
        , ['\x1f1e0' .. '\x1f1ff']
        ]

isIdentifierFirstLetter :: Char -> Bool
isIdentifierFirstLetter ch = isAlpha ch || ch `elem` ("_" :: String) || ch `elem` emojis

isIdentifierLetter :: Char -> Bool
isIdentifierLetter ch = isAlphaNum ch || ch `elem` ("_'" :: String) || ch `elem` emojis

data Offsets = Offsets {
        oStart::Int, -- the starting offset of the token (including whitespace)
        oTotalLength::Int, -- (the total length of the the token)
        oTokenLength::Int, -- (the length of the token excluding trivia)
        oTrueStart :: SourcePos, -- start pos of the token
        oSourcePos::SourcePos, -- start pos of the lexeme
        oEndPos::SourcePos}
    deriving (Show, Eq, Ord , Data)
type Lexer = Parsec Void Text

-- type Lexer = Parsec Void Text ETokenStream

data Trivia = WhiteSpace Text | LineComment Text | BlockComment Text
    deriving (Show, Eq, Ord , Data)

data ETok = ETok
    { offsets :: Offsets
    , trivia :: [Trivia]
    , lexeme :: Lexeme
    , capture :: Text
    }
    deriving (Eq, Ord,Show , Data)

instance Pr.Pretty ETok where
    pretty = Pr.unAnnotate . uncurry (Pr.<>) .  prettySplitComments

prettySplitComments :: ETok -> (Pr.Doc ann, Pr.Doc ann)
prettySplitComments (ETok _ tr _ capture) = (Pr.hcat [Pr.pretty t Pr.<> Pr.hardline | LineComment t <- tr],Pr.pretty capture)


totalLength :: ETok -> Int
totalLength = oTotalLength . offsets

trueLength :: ETok -> Int
trueLength = oTokenLength . offsets

-- tokenStart :: ETok -> Int
-- tokenStart (ETok{offsets = (Offsets _  s _ _ _)}) = s
tokenOffset :: ETok -> Int
tokenOffset = oStart . offsets
tokenStartOffset :: ETok -> Int
tokenStartOffset t = oStart o + (oTotalLength o - oTokenLength o)
    where o = offsets t

trueStart :: ETok -> SourcePos
trueStart = oTrueStart . offsets

tokenSourcePos :: ETok -> SourcePos
tokenSourcePos = oSourcePos . offsets

sourcePosAfter :: ETok -> SourcePos
sourcePosAfter = oEndPos . offsets

makeToken :: Offsets -> [Trivia] -> Lexeme -> Text -> ETok
makeToken = ETok

--make an empty token that precedes the given token with the given lexeme
nullBefore :: Lexeme -> ETok -> ETok
nullBefore lex tok = ETok offs [] lex ""
    where
        sp = tokenSourcePos tok
        offs =  Offsets (tokenStartOffset tok) 0 0 sp sp sp
newtype LexerError = LexerError String
    deriving (Show)

runLexer :: Text -> Maybe FilePath -> Either LexerError ETokenStream
runLexer txt fp = case runParser eLex (fromMaybe "Lexer" fp) txt of
  Left peb -> Left $ LexerError $ errorBundlePretty peb
  Right ets -> Right $ ETokenStream txt ets


eLex :: Lexer [ETok]
eLex =
    do
        main <- many $ try aToken
        end <- pEOF
        return $ main ++ [end]

aToken :: Lexer ETok
aToken = do
    start <- getOffset
    startPos <- getSourcePos
    whitespace <- pTrivia
    tokenOffset_ <- getOffset
    tokenStart <- getSourcePos
    (tok,cap) <- aLexeme
    tokenEnd <- getOffset
    endPos <- getSourcePos
    return $ makeToken (Offsets start (tokenEnd - start) (tokenEnd - tokenOffset_) startPos tokenStart endPos) whitespace tok cap

pEOF :: Lexer ETok
pEOF = do
    start <- getOffset
    startPos <- getSourcePos
    whitespace <- pTrivia
    wse <- getOffset
    tokenStart <- getSourcePos
    eof
    tokenEnd <- getOffset
    endPos <- getSourcePos
    return $ makeToken (Offsets start (tokenEnd - start) (tokenEnd - wse) startPos tokenStart endPos) whitespace L_EOF ""


aLexeme :: Lexer (Lexeme,Text)
aLexeme = aLexemeStrict <|> pFallback

aLexemeStrict :: Lexer (Lexeme,Text)
aLexemeStrict =
    try
        pNumber
        <|> try  (choice (map pLexeme lexemes) <?> "Lexeme")
        <|> try pIdentifier
        <|> try pQuotedIdentifier
        <|> try pMetaVar


pNumber :: Lexer (Lexeme,Text)
pNumber = do
    v <- takeWhile1P Nothing (`elem` ['1','2','3','4','5','6','7','8','9','0'])
    let n = read $ T.unpack v
    return (LIntLiteral n,v)
    <?> "Numeric Literal"

pMetaVar :: Lexer (Lexeme,Text)
pMetaVar = do
    amp <- chunk "&"
    (_,cap) <- pIdentifier
    return (LMetaVar cap,amp `T.append` cap)

pIdentifier :: Lexer (Lexeme,Text)
pIdentifier = do
    firstLetter <- takeWhile1P Nothing isIdentifierFirstLetter
    rest <- takeWhileP Nothing isIdentifierLetter
    let ident = T.append firstLetter rest
    -- traceM $ T.unpack . T.pack $ map chr $ map ord $ T.unpack ident
    return ( LIdentifier ident, ident)
    <?> "Identifier"

pQuotedIdentifier :: Lexer (Lexeme,Text)
pQuotedIdentifier = do
    l <- quoted
    return (LIdentifier l,l)

pFallback :: Lexer (Lexeme,Text)
pFallback = do
    q <- T.pack <$> someTill anySingle (lookAhead $ try somethingValid)
    return (LUnexpected  q,q)
  where
    somethingValid :: Lexer ()
    somethingValid = void pTrivia <|> void aLexemeStrict <|> eof

pLexeme :: (Text, Lexeme) -> Lexer (Lexeme,Text)
pLexeme (s, l) = do
    tok <- string s
    notFollowedBy $ if isIdentifierLetter $ T.last tok then nonIden else empty
    return (l,tok)
    <?> "Lexeme :" ++ show l
    where
        nonIden = takeWhile1P Nothing isIdentifierLetter

pTrivia :: Lexer [Trivia]
pTrivia = many (whiteSpace <|> lineComment <|> blockComment)

whiteSpace :: Lexer Trivia
whiteSpace = do
    s <- some spaceChar
    return $ WhiteSpace $ T.pack s

quoted :: Lexer Text
quoted = do
    open <- char '\"'
    (body,end) <- manyTill_ anySingle $ char '\"'
    return $ T.pack  $ open:body++[end]

lineEnd :: Lexer [Char]
lineEnd = T.unpack <$> eol <|> ( eof >> return [])

lineComment :: Lexer Trivia
lineComment = do
    _ <- try (chunk "$")
    (text,end) <- manyTill_ anySingle lineEnd
    return $ LineComment $ T.pack ('$' : text++end)

blockComment :: Lexer Trivia
blockComment = do
    _ <- try (chunk "/*")
    text <- manyTill L.anySingle (lookAhead (void(chunk "*/") <|>eof))
    cl <- optional $ chunk "*/"
    let cl' = fromMaybe "" cl
    return $ BlockComment $ T.concat ["/*",T.pack text ,cl' ]


data ETokenStream = ETokenStream
    { streamSourceText :: Text
    , streamTokens :: [ETok]
    }
instance Stream ETokenStream where
    type Token ETokenStream = ETok
    type Tokens ETokenStream = [ETok]
    tokenToChunk _ x = [x]
    tokensToChunk _ xs = xs
    chunkToTokens _ = id
    chunkLength _ = length
    chunkEmpty _ [] = True
    chunkEmpty _ _ = False
    take1_ :: ETokenStream -> Maybe (Token ETokenStream, ETokenStream)
    take1_ (ETokenStream _ (x : xs)) = Just (x, buildStream xs)
    take1_ (ETokenStream _ []) = Nothing
    takeN_ :: Int -> ETokenStream -> Maybe (Tokens ETokenStream, ETokenStream)
    takeN_ n xs | n <= 0 = Just ([], xs)
    takeN_ _ (ETokenStream _ []) = Nothing
    takeN_ n (ETokenStream _ xs) = Just (take n xs, buildStream $ drop n xs)
    takeWhile_ :: (Token ETokenStream -> Bool) -> ETokenStream -> (Tokens ETokenStream, ETokenStream)
    takeWhile_ p (ETokenStream _ xs) =
        (a, buildStream b)
      where
        (a, b) = span p xs

-- (takeWhile p xs,ETokenStream $ dropWhile p xs)

buildStream :: [ETok] -> ETokenStream
buildStream xs = case NE.nonEmpty xs of
    Nothing -> ETokenStream "" xs
    Just _ -> ETokenStream (T.pack "showTokens pxy s") xs

instance VisualStream ETokenStream where
    showTokens _ =  L.unpack . reformList
    tokensLength _ = sum . fmap trueLength

-- https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
instance TraversableStream ETokenStream where
    reachOffset o PosState{..} =
        ( Just (prefix ++ restOfLine)
        , PosState
            { pstateInput = buildStream post
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = prefix
            }
        )
      where
        prefix =
            if sameLine
                then pstateLinePrefix ++ preLine
                else preLine
        sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
        newSourcePos =
            case post of
                [] -> pstateSourcePos
                (x : _) -> tokenSourcePos x
        (pre, post) :: ([ETok], [ETok]) = splitAt (o - pstateOffset) (streamTokens pstateInput)
        (preStr, postStr) = (maybe "" (showTokens pxy) (NE.nonEmpty pre), maybe "" (showTokens pxy) (NE.nonEmpty post))
        preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
        restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy ETokenStream
pxy = Proxy



-- instance Show ETok where
--     show (ETok _ _ _ q) = show q



-- instance TraversableStream ETokenStream where
--     reachOffset i s = (Nothing, s)
--     reachOffsetNoLine i s = s 