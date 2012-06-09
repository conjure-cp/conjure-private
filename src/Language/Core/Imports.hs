-- mostly for library dependencies.
-- defines a specialised prelude. kind-of.
-- things in Language.Core.* should import this.
-- also see "Language.Core"

module Language.Core.Imports
    ( module X
    , stringToDoc, stringToText
    , textToDoc, textToString
    , prettyListDoc, parensIf
    , setEq
    , padRight, padLeft, padCenter
    ) where

import Control.Applicative    as X ( Applicative, (<$>), (<*>), (<*), (*>) )
import Control.Arrow          as X ( first, second )

import Control.Monad             as X ( MonadPlus, void, mzero, msum, when, unless, zipWithM
                                      , (<=<), foldM )
import Control.Monad.Identity    as X ( Identity, runIdentity )
import Control.Monad.Error       as X ( MonadError, throwError, catchError, ErrorT, runErrorT )
import Control.Monad.Reader      as X ( MonadReader )
import Control.Monad.State       as X ( MonadState, gets, modify )
import Control.Monad.Writer      as X ( MonadWriter, tell, listen, WriterT, runWriterT, execWriterT )
import Control.Monad.List        as X ( ListT, runListT )
import Control.Monad.IO.Class    as X ( MonadIO, liftIO )
import Control.Monad.Trans.Class as X ( MonadTrans, lift )
import Control.Monad.Trans.Maybe as X ( MaybeT(..), runMaybeT )

import Data.Default      as X ( Default, def )
import Data.List         as X ( intersperse, minimumBy, nub, groupBy, sortBy )
import Data.Maybe        as X ( catMaybes, listToMaybe )
import Data.Monoid       as X ( Monoid, mappend, mconcat, Any(..) )
import Data.Ord          as X ( comparing )
import Data.Foldable     as X ( forM_ )
import Data.Traversable  as X ( forM )

import Data.Text        as X ( Text, stripPrefix )
import Text.PrettyPrint as X ( Doc, nest, punctuate, sep, vcat, (<+>), ($$) )

import Safe as X ( headNote, tailNote )

import Utils as X ( ppShow, ppPrint )

import Nested as X

import Debug.Trace as X ( trace )

import qualified Data.Text as T
import qualified Text.PrettyPrint as Pr
import qualified Data.Set as S


textToDoc :: Text -> Doc
textToDoc = stringToDoc . textToString

textToString :: Text -> String
textToString = T.unpack

stringToDoc :: String -> Doc
stringToDoc = Pr.text

stringToText :: String -> Text
stringToText = T.pack

prettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
prettyListDoc wrap punc = wrap . sep . punctuate punc

setEq :: Ord a => [a] -> [a] -> Bool
setEq xs ys = S.fromList xs == S.fromList ys

parensIf :: Bool -> Doc -> Doc
parensIf = wrapIf Pr.parens
    where
        wrapIf :: (Doc -> Doc) -> Bool -> Doc -> Doc
        wrapIf wrap c = if c then wrap else id

padRight :: Int -> Char -> String -> String
padRight n ch s = s ++ replicate (n - length s) ch

padLeft :: Int -> Char -> String -> String
padLeft n ch s = replicate (n - length s) ch ++ s

padCenter :: Int -> Char -> String -> String
padCenter n ch s = replicate (div diff 2) ch ++ s ++ replicate (diff - div diff 2) ch
    where
        diff = n - length s
