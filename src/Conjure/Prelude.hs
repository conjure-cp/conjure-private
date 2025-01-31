{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}


module Conjure.Prelude
    ( module X
    , stringToDoc
    , textToDoc
    , padRight, padLeft, padCenter
    , pairWithContents
    , withRest, withAfter, withBefore
    , T.Text, stringToText, textToString
    , sameLength
    , concatMapM, concatForM
    , timedIO, timedPutStrLn
    , tick
    , isLeft, isRight
    , tracing
    , allCombinations
    , sortOn, sortNub
    , maybeRead
    , padShowInt
    , decodeFromFile
    , RandomM(..)
    , fst3, snd3, thd3
    , fst4, snd4, thd4, fourth4
    , (|>)
    , allNats
    , jsonOptions
    , Proxy(..)
    , MonadFailDoc(..), na
    , MonadFail (..)
    , allContexts, ascendants
    , dropExtension, dropDirs
    , MonadLog(..), LogLevel(..), runLoggerPipeIO, ignoreLogs
    , logInfo, logWarn, logDebug, logDebugVerbose
    , histogram
    , ExceptT(..)
    , sh
    , scope
    , getAllDirs, getAllFiles, getAllFilesWithSuffix
    , removeFileIfExists, readFileIfExists, removeDirectoryIfExists
    , setRandomSeed, randomRIO
    , nchoosek
    , JSONValue
    , isTopMostZ
    , getDirectoryContents
    , RunStateAsWriter, runStateAsWriterT, sawTell
    , stripPostfix
    , Doc
    ) where

import GHC.Err as X ( error )
import GHC.Stack as X ( HasCallStack )

-- basic data types
import Data.Bool as X ( Bool(..), (||), (&&), not, otherwise )
import Data.Int as X ( Int )
import GHC.Integer as X ( Integer )
import GHC.Float as X ( sqrt, (**) )
import GHC.Exts as X ( Double )
import GHC.Real as X ( Fractional(..), Integral(..), fromIntegral, (^), Real(..), round, odd, even )
import GHC.Enum as X ( Enum(..), Bounded(..) )
import Data.Char as X ( Char, toLower, isSpace )
import Data.String as X ( String, IsString(..) )

-- basic type classes
import Data.Eq as X ( Eq(..) )
import Data.Ord as X ( Ord(..), Ordering(..), comparing )
import Text.Show as X ( Show(..), showString, showParen )
import Text.Read as X ( Read(..), reads )
import GHC.Num as X ( Num(..) )

-- some more type classes
import GHC.Generics as X ( Generic )
import Data.Functor as X ( Functor(..) )
import Control.Applicative as X ( Applicative(..), (<$>), (<*), (*>), (<|>), many, some, optional )
import qualified Control.Monad ( fail )
import Control.Monad.Fail

import Control.Monad                as X ( Monad(return, (>>), (>>=))
                                         , (<=<), (>=>), (=<<), ap, join
                                         , guard, void, when, unless
                                         , zipWithM, zipWithM_, foldM, filterM, replicateM
                                         , MonadPlus(..), mzero, msum)
import Control.Monad.Trans.Class    as X ( MonadTrans(lift) )
import Control.Monad.Identity       as X ( Identity, runIdentity )
import Control.Monad.IO.Class       as X ( MonadIO, liftIO )
import Control.Monad.State.Strict   as X ( MonadState, StateT(..), get, gets, modify
                                         , evalStateT, runStateT, evalState, runState )
import Control.Monad.State.Strict ( put ) -- only for defining instances
import Control.Monad.Trans.Identity as X ( IdentityT(..) )
import Control.Monad.Trans.Maybe    as X ( MaybeT(..), runMaybeT )
import Control.Monad.Writer.Strict  as X ( MonadWriter(listen, tell), WriterT(runWriterT), execWriterT, runWriter )
import Control.Monad.Reader         as X ( MonadReader(ask), ReaderT(..), runReaderT, asks )


import Control.Arrow                as X ( first, second, (***), (&&&) )
import Control.Category             as X ( (<<<), (>>>) )


import Data.Data         as X ( Data, Typeable )
import Data.Default      as X ( Default, def )
import Data.Either       as X ( Either(..), either, lefts, rights, partitionEithers, fromRight, fromLeft )
import Data.Function     as X ( id, const, flip, on, ($), (.) )
import Data.List         as X ( (\\), intercalate, intersperse, minimumBy, nub, nubBy
                              , group, groupBy, sort, sortBy
                              , genericLength, genericIndex, genericTake
                              , isSuffixOf, isPrefixOf, isInfixOf
                              , stripPrefix
                              , subsequences, transpose, elemIndex
                              , replicate, length
                              , (++), map, null, reverse, lookup, elem, unlines, words
                              , zipWith, concatMap, lines, notElem, foldr
                              , sum, product, unzip, zip, zip3, foldr1, foldl
                              , unzip3, repeat, unwords, intersect
                              , take, drop
                              , takeWhile, dropWhile, span
                              , head, init, tail, last
                              , inits, tails
                              , findIndex
                              , filter, partition
                              , cycle
                              )
import Data.List.Split   as X ( splitOn, chunksOf )
import Data.Maybe        as X ( Maybe(..), catMaybes, listToMaybe, fromMaybe, maybe, maybeToList, mapMaybe
                              , isNothing, isJust )
import Data.Semigroup    as X ( Semigroup )
import Data.Monoid       as X ( Monoid(mempty, mappend), mconcat, Any(..) )
import Data.Tuple        as X ( fst, snd, swap, curry, uncurry )

import Data.Foldable     as X ( Foldable, mapM_, forM_, sequence_, fold, foldMap, toList, maximum, minimum
                              , and, or, all, any
                              , concat
                              )
import Data.Traversable  as X ( Traversable, mapM, forM, sequence )

import System.IO as X ( FilePath, IO, putStr, putStrLn, print, writeFile, appendFile, getLine )
import System.IO.Error ( isDoesNotExistError, ioeGetErrorType )
import Control.Exception as X ( catch, throwIO, SomeException )

import Data.Proxy as X ( Proxy(..) )

-- template-haskell
import qualified Language.Haskell.TH as TH ( Q )

-- safe
import Safe as X ( at, atNote, atMay, readMay, readNote, headNote, fromJustNote )

-- hashable
import Data.Hashable as X ( Hashable(..), hash )

-- cereal
import Data.Serialize as X ( Serialize, encode, decode )
import qualified Data.Serialize

-- aeson
import Data.Aeson as X ( ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON )
import qualified Data.Aeson.Types as JSON

-- QuickCheck
import Test.QuickCheck ( Gen )


-- uniplate
import Data.Generics.Uniplate.Data as X
    ( transform, transformBi
    , transformM, transformBiM
    , descend, descendM
    , descendBi, descendBiM
    , rewriteBi
    , universe, universeBi
    , children, childrenBi
    , uniplate
    )
import Data.Generics.Uniplate.Zipper as Zipper ( Zipper, down, right, up, hole )

-- pipes
import qualified Pipes

-- shelly
import Shelly ( Sh, shelly, print_stdout, print_stderr )

-- ansi-terminal
import System.Console.ANSI ( clearScreen, setCursorPosition )

import System.Random ( StdGen, mkStdGen, setStdGen, randomRIO )

import qualified Data.ByteString as ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint.Annotated.HughesPJ as Pr

-- containers
import qualified Data.Set as S

-- strict
import System.IO.Strict ( readFile )

import System.Directory as X
    ( doesDirectoryExist, doesFileExist
    , createDirectoryIfMissing
    )
import System.Directory ( removeDirectoryRecursive, removeFile )
import qualified System.Directory ( getDirectoryContents )
import System.Environment as X ( getArgs )
import System.FilePath as X ( (</>) )
import System.CPUTime ( getCPUTime )

-- time
import Data.Time.Clock ( getCurrentTime )

-- timeit
import System.TimeIt as X ( timeIt, timeItNamed )

import Debug.Trace as X ( trace, traceM )
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import Text.PrettyPrint.Annotated.HughesPJ ((<+>))
-- import Prettyprinter (PageWidth(AvailablePerLine))
-- import Prettyprinter.Render.String (renderString)



type EssenceDocAnnotation = ()

type Doc = Pr.Doc EssenceDocAnnotation

-- instance Eq Doc where
--     a == b = show a == show b
--compats
-- hang :: Doc -> Int ->Doc -> Doc
-- hang a n b = a <+> Pr.hang n b

-- hcat :: [Doc] -> Doc 
-- hcat = Pr.hcat

-- fsep :: [Doc] -> Doc
-- fsep = Pr.fillSep

-- cat :: [Doc] -> Doc
-- cat = Pr.cat

-- nest :: Int -> Doc -> Doc
-- nest = Pr.nest


tracing :: Show a => String -> a -> a
tracing s a = trace ("tracing " ++ s ++ ": " ++ show a) a

stringToText :: String -> T.Text
stringToText = T.pack

textToString :: T.Text -> String
textToString = T.unpack

stringToDoc :: String -> Doc
stringToDoc = Pr.text

textToDoc :: T.Text -> Doc
textToDoc = stringToDoc . textToString

padRight :: Int -> Char -> String -> String
padRight n ch s = s ++ replicate (n - length s) ch

padLeft :: Int -> Char -> String -> String
padLeft n ch s = replicate (n - length s) ch ++ s

padCenter :: Int -> Char -> String -> String
padCenter n ch s = replicate (div diff 2) ch ++ s ++ replicate (diff - div diff 2) ch
    where
        diff = n - length s

pairWithContents :: FilePath -> IO (FilePath, T.Text)
pairWithContents fp = do
    con <- T.readFile fp
    return (fp,con)

-- the fst component: generate a list yielding the elements of the input list in order
-- the snd component: is all those elements except the fst.
withRest :: [a] -> [(a,[a])]
withRest [] = []
withRest (x:xs) = (x,xs) : map (second (x:)) (withRest xs)

-- generate a list yielding the elements of the input list in order in the fst component.
-- the snd component is all those elements to the right of fst.
withAfter :: [a] -> [(a,[a])]
withAfter [] = []
withAfter (x:xs) = (x,xs) : withAfter xs

-- generate a list yielding the elements of the input list in order in the fst component.
-- the snd component is all those elements to the left of fst.
withBefore :: [a] -> [(a,[a])]
withBefore = reverse . withAfter . reverse


sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

concatForM :: (Functor m, Monad m) => [a] -> (a -> m [b]) -> m [b]
concatForM f xs = concatMapM xs f

timedIO :: IO a -> IO (a, Double)
timedIO io = do
    start <- getCPUTime
    a <- io
    end   <- getCPUTime
    let diff = fromIntegral (end - start) / ((10 :: Double) ^ (12 :: Int))
    return (a, diff)

tick :: MonadIO m => Doc -> m ()
tick msg = do
    time <- liftIO getCPUTime
    let seconds = fromIntegral time / ((10 :: Double) ^ (12 :: Int))
    traceM $ show seconds ++ "\t" ++ show msg

timedPutStrLn :: String -> IO ()
timedPutStrLn str = do
    t <- getCurrentTime
    putStrLn (unwords [show t, str])

isLeft :: Either a b -> Bool
isLeft Left{} = True
isLeft _      = False

isRight :: Either a b -> Bool
isRight Right{} = True
isRight _       = False

allCombinations :: [(a,[b])] -> [[(a,b)]]
allCombinations [] = [[]]
allCombinations ((x,ys):qs) = concat [ [ (x,y) : ws | y <- ys ] | ws <- allCombinations qs ]

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (comparing f)

sortNub :: Ord a => [a] -> [a]
sortNub = S.toList . S.fromList


instance Serialize T.Text where
    put = Data.Serialize.put . T.unpack
    get = T.pack <$> Data.Serialize.get

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

padShowInt :: Show a => Int -> a -> String
padShowInt n i = let s = show i in replicate (n - length s) '0' ++ s

decodeFromFile :: (Serialize a, MonadFail IO) => FilePath -> IO a
decodeFromFile path = do
    con <- ByteString.readFile path
    either (fail) return (decode con)

class Monad m => RandomM m where
    get_stdgen :: m StdGen
    set_stdgen :: StdGen -> m ()


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

thd4 :: (a,b,c,d) -> c
thd4 (_,_,c,_) = c

fourth4 :: (a,b,c,d) -> d
fourth4 (_,_,_,d) = d

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

allNats :: [Integer]
allNats = [1..]


jsonOptions :: JSON.Options
jsonOptions = JSON.defaultOptions
    { JSON.allNullaryToStringTag = True
    , JSON.omitNothingFields = True
    , JSON.sumEncoding = JSON.ObjectWithSingleField
    }


class (Functor m, Applicative m, Monad m,MonadFail m) => MonadFailDoc m where
    failDoc :: Doc -> m a

na :: MonadFailDoc m => Doc -> m a
na message = failDoc ("N/A:" <+> message)

instance MonadFail Identity where
    fail = error
instance MonadFailDoc Identity where
    failDoc = Control.Monad.fail . show

instance MonadFailDoc Maybe where
    failDoc = const Nothing

instance (a ~ Doc) => MonadFailDoc (Either a) where
    failDoc = Left

instance MonadFail (Either Doc) where
    fail = failDoc . stringToDoc

instance MonadFailDoc m => MonadFailDoc (IdentityT m) where
    failDoc = lift . failDoc

instance (Functor m, Monad m) => MonadFailDoc (MaybeT m) where
    failDoc = const $ MaybeT $ return Nothing

instance (MonadFailDoc m) => MonadFailDoc (ExceptT m) where
    failDoc = ExceptT . return . Left

instance (MonadFailDoc m) => MonadFailDoc (StateT st m) where
    failDoc = lift . failDoc

instance (MonadFailDoc m, Monoid w) => MonadFailDoc (WriterT w m) where
    failDoc = lift . failDoc

instance MonadFailDoc m => MonadFailDoc (ReaderT r m) where
    failDoc = lift . failDoc

instance MonadFail Gen where
    fail = error
instance MonadFailDoc Gen where
    failDoc = Control.Monad.fail . show


instance MonadFailDoc m => MonadFailDoc (Pipes.Proxy a b c d m) where
    failDoc = lift . failDoc

instance MonadFailDoc TH.Q where
    failDoc = Control.Monad.fail . show


newtype ExceptT m a = ExceptT { runExceptT :: m (Either Doc a) }

instance (Functor m) => Functor (ExceptT m) where
    fmap f = ExceptT . fmap (fmap f) . runExceptT

instance (Functor m, Monad m) => Applicative (ExceptT m) where
    pure =  ExceptT . return . Right
    (<*>) = ap

instance (Monad m) => Monad (ExceptT m) where
    return = pure
    m >>= k = ExceptT $ do
        a <- runExceptT m
        case a of
            Left e -> return (Left e)
            Right x -> runExceptT (k x)
    -- fail = ExceptT . return . Left . stringToDoc


instance (MonadFailDoc m) => MonadFail (ExceptT m) where
    fail = ExceptT . return . Left . stringToDoc

instance MonadIO m => MonadIO (ExceptT m) where
    liftIO comp = ExceptT $ do
        res <- liftIO comp
        return (Right res)

instance MonadTrans ExceptT where
    lift comp = ExceptT $ do
        res <- comp
        return (Right res)

instance MonadState s m => MonadState s (ExceptT m) where
    get = lift get
    put = lift . put



allContexts :: Data b => Zipper a b -> [Zipper a b]
allContexts z0 = concatMap subtreeOf (allSiblings z0)
    where
        -- the input has to be the left most
        allSiblings :: Zipper a b -> [Zipper a b]
        allSiblings z = z : maybe [] allSiblings (right z)

        subtreeOf :: Data b => Zipper a b -> [Zipper a b]
        subtreeOf z = z : maybe [] allContexts (down z)

ascendants :: Zipper a b -> [b]
ascendants z = hole z : maybe [] ascendants (Zipper.up z)


-- | splits from the "."s, drops the last component, glues back together what's left
dropExtension :: FilePath -> FilePath
dropExtension = intercalate "." . init . splitOn "."

-- | splits from the "/"s, drops all but last component, returns what's left
dropDirs :: FilePath -> FilePath
dropDirs = last . splitOn "/"


class (Functor m, Applicative m, Monad m) => MonadLog m where
    log :: LogLevel -> Doc -> m ()

data LogLevel
    = LogNone
    | LogInfo
    | LogFollow
    | LogWarn
    | LogDebug
    | LogDebugVerbose
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Serialize LogLevel
instance Hashable  LogLevel
instance ToJSON    LogLevel where toJSON = genericToJSON jsonOptions
instance FromJSON  LogLevel where parseJSON = genericParseJSON jsonOptions

instance Default LogLevel where def = LogInfo

logInfo :: MonadLog m => Doc -> m ()
logInfo = log LogInfo

logWarn :: MonadLog m => Doc -> m ()
logWarn = log LogWarn

logDebug :: MonadLog m => Doc -> m ()
logDebug = log LogDebug

logDebugVerbose :: MonadLog m => Doc -> m ()
logDebugVerbose = log LogDebugVerbose

instance MonadLog m => MonadLog (ReaderT r m) where
    log l m = lift (log l m)

instance (MonadLog m, Monoid w) => MonadLog (WriterT w m) where
    log l m = lift (log l m)

instance MonadLog m => MonadLog (StateT st m) where
    log l m = lift (log l m)

instance MonadLog m => MonadLog (ExceptT m) where
    log l m = log l m >> ExceptT (return (Right ()))

instance (Applicative m, Monad m) => MonadLog (IdentityT m) where
    log _ _ = return ()

instance Monad m => MonadLog (Pipes.Proxy a b () (Either (LogLevel, Doc) d) m) where
    log l m = Pipes.yield (Left (l,m))

ignoreLogs :: Monad m => IdentityT m a -> m a
ignoreLogs = runIdentityT

runLoggerPipeIO :: MonadIO m => LogLevel -> Pipes.Producer (Either (LogLevel, Doc) a) m r -> m r
runLoggerPipeIO l logger = Pipes.runEffect $ Pipes.for logger each
    where
        each (Left (lvl, msg)) =
            when (lvl <= l) $ do
                let txt = Pr.renderStyle (Pr.style { Pr.lineLength = 200 }) msg
                -- let txt = renderString $ (Pr.layoutPretty $ Pr.LayoutOptions (AvailablePerLine 200 1.0)) msg
                when ("[" `isPrefixOf` txt) $ do
                    liftIO clearScreen
                    liftIO (setCursorPosition 0 0)
                liftIO $ putStrLn txt
        each _ = return ()

histogram :: Ord a => [a] -> [(a, Integer)]
histogram = map (head &&& genericLength) . group . sort

sh :: Sh a -> IO a
sh = shelly . print_stdout False . print_stderr False

scope :: MonadState st m => m a -> m a
scope ma = do
    st <- gets id
    a <- ma
    modify (const st)
    return a

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents x = System.Directory.getDirectoryContents x `catch` (\ (_ :: SomeException) -> return [] )

getAllDirs :: FilePath -> IO [FilePath]
getAllDirs x = do
    let dots i = not ( i == "." || i == ".." )
    isDir <- doesDirectoryExist x
    ys' <- getDirectoryContents x
    let ys = filter dots ys'
    ([x | isDir] ++) <$> concatMapM getAllDirs (map (x </>) ys)

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles x = do
    let dots i = not ( i == "." || i == ".." )
    ys' <- getDirectoryContents x
    let ys = filter dots ys'
    (x :) <$> concatMapM getAllFiles (map (x </>) ys)

getAllFilesWithSuffix :: String -> FilePath -> IO [FilePath]
getAllFilesWithSuffix suffix fp = filter (suffix `isSuffixOf`) <$> getAllFiles fp

-- from http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = removeFile f `catch` handleExists
    where
        handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists f = (Just <$> readFile f) `catch` handleExists
    where
        handleExists e
            | ioeGetErrorType e == InvalidArgument = return Nothing -- handle non-text files gracefully
            | isDoesNotExistError e = return Nothing
            | otherwise = trace (show e) $ throwIO e

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists f = removeDirectoryRecursive f `catch` handleExists
    where
        handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e


setRandomSeed :: Int -> IO ()
setRandomSeed = setStdGen . mkStdGen

nchoosek :: Integral a => (a -> a) -> a -> a -> a
nchoosek f n k = f n `div` (f k * f (n-k))

type JSONValue = JSON.Value

-- | return true if this is a top-most zipper.
--   i.e. we cannot go any more up.
isTopMostZ :: Zipper a b -> Bool
isTopMostZ = isNothing . up


class RunStateAsWriter s where
    -- | We don't have Writer monads around here, they leak space.
    runStateAsWriterT :: (Monad m, Default s) => StateT s m a -> m (a, s)

instance RunStateAsWriter [s] where
    runStateAsWriterT m = do
        (a, out) <- runStateT m def
        return (a, reverse out)

instance RunStateAsWriter ([a],[b]) where
    runStateAsWriterT m = do
        (x, (a,b)) <- runStateT m def
        return (x, (reverse a, reverse b))

sawTell :: (MonadState s m, Monoid s) => s -> m ()
sawTell xs = modify (xs `mappend`)


stripPostfix :: Eq a => [a] -> [a] -> Maybe [a]
stripPostfix postfix list =
    case stripPrefix (reverse postfix) (reverse list) of
        Nothing -> Nothing
        Just rest -> Just (reverse rest)

