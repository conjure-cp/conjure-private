module Conjure.Process.Enumerate
    ( EnumerateDomain
    , enumerateDomain
    , enumerateInConstant
    , EnumerateDomainNoIO(..)
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.AdHoc
import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Definition
import Conjure.Language.NameGen

import Conjure.UI.IO
import Conjure.UI as UI ( UI(..), OutputFormat(..) )
import {-# SOURCE #-} Conjure.UI.MainHelper

-- temporary
import System.IO.Temp ( withSystemTempDirectory )

-- pipes
import qualified Pipes


-- | This class is only to track where `enumerateDomain` might get called.
--   It is essentially MonadIO, but doesn't allow arbitrary IO.
class (Functor m, Applicative m, Monad m, MonadUserError m) => EnumerateDomain m where liftIO' :: IO a -> m a
instance EnumerateDomain IO where liftIO' = id
instance EnumerateDomain m => EnumerateDomain (IdentityT m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (MaybeT m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (ExceptT m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (ReaderT r m) where liftIO' = lift . liftIO'
instance (EnumerateDomain m, Monoid w) => EnumerateDomain (WriterT w m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (StateT st m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (Pipes.Proxy a b c d m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (NameGenM m) where liftIO' = lift . liftIO'
instance (EnumerateDomain m, MonadFail m) => EnumerateDomain (UserErrorT m) where liftIO' = lift . liftIO'

-- | Use this if you don't want to allow a (EnumerateDomain m => m a) computation actually do IO.
data EnumerateDomainNoIO a = Done a | TriedIO | Failed Doc
    deriving (Show)

instance Eq a => Eq (EnumerateDomainNoIO a) where
     (Done a) == (Done b) = a ==b
     TriedIO == TriedIO = True
     (Failed _) == (Failed _) = True
     _ == _ = False


instance Functor EnumerateDomainNoIO where
    fmap _ (Failed msg) = Failed msg
    fmap _ TriedIO      = TriedIO
    fmap f (Done x)     = Done (f x)

instance Applicative EnumerateDomainNoIO where
    pure = Done
    (<*>) = ap

instance Monad EnumerateDomainNoIO where
    Failed msg >>= _ = Failed msg
    TriedIO    >>= _ = TriedIO
    Done x     >>= f = f x

instance MonadFailDoc EnumerateDomainNoIO where
    failDoc = Failed
instance MonadFail EnumerateDomainNoIO where
    fail = Failed . stringToDoc 

instance MonadUserError EnumerateDomainNoIO where
    userErr docs = Failed (vcat $ "User error:" : docs)

instance NameGen EnumerateDomainNoIO where
    nextName _ = failDoc "nextName{EnumerateDomainNoIO}"
    exportNameGenState = failDoc "exportNameGenState{EnumerateDomainNoIO}"
    importNameGenState _ = failDoc "importNameGenState{EnumerateDomainNoIO}"

instance EnumerateDomain EnumerateDomainNoIO where liftIO' _ = TriedIO

enumerateDomainMax :: Int
enumerateDomainMax = 10000

minionTimelimit :: Int
minionTimelimit = 60

savilerowTimelimit :: Int
savilerowTimelimit = 60 * 1000

enumerateDomain :: (MonadFailDoc m, EnumerateDomain m) => Domain () Constant -> m [Constant]

enumerateDomain d | not (null [ () | ConstantUndefined{} <- universeBi d ]) =
    bug $ vcat [ "called enumerateDomain with a domain that has undefinedness values in it."
               , pretty d
               ]

enumerateDomain DomainBool = return [ConstantBool False, ConstantBool True]
enumerateDomain (DomainInt _ []) = failDoc "enumerateDomain: infinite domain"
enumerateDomain (DomainInt _ rs) = concatMapM enumerateRange rs
enumerateDomain (DomainUnnamed _ (ConstantInt t n)) = return (map (ConstantInt t) [1..n])
enumerateDomain (DomainEnum _dName (Just rs) _mp) = concatMapM enumerateRange rs
enumerateDomain (DomainTuple ds) = do
    inners <- mapM enumerateDomain ds
    return $ map (ConstantAbstract . AbsLitTuple) (sequence inners)
enumerateDomain (DomainMatrix (DomainInt t indexDom) innerDom) = do
    inners <- enumerateDomain innerDom
    indexInts <- rangesInts indexDom
    return
        [ ConstantAbstract (AbsLitMatrix (DomainInt t indexDom) vals)
        | vals <- replicateM (length indexInts) inners
        ]

-- the sledgehammer approach
enumerateDomain d = liftIO' $ withSystemTempDirectory ("conjure-enumerateDomain-" ++ show (hash d)) $ \ tmpDir -> do
    let model = Model { mLanguage = LanguageVersion "Essence" [1,0]
                      , mStatements = [Declaration (FindOrGiven Find "x" (fmap Constant d))]
                      , mInfo = def
                      }
    let essenceFile = tmpDir </> "out.essence"
    let outDir = tmpDir </> "outDir"
    writeModel 120 Plain (Just essenceFile) model
    let
        solve :: IO ()
        solve = let ?typeCheckerMode = StronglyTyped in ignoreLogs $ runNameGen () $ mainWithArgs Solve
            { UI.essence                    = essenceFile
            , validateSolutionsOpt          = False
            , outputDirectory               = outDir
            , savilerowOptions              =
                [ "-O0"
                , "-preprocess"    , "None"
                , "-timelimit"     , show savilerowTimelimit
                ]
            , solverOptions                 =
                [ "-cpulimit"      , show minionTimelimit
                ]
            , solver                        = "minion"
            , graphSolver                   = False
            , cgroups                       = False
            , nbSolutions                   = show enumerateDomainMax
            , copySolutions                 = False
            , solutionsInOneFile            = False
            , runsolverCPUTimeLimit         = Nothing
            , runsolverWallTimeLimit        = Nothing
            , runsolverMemoryLimit          = Nothing
            , logLevel                      = LogNone
            -- default values for the rest
            , essenceParams                 = []
            , numberingStart                = 1
            , smartFilenames                = False
            , verboseTrail                  = False
            , rewritesTrail                 = False
            , logRuleFails                  = False
            , logRuleSuccesses              = False
            , logRuleAttempts               = False
            , logChoices                    = False
            , portfolio                     = Nothing
            , strategyQ                     = "f"
            , strategyA                     = "c"
            , representations               = Nothing
            , representationsFinds          = Nothing
            , representationsGivens         = Nothing
            , representationsAuxiliaries    = Nothing
            , representationsQuantifieds    = Nothing
            , representationsCuts           = Nothing
            , channelling                   = False
            , representationLevels          = True
            , unnamedSymmetryBreaking       = "none"
            , followModel                   = ""
            , useExistingModels             = []
            , seed                          = Nothing
            , limitModels                   = Nothing
            , limitTime                     = Nothing
            , outputFormat                  = UI.Plain
            , lineWidth                     = 120
            , responses                     = ""
            , responsesRepresentation       = ""
            , generateStreamliners          = ""
            }
    -- catching the (SR timeout) error, and raising a user error
    catch solve $ \ (e :: SomeException) -> userErr1 $ vcat
        [ "Enumerate domain: too many."
        , "When working on domain:" <++> pretty d
        , "Exception:" <++> pretty (show e)
        ]
    solutions   <- filter (".solution" `isSuffixOf`) <$> getDirectoryContents outDir
    when (length solutions >= enumerateDomainMax) $ userErr1 $ vcat
        [ "Enumerate domain: too many."
        , "Gave up after" <+> pretty (length solutions) <+> "solutions."
        , "When working on domain:" <++> pretty d
        ]
    enumeration <- fmap concat $ forM solutions $ \ solutionFile -> do
        Model _ decls _ <- readModelFromFile (outDir </> solutionFile)
        let (enumeration, errs) = mconcat
                [ case decl of
                    Declaration (Letting "x" x) | Just c <- e2c x -> ([c], [])
                    _ -> ([], [decl])
                | decl <- decls ]
        if null errs
            then return enumeration
            else failDoc $ vcat $ "enumerateDomain, not Constants!"
                             : ("When working on domain:" <++> pretty d)
                             :  map pretty errs
                             ++ map (pretty . show) errs
    removeDirectoryIfExists outDir
    removeDirectoryIfExists tmpDir
    return enumeration


enumerateRange :: MonadFailDoc m => Range Constant -> m [Constant]
enumerateRange (RangeSingle x) = return [x]
enumerateRange (RangeBounded (ConstantInt t x) (ConstantInt _ y)) = return $ ConstantInt t <$> [x..y]
enumerateRange RangeBounded{} = failDoc "enumerateRange RangeBounded"
enumerateRange RangeOpen{} = failDoc "enumerateRange RangeOpen"
enumerateRange RangeLowerBounded{} = failDoc "enumerateRange RangeLowerBounded"
enumerateRange RangeUpperBounded{} = failDoc "enumerateRange RangeUpperBounded"

enumerateInConstant :: MonadFailDoc m => Constant -> m [Constant]
enumerateInConstant constant = case constant of
    ConstantAbstract (AbsLitMatrix _  xs) -> return xs
    ConstantAbstract (AbsLitSet       xs) -> return xs
    ConstantAbstract (AbsLitMSet      xs) -> return xs
    ConstantAbstract (AbsLitFunction  xs) -> return [ ConstantAbstract (AbsLitTuple [i,j]) | (i,j) <- xs ]
    ConstantAbstract (AbsLitSequence  xs) -> return [ ConstantAbstract (AbsLitTuple [i,j])
                                                    | (i',j) <- zip allNats xs
                                                    , let i = fromInt i'
                                                    ]
    ConstantAbstract (AbsLitRelation  xs) -> return $ map (ConstantAbstract . AbsLitTuple) xs
    ConstantAbstract (AbsLitPartition xs) -> return $ map (ConstantAbstract . AbsLitSet) xs
    ConstantAbstract (AbsLitPermutation xss) ->
        let
            enumPerm [] = []
            enumPerm xs = [ ConstantAbstract (AbsLitTuple [i,j]) | (i,j) <- zip xs (tail xs) ] ++
                          [ ConstantAbstract (AbsLitTuple [last xs, head xs]) ]
        in
            return $ concatMap enumPerm xss
    TypedConstant c _                     -> enumerateInConstant c
    _ -> failDoc $ vcat [ "enumerateInConstant"
                     , "constant:" <+> pretty constant
                     ]
