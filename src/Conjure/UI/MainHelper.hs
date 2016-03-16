{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.UI.MainHelper ( mainWithArgs ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.UI ( UI(..), OutputFormat(..) )
import Conjure.UI.IO ( readModel, readModelFromFile, readModelPreambleFromFile, writeModel )
import Conjure.UI.Model ( parseStrategy, outputModels )
import qualified Conjure.UI.Model as Config ( Config(..) )
import Conjure.UI.TranslateParameter ( translateParameter )
import Conjure.UI.TranslateSolution ( translateSolution )
import Conjure.UI.ValidateSolution ( validateSolution )
import Conjure.UI.TypeCheck ( typeCheckModel_StandAlone )
import Conjure.UI.LogFollow ( refAnswers )
import Conjure.UI.Split ( outputSplittedModels, removeUnusedDecls )
import Conjure.UI.VarSymBreaking ( outputVarSymBreaking )
import Conjure.UI.ParameterGenerator ( parameterGenerator )
import Conjure.UI.NormaliseQuantified ( normaliseQuantifiedVariables )

import Conjure.Language.Definition ( Model(..), Statement(..), Declaration(..), FindOrGiven(..) )
import Conjure.Language.NameGen ( runNameGen )
import Conjure.Language.Pretty ( pretty, prettyList, renderNormal, render )
import Conjure.Language.ModelDiff ( modelDiffIO )
import Conjure.Rules.Definition ( viewAuto, Strategy(..) )
import Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.Language.NameResolution ( resolveNamesMulti )

-- base
import System.IO ( Handle, hSetBuffering, stdout, BufferMode(..) )
import GHC.IO.Handle ( hIsEOF, hClose, hGetLine )

-- shelly
import Shelly ( runHandle, lastStderr, lastExitCode, errExit, Sh )

-- text
import qualified Data.Text as T ( unlines, isInfixOf )

-- parallel-io
import Control.Concurrent.ParallelIO.Global ( parallel, parallel_, stopGlobalPool )


mainWithArgs :: forall m . (MonadIO m, MonadLog m, MonadFail m, MonadUserError m, EnumerateDomain m) => UI -> m ()
mainWithArgs Modelling{..} = do
    model <- readModelFromFile essence
    liftIO $ hSetBuffering stdout LineBuffering
    liftIO $ maybe (return ()) setRandomSeed seed
    case savedChoices of
        Just f  -> refAnswers f
        Nothing -> return ()

    let
        parseStrategy_ s = maybe (userErr1 ("Not a valid strategy:" <+> pretty strategyQ))
                                 return
                                 (parseStrategy s)

    config <- do
        strategyQ'                  <- parseStrategy_ strategyQ
        strategyA'                  <- parseStrategy_ strategyA
        representations'            <- maybe (return strategyA')       parseStrategy_ representations
        representationsFinds'       <- maybe (return representations') parseStrategy_ representationsFinds
        representationsGivens'      <- maybe (return representations') parseStrategy_ representationsGivens
        representationsAuxiliaries' <- maybe (return representations') parseStrategy_ representationsAuxiliaries
        representationsQuantifieds' <- maybe (return representations') parseStrategy_ representationsQuantifieds
        representationsCuts'        <- maybe (return representations') parseStrategy_ representationsCuts

        case fst (viewAuto strategyQ') of
            Compact -> userErr1 "The Compact heuristic isn't supported for questions."
            _       -> return ()

        return Config.Config
            { Config.outputDirectory            = outputDirectory
            , Config.logLevel                   = logLevel
            , Config.verboseTrail               = verboseTrail
            , Config.rewritesTrail              = rewritesTrail
            , Config.logRuleFails               = logRuleFails
            , Config.logRuleSuccesses           = logRuleSuccesses
            , Config.logRuleAttempts            = logRuleAttempts
            , Config.logChoices                 = logChoices
            , Config.strategyQ                  = strategyQ'
            , Config.strategyA                  = strategyA'
            , Config.representations            = representations'
            , Config.representationsFinds       = representationsFinds'
            , Config.representationsGivens      = representationsGivens'
            , Config.representationsAuxiliaries = representationsAuxiliaries'
            , Config.representationsQuantifieds = representationsQuantifieds'
            , Config.representationsCuts        = representationsCuts'
            , Config.channelling                = channelling
            , Config.representationLevels       = representationLevels
            , Config.limitModels                = if limitModels == Just 0 then Nothing else limitModels
            , Config.numberingStart             = numberingStart
            , Config.smartFilenames             = smartFilenames
            , Config.lineWidth                  = lineWidth
            }
    runNameGen $ outputModels config model
mainWithArgs TranslateParameter{..} = do
    when (null eprime      ) $ userErr1 "Mandatory field --eprime"
    when (null essenceParam) $ userErr1 "Mandatory field --essence-param"
    let outputFilename = fromMaybe (dropExtension essenceParam ++ ".eprime-param") eprimeParam
    output <- runNameGen $ join $ translateParameter
                    <$> readModelPreambleFromFile eprime
                    <*> readModelFromFile essenceParam
    writeModel lineWidth outputFormat (Just outputFilename) output
mainWithArgs TranslateSolution{..} = do
    when (null eprime        ) $ userErr1 "Mandatory field --eprime"
    when (null eprimeSolution) $ userErr1 "Mandatory field --eprime-solution"
    output <- runNameGen $ join $ translateSolution
                    <$> readModelPreambleFromFile eprime
                    <*> maybe (return def) readModelFromFile essenceParamO
                    <*> readModelFromFile eprimeSolution
    let outputFilename = fromMaybe (dropExtension eprimeSolution ++ ".solution") essenceSolutionO
    writeModel lineWidth outputFormat (Just outputFilename) output
mainWithArgs ValidateSolution{..} = do
    when (null essence        ) $ userErr1 "Mandatory field --essence"
    when (null essenceSolution) $ userErr1 "Mandatory field --solution"
    essence2  <- readModelFromFile essence
    param2    <- maybe (return def) readModelFromFile essenceParamO
    solution2 <- readModelFromFile essenceSolution
    [essence3, param3, solution3] <- runNameGen $ resolveNamesMulti [essence2, param2, solution2]
    runNameGen $ validateSolution essence3 param3 solution3
mainWithArgs Pretty{..} = do
    model0 <- readModelFromFile essence
    let model1 = model0
                    |> (if normaliseQuantified then normaliseQuantifiedVariables else id)
                    |> (if removeUnused then removeUnusedDecls else id)
    writeModel lineWidth outputFormat Nothing model1
mainWithArgs Diff{..} =
    join $ modelDiffIO
        <$> readModelFromFile file1
        <*> readModelFromFile file2
mainWithArgs TypeCheck{..} =
    void $ runNameGen $ join $ typeCheckModel_StandAlone <$> readModelFromFile essence
mainWithArgs Split{..} = do
    model <- readModelFromFile essence
    outputSplittedModels outputDirectory model
mainWithArgs SymmetryDetection{..} = do
    let jsonFilePath = if null json then essence ++ "-json" else json
    model <- readModelFromFile essence
    outputVarSymBreaking jsonFilePath model
mainWithArgs ParameterGenerator{..} = do
    when (null essenceOut) $ userErr1 "Mandatory field --essence-out"
    model  <- readModelFromFile essence
    output <- parameterGenerator model
    writeModel lineWidth outputFormat (Just essenceOut) output
mainWithArgs config@Solve{..} = do
    -- some sanity checks
    essenceM <- readModelFromFile essence
    essenceParamsParsed <- forM essenceParams $ \ f -> do
        p <- readModelFromFile f
        return (f, p)
    let givens = [ nm | Declaration (FindOrGiven Given nm _) <- mStatements essenceM ]
              ++ [ nm | Declaration (GivenDomainDefnEnum nm) <- mStatements essenceM ]
    when (not (null givens) && null essenceParams) $
        userErr1 $ vcat
            [ "The problem specification is parameterised, but no *.param files are given."
            , "Parameters:" <+> prettyList id "," givens
            ]
    let isEmptyParam Model{mStatements=[]} = True
        isEmptyParam _ = False
        hasNonEmptyParams =
            null essenceParams ||                               -- either no params were given
            all (isEmptyParam . snd) essenceParamsParsed        -- or all those given were empty
    when (null givens && not hasNonEmptyParams) $
        userErr1 "The problem specification is _not_ parameterised, but *.param files are given."

    savedHashes <- do
        mfile <- liftIO $ readFileIfExists (outputDirectory </> "conjure.hashes")
        case mfile of
            Nothing -> return []
            Just file -> return (lines file)

    -- start the show!
    (eprimes, newHashes) <- runWriterT $
        doIfNotCached
            ( sort (mStatements essenceM)
            -- when the following flags change, invalidate hash
            -- nested tuples, because :(
            , ( numberingStart
              , smartFilenames
              , strategyQ
              , strategyA
              )
            , ( representations
              , representationsFinds
              , representationsGivens
              , representationsAuxiliaries
              , representationsQuantifieds
              , representationsCuts
              )
            , ( channelling
              , representationLevels
              , seed
              , limitModels
              , limitTime
              , outputFormat
              )
            )
            savedHashes
            (pp logLevel "Using cached models." >> getEprimes)
            conjuring
    liftIO $ writeFile (outputDirectory </> "conjure.hashes") (unlines newHashes)
    msolutions <- liftIO $ savileRows eprimes essenceParamsParsed
    case msolutions of
        Left msg        -> userErr msg
        Right solutions -> when validateSolutionsOpt $ liftIO $ validating solutions
    liftIO stopGlobalPool

    where
        conjuring :: m [FilePath]
        conjuring = do
            pp logLevel $ "Generating models for" <+> pretty essence
            -- tl;dr: rm -rf outputDirectory
            -- removeDirectoryRecursive gets upset if the dir doesn't exist.
            -- terrible solution: create the dir if it doesn't exists, rm -rf after that.
            liftIO $ createDirectoryIfMissing True outputDirectory >> removeDirectoryRecursive outputDirectory
            let modelling = let savedChoices = def
                            in  Modelling{..}                   -- construct a Modelling UI, copying all relevant fields
                                                                -- from the given Solve UI
            mainWithArgs modelling
            eprimes <- getEprimes
            when (null eprimes) $ bug "Failed to generate models."
            pp logLevel $ "Generated models:" <+> prettyList id "," eprimes
            pp logLevel $ "Saved under:" <+> pretty outputDirectory
            return eprimes

        getEprimes :: m [FilePath]
        getEprimes = filter (".eprime" `isSuffixOf`) <$> liftIO (getDirectoryContents outputDirectory)

        combineResults :: [Either e [a]] -> Either e [a]
        combineResults = fmap concat . sequence

        savileRows :: [FilePath] -> [(FilePath, Model)] -> IO (Either [Doc] [(FilePath, FilePath, FilePath)])
        savileRows eprimes params = fmap combineResults $
            if null params
                then parallel [ savileRowNoParam config m
                              | m <- eprimes
                              ]
                else parallel [ savileRowWithParams config m p
                              | m <- eprimes
                              , p <- params
                              ]

        validating :: [(FilePath, FilePath, FilePath)] -> IO ()
        validating solutions =
            if null essenceParams
                then parallel_ [ validateSolutionNoParam config sol
                               | (_, _, sol) <- solutions ]
                else parallel_ [ validateSolutionWithParams config sol p
                               | (_, p, sol) <- solutions ]


pp :: MonadIO m => LogLevel -> Doc -> m ()
pp LogNone = const $ return ()
pp _       = liftIO . putStrLn . renderNormal


savileRowNoParam
    :: UI
    -> FilePath
    -> IO (Either
        [Doc]               -- user error
        [ ( FilePath        -- model
          , FilePath        -- param
          , FilePath        -- solution
          ) ])
savileRowNoParam ui@Solve{..} modelPath = sh $ errExit False $ do
    pp logLevel $ hsep ["Savile Row:", pretty modelPath]
    let outBase = dropExtension modelPath
    eprimeModel <- liftIO $ readModelFromFile (outputDirectory </> modelPath)
    let srArgs = srMkArgs ui outBase modelPath
    (stdoutSR, solutions) <- partitionEithers <$> runHandle "savilerow" srArgs
                                (liftIO . srStdoutHandler
                                    ( outputDirectory, outBase
                                    , modelPath, "<no param file>"
                                    , eprimeModel, def
                                    , lineWidth, outputFormat
                                    )
                                    (1::Int))
    srCleanUp (stringToText $ unlines stdoutSR) solutions
savileRowNoParam _ _ = bug "savileRowNoParam"


savileRowWithParams
    :: UI
    -> FilePath
    -> (FilePath, Model)
    -> IO (Either
        [Doc]               -- user error
        [ ( FilePath        -- model
          , FilePath        -- param
          , FilePath        -- solution
          ) ])
savileRowWithParams ui@Solve{..} modelPath (paramPath, essenceParam) = sh $ errExit False $ do
    pp logLevel $ hsep ["Savile Row:", pretty modelPath, pretty paramPath]
    let outBase = dropExtension modelPath ++ "-" ++ dropDirs (dropExtension paramPath)
    eprimeModel  <- liftIO $ readModelFromFile (outputDirectory </> modelPath)
    let
        -- this is a bit tricky.
        -- we want to preserve user-erors, and not raise them as errors using IO.fail
        runTranslateParameter :: IO (Either [Doc] Model)
        runTranslateParameter = runUserErrorT $ ignoreLogs $ runNameGen $ translateParameter eprimeModel essenceParam
    eprimeParam' <- liftIO runTranslateParameter
    case eprimeParam' of
        Left err -> return (Left err)
        Right eprimeParam -> do
            liftIO $ writeFile (outputDirectory </> outBase ++ ".eprime-param") (render lineWidth eprimeParam)
            let srArgs = "-in-param"
                       : stringToText (outputDirectory </> outBase ++ ".eprime-param")
                       : srMkArgs ui outBase modelPath
            (stdoutSR, solutions) <- partitionEithers <$> runHandle "savilerow" srArgs
                                        (liftIO . srStdoutHandler
                                            ( outputDirectory, outBase
                                            , modelPath, paramPath
                                            , eprimeModel, essenceParam
                                            , lineWidth, outputFormat
                                            )
                                            (1::Int))
            srCleanUp (stringToText $ unlines stdoutSR) solutions
savileRowWithParams _ _ _ = bug "savileRowWithParams"


srMkArgs :: UI -> FilePath -> FilePath -> [Text]
srMkArgs Solve{..} outBase modelPath =
    [ "-in-eprime"      , stringToText $ outputDirectory </> modelPath
    , "-out-minion"     , stringToText $ outputDirectory </> outBase ++ ".eprime-minion"
    , "-out-aux"        , stringToText $ outputDirectory </> outBase ++ ".eprime-aux"
    , "-out-info"       , stringToText $ outputDirectory </> outBase ++ ".eprime-info"
    , "-run-solver"
    , "-minion"
    , "-num-solutions"  , "1"
    , "-solutions-to-stdout-one-line"
    ] ++ map stringToText (words savilerowOptions)
      ++ if null minionOptions then [] else [ "-solver-options", stringToText minionOptions ]
srMkArgs _ _ _ = bug "srMkArgs"


srStdoutHandler
    :: (FilePath, FilePath, FilePath, FilePath, Model, Model, Int, OutputFormat)
    -> Int
    -> Handle
    -> IO [Either String (FilePath, FilePath, FilePath)]
srStdoutHandler
        args@( outputDirectory, outBase
             , modelPath, paramPath
             , eprimeModel, essenceParam
             , lineWidth, outputFormat
             )
        solutionNumber h = do
    eof <- hIsEOF h
    if eof
        then do
            hClose h
            return []
        else do
            line <- hGetLine h
            case stripPrefix "Solution: " line of
                Just solutionText -> do
                    let mkFilename ext = outputDirectory </> outBase ++ "-solution" ++ paddedNum 6 '0' solutionNumber ++ ext
                    let filenameEprimeSol  = mkFilename ".eprime-solution"
                    let filenameEssenceSol = mkFilename ".solution"
                    eprimeSol  <- readModel id ("<memory>", stringToText solutionText)
                    writeFile filenameEprimeSol  (render lineWidth eprimeSol)
                    essenceSol <- ignoreLogs $ runNameGen $ translateSolution eprimeModel essenceParam eprimeSol
                    writeModel lineWidth outputFormat (Just filenameEssenceSol) essenceSol
                    fmap (Right (modelPath, paramPath, filenameEssenceSol) :)
                         (srStdoutHandler args (solutionNumber+1) h)
                Nothing ->
                    fmap (Left line :)
                         (srStdoutHandler args solutionNumber h)


srCleanUp :: Text -> sols -> Sh (Either [Doc] sols)
srCleanUp stdoutSR solutions = do
    stderrSR   <- lastStderr
    exitCodeSR <- lastExitCode
    if
        | exitCodeSR == 0 -> return (Right solutions)
        | T.isInfixOf "where false" (T.unlines [stdoutSR, stderrSR]) ->
            return (Left [vcat [ "Invalid instance, a where statement evaluated to false."
                               , "(It can be an implicit where statement added by Conjure.)"
                               ]])
        | otherwise -> bug $ vcat [ "Savile Row stdout:"    <+> pretty stdoutSR
                                  , "Savile Row stderr:"    <+> pretty stderrSR
                                  , "Savile Row exit-code:" <+> pretty exitCodeSR
                                  ]


validateSolutionNoParam :: UI -> FilePath -> IO ()
validateSolutionNoParam Solve{..} solutionPath = do
    pp logLevel $ hsep ["Validating solution:", pretty solutionPath]
    essenceM <- readModelFromFile essence
    solution <- readModelFromFile solutionPath
    [essenceM2, solution2] <- ignoreLogs $ runNameGen $ resolveNamesMulti [essenceM, solution]
    result   <- runExceptT $ ignoreLogs $ runNameGen $ validateSolution essenceM2 def solution2
    case result of
        Left err -> bug err
        Right () -> return ()
validateSolutionNoParam _ _ = bug "validateSolutionNoParam"


validateSolutionWithParams :: UI -> FilePath -> FilePath -> IO ()
validateSolutionWithParams Solve{..} solutionPath paramPath = do
    pp logLevel $ hsep ["Validating solution:", pretty paramPath, pretty solutionPath]
    essenceM <- readModelFromFile essence
    param    <- readModelFromFile paramPath
    solution <- readModelFromFile solutionPath
    [essenceM2, param2, solution2] <- ignoreLogs $ runNameGen $ resolveNamesMulti [essenceM, param, solution]
    result   <- runExceptT $ ignoreLogs $ runNameGen $ validateSolution essenceM2 param2 solution2
    case result of
        Left err -> bug err
        Right () -> return ()
validateSolutionWithParams _ _ _ = bug "validateSolutionWithParams"


doIfNotCached
    :: (Monad m, Hashable h)
    => h                        -- thing to hash
    -> [String]                 -- saved hashes
    -> m a                      -- the result from cache
    -> m a                      -- the action
    -> WriterT [String] m a     -- the results and accumulating new hashes in the writer
doIfNotCached (show . hash -> h) savedHashes getResult act = do
    tell [h]
    lift $ if h `elem` savedHashes
            then getResult
            else act
