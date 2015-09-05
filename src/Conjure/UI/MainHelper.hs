{-# LANGUAGE RecordWildCards #-}

module Conjure.UI.MainHelper ( mainWithArgs ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError ( MonadUserError(..) )
import Conjure.UI ( UI(..) )
import Conjure.UI.IO ( readModel, readModelFromFile, readModelPreambleFromFile, writeModel, EssenceFileMode(..) )
import Conjure.UI.Model ( parseStrategy, outputModels )
import qualified Conjure.UI.Model as Config ( Config(..) )
import Conjure.UI.RefineParam ( refineParam )
import Conjure.UI.TranslateSolution ( translateSolution )
import Conjure.UI.ValidateSolution ( validateSolution )
import Conjure.UI.TypeCheck ( typeCheckModel_StandAlone )
import Conjure.UI.LogFollow ( refAnswers )
import Conjure.UI.Split ( outputSplittedModels )
import Conjure.UI.VarSymBreaking ( outputVarSymBreaking )
import Conjure.UI.ParameterGenerator ( parameterGenerator )

import Conjure.Language.Definition ( Model(..), Statement(..), Declaration(..), FindOrGiven(..) )
import Conjure.Language.NameGen ( runNameGen )
import Conjure.Language.Pretty ( pretty, prettyList, renderNormal, renderWide )
import Conjure.Language.ModelDiff ( modelDiffIO )
import Conjure.Rules.Definition ( viewAuto, Strategy(..) )
import Conjure.Process.Enumerate ( EnumerateDomain )

-- base
import System.IO ( hSetBuffering, stdout, BufferMode(..) )
import GHC.IO.Handle ( hIsEOF, hClose, hGetLine )

-- shelly
import Shelly ( runHandle, lastStderr )

-- text
import qualified Data.Text as T ( null )
    

mainWithArgs :: (MonadIO m, MonadLog m, MonadFail m, MonadUserError m, EnumerateDomain m) => UI -> m ()
mainWithArgs Modelling{..} = do
    model <- readModelFromFile essence
    liftIO $ hSetBuffering stdout NoBuffering
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
            , Config.limitModels                = if limitModels == Just 0 then Nothing else limitModels
            , Config.numberingStart             = numberingStart
            , Config.smartFilenames             = smartFilenames
            }
    runNameGen $ outputModels config model
mainWithArgs RefineParam{..} = do
    when (null eprime      ) $ userErr1 "Mandatory field --eprime"
    when (null essenceParam) $ userErr1 "Mandatory field --essence-param"
    let outputFilename = fromMaybe (dropExtension essenceParam ++ ".eprime-param") eprimeParam
    output <- runNameGen $ join $ refineParam
                    <$> readModelPreambleFromFile eprime
                    <*> readModelFromFile essenceParam
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               (Just outputFilename)
               output
mainWithArgs TranslateSolution{..} = do
    when (null eprime        ) $ userErr1 "Mandatory field --eprime"
    when (null eprimeSolution) $ userErr1 "Mandatory field --eprime-solution"
    output <- runNameGen $ join $ translateSolution
                    <$> readModelPreambleFromFile eprime
                    <*> maybe (return def) readModelFromFile essenceParamO
                    <*> readModelFromFile eprimeSolution
    let outputFilename = fromMaybe (dropExtension eprimeSolution ++ ".solution") essenceSolutionO
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               (Just outputFilename) output
mainWithArgs ValidateSolution{..} = do
    when (null essence        ) $ userErr1 "Mandatory field --essence"
    when (null essenceSolution) $ userErr1 "Mandatory field --solution"
    join $ validateSolution
        <$> readModelFromFile essence
        <*> maybe (return def) readModelFromFile essenceParamO
        <*> readModelFromFile essenceSolution
mainWithArgs Pretty{..} = do
    model <- readModelFromFile essence
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               Nothing model
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
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               (Just essenceOut)
               output
mainWithArgs config@Solve{..} = do
    -- some sanity checks
    essenceM <- readModelFromFile essence
    let givens = [ nm | Declaration (FindOrGiven Given nm _) <- mStatements essenceM ]
    when (not (null givens) && null essenceParams) $
        userErr1 $ vcat
            [ "The problem specification is parameterised, but no *.param files are given."
            , "Parameters:" <+> prettyList id "," givens
            ]
    when (null givens && not (null essenceParams)) $
        userErr1 "The problem specification is _not_ parameterised, but *.param files are given."

    -- start the show!
    eprimes   <- conjuring
    solutions <- liftIO $ savileRows eprimes
    when validateSolutionsOpt $ liftIO $ validating solutions

    where
        conjuring = do
            pp $ "Generating models for" <+> pretty essence
            -- tl;dr: rm -rf outputDirectory
            -- removeDirectoryRecursive gets upset if the dir doesn't exist.
            -- terrible solution: create the dir if it doesn't exists, rm -rf after that.
            liftIO $ createDirectoryIfMissing True outputDirectory >> removeDirectoryRecursive outputDirectory
            let modelling = let savedChoices = def
                            in  Modelling{..}                   -- construct a Modelling UI, copying all relevant fields
                                                                -- from the given Solve UI
            mainWithArgs modelling
            eprimes <- filter (".eprime" `isSuffixOf`) <$> liftIO (getDirectoryContents outputDirectory)
            when (null eprimes) $ bug "Failed to generate models."
            pp $ "Generated models:" <+> vcat (map pretty eprimes)
            pp $ "Saved under:" <+> pretty outputDirectory
            return eprimes

        savileRows eprimes = fmap concat $ sequence $
            if null essenceParams
                then [ savileRowNoParam    config m   | m <- eprimes ]
                else [ savileRowWithParams config m p | m <- eprimes, p <- essenceParams ]

        validating solutions = sequence_ $
            if null essenceParams
                then [ validateSolutionNoParam    config sol   | (_, _, sol) <- solutions ]
                else [ validateSolutionWithParams config sol p | (_, p, sol) <- solutions ]


pp :: MonadIO m => Doc -> m ()
pp = liftIO . putStrLn . renderWide


savileRowNoParam :: UI -> FilePath -> IO [ ( FilePath       -- model
                                           , FilePath       -- param
                                           , FilePath       -- solution
                                           ) ]
savileRowNoParam Solve{..} modelPath = sh $ do
    pp $ hsep ["Savile Row:", pretty modelPath]
    let outBase = dropExtension modelPath
    eprimeModel <- liftIO $ readModelFromFile (outputDirectory </> modelPath)
    let args =
            [ "-in-eprime"      , stringToText $ outputDirectory </> outBase ++ ".eprime"
            , "-out-minion"     , stringToText $ outputDirectory </> outBase ++ ".eprime-minion"
            , "-out-aux"        , stringToText $ outputDirectory </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputDirectory </> outBase ++ ".eprime-info"
            , "-run-solver"
            , "-minion"
            , "-num-solutions"  , "1"
            , "-solutions-to-stdout-one-line"
            ] ++ map stringToText (words savilerowOptions)
              ++ [ "-solver-options", stringToText minionOptions ]
    let stdoutHandler i h = do
            eof <- liftIO $ hIsEOF h
            if eof
                then do
                    liftIO $ hClose h
                    return []
                else do
                    line <- liftIO $ hGetLine h
                    case stripPrefix "Solution: " line of
                        Just solutionText -> do
                            eprimeSol  <- liftIO $ readModel id ("<memory>", stringToText solutionText)
                            essenceSol <- liftIO $ ignoreLogs $ runNameGen $ translateSolution eprimeModel def eprimeSol
                            let filename = outputDirectory </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                            liftIO $ writeFile filename (renderNormal essenceSol)
                            rest <- stdoutHandler (i+1) h
                            return ((modelPath, "<no param file>", filename) : rest)
                        Nothing -> stdoutHandler i h
    solutions <- runHandle "savilerow" args (stdoutHandler (1::Int))
    stderrSR <- lastStderr
    if not (T.null stderrSR)
        then bug (pretty stderrSR)
        else return solutions
savileRowNoParam _ _ = bug "savileRowNoParam"


savileRowWithParams :: UI -> FilePath -> FilePath -> IO [ ( FilePath       -- model
                                                          , FilePath       -- param
                                                          , FilePath       -- solution
                                                          ) ]
savileRowWithParams Solve{..} modelPath paramPath = sh $ do
    pp $ hsep ["Savile Row:", pretty modelPath, pretty paramPath]
    let outBase = dropExtension modelPath ++ "-" ++ dropDirs (dropExtension paramPath)
    eprimeModel  <- liftIO $ readModelFromFile (outputDirectory </> modelPath)
    essenceParam <- liftIO $ readModelFromFile paramPath
    eprimeParam  <- liftIO $ ignoreLogs $ runNameGen $ refineParam eprimeModel essenceParam
    liftIO $ writeFile (outputDirectory </> outBase ++ ".eprime-param") (renderNormal eprimeParam)
    let args =
            [ "-in-eprime"      , stringToText $ outputDirectory </> modelPath
            , "-in-param"       , stringToText $ outputDirectory </> outBase ++ ".eprime-param"
            , "-out-minion"     , stringToText $ outputDirectory </> outBase ++ ".eprime-minion"
            , "-out-aux"        , stringToText $ outputDirectory </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputDirectory </> outBase ++ ".eprime-info"
            , "-run-solver"
            , "-minion"
            , "-num-solutions"  , "1"
            , "-solutions-to-stdout-one-line"
            ] ++ map stringToText (words savilerowOptions)
            ++ [ "-solver-options", stringToText minionOptions ]
    let stdoutHandler i h = do
            eof <- liftIO $ hIsEOF h
            if eof
                then do
                    liftIO $ hClose h
                    return []
                else do
                    line <- liftIO $ hGetLine h
                    case stripPrefix "Solution: " line of
                        Just solutionText -> do
                            eprimeSol  <- liftIO $ readModel id ("<memory>", stringToText solutionText)
                            essenceSol <- liftIO $ ignoreLogs $ runNameGen $ translateSolution eprimeModel essenceParam eprimeSol
                            let filename = outputDirectory </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                            liftIO $ writeFile filename (renderNormal essenceSol)
                            rest <- stdoutHandler (i+1) h
                            return ((modelPath, paramPath, filename) : rest)
                        Nothing -> stdoutHandler i h
    solutions <- runHandle "savilerow" args (stdoutHandler (1::Int))
    stderrSR <- lastStderr
    if not (T.null stderrSR)
        then bug (pretty stderrSR)
        else return solutions
savileRowWithParams _ _ _ = bug "savileRowWithParams"


validateSolutionNoParam :: UI -> FilePath -> IO ()
validateSolutionNoParam Solve{..} solutionPath = do
    pp $ hsep ["Validating solution:", pretty solutionPath]
    essenceM <- readModelFromFile essence
    solution <- readModelFromFile solutionPath
    result   <- runExceptT $ ignoreLogs $ validateSolution essenceM def solution
    case result of
        Left err -> bug err
        Right () -> return ()
validateSolutionNoParam _ _ = bug "validateSolutionNoParam"


validateSolutionWithParams :: UI -> FilePath -> FilePath -> IO ()
validateSolutionWithParams Solve{..} solutionPath paramPath = do
    pp $ hsep ["Validating solution:", pretty paramPath, pretty solutionPath]
    essenceM <- readModelFromFile essence
    param    <- readModelFromFile paramPath
    solution <- readModelFromFile solutionPath
    result   <- runExceptT $ ignoreLogs $ validateSolution essenceM param solution
    case result of
        Left err -> bug err
        Right () -> return ()
validateSolutionWithParams _ _ _ = bug "validateSolutionWithParams"

