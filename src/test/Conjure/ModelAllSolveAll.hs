{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conjure.ModelAllSolveAll ( tests, TestTimeLimit(..) ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.Type ( TypeCheckerMode(..) )
import Conjure.UI.IO
import Conjure.UI.TranslateParameter
import Conjure.UI.TranslateSolution
import Conjure.UI.ValidateSolution
import Conjure.UI.MainHelper ( savilerowScriptName, mainWithArgs )
import Conjure.Language.NameResolution ( resolveNamesMulti )
import Conjure.UserError ( runUserErrorT )
import Conjure.UI ( ui )

-- base
import System.Environment ( getEnvironment )
import System.Environment ( withArgs )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( Assertion, testCaseSteps )
import Test.Tasty.Options ( IsOption(..) )

-- shelly
import Shelly ( run, errExit, lastStderr, lastExitCode )

-- cmdargs
import System.Console.CmdArgs ( cmdArgs )

-- text
import qualified Data.Text as T ( isInfixOf, lines, unlines )
import qualified Data.Text.IO as T ( readFile )

-- containers
import qualified Data.Set as S ( fromList, toList, empty, null, difference )

-- Diff
-- Diff
import Data.Algorithm.Diff ( getGroupedDiff, PolyDiff (..) )
import Data.Algorithm.DiffOutput ( ppDiff )


srOptionsMk :: String -> [Text]
srOptionsMk srExtraOptions =
    [ "-run-solver"
    -- , "-timelimit"      , "1200000"
    -- , "-solver-options" , "-cpulimit 1200"
    , "-all-solutions"
    , "-preprocess"     , "None"
    , "-S0"
    ] ++ map stringToText (words srExtraOptions)


-- | Which tests are we running?
data TestTimeLimit = TestTimeLimit Int          -- lower bound, in seconds, default 0
                                   Int          -- upper bound, in seconds, default 10
    deriving (Eq, Ord, Typeable)

instance IsOption TestTimeLimit where
    defaultValue = TestTimeLimit 0 10

    parseValue inp =
        case splitOn "-" inp of
            [i] ->
                case readMay i of
                    Nothing -> Nothing
                    Just u  -> Just (TestTimeLimit 0 u)
            [i,j] ->
                case (readMay i, readMay j) of
                    (Just l, Just u) -> Just (TestTimeLimit l u)
                    _ -> Nothing
            _ -> bug "parseValue{TestTimeLimit}"

    optionName = return "limit-time"
    optionHelp = return $ unlines [ "Select which tests to run by their expected times."
                                  , "Only tests which take less than the given value will be run."
                                  , "See `expected-time.txt` files in the `tests/exhaustive` directory."
                                  , "Default is 10."
                                  ]


tests ::
    HasCallStack =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    IO (TestTimeLimit -> TestTree)
tests = do
    srExtraOptions <- do
        env <- getEnvironment
        return $ fromMaybe "-O0" (lookup "SR_OPTIONS" env)
    let srOptions = srOptionsMk srExtraOptions
    putStrLn $ "Using Savile Row options: " ++ unwords (map textToString srOptions)
    let baseDir = "tests/exhaustive"
    dirs <- mapM (isTestDir baseDir) =<< getAllDirs baseDir
    let testCases tl = concatMap (testSingleDir tl srOptions) (catMaybes dirs)
    return $ \ tl -> testGroup "exhaustive" (testCases tl)


data TestDirFiles = TestDirFiles
    { name           :: String          -- a name for the test case
    , expectedTime   :: Int             -- how long do we expect this test to run (in seconds) (default: 0)
    , tBaseDir       :: FilePath        -- dir
    , outputsDir     :: FilePath        -- dir
    , expectedsDir   :: FilePath        -- dir
    , essenceFile    :: FilePath        -- dir + filename
    , paramFiles     :: [FilePath]      -- filename
    , expectedModels :: [FilePath]      -- filename
    , expectedSols   :: [FilePath]      -- filename
    }
    deriving Show


-- returns True if the argument points to a directory that is not hidden
isTestDir :: HasCallStack => FilePath -> FilePath -> IO (Maybe TestDirFiles)
isTestDir baseDir dir = do
    dirContents <- getDirectoryContents dir
    expectedTime <-
        if "expected-time.txt" `elem` dirContents
            then fromMaybe 0 . readMay . textToString <$> T.readFile (dir ++ "/expected-time.txt")
            else return 0
    let essenceFiles = filter (".essence" `isSuffixOf`) dirContents
    case essenceFiles of
        [f] -> Just <$> do
            let params = filter (".param"  `isSuffixOf`) dirContents
            expecteds <- do
                let dirExpected = dir </> "expected"
                isDir <- doesDirectoryExist dirExpected
                if isDir
                    then getDirectoryContents dirExpected
                    else return []
            return TestDirFiles
                { name           = drop (length baseDir + 1) dir
                , tBaseDir       = dir
                , outputsDir     = dir </> "outputs"
                , expectedsDir   = dir </> "expected"
                , essenceFile    = dir </> f
                , paramFiles     = params
                , expectedModels = filter (".eprime"   `isSuffixOf`) expecteds
                , expectedSols   = filter (".solution" `isSuffixOf`) expecteds
                , expectedTime   = expectedTime
                }
        _ -> return Nothing


type Step = String -> Assertion


-- the first FilePath is the base directory for the exhaustive tests
-- we know at this point that the second FilePath points to a directory D,
-- which contains + an Essence file D/D.essence
--                + D/*.param files if required
--                + D/expected for the expected output files
testSingleDir ::
    HasCallStack =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    TestTimeLimit -> [Text] -> TestDirFiles -> [TestTree]
testSingleDir (TestTimeLimit timeLimitMin timeLimitMax) srOptions t@TestDirFiles{..} =
    if shouldRun
        then return $ testCaseSteps (map (\ ch -> if ch == '/' then '.' else ch) name) $ \ step -> do
                conjuring step
                sequence_ (savileRows step)
                validating step
                checkExpectedAndExtraFiles step srOptions t
                equalNumberOfSolutions step t
                noDuplicateSolutions step t
        else []
    where

        shouldRun = or [ timeLimitMax == 0
                       , timeLimitMin <= expectedTime && expectedTime <= timeLimitMax
                       ]

        conjuring step = do
            void (step "Conjuring")
            removeDirectoryIfExists outputsDir
            -- generate the eprimes
            modelAll tBaseDir outputsDir essenceFile

        savileRows step =
            if null paramFiles
                then [ savileRowNoParam    step srOptions t m   | m <- expectedModels ]
                else [ savileRowWithParams step srOptions t m p | m <- expectedModels
                                                                , p <- paramFiles     ]

        validating step =
            if null paramFiles
                then validateSolutionNoParam    step t expectedSols
                else validateSolutionWithParams step t [ ( p
                                                         , [ s | s <- expectedSols
                                                               , (dropExtension p ++ "-solution") `isInfixOf` dropExtension s
                                                               ]
                                                         )
                                                       | p <- paramFiles
                                                       ]


savileRowNoParam ::
    HasCallStack =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Step -> [Text] -> TestDirFiles -> FilePath -> Assertion
savileRowNoParam step srOptions TestDirFiles{..} modelPath = do
    step (unwords ["Savile Row:", modelPath])
    let outBase = dropExtension modelPath
    fileShouldExist (outputsDir </> outBase ++ ".eprime")
    (stdoutSR, stderrSR, exitCodeSR) <-
        sh $ errExit False $ do
            stdoutSR <- run savilerowScriptName $
                [ "-in-eprime"      , stringToText $ outputsDir </> outBase ++ ".eprime"
                , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
                , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
                , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
                , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
                ] ++ srOptions
            stderrSR   <- lastStderr
            exitCodeSR <- lastExitCode
            return (stdoutSR, stderrSR, exitCodeSR)
    if
        | exitCodeSR == 0 -> do
            eprimeModel       <- readModelInfoFromFile (outputsDir </> modelPath)
            nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                      <$> getDirectoryContents outputsDir
            forM_ (take nbEprimeSolutions allNats) $ \ i -> do
                let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ padLeft 6 '0' (show i)
                eprimeSolution <- runLoggerPipeIO LogDebug $ readParamOrSolutionFromFile eprimeModel (outputsDir </> eprimeSolutionPath)
                res <- runUserErrorT $ ignoreLogs $ runNameGen () $
                            translateSolution eprimeModel def eprimeSolution
                case res of
                    Left errs -> assert $ vcat errs
                    Right s  -> do
                        let filename = outputsDir </> outBase ++ "-solution" ++ padLeft 6 '0' (show i) ++ ".solution"
                        writeFile filename (renderNormal s)

        | T.isInfixOf "where false" (T.unlines [stdoutSR, stderrSR]) -> return ()
        | otherwise -> assert $ vcat [ "Savile Row stdout:"    <+> pretty stdoutSR
                                     , "Savile Row stderr:"    <+> pretty stderrSR
                                     , "Savile Row exit-code:" <+> pretty exitCodeSR
                                     ]


savileRowWithParams ::
    HasCallStack =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Step -> [Text] -> TestDirFiles -> FilePath -> FilePath -> Assertion
savileRowWithParams step srOptions TestDirFiles{..} modelPath paramPath = do
    step (unwords ["Savile Row:", modelPath, paramPath])
    fileShouldExist (outputsDir </> modelPath)
    fileShouldExist (tBaseDir   </> paramPath)
    eprimeModel <- readModelInfoFromFile (outputsDir </> modelPath)
    param       <- runLoggerPipeIO LogDebug $ readParamOrSolutionFromFile eprimeModel (tBaseDir   </> paramPath)
    eprimeParam <- ignoreLogs $ runNameGen () $ translateParameter False eprimeModel param
    let outBase = dropExtension modelPath ++ "-" ++ dropExtension paramPath
    writeFile (outputsDir </> outBase ++ ".eprime-param") (renderNormal eprimeParam)
    (stdoutSR, stderrSR, exitCodeSR) <-
        sh $ errExit False $ do
            stdoutSR <- run savilerowScriptName $
                [ "-in-eprime"      , stringToText $ outputsDir </> modelPath
                , "-in-param"       , stringToText $ outputsDir </> outBase ++ ".eprime-param"
                , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
                , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
                , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
                , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
                ] ++ srOptions
            stderrSR   <- lastStderr
            exitCodeSR <- lastExitCode
            return (stdoutSR, stderrSR, exitCodeSR)
    let stdouterrSR = T.unlines [stdoutSR, stderrSR]
    if
        | exitCodeSR == 0 && not (T.isInfixOf "Exception" stdouterrSR) -> do
            nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                      <$> getDirectoryContents outputsDir
            forM_ (take nbEprimeSolutions allNats) $ \ i -> do
                let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ padLeft 6 '0' (show i)
                eprimeSolution <- runLoggerPipeIO LogDebug $ readParamOrSolutionFromFile eprimeModel (outputsDir </> eprimeSolutionPath)
                res <- runUserErrorT $ ignoreLogs $ runNameGen () $
                            translateSolution eprimeModel param eprimeSolution
                case res of
                    Left errs -> assert $ vcat errs
                    Right s  -> do
                        let filename = outputsDir </> outBase ++ "-solution" ++ padLeft 6 '0' (show i) ++ ".solution"
                        writeFile filename (renderNormal s)
        | T.isInfixOf "where false" stdouterrSR -> return ()
        | otherwise -> assert $ vcat [ "Savile Row stdout:"    <+> pretty stdoutSR
                                     , "Savile Row stderr:"    <+> pretty stderrSR
                                     , "Savile Row exit-code:" <+> pretty exitCodeSR
                                     ]


validateSolutionNoParam ::
    HasCallStack =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Step -> TestDirFiles -> [FilePath] -> Assertion
validateSolutionNoParam step TestDirFiles{..} solutionPaths = do
    step "Validating solutions"
    essence <- readModelFromFile essenceFile
    forM_ solutionPaths $ \ solutionPath -> do
        step (unwords ["Validating solution:", solutionPath])
        fileShouldExist (outputsDir </> solutionPath)
        solution <- runLoggerPipeIO LogDebug $ readParamOrSolutionFromFile essence (outputsDir </> solutionPath)
        result   <- runUserErrorT $ ignoreLogs $ runNameGen () $ do
            [essence2, param2, solution2] <- resolveNamesMulti [essence, def, solution]
            validateSolution essence2 param2 solution2
        case result of
            Left errs -> assert $ vcat errs
            Right () -> return ()


validateSolutionWithParams ::
    HasCallStack =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Step -> TestDirFiles -> [(FilePath, [FilePath])] -> Assertion
validateSolutionWithParams step TestDirFiles{..} paramSolutionPaths = do
    step "Validating solutions"
    essence <- readModelFromFile essenceFile
    forM_ paramSolutionPaths $ \ (paramPath, solutionPaths) -> do
        fileShouldExist (tBaseDir </> paramPath)
        param <- runLoggerPipeIO LogDebug $ readParamOrSolutionFromFile essence (tBaseDir </> paramPath)
        forM_ solutionPaths $ \ solutionPath -> do
            step (unwords ["Validating solution:", paramPath, solutionPath])
            fileShouldExist (outputsDir </> solutionPath)
            solution <- runLoggerPipeIO LogDebug $ readParamOrSolutionFromFile essence (outputsDir </> solutionPath)
            result   <- runUserErrorT $ ignoreLogs $ runNameGen () $ do
                [essence2, param2, solution2] <- resolveNamesMulti [essence, param, solution]
                validateSolution essence2 param2 solution2
            case result of
                Left errs -> assert $ vcat errs
                Right () -> return ()


checkExpectedAndExtraFiles :: Step -> [Text] -> TestDirFiles -> Assertion
checkExpectedAndExtraFiles step srOptions TestDirFiles{..} = do
    step "Checking"
    let
        relevantExts :: [String]
        relevantExts = [".eprime", ".eprime-param"]
                    ++ [".solution" | "-sat" `notElem` srOptions]       -- do not diff each individual solution
                                                                        -- if we are using a sat solver

        relevantFile :: FilePath -> Bool
        relevantFile f = or [ suffix `isSuffixOf` f
                            | suffix <- relevantExts
                            ]
    expecteds <- do
        b <- doesDirectoryExist expectedsDir
        if b
            then S.fromList . filter relevantFile <$> getDirectoryContents expectedsDir
            else return S.empty
    outputs   <- do
        b <- doesDirectoryExist outputsDir
        if b
            then S.fromList . filter relevantFile <$> getDirectoryContents outputsDir
            else return S.empty
    let extras = S.difference outputs expecteds

    step "Checking extra files"
    unless (S.null extras) $ assert $ "Unexpected files:" <+> prettyList id ", " (S.toList extras)

    step "Checking expected files"
    forM_ expecteds $ \ item -> do
        step (unwords ["Checking expected file:", item])
        let expectedPath  = expectedsDir </> item
        let generatedPath = outputsDir   </> item
        isFile <- doesFileExist generatedPath
        if isFile
            then do
                e <- T.lines <$> T.readFile expectedPath
                g <- takeWhile (/= "$ Conjure's") . T.lines <$> T.readFile generatedPath
                let
                    fmapDiff f (First x) = First (f x)
                    fmapDiff f (Second x) = Second (f x)
                    fmapDiff f (Both x y) = Both (f x) (f y)

                    isBoth Both{} = True
                    isBoth _ = False

                    diffs = filter (not . isBoth) $ getGroupedDiff e g
                    diffsString = fmap (fmapDiff (fmap textToString)) diffs

                unless (null diffs) $
                    assert $ vcat ["files differ.", pretty (ppDiff diffsString)]
            else assert $ pretty $ "file doesn't exist: " ++ generatedPath


equalNumberOfSolutions :: HasCallStack => Step -> TestDirFiles -> Assertion
equalNumberOfSolutions step TestDirFiles{..} = do
    step "Checking number of solutions"
    dirShouldExist outputsDir
    models    <- sort . filter (".eprime"       `isSuffixOf`) <$> getDirectoryContents outputsDir
    params    <- sort . filter (".eprime-param" `isSuffixOf`) <$> getDirectoryContents outputsDir
    solutions <- filter (".solution"     `isSuffixOf`) <$> getDirectoryContents outputsDir
    let
        grouped :: [ ( Maybe String             -- the parameter
                     , [(String, Int)]          -- model, number of solutions
                     ) ]
        grouped =
            (if null params
                then
                    [ (Nothing, (model, length $ filter (solnPrefix `isPrefixOf`) solutions))
                    | model' <- models
                    , let model = splitOn "." model' |> head
                    , let solnPrefix = model
                    ]
                else
                    [ (Just param, (model, length $ filter (solnPrefix `isPrefixOf`) solutions))
                    | model          <- map (head . splitOn ".") models
                    , [model2,param] <- map (splitOn "-" . head . splitOn ".") params
                    , model == model2
                    , let solnPrefix = model ++ "-" ++ param
                    ])
            |> sortBy  (comparing fst)
            |> groupBy ((==) `on` fst)
            |> map (\ grp -> (fst (head grp), map snd grp) )
    let
        differentOnes :: [(Maybe String, [(String, Int)])]
        differentOnes =
            [ this
            | this@(_, modelSols) <- grouped
            , let nbSols = map snd modelSols |> nub
            , length nbSols > 1
            ]

    unless (null differentOnes) $
        assert $ vcat
            [ maybe
                id
                (\ p -> hang ("For parameter" <+> pretty p) 4 )
                param
                $ vcat [ "Model" <+> pretty model <+> "has" <+> pretty nbSols <+> "solutions."
                       | (model, nbSols) <- modelSols
                       ]
            | (param, modelSols) <- differentOnes
            ]


noDuplicateSolutions :: HasCallStack =>(?typeCheckerMode :: TypeCheckerMode) => Step -> TestDirFiles -> Assertion
noDuplicateSolutions step TestDirFiles{..} = do
    step "Checking duplicate solutions"
    dirShouldExist outputsDir
    models    <- sort . filter (".eprime"       `isSuffixOf`) <$> getDirectoryContents outputsDir
    params    <- sort . filter (".eprime-param" `isSuffixOf`) <$> getDirectoryContents outputsDir
    solutions <- filter (".solution"     `isSuffixOf`) <$> getDirectoryContents outputsDir
    solutionContents <- forM solutions $ \ s -> do m <-runLoggerPipeIO LogDebug (readParamOrSolutionFromFile (def :: Model) (outputsDir </> s))
                                                   return (s, m)
    let
        grouped :: [ ( Maybe String             -- the parameter
                     , [(String, [String])]     -- model, duplicate solutions
                     ) ]
        grouped =
            (if null params
                then
                    [ (Nothing, (model, duplicateSolutions))
                    | model' <- models
                    , let model = splitOn "." model' |> head
                    , let solnPrefix = model
                    , let thisSolutions = filter ((solnPrefix `isPrefixOf`) . fst) solutionContents
                    , let duplicateSolutions =
                            [ s1name
                            | (s1name, s1) <- thisSolutions
                            , not $ null [ s2name | (s2name, s2) <- thisSolutions
                                         , s1name /= s2name && s1 == s2
                                         ]
                            ]
                    , not (null duplicateSolutions)
                    ]
                else
                    [ (Just param, (model, duplicateSolutions))
                    | model          <- map (head . splitOn ".") models
                    , [model2,param] <- map (splitOn "-" . head . splitOn ".") params
                    , model == model2
                    , let solnPrefix = model ++ "-" ++ param
                    , let thisSolutions = filter ((solnPrefix `isPrefixOf`) . fst) solutionContents
                    , let duplicateSolutions =
                            [ s1name
                            | (s1name, s1) <- thisSolutions
                            , not $ null [ s2name | (s2name, s2) <- thisSolutions
                                         , s1name /= s2name && s1 == s2
                                         ]
                            ]
                    , not (null duplicateSolutions)
                    ])
            |> sortBy  (comparing fst)
            |> groupBy ((==) `on` fst)
            |> map (\ grp -> (fst (head grp), map snd grp) )

    unless (null grouped) $
        assert $ vcat
            [ case param of
                Nothing -> "For model" <+> pretty model <++> rest
                Just p  -> "For parameter" <+> pretty p <> ", for model" <+> pretty model <++> rest
            | (param, duplicateSols) <- grouped
            , (model, sols) <- duplicateSols
            , let rest = "Duplicate solutions:" <++> prettyList id "," sols
            ]


assert :: HasCallStack => Doc -> Assertion
assert doc = error (renderNormal doc)


dirShouldExist :: HasCallStack => FilePath -> Assertion
dirShouldExist d = do
    b <- doesDirectoryExist d
    unless b $
        assert $ pretty ("dir does not exist: " ++ d)

fileShouldExist :: HasCallStack => FilePath -> Assertion
fileShouldExist f = do
    b <- doesFileExist f
    unless b $
        assert $ pretty ("file does not exist: " ++ f)


modelAll ::
    HasCallStack =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    FilePath -> FilePath -> FilePath -> IO ()
modelAll tBaseDir dir essenceFile = do
    additionalArgs <- catch (words . textToString <$> T.readFile (tBaseDir ++ "/additional-arguments.txt"))
                            (\ (_ :: SomeException) -> return [] )
    args <- withArgs (defaultArgs ++ additionalArgs) (cmdArgs ui)
    ignoreLogs $ runNameGen () $ mainWithArgs args
    where
        defaultArgs = [ "modelling"
                      , essenceFile
                      , "--output-directory=" ++ dir
                      , "-qf"
                      , "-ax"
                      , "--smart-filenames"
                      , "--line-width=120"
                      , "--channelling=yes"
                      , "--representation-levels=yes"
                      ]

