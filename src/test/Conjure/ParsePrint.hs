{-# LANGUAGE RecordWildCards #-}

module Conjure.ParsePrint (tests) where

-- conjure

import Conjure.Language.Definition (Model)
import Conjure.Language.NameGen (runNameGen)
import Conjure.Language.Pretty 
import Conjure.Language.Type (TypeCheckerMode (..))
import Conjure.Prelude
import Conjure.UI (OutputFormat (..))
import Conjure.UI.IO (readModelFromFile, writeModel)
import Conjure.UI.TypeCheck (typeCheckModel_StandAlone)
import Conjure.UserError

-- base
import System.Info (os)

-- tasty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCaseSteps)

import Data.Aeson (decode')
import qualified Data.Aeson as JSON
import Data.Aeson.Diff
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text
import Data.Text.Encoding (encodeUtf8Builder)
import qualified Data.Aeson.Types as JSON

tests ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    IO TestTree
tests = do
    let baseDir = "tests/parse_print"
    dirs <- mapM (isTestDir baseDir) =<< getAllDirs baseDir
    let testCases = map testSingleDir (catMaybes dirs)
    return (testGroup "parse_print" testCases)

data TestDirFiles = TestDirFiles
    { name :: String -- a name for the test case
    , tBaseDir :: FilePath -- dir
    , essenceFile :: FilePath -- dir + filename
    }
    deriving (Show)

-- returns True if the argument points to a directory that is not hidden
isTestDir :: FilePath -> FilePath -> IO (Maybe TestDirFiles)
isTestDir baseDir dir = do
    essenceFiles <- filter (".essence" `isSuffixOf`) <$> getDirectoryContents dir
    case essenceFiles of
        [f] ->
            return $
                Just
                    TestDirFiles
                        { name = drop (length baseDir + 1) dir
                        , tBaseDir = dir
                        , essenceFile = dir </> f
                        }
        _ -> return Nothing

-- the first FilePath is the base directory for the parse_print tests
-- we know at this point that the second FilePath points to a directory D,
-- which contains + an Essence file D/D.essence
testSingleDir ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    TestDirFiles ->
    TestTree
testSingleDir TestDirFiles{..} = testCaseSteps (map (\ch -> if ch == '/' then '.' else ch) name) $ \step -> do
    step "Conjuring"
    model_ <- runUserErrorT (readModelFromFile essenceFile)
    let tyCheck :: Model -> Either Doc ()
        tyCheck m = runNameGen () $ ignoreLogs $ void $ typeCheckModel_StandAlone m
    let result :: Either Doc Model
        result = case model_ of
                    Left err -> userErr err
                    Right model -> Right model
    case result of
        Left "" -> do
            removeFileIfExists (tBaseDir </> "stderr")
            removeFileIfExists (tBaseDir </> "stdout")
        Left err -> do
            writeFile (tBaseDir </> "stderr") (renderNormal err)
            removeFileIfExists (tBaseDir </> "stdout")
        Right model -> do
            writeModel 120 ASTJSON (Just (tBaseDir </> "model.json")) model
            writeModel 120 Plain (Just (tBaseDir </> "stdout")) model
            case tyCheck model of
                Left err -> writeFile (tBaseDir </> "typecheck") (renderNormal err)
                Right () -> writeFile (tBaseDir </> "typecheck") ""
            removeFileIfExists (tBaseDir </> "stderr")

    let fixWindowsPaths :: String -> String
        fixWindowsPaths
            | os `elem` ["mingw32"] = fixBackslashes
            | otherwise = id

        fixBackslashes :: String -> String
        fixBackslashes ('/' : '\\' : xs) = "/\\" ++ fixBackslashes xs
        fixBackslashes ('\\' : '/' : xs) = "\\/" ++ fixBackslashes xs
        fixBackslashes ('\\' : xs) = '/' : fixBackslashes xs
        fixBackslashes [] = []
        fixBackslashes (x : xs) = x : fixBackslashes xs

        readIfExists :: FilePath -> IO String
        readIfExists f = fromMaybe "" <$> readFileIfExists f

    step "Checking stderr"
    stderrG <- fixWindowsPaths <$> readIfExists (tBaseDir </> "stderr")
    stderrE <- readIfExists (tBaseDir </> "stderr.expected")
    unless (stderrE == stderrG) $
        assertFailure $
            renderNormal $
                vcat
                    [ "unexpected stderr:" <++> pretty stderrG
                    , "was expecting:    " <++> pretty stderrE
                    ]

    step "Checking stdout"
    stdoutG <- fixWindowsPaths <$> readIfExists (tBaseDir </> "stdout")
    stdoutE <- readIfExists (tBaseDir </> "stdout.expected")
    unless (stdoutE == stdoutG) $
        assertFailure $
            renderNormal $
                vcat
                    [ "unexpected stdout:" <++> pretty stdoutG
                    , "was expecting:    " <++> pretty stdoutE
                    ]

    step "Checking Generated Representation"
    modelG <- fixWindowsPaths <$> readIfExists (tBaseDir </> "model.json")
    unless (null modelG) $ do
        modelE <- readIfExists (tBaseDir </> "model.expected.json")
        let diffs = do
                jGiven <- stringToJson modelG
                jReference <- stringToJson modelE
                let Patch ds = diff jGiven jReference
                return ds
        case diffs of
            Nothing -> assertFailure $ "JSON parser error in" ++ modelE
            Just [] -> return ()
            Just ops -> assertFailure $ renderNormal $ vcat ["Difference in json:" <++> vcat (map (stringToDoc . show) ops)]

    step "Checking Types"
    typecheckG <- fixWindowsPaths <$> readIfExists (tBaseDir </> "typecheck")
    unless (null typecheckG) $ do
        typecheckE <- readIfExists (tBaseDir </> "typecheck.expected")
        unless (typecheckE == typecheckG) $
            assertFailure $
                renderNormal $
                    vcat
                        [ "unexpected typeError:" <++> pretty typecheckG
                        , "was expecting:    " <++> pretty typecheckE
                        ]

stringToJson :: String -> Maybe JSON.Value
stringToJson "" = Just JSON.emptyObject
stringToJson s = decode' $ toLazyByteString $ encodeUtf8Builder $ Data.Text.pack s
