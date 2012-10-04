{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ( isSuffixOf )
import System.Environment ( getArgs )

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.ConjureRefn ( conjureRefn )
import Language.E.Pipeline.Groom ( groomSpec )


main :: IO ()
main = do
    args <- getArgs

    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    let refnFilenames = filter (".rule" `isSuffixOf`) args
    -- when (null refnFilenames)
    --     $ error "Warning: no *.rule file is given."

    specPair  <- pairWithContents specFilename
    refnPairs <- mapM pairWithContents refnFilenames

    [spec ] <- runCompEIO (readSpec specPair)
    [refns] <- runCompEIO (concat <$> mapM readRuleRefn refnPairs)

    outSpecs <- runCompEIO (conjureRefn spec refns) >>= concatMapM groomSpec

    -- putStrLn "[ === Generated === ]"
    -- putStrLn ""
    -- mapM_ (putStrLn . renderPretty) outSpecs

    -- writeSpecs (dropExtEssence specFilename) "refn" outSpecs
    writeSpecs specFilename "refn" outSpecs
