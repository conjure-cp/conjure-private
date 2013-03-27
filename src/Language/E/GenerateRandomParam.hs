{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.GenerateRandomParam ( generateRandomParam ) where

import Language.E
import Language.E.DomainOf(domainOf)
import Language.E.Up.Debug(upBug)
import Language.E.Up.IO(getSpec)
import Language.E.Up.ReduceSpec(reduceSpec,removeNegatives)

import Language.E.GenerateRandomParam.Data
import Language.E.GenerateRandomParam.HandleDomain
import Language.E.GenerateRandomParam.EvalChoice

import Text.Groom(groom)


generateRandomParam :: (MonadConjure m, RandomM m) => Essence -> m EssenceParam
generateRandomParam essence' = do
    essence <- removeNegatives essence'
    let stripped@(Spec _ f) = stripDecVars essence
    (Spec v e) <- reduceSpec stripped
    let es = statementAsList e

    --mkLog "Spec" (vcat $ map (\a -> prettyAsPaths a <+> "\n" ) (statementAsList f) )
    mkLog "GivensSpec"   (pretty f)
    mkLog "Reduced   " $ pretty es <+> "\n"

    doms <-  mapM domainOf es
    {-mkLog "D" (sep $ map (\a -> prettyAsPaths a <+> "\n" ) doms )-}

    choices <-  mapM handleDomain doms
    mkLog "Choices" (sep . map pretty $ choices )

    givens <- mapM evalChoice choices

    let lettings = zipWith makeLetting es givens
    mkLog "Lettings" (vcat $ map pretty lettings)
    --mkLog "Lettings" (vcat $ map (\a -> prettyAsBoth a <+> "\n" ) lettings )

    let essenceParam = Spec v (listAsStatement lettings )
    --mkLog "EssenceParam" (pretty essenceParam)

    return essenceParam

makeLetting :: E -> E -> E
makeLetting given val =
    [xMake| topLevel.letting.name := [getRef given]
          | topLevel.letting.expr := [val]|]

    where
    getRef :: E -> E
    getRef [xMatch|  _  := topLevel.declaration.given.name.reference
                  | [n] := topLevel.declaration.given.name |] = n
    getRef e = _bug "getRef: should not happen" [e]

stripDecVars :: Essence -> Essence
stripDecVars (Spec v x) = Spec v y
    where
        xs = statementAsList x
        ys = filter stays xs
        y  = listAsStatement ys

        stays [xMatch| _ := topLevel.declaration.given |] = True
        stays [xMatch| _ := topLevel.letting           |] = True
        stays [xMatch| _ := topLevel.where             |] = True
        stays _ = False


_cartesianProduct :: [a] -> [b] -> [(a,b)]
xs `_cartesianProduct` ys = [(x,y) | x <- xs, y <- ys ]



{-
     Run easily from GHCI with
     _x  =<<  _r _i
-}
_r :: IO Essence -> IO [(Either Doc EssenceParam, LogTree)]
_r sp = do
    seed <- getStdGen
    spec <- sp
    return $ runCompE "gen" (set_stdgen seed >> generateRandomParam spec)

_d :: Choice -> IO [(Either Doc E, LogTree)]
_d c = do
    seed <- getStdGen
    return $ runCompE "gen" (set_stdgen seed >> evalChoice c)

_x :: [(Either Doc a, LogTree)] -> IO ()
_x ((_, lg):_) =   print (pretty lg)
_x _ = return ()

_getTest :: FilePath -> IO Spec
_getTest f = getSpec $ "/Users/bilalh/CS/conjure/test/generateParams/" ++ f  ++ ".essence"

_b :: IO Spec
_b = _getTest "bool"

_e :: IO Spec
_e = _getTest "enum-1"

_f :: IO Spec
_f = _getTest "_func/bijective-int-int"
_f2 :: IO Spec
_f2 = _getTest "_func/bijective-int-matrix"

_i :: IO Spec
_i = _getTest "int-1"
_i2 :: IO Spec
_i2 = _getTest "int-2"

_l :: IO Spec
_l = _getTest "letting-1"

_p :: IO Spec
_p = _getTest "partition-1"

_n :: IO Spec
_n = _getTest "relation"
_n2 :: IO Spec
_n2 = _getTest "relation-all"
_nc :: IO Spec
_nc = _getTest "relation-complex"
_ns :: IO Spec
_ns = _getTest "relation-set"

_m :: IO Spec
_m = _getTest "matrixes-0"
_m2 :: IO Spec
_m2 = _getTest "matrixes"
_ms :: IO Spec
_ms = _getTest "matrixes-set"

_t :: IO Spec
_t = _getTest "tuples-0"
_t2 :: IO Spec
_t2 = _getTest "tuples"
_t3 :: IO Spec
_t3 = _getTest "tuples-set"
_t4 :: IO Spec
_t4 = _getTest "tuples-set-2"

_s :: IO Spec
_s = _getTest "set-size"
_s2 :: IO Spec
_s2 = _getTest "set-all"
_s3 :: IO Spec
_s3 = _getTest "set-max"
_s4 :: IO Spec
_s4 = _getTest "set-min"
_s5 :: IO Spec
_s5 = _getTest "set-minMax"
_sn :: IO Spec
_sn = _getTest "set-nested-1"
_sb :: IO Spec
_sb = _getTest "set-nobounds"
_sb2 :: IO Spec
_sb2 = _getTest "set-nobounds-2"
_sn2 :: IO Spec
_sn2 = _getTest "set-nested-2"
_lots :: IO Spec
_lots = _getTest "lots"

_bug :: String -> [E] -> t
_bug  s = upBug  ("GenerateRandomParam: " ++ s)
_bugg :: String -> t
_bugg s = _bug s []

