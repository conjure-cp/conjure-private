{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Conjure.RepresentationsTest ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Process.Enumerate ( EnumerateDomainNoIO(..) )
import Conjure.Representations ( downC, up, downC1, up1 )

-- tasty
import Test.Tasty
import Test.Tasty.HUnit ( Assertion, testCase, assertFailure, (@?=) )
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC


tests ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    TestTree
tests = testGroup "representations"

    [ testCase "bool #1" $
        let
            highDomain = DomainBool
            highConstant = ConstantBool False
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testCase "bool #2" $
        let
            highDomain = DomainBool
            highConstant = ConstantBool True
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testCase "int #1" $
        let
            highDomain = intDomain 1 4
            highConstant = (ConstantInt TagInt) 3
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testCase "matrix of bool" $
        let
            highDomain = DomainMatrix (intDomain 1 3) DomainBool
            highConstant = ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testCase "matrix of int" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (intDomain 1 5)
            highConstant = ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2, (ConstantInt TagInt) 3, (ConstantInt TagInt) 5]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testCase "matrix 2d of bool" $
        let
            highDomain =
                DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) DomainBool)
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantBool False, ConstantBool True ]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantBool True , ConstantBool False]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantBool True , ConstantBool True ]
                    ]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testCase "matrix 2d of int" $
        let
            highDomain =
                DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 3, (ConstantInt TagInt) 7]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 2, (ConstantInt TagInt) 8]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 0, (ConstantInt TagInt) 1]
                    ]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testCase "(bool, int)" $
        let
            highDomain = DomainTuple [DomainBool, intDomain 1 3]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2]
            low = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, (ConstantInt TagInt) 2      )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "(bool, int, bool)" $
        let
            highDomain = DomainTuple [DomainBool, intDomain 1 3, DomainBool]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2, ConstantBool True]
            low = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, (ConstantInt TagInt) 2      )
                  , ( "x_3", DomainBool   , ConstantBool True  )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "((bool, int), bool)" $
        let
            highDomain = DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2], ConstantBool True]
            mid = [ ( "x_1", DomainTuple [DomainBool, intDomain 1 3], ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2] )
                  , ( "x_2", DomainBool, ConstantBool True )
                  ]
            low = [ ( "x_1_1", DomainBool   , ConstantBool False )
                  , ( "x_1_2", intDomain 1 3, (ConstantInt TagInt) 2      )
                  , ( "x_2"  , DomainBool   , ConstantBool True  )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "(bool, (int, bool))" $
        let
            highDomain = DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool]]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool True]]
            mid = [ ( "x_1", DomainBool, ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainBool], ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool True] )
                  ]
            low = [ ( "x_1"  , DomainBool   , ConstantBool False )
                  , ( "x_2_1", intDomain 1 3, (ConstantInt TagInt) 2      )
                  , ( "x_2_2", DomainBool   , ConstantBool True  )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "(bool, int, bool, int)" $
        let
            highDomain = DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2, ConstantBool True, (ConstantInt TagInt) 4]
            low = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, (ConstantInt TagInt) 2      )
                  , ( "x_3", DomainBool   , ConstantBool True  )
                  , ( "x_4", intDomain 2 5, (ConstantInt TagInt) 4      )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "((bool, int), (bool, int))" $
        let
            highDomain = DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2], ConstantAbstract $ AbsLitTuple [ConstantBool True, (ConstantInt TagInt) 4]]
            mid = [ ( "x_1", DomainTuple [DomainBool, intDomain 1 3], ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2] )
                  , ( "x_2", DomainTuple [DomainBool, intDomain 2 5], ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4] )
                  ]
            low = [ ( "x_1_1", DomainBool   , ConstantBool False )
                  , ( "x_1_2", intDomain 1 3, (ConstantInt TagInt) 2      )
                  , ( "x_2_1", DomainBool   , ConstantBool True  )
                  , ( "x_2_2", intDomain 2 5, (ConstantInt TagInt) 4      )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "(bool, (int, (bool, int)))" $
        let
            highDomain = DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantAbstract $ AbsLitTuple [ConstantBool True, (ConstantInt TagInt) 4]]]
            mid = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]
                           , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantAbstract $ AbsLitTuple [ConstantBool True, (ConstantInt TagInt) 4]] )
                  ]
            low = [ ( "x_1"    , DomainBool   , ConstantBool False )
                  , ( "x_2_1"  , intDomain 1 3, (ConstantInt TagInt) 2      )
                  , ( "x_2_2_1", DomainBool   , ConstantBool True  )
                  , ( "x_2_2_2", intDomain 2 5, (ConstantInt TagInt) 4      )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "(bool, (int, bool), int)" $
        let
            highDomain = DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool True], (ConstantInt TagInt) 4]
            mid = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainBool], ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool True] )
                  , ( "x_3", intDomain 2 5, (ConstantInt TagInt) 4 )
                  ]
            low = [ ( "x_1"  , DomainBool   , ConstantBool False )
                  , ( "x_2_1", intDomain 1 3, (ConstantInt TagInt) 2      )
                  , ( "x_2_2", DomainBool   , ConstantBool True  )
                  , ( "x_3"  , intDomain 2 5, (ConstantInt TagInt) 4      )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "(((bool, int), bool), int)" $
        let
            highDomain = DomainTuple [DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2], ConstantBool True], (ConstantInt TagInt) 4]
            mid = [ ( "x_1", DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool]
                           , ConstantAbstract $ AbsLitTuple [ ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2], ConstantBool True] )
                  , ( "x_2", intDomain 2 5, (ConstantInt TagInt) 4 )
                  ]
            low = [ ( "x_1_1_1", DomainBool   , ConstantBool False )
                  , ( "x_1_1_2", intDomain 1 3, (ConstantInt TagInt) 2      )
                  , ( "x_1_2"  , DomainBool   , ConstantBool True  )
                  , ( "x_2"    , intDomain 2 5, (ConstantInt TagInt) 4      )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "matrix of (bool, int)" $
        let
            highDomain =
                DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 0 9])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 0]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 3]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 4]
                    ]
            low = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 0, (ConstantInt TagInt) 3, (ConstantInt TagInt) 4] )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "matrix of (bool, int, bool)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2, ConstantBool True]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 3, ConstantBool False]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 4, ConstantBool False]
                    ]
            low = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2     , (ConstantInt TagInt) 3     , (ConstantInt TagInt) 4     ] )
                  , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "matrix of ((bool, int), bool)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2], ConstantBool True]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 3], ConstantBool False]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4], ConstantBool False]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                                [ ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2]
                                , ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 3]
                                , ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4]
                                ] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ]
            low = [ ( "x_1_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_1_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2     , (ConstantInt TagInt) 3     , (ConstantInt TagInt) 4     ] )
                  , ( "x_2"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "matrix of (bool, (int, bool))" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 0 9, DomainBool]])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 0, ConstantBool True]]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 3, ConstantBool False]]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantBool True]]
                    ]
            mid = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9, DomainBool])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                               [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 0, ConstantBool True]
                               , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 3, ConstantBool False]
                               , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantBool True]
                               ] )
                  ]
            low = [ ( "x_1"  , DomainMatrix (intDomain 1 3) DomainBool
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2_1", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 0, (ConstantInt TagInt) 3, (ConstantInt TagInt) 4] )
                  , ( "x_2_2", DomainMatrix (intDomain 1 3) DomainBool
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True, ConstantBool False, ConstantBool True] )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "matrix of (bool, int, bool, int)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2, ConstantBool True , (ConstantInt TagInt) 4]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 3, ConstantBool False, (ConstantInt TagInt) 6]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4, ConstantBool False, (ConstantInt TagInt) 8]
                    ]
            low = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2     , (ConstantInt TagInt) 3     , (ConstantInt TagInt) 4     ] )
                  , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  , ( "x_4", DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4     , (ConstantInt TagInt) 6     , (ConstantInt TagInt) 8     ] )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "matrix of ((bool, int), (bool, int))" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2], ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4]]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 3], ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 6]]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4], ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 8]]
                    ]
            mid = [ ( "x_1"
                    , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3])
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2]
                        , ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 3]
                        , ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4]
                        ] )
                  , ( "x_2"
                    , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 2 5])
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4]
                        , ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 6]
                        , ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 8]
                        ] )
                  ]
            low = [ ( "x_1_1", DomainMatrix   (intDomain 1 3) DomainBool
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_1_2", DomainMatrix   (intDomain 1 3) (intDomain 1 3)
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2     , (ConstantInt TagInt) 3     , (ConstantInt TagInt) 4     ] )
                  , ( "x_2_1", DomainMatrix   (intDomain 1 3) DomainBool
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  , ( "x_2_2", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4     , (ConstantInt TagInt) 6     , (ConstantInt TagInt) 8     ] )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "matrix of (bool, (int, (bool, int)))" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4]]]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 3, ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 6]]]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 8]]]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                               [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4]]
                               , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 3, ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 6]]
                               , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 8]]
                               ] )
                  ]
            low = [ ( "x_1"    , DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
                  , ( "x_2_1"  , DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2     , (ConstantInt TagInt) 3     , (ConstantInt TagInt) 4     ])
                  , ( "x_2_2_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
                  , ( "x_2_2_2", DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4     , (ConstantInt TagInt) 6     , (ConstantInt TagInt) 8     ])
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "matrix of (bool, (int, bool), int)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool True ], (ConstantInt TagInt) 4]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 3, ConstantBool False], (ConstantInt TagInt) 6]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantBool False], (ConstantInt TagInt) 8]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (DomainTuple [intDomain 1 3, DomainBool])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                               [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool True ]
                               , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 3, ConstantBool False]
                               , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantBool False]
                               ] )
                  , ( "x_3", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4, (ConstantInt TagInt) 6, (ConstantInt TagInt) 8]
                           )
                  ]
            low = [ ( "x_1"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
                  , ( "x_2_1", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2     , (ConstantInt TagInt) 3     , (ConstantInt TagInt) 4     ])
                  , ( "x_2_2", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
                  , ( "x_3"  , DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4     , (ConstantInt TagInt) 6     , (ConstantInt TagInt) 8     ])
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "matrix of (((bool, int), bool), int)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2], ConstantBool True ], (ConstantInt TagInt) 4]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 3], ConstantBool False], (ConstantInt TagInt) 6]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4], ConstantBool False], (ConstantInt TagInt) 8]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool,intDomain 1 3],DomainBool])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                               [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2], ConstantBool True ]
                               , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 3], ConstantBool False]
                               , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4], ConstantBool False]
                               ])
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4, (ConstantInt TagInt) 6, (ConstantInt TagInt) 8]
                           )
                  ]
            low = [ ( "x_1_1_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
                  , ( "x_1_1_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2     , (ConstantInt TagInt) 3     , (ConstantInt TagInt) 4     ])
                  , ( "x_1_2"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
                  , ( "x_2"    , DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4     , (ConstantInt TagInt) 6     , (ConstantInt TagInt) 8     ])
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "matrix 2d of (((bool, int), bool), int)" $
        let
            highDomain =
                DomainMatrix (intDomain 1 2)
                    (DomainMatrix (intDomain 1 3)
                        (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5]))
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                    [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 2], ConstantBool True ], (ConstantInt TagInt) 4]
                        , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 3], ConstantBool False], (ConstantInt TagInt) 6]
                        , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 4], ConstantBool False], (ConstantInt TagInt) 8]
                        ]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, (ConstantInt TagInt) 4], ConstantBool True ], (ConstantInt TagInt) 4]
                        , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 5], ConstantBool False], (ConstantInt TagInt) 7]
                        , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , (ConstantInt TagInt) 6], ConstantBool False], (ConstantInt TagInt) 9]
                        ]
                    ]
            mid =
                [ ( "x_1" , DomainMatrix (intDomain 1 2)
                              (DomainMatrix (intDomain 1 3)
                                  (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]))
                          , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                              [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                                  [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False,(ConstantInt TagInt) 2],ConstantBool True]
                                  , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False,(ConstantInt TagInt) 3],ConstantBool False]
                                  , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True,(ConstantInt TagInt) 4],ConstantBool False]
                                  ]
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                                  [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False,(ConstantInt TagInt) 4],ConstantBool True]
                                  , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True,(ConstantInt TagInt) 5],ConstantBool False]
                                  , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True,(ConstantInt TagInt) 6],ConstantBool False]
                                  ]
                              ] )
                , ( "x_2" , DomainMatrix (intDomain 1 2)
                                  (DomainMatrix (intDomain 1 3)
                                      (intDomain 2 5))
                          , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4,(ConstantInt TagInt) 6,(ConstantInt TagInt) 8]
                                , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4,(ConstantInt TagInt) 7,(ConstantInt TagInt) 9]
                                ] )
                ]
            low =
                [ ( "x_1_1_1" , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) DomainBool)
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                  [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False,ConstantBool False,ConstantBool True]
                                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False,ConstantBool True,ConstantBool True]
                                  ] )
                , ( "x_1_1_2" , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) (intDomain 1 3))
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                  [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 3,(ConstantInt TagInt) 4]
                                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4,(ConstantInt TagInt) 5,(ConstantInt TagInt) 6]
                                  ] )
                , ( "x_1_2"   , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) DomainBool)
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                  [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool False,ConstantBool False]
                                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool False,ConstantBool False]
                                  ] )
                , ( "x_2"     , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) (intDomain 2 5))
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                  [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4,(ConstantInt TagInt) 6,(ConstantInt TagInt) 8]
                                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 4,(ConstantInt TagInt) 7,(ConstantInt TagInt) 9]
                                  ] )
                ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "(bool, bool, bool)" $ testCasesAuto "x"
        ( DomainTuple [DomainBool, DomainBool, DomainBool] )
        ( ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantBool False, ConstantBool True] )

    , testCase "(bool, matrix of int) {auto}" $ testCasesAuto "x"
        ( DomainTuple
            [ DomainBool
            , DomainMatrix (intDomain 1 3) (intDomain 0 9)
            ] )
        ( ConstantAbstract $ AbsLitTuple
            [ ConstantBool False
            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2, (ConstantInt TagInt) 4, (ConstantInt TagInt) 5]
            ] )

    , testCase "(bool, matrix of int)" $
        let
            highDomain =
                DomainTuple
                    [ DomainBool
                    , DomainMatrix (intDomain 1 3) (intDomain 0 9)
                    ]
            highConstant =
                ConstantAbstract $ AbsLitTuple
                    [ ConstantBool False
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2, (ConstantInt TagInt) 4, (ConstantInt TagInt) 5]
                    ]
            low =
                [ ( "x_1", DomainBool,ConstantBool False)
                , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                         , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 4,(ConstantInt TagInt) 5] )
                ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "(bool, matrix of (int, bool)) {auto}" $ testCasesAuto "x"
        ( DomainTuple
            [ DomainBool
            , DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9, DomainBool])
            ] )
        ( ConstantAbstract $ AbsLitTuple
            [ ConstantBool False
            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool False]
                , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantBool True]
                , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantBool False]
                ]
            ] )

    , testCase "(bool, matrix of (int, bool))" $
        let
            highDomain =
                DomainTuple
                    [ DomainBool
                    , DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9, DomainBool])
                    ]
            highConstant =
                ConstantAbstract $ AbsLitTuple
                    [ ConstantBool False
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool False]
                        , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantBool True]
                        , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantBool False]
                        ]
                    ]
            mid =
                [ ( "x_1" , DomainBool , ConstantBool False )
                , ( "x_2" , DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9,DomainBool])
                          , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                              [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2,ConstantBool False]
                              , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4,ConstantBool True]
                              , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5,ConstantBool False]
                              ] )
                ]
            low =
                [ ( "x_1"   , DomainBool , ConstantBool False )
                , ( "x_2_1" , DomainMatrix (intDomain 1 3) (intDomain 0 9) , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 4,(ConstantInt TagInt) 5] )
                , ( "x_2_2" , DomainMatrix (intDomain 1 3) DomainBool      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False,ConstantBool True,ConstantBool False] )
                ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "(bool, matrix of (int, matrix of int)) {auto}" $ testCasesAuto "x"
        ( DomainTuple
            [ DomainBool
            , DomainMatrix (intDomain 1 3) (DomainTuple
                [ intDomain 0 9
                , DomainMatrix (intDomain 1 2) (intDomain 0 9)
                ])
            ] )
        ( ConstantAbstract $ AbsLitTuple
            [ ConstantBool False
            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 1, (ConstantInt TagInt) 3]]
                , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 3, (ConstantInt TagInt) 5]]
                , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 5, (ConstantInt TagInt) 6]]
                ]
            ] )

    , testCase "(bool, matrix of (int, matrix of int))" $
        let
            highDomain =
                DomainTuple
                    [ DomainBool
                    , DomainMatrix (intDomain 1 3) (DomainTuple
                        [ intDomain 0 9
                        , DomainMatrix (intDomain 1 2) (intDomain 0 9)
                        ])
                    ]
            highConstant =
                ConstantAbstract $ AbsLitTuple
                    [ ConstantBool False
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 1, (ConstantInt TagInt) 3]]
                        , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 3, (ConstantInt TagInt) 5]]
                        , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 5, (ConstantInt TagInt) 6]]
                        ]
                    ]
            mid =
                [ ( "x_1" , DomainBool,ConstantBool False )
                , ( "x_2" , DomainMatrix   (intDomain 1 3) (DomainTuple [intDomain 0 9,DomainMatrix (intDomain 1 2) (intDomain 0 9)])
                          , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                              [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2,ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 1,(ConstantInt TagInt) 3]]
                              , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 4,ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 3,(ConstantInt TagInt) 5]]
                              , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5,ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 5,(ConstantInt TagInt) 6]]
                              ] )
                ]
            low =
                [ ( "x_1"   , DomainBool,ConstantBool False )
                , ( "x_2_1" , DomainMatrix   (intDomain 1 3) (intDomain 0 9)
                            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 4,(ConstantInt TagInt) 5])
                , ( "x_2_2" , DomainMatrix   (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
                            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                                [ ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 1,(ConstantInt TagInt) 3]
                                , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 3,(ConstantInt TagInt) 5]
                                , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 5,(ConstantInt TagInt) 6]
                                ] )
                ]
        in  testCases "x" highDomain highConstant Just mid low

-- Explicit
    , testCase "Explicit: set (size 4) of int {auto}" $ testCasesAuto "x"
        ( DomainSet
            Set_Explicit
            (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 4)))
            (intDomain 0 9) )
        ( ConstantAbstract $ AbsLitSet
            [(ConstantInt TagInt) 2, (ConstantInt TagInt) 3, (ConstantInt TagInt) 5, (ConstantInt TagInt) 6] )

    , testCase "Explicit: set (size 4) of int" $
        let
            highDomain =
                DomainSet
                    Set_Explicit
                    (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 4)))
                    (intDomain 0 9)
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [(ConstantInt TagInt) 2, (ConstantInt TagInt) 3, (ConstantInt TagInt) 5, (ConstantInt TagInt) 6]
            low =
                [ ( "x_Explicit"
                  , DomainMatrix   (intDomain 1 4) (intDomain 0 9)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                        [(ConstantInt TagInt) 2,(ConstantInt TagInt) 3,(ConstantInt TagInt) 5,(ConstantInt TagInt) 6]
                  ) ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "Explicit: set (size 4) of set (size 2) of int {auto}" $ testCasesAuto "x"
        ( DomainSet Set_Explicit (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 4)))
            ( DomainSet Set_Explicit (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 2)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 3]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 5, (ConstantInt TagInt) 6]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 5, (ConstantInt TagInt) 7]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 5, (ConstantInt TagInt) 8]
            ] )

    , testCase "Explicit: set (size 4) of set (size 2) of int" $
        let
            highDomain =
                DomainSet Set_Explicit (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 4)))
                    (DomainSet Set_Explicit (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 2)))
                        (intDomain 0 9))
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 3]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 5, (ConstantInt TagInt) 6]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 5, (ConstantInt TagInt) 7]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 5, (ConstantInt TagInt) 8]
                    ]
            mid =
                [ ( "x_ExplicitR3"
                  , DomainMatrix   (intDomain 1 4) (DomainSet Set_Explicit (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 2))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                        [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 3]
                        , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 5, (ConstantInt TagInt) 6]
                        , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 5, (ConstantInt TagInt) 7]
                        , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 5, (ConstantInt TagInt) 8]
                        ]
                  ) ]
            low =
                [ ( "x_ExplicitR3_Explicit"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                        [ ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 2, (ConstantInt TagInt) 3]
                        , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 5, (ConstantInt TagInt) 6]
                        , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 5, (ConstantInt TagInt) 7]
                        , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [(ConstantInt TagInt) 5, (ConstantInt TagInt) 8]
                        ]
                  ) ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "Explicit: set (size 4) of set (size 2) of (int, bool) {auto}" $ testCasesAuto "x"
        ( DomainSet Set_Explicit (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 4)))
            ( DomainSet Set_Explicit (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 2)))
                (DomainTuple [intDomain 0 9, DomainBool])
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool False]
                          , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 3, ConstantBool True ]
                          ]
            , ConstantAbstract $ AbsLitSet [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantBool True ]
                          , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 6, ConstantBool True ]
                          ]
            , ConstantAbstract $ AbsLitSet [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantBool True ]
                          , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 7, ConstantBool False]
                          ]
            , ConstantAbstract $ AbsLitSet [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantBool False]
                          , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 8, ConstantBool False]
                          ]
            ] )

    , testCase "Explicit: set (size 4) of (int, set (size 2) of (int, bool)) {auto}" $ testCasesAuto "x"
        ( DomainSet Set_Explicit (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 4)))
            ( DomainTuple
                [ intDomain 0 8
                , DomainSet Set_Explicit (SetAttr (SizeAttr_Size ((ConstantInt TagInt) 2)))
                    (DomainTuple [intDomain 0 9, DomainBool])
                ]
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitTuple
                [ (ConstantInt TagInt) 1
                , ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 2, ConstantBool False]
                    , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 3, ConstantBool True ]
                    ]
                ]
            , ConstantAbstract $ AbsLitTuple
                [ (ConstantInt TagInt) 2
                , ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantBool True ]
                    , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 6, ConstantBool True ]
                    ]
                ]
            , ConstantAbstract $ AbsLitTuple
                [ (ConstantInt TagInt) 3
                , ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantBool True ]
                    , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 7, ConstantBool False]
                    ]
                ]
            , ConstantAbstract $ AbsLitTuple
                [ (ConstantInt TagInt) 4
                , ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 5, ConstantBool False]
                    , ConstantAbstract $ AbsLitTuple [(ConstantInt TagInt) 8, ConstantBool False]
                    ]
                ]
            ] )

-- ExplicitVarSizeWithMarker
    , testCase "ExplicitVarSizeWithMarker: set (maxSize 4) of int {auto}" $ testCasesAuto "x"
        ( DomainSet
            Set_ExplicitVarSizeWithMarker
            (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
            (intDomain 0 9) )
        ( ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5] )

    , testCase "ExplicitVarSizeWithMarker: set (maxSize 4) of int" $
        let
            highDomain =
                DomainSet Set_ExplicitVarSizeWithMarker (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4))) (intDomain 0 9)
            highConstant =
                ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
            low =
                [ ( "x_ExplicitVarSizeWithMarker_Marker"
                  , intDomain 0 4
                  , (ConstantInt TagInt) 2
                  )
                , ( "x_ExplicitVarSizeWithMarker_Values"
                  , DomainMatrix (intDomain 1 4) (intDomain 0 9)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 5,(ConstantInt TagInt) 0,(ConstantInt TagInt) 0]
                  )
                ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "ExplicitVarSizeWithMarker: set (maxSize 4) of set (maxSize 3) int {auto}" $ testCasesAuto "x"
        ( DomainSet Set_ExplicitVarSizeWithMarker (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
            ( DomainSet Set_ExplicitVarSizeWithMarker (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3, (ConstantInt TagInt) 4, (ConstantInt TagInt) 6]
            ]
        )

    , testCase "ExplicitVarSizeWithMarker: set (maxSize 4) of set (maxSize 3) int" $
        let
            highDomain =
                DomainSet Set_ExplicitVarSizeWithMarker (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
                    ( DomainSet Set_ExplicitVarSizeWithMarker (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3)))
                        (intDomain 0 9) )
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3, (ConstantInt TagInt) 4, (ConstantInt TagInt) 6]
                    ]
            mid =
                [ ( "x_ExplicitVarSizeWithMarkerR5_Marker"
                  , intDomain 0 4
                  , (ConstantInt TagInt) 3
                  )
                , ( "x_ExplicitVarSizeWithMarkerR5_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainSet Set_ExplicitVarSizeWithMarker (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
                      , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2,(ConstantInt TagInt) 5]
                      , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3,(ConstantInt TagInt) 4,(ConstantInt TagInt) 6]
                      , ConstantAbstract $ AbsLitSet []
                      ]
                  )
                ]
            low =
                [ ( "x_ExplicitVarSizeWithMarkerR5_Marker"
                  , intDomain 0 4
                  , (ConstantInt TagInt) 3
                  )
                , ( "x_ExplicitVarSizeWithMarkerR5_Values_ExplicitVarSizeWithMarker_Marker"
                  , DomainMatrix   (intDomain 1 4) (intDomain 0 3)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [(ConstantInt TagInt) 1,(ConstantInt TagInt) 2,(ConstantInt TagInt) 3,(ConstantInt TagInt) 0]
                  )
                , ( "x_ExplicitVarSizeWithMarkerR5_Values_ExplicitVarSizeWithMarker_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 1 3) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 0,(ConstantInt TagInt) 0]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 5,(ConstantInt TagInt) 0]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 3,(ConstantInt TagInt) 4,(ConstantInt TagInt) 6]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 0,(ConstantInt TagInt) 0,(ConstantInt TagInt) 0]
                      ]
                  )
                ]
        in  testCases "x" highDomain highConstant Just mid low

-- ExplicitVarSizeWithFlags
    , testCase "ExplicitVarSizeWithFlags: set (maxSize 4) of int {auto}" $ testCasesAuto "x"
        ( DomainSet
            Set_ExplicitVarSizeWithFlags
            (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
            (intDomain 0 9) )
        ( ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5] )

    , testCase "ExplicitVarSizeWithFlags: set (maxSize 4) of int" $
        let
            highDomain =
                DomainSet Set_ExplicitVarSizeWithFlags (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4))) (intDomain 0 9)
            highConstant =
                ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
            low =
                [ ( "x_ExplicitVarSizeWithFlags_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool False,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlags_Values"
                  , DomainMatrix   (intDomain 1 4) (intDomain 0 9)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 5,(ConstantInt TagInt) 0,(ConstantInt TagInt) 0]
                  )
                ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "ExplicitVarSizeWithFlags: set (maxSize 4) of set (maxSize 3) int {auto}" $ testCasesAuto "x"
        ( DomainSet Set_ExplicitVarSizeWithFlags (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
            ( DomainSet Set_ExplicitVarSizeWithFlags (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3, (ConstantInt TagInt) 4, (ConstantInt TagInt) 6]
            ]
        )

    , testCase "ExplicitVarSizeWithFlags: set (maxSize 4) of set (maxSize 3) int" $
        let
            highDomain =
                DomainSet Set_ExplicitVarSizeWithFlags (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
                    ( DomainSet Set_ExplicitVarSizeWithFlags (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3)))
                        (intDomain 0 9) )
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3, (ConstantInt TagInt) 4, (ConstantInt TagInt) 6]
                    ]
            mid =
                [ ( "x_ExplicitVarSizeWithFlagsR4_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool True,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlagsR4_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainSet Set_ExplicitVarSizeWithFlags (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
                      , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2,(ConstantInt TagInt) 5]
                      , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3,(ConstantInt TagInt) 4,(ConstantInt TagInt) 6]
                      , ConstantAbstract $ AbsLitSet []
                      ]
                  )
                ]
            low =
                [ ( "x_ExplicitVarSizeWithFlagsR4_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool True,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlagsR4_Values_ExplicitVarSizeWithFlags_Flags"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 1 3) DomainBool)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool False,ConstantBool False]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool True,ConstantBool False]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool True,ConstantBool True]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False,ConstantBool False,ConstantBool False]
                      ]
                  )
                , ( "x_ExplicitVarSizeWithFlagsR4_Values_ExplicitVarSizeWithFlags_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 1 3) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 0,(ConstantInt TagInt) 0]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 2,(ConstantInt TagInt) 5,(ConstantInt TagInt) 0]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 3,(ConstantInt TagInt) 4,(ConstantInt TagInt) 6]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [(ConstantInt TagInt) 0,(ConstantInt TagInt) 0,(ConstantInt TagInt) 0]
                      ]
                  ) ]
        in  testCases "x" highDomain highConstant Just mid low

-- Occurrence
    , testCase "Occurrence: set (maxSize 4) of int {auto}" $ testCasesAuto "x"
        ( DomainSet
            Set_Occurrence
            (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
            (intDomain 0 9) )
        ( ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5] )

    , testCase "Occurrence: set (maxSize 4) of int" $
        let
            highDomain =
                DomainSet Set_Occurrence (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4))) (intDomain 0 9)
            highConstant =
                ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
            low =
                [ ( "x_Occurrence"
                  , DomainMatrix   (intDomain 0 9) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 0 9)
                      [ ConstantBool False
                      , ConstantBool False
                      , ConstantBool True -- 2
                      , ConstantBool False
                      , ConstantBool False
                      , ConstantBool True -- 5
                      , ConstantBool False
                      , ConstantBool False
                      , ConstantBool False
                      , ConstantBool False
                      ]
                  )
                ]
        in  testCases "x" highDomain highConstant Just low low

    , testCase "ExplicitVarSizeWithMarker & Occurrence: set (maxSize 4) of set (maxSize 3) int {auto}" $ testCasesAuto "x"
        ( DomainSet Set_ExplicitVarSizeWithMarker (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
            ( DomainSet Set_Occurrence (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3, (ConstantInt TagInt) 4, (ConstantInt TagInt) 6]
            ]
        )

    , testCase "ExplicitVarSizeWithMarker & Occurrence: set (maxSize 4) of set (maxSize 3) int" $
        let
            highDomain =
                DomainSet Set_ExplicitVarSizeWithMarker (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
                    ( DomainSet Set_Occurrence (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3)))
                        (intDomain 0 9) )
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3, (ConstantInt TagInt) 4, (ConstantInt TagInt) 6]
                    ]
            mid =
                [ ( "x_ExplicitVarSizeWithMarkerR2_Marker"
                  , intDomain 0 4
                  , (ConstantInt TagInt) 3
                  )
                , ( "x_ExplicitVarSizeWithMarkerR2_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainSet Set_Occurrence (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
                      , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2,(ConstantInt TagInt) 5]
                      , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3,(ConstantInt TagInt) 4,(ConstantInt TagInt) 6]
                      , ConstantAbstract $ AbsLitSet []
                      ]
                  )
                ]
            low =
                [ ( "x_ExplicitVarSizeWithMarkerR2_Marker"
                  , intDomain 0 4
                  , (ConstantInt TagInt) 3
                  )
                , ( "x_ExplicitVarSizeWithMarkerR2_Values_Occurrence"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 0 9) DomainBool)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 2
                          [ ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False
                          , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 2,5
                          [ ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False
                          , ConstantBool True , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 3,4,6
                          [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool True
                          , ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- {}
                          [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False
                          , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      ]
                  )
                ]
        in  testCases "x" highDomain highConstant Just mid low

    , testCase "ExplicitVarSizeWithFlags & Occurrence: set (maxSize 4) of set (maxSize 3) int {auto}" $ testCasesAuto "x"
        ( DomainSet Set_ExplicitVarSizeWithFlags (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
            ( DomainSet Set_Occurrence (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
            , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3, (ConstantInt TagInt) 4, (ConstantInt TagInt) 6]
            ]
        )

    , testCase "ExplicitVarSizeWithFlags & Occurrence: set (maxSize 4) of set (maxSize 3) int" $
        let
            highDomain =
                DomainSet Set_ExplicitVarSizeWithFlags (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 4)))
                    ( DomainSet Set_Occurrence (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3)))
                        (intDomain 0 9) )
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2, (ConstantInt TagInt) 5]
                    , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3, (ConstantInt TagInt) 4, (ConstantInt TagInt) 6]
                    ]
            mid =
                [ ( "x_ExplicitVarSizeWithFlagsR2_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool True,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlagsR2_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainSet Set_Occurrence (SetAttr (SizeAttr_MaxSize ((ConstantInt TagInt) 3))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2]
                      , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 2,(ConstantInt TagInt) 5]
                      , ConstantAbstract $ AbsLitSet [(ConstantInt TagInt) 3,(ConstantInt TagInt) 4,(ConstantInt TagInt) 6]
                      , ConstantAbstract $ AbsLitSet []
                      ]
                  )
                ]
            low =
                [ ( "x_ExplicitVarSizeWithFlagsR2_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool True,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlagsR2_Values_Occurrence"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 0 9) DomainBool)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 2
                          [ ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False
                          , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 2,5
                          [ ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False
                          , ConstantBool True , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 3,4,6
                          [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool True
                          , ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- {}
                          [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False
                          , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      ]
                  )
                ]
        in  testCases "x" highDomain highConstant Just mid low

    ]


testCases ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    Name ->                                     -- high level variable name
    Domain HasRepresentation Constant ->        -- high level domain
    Constant ->                                 -- high level value (constant)
    (forall a . a -> Maybe a) ->                -- `const Nothing` - if going one level downC produces Nothing
                                                -- `Just`          - if going one level downC produces (Just mid)
    [(Name, Domain HasRepresentation Constant, Constant)] ->    -- "mid" result, if we go one level down
    [(Name, Domain HasRepresentation Constant, Constant)] ->    -- "low" result, if we go all the way down
    Assertion
testCases highName highDomain highConstant mkMid mid low = do
    downC1Test  (highName, highDomain, highConstant) (mkMid mid)
    downTest    (highName, highDomain, highConstant) low
    up1Test     (highName, highDomain) (map dropDomain mid) (highName, highConstant)
    upTest      (highName, highDomain) (map dropDomain low) (highName, highConstant)
    downUp1Test (highName, highDomain, highConstant)
    downUpTest  (highName, highDomain, highConstant)

downC1Test ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, Domain HasRepresentation Constant, Constant) ->
    Maybe [(Name, Domain HasRepresentation Constant, Constant)] ->
    Assertion
downC1Test high low' =
    case downC1 high of
        TriedIO -> assertFailure "TriedIO"
        Failed err -> assertFailure (show err)
        Done low -> Pr low @?= Pr low'

downTest ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, Domain HasRepresentation Constant, Constant) ->
    [(Name, Domain HasRepresentation Constant, Constant)] ->
    Assertion
downTest high lows' =
    case downC high of
        TriedIO -> assertFailure "TriedIO"
        Failed err -> assertFailure (show err)
        Done lows -> Pr lows @?= Pr lows'

up1Test ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, Domain HasRepresentation Constant) ->
    [(Name, Constant)] ->
    (Name, Constant) ->
    Assertion
up1Test info lows high' =
    case up1 info lows of
        TriedIO -> assertFailure "TriedIO"
        Failed err -> assertFailure (show err)
        Done high -> Pr high @?= Pr high'

upTest ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, Domain HasRepresentation Constant) ->
    [(Name, Constant)] ->
    (Name, Constant) ->
    Assertion
upTest info lows high' =
    case up lows info of
        TriedIO -> assertFailure "TriedIO"
        Failed err -> assertFailure (show err)
        Done high -> Pr high @?= Pr high'


testCasesAuto ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    Name ->                                         -- high level variable name
    Domain HasRepresentation Constant ->            -- high level domain
    Constant ->                                     -- high level value (constant)
    Assertion
testCasesAuto highName highDomain highConstant = do
    downUp1Test (highName, highDomain, highConstant)
    downUpTest  (highName, highDomain, highConstant)


downUp1Test ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, Domain HasRepresentation Constant, Constant) ->
    Assertion
downUp1Test high =
    case downC1 high of
        TriedIO -> assertFailure "TriedIO"
        Failed err -> assertFailure (show err)
        Done mlows -> do
            let lows = maybe [dropDomain high] (map dropDomain) mlows   -- use high if we cannot go downC1
            case up1 (dropConstant high) lows of
                TriedIO -> assertFailure "TriedIO"
                Failed err -> assertFailure (show err)
                Done high' -> Pr high' @?= Pr (dropDomain high)

downUpTest ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, Domain HasRepresentation Constant, Constant) ->
    Assertion
downUpTest high =
    case downC high of
        TriedIO -> assertFailure "TriedIO"
        Failed err -> assertFailure (show err)
        Done lows ->
            case up (map dropDomain lows) (dropConstant high) of
                TriedIO -> assertFailure "TriedIO"
                Failed err -> assertFailure (show err)
                Done high' -> Pr high' @?= Pr (dropDomain high)


intDomain :: Default r => Integer -> Integer -> Domain r Constant
intDomain lb ub = defRepr $ mkDomainIntB ((ConstantInt TagInt) lb) ((ConstantInt TagInt) ub)

dropConstant :: (a,b,c) -> (a,b)
dropConstant (a,b,_) = (a,b)

dropDomain :: (a,b,c) -> (a,c)
dropDomain (a,_,c) = (a,c)


data Pr a = Pr a
    deriving Eq

instance Show (Pr [(Name, Domain HasRepresentation Constant, Constant)]) where
    show (Pr xs) = show $ vcat $ concatMap show' xs
        where
            show' (name, dom, cons) = [ hang (pretty name) 4 $ vcat
                                        [ ":" <+> pretty dom
                                        , "=" <+> pretty cons
                                        ] ]

instance Show (Pr (Maybe [(Name, Domain HasRepresentation Constant, Constant)])) where
    show (Pr Nothing) = "Nothing"
    show (Pr (Just xs)) = show (Pr xs)

instance Show (Pr (Name, Constant)) where
    show (Pr (name, cons)) = show $ pretty name <+> "=" <+> pretty cons

instance Show (Pr [(Name, Constant)]) where
    show (Pr xs) = intercalate "\n" $ map (show . Pr) xs

