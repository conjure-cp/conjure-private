{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ( (<$>) )
import Data.Maybe ( Maybe(..) )
import Data.List ( sort, isSuffixOf, intercalate )
import System.Directory ( getDirectoryContents, createDirectoryIfMissing )
import Control.Exception ( catch, IOException )


main :: IO ()
main = do
    let opDir   = "src/Conjure/Language/Expression/Op"
    let outFile = "src/Conjure/Language/Expression/Op/Internal/Generated.hs"
    createDirectoryIfMissing True "src/Conjure/Language/Expression/Op/Internal"
    operators <- sort . map (head . splitOn '.')
                      . filter (".hs" `isSuffixOf`)
                  <$> getDirectoryContents opDir
    let datName = "Operator"
    let consModifier m = "MkOp" ++ m ++ " (Op" ++ m ++ " x)"
    let patModifier m = "MkOp" ++ m ++ " x"

    let outText = unlines $ concat
            [ [ "{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}"
              , "{-# LANGUAGE UndecidableInstances #-}"
              , ""
              , "module Conjure.Language.Expression.Op.Internal.Generated" ]
            , [ "    ( Op(..)"
              , "    , valuesInIntDomain"
              ]
            , [ "    , Op" ++ m ++ "(..)" | m <- operators ]
            , [ "    ) where"
              , ""
              , "-- conjure"
              , "import Conjure.Prelude"
              , "import Conjure.Language.Expression.Op.Internal.Common"
              , ""
              ]

            , [ "import Conjure.Language.Expression.Op." ++ m
              | m <- operators
              ]

            , [ ""
              , "data Op x"
              ]
            , [ "    = " ++ consModifier (head operators)        ]
            , [ "    | " ++ consModifier m | m <- tail operators ]
            , [ "    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)"
              , ""
              , "instance Serialize x => Serialize (Op x)"
              , "instance Hashable  x => Hashable  (Op x)"
              , "instance ToJSON    x => ToJSON    (Op x) where toJSON = genericToJSON jsonOptions"
              , "instance FromJSON  x => FromJSON  (Op x) where parseJSON = genericParseJSON jsonOptions"
              , ""
              , ""
              ]

            , concat
                [ [ "instance Op" ++ m ++ " x :< Op x where"
                  , "    inject = MkOp" ++ m
                  , "    project (MkOp" ++ m ++ " x) = return x"
                  , "    project _ = failDoc \"projecting Op" ++ m ++ "\""
                  ]
                | m <- operators
                ]

            , [ ""
              , "instance ( Pretty x"
              , "         , Data x"
              , "         , ExpressionLike x"
              , "         , ReferenceContainer x"
              , "         , TypeOf x"
              , "         , Domain () x :< x"
              , "         ) => TypeOf (Op x) where"
              ]
            , [ "    typeOf (" ++ patModifier m ++ ") = typeOf x"
              | m <- operators
              ]

            , [ ""
              , let context = intercalate "\n         , " [ "Op" ++ m ++ " x :< x" | m <- operators ]
                in  "instance ( " ++ context ++ ") => SimplifyOp Op x where"
              ]
            , [ "    simplifyOp (" ++ patModifier m ++ ") = simplifyOp x"
              | m <- operators
              ]

            , [ ""
              , "instance (Pretty x, ExpressionLike x) => Pretty (Op x) where"
              ]
            , [ "    prettyPrec prec (" ++ patModifier m ++ ") = prettyPrec prec x"
              | m <- operators
              ]

            , [ ""
              , "instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (Op x) where"
              ]
            , [ "    varSymBreakingDescription (" ++ patModifier m ++ ") = varSymBreakingDescription x"
              | m <- operators
              ]

            ]

    outText' <- catch (Just <$> readFile outFile)
                      (\ (e :: IOException) -> return Nothing )
    if and [ Just (length outText) /= (length <$> outText')
           , Just outText /= outText'
           ]
        then do
            putStrLn $ "Generating " ++ outFile
            writeFile outFile outText
        else
            putStrLn $ "Reusing " ++ outFile

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn ch (ch2:rest) | ch == ch2 = splitOn ch rest
splitOn ch rest =
    let (before, after) = span (/=ch) rest
    in  before : splitOn ch after
