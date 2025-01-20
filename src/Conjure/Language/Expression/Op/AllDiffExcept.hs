{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.AllDiffExcept where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpAllDiffExcept x = OpAllDiffExcept x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAllDiffExcept x)
instance Hashable  x => Hashable  (OpAllDiffExcept x)
instance ToJSON    x => ToJSON    (OpAllDiffExcept x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAllDiffExcept x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpAllDiffExcept x) where
    typeOf p@(OpAllDiffExcept x n) = do
        tyX <- typeOf x
        tyN <- typeOf n
        case tyN of
            TypeInt TagInt -> return ()
            TypeInt TaggedInt{} -> return ()
            TypeInt (TagEnum _) -> return ()
            _ -> raiseTypeError p
        case tyX of
            TypeList{} -> return TypeBool
            TypeMatrix{} -> return TypeBool
            _ -> raiseTypeError p

instance SimplifyOp OpAllDiffExcept x where
    simplifyOp _ = na "simplifyOp{OpAllDiffExcept}"

instance Pretty x => Pretty (OpAllDiffExcept x) where
    prettyPrec _ (OpAllDiffExcept a b) = "alldifferent_except" <> prettyList prParens "," [a, b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpAllDiffExcept x) where
    varSymBreakingDescription (OpAllDiffExcept x y) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpAllDiffExcept")
        , ("children", JSON.Array $ V.fromList [ varSymBreakingDescription x
                                               , varSymBreakingDescription y
                                               ])
        ]
