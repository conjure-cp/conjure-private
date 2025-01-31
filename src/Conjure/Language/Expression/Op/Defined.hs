{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Defined where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpDefined x = OpDefined x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDefined x)
instance Hashable  x => Hashable  (OpDefined x)
instance ToJSON    x => ToJSON    (OpDefined x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDefined x) where parseJSON = genericParseJSON jsonOptions

instance (Pretty x, TypeOf x) => TypeOf (OpDefined x) where
    typeOf p@(OpDefined x) = do
        ty <- typeOf x
        case ty of
            TypeFunction a _  -> return (TypeSet a)
            TypePermutation a -> return (TypeSet a)
            TypeSequence _    -> return (TypeSet (TypeInt TagInt))
            _                 -> raiseTypeError p

instance SimplifyOp OpDefined x where
    simplifyOp _ = na "simplifyOp{OpDefined}"

instance Pretty x => Pretty (OpDefined x) where
    prettyPrec _ (OpDefined a) = "defined" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpDefined x) where
    varSymBreakingDescription (OpDefined a) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpDefined")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
