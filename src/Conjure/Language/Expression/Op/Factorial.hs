{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Factorial where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpFactorial x = OpFactorial x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFactorial x)
instance Hashable  x => Hashable  (OpFactorial x)
instance ToJSON    x => ToJSON    (OpFactorial x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFactorial x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpFactorial x) where
    typeOf p@(OpFactorial a) = do
        TypeInt t <- typeOf a
        case t of
            TagInt -> return ()
            TaggedInt _ -> return ()
            _ -> raiseTypeError p
        return (TypeInt t)

instance SimplifyOp OpFactorial x where
    simplifyOp _ = na "simplifyOp{OpFactorial}"

instance Pretty x => Pretty (OpFactorial x) where
    prettyPrec _ (OpFactorial a) = "factorial" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpFactorial x) where
    varSymBreakingDescription (OpFactorial a) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpFactorial")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
