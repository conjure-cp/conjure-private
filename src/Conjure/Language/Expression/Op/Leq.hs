{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Leq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpLeq x = OpLeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpLeq x)
instance Hashable  x => Hashable  (OpLeq x)
instance ToJSON    x => ToJSON    (OpLeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpLeq x) where
    opLexeme _ = L_Leq

instance (TypeOf x, Pretty x) => TypeOf (OpLeq x) where
    typeOf p@(OpLeq a b) = sameToSameToBool p a b [] $ \case
        TypeBool -> True
        TypeInt{} | ?typeCheckerMode == RelaxedIntegerTags -> True
        TypeInt TagInt -> True
        TypeInt TaggedInt{} -> True
        TypeInt TagEnum{} -> True
        _ -> False

instance SimplifyOp OpLeq x where
    simplifyOp _ = na "simplifyOp{OpLeq}"

instance Pretty x => Pretty (OpLeq x) where
    prettyPrec prec op@(OpLeq a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpLeq x) where
    varSymBreakingDescription (OpLeq a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpLeq")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
