{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Geq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpGeq x = OpGeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpGeq x)
instance Hashable  x => Hashable  (OpGeq x)
instance ToJSON    x => ToJSON    (OpGeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpGeq x) where
    opLexeme _ = L_Geq

instance (TypeOf x, Pretty x) => TypeOf (OpGeq x) where
    typeOf p@(OpGeq a b) = sameToSameToBool p a b [] $ \case
        TypeBool -> True
        TypeInt{} | ?typeCheckerMode == RelaxedIntegerTags -> True
        TypeInt TagInt -> True
        TypeInt TaggedInt{} -> True
        TypeInt TagEnum{} -> True
        _ -> False

instance SimplifyOp OpGeq x where
    simplifyOp _ = na "simplifyOp{OpGeq}"

instance Pretty x => Pretty (OpGeq x) where
    prettyPrec prec op@(OpGeq a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpGeq x) where
    varSymBreakingDescription (OpGeq a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpGeq")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
