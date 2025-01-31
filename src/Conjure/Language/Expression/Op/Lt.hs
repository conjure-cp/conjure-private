{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Lt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpLt x = OpLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpLt x)
instance Hashable  x => Hashable  (OpLt x)
instance ToJSON    x => ToJSON    (OpLt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpLt x) where
    opLexeme _ = L_Lt

instance (TypeOf x, Pretty x) => TypeOf (OpLt x) where
    typeOf p@(OpLt a b) = sameToSameToBool p a b [] $ \case
        TypeBool -> True
        TypeInt{} | ?typeCheckerMode == RelaxedIntegerTags -> True
        TypeInt TagInt -> True
        TypeInt TaggedInt{} -> True
        TypeInt TagEnum{} -> True
        _ -> False

instance SimplifyOp OpLt x where
    simplifyOp _ = na "simplifyOp{OpLt}"

instance Pretty x => Pretty (OpLt x) where
    prettyPrec prec op@(OpLt a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpLt x) where
    varSymBreakingDescription (OpLt a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpLt")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
