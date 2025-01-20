{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Div where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpDiv x = OpDiv x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDiv x)
instance Hashable  x => Hashable  (OpDiv x)
instance ToJSON    x => ToJSON    (OpDiv x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDiv x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpDiv x) where
    opLexeme _ = L_Div

instance (TypeOf x, Pretty x, Data x) => TypeOf (OpDiv x) where
    typeOf p@(OpDiv a b) =
      if ?typeCheckerMode == RelaxedIntegerTags
         then intToIntToInt p a b
         else intToIntToIntStrict p a b

instance SimplifyOp OpDiv x where
    simplifyOp _ = na "simplifyOp{OpDiv}"

instance Pretty x => Pretty (OpDiv x) where
    prettyPrec prec op@(OpDiv a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpDiv x) where
    varSymBreakingDescription (OpDiv a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpDiv")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
