{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Minus where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpMinus x = OpMinus x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMinus x)
instance Hashable  x => Hashable  (OpMinus x)
instance ToJSON    x => ToJSON    (OpMinus x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMinus x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpMinus x) where
    opLexeme _ = L_Minus

instance (TypeOf x, Pretty x) => TypeOf (OpMinus x) where
    typeOf p@(OpMinus a b) = sameToSameToSame p a b
        [ TypeSet TypeAny
        , TypeMSet TypeAny
        , TypeFunction TypeAny TypeAny
        , TypeRelation [TypeAny]
        ]
        (\ ty -> case (?typeCheckerMode, ty) of
                    (StronglyTyped, TypeInt TagInt) -> True
                    (StronglyTyped, TypeInt TaggedInt{}) -> True
                    (RelaxedIntegerTags, TypeInt{}) -> True
                    _ -> False
        )

instance SimplifyOp OpMinus x where
    simplifyOp _ = na "simplifyOp{OpMinus}"

instance Pretty x => Pretty (OpMinus x) where
    prettyPrec prec op@(OpMinus a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpMinus x) where
    varSymBreakingDescription (OpMinus a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpMinus")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
