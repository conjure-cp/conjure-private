{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.DotLt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpDotLt x = OpDotLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDotLt x)
instance Hashable  x => Hashable  (OpDotLt x)
instance ToJSON    x => ToJSON    (OpDotLt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDotLt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpDotLt x) where
    opLexeme _ = L_DotLt

instance (TypeOf x, Pretty x) => TypeOf (OpDotLt x) where
    typeOf (OpDotLt a b) = sameToSameToBool a b

instance (Pretty x, TypeOf x) => DomainOf (OpDotLt x) x where
    domainOf op = mkDomainAny ("OpDotLt:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpDotLt where
    evaluateOp (OpDotLt x y) = return $ ConstantBool $ x < y

instance SimplifyOp OpDotLt x where
    simplifyOp _ = na "simplifyOp{OpDotLt}"

instance Pretty x => Pretty (OpDotLt x) where
    prettyPrec prec op@(OpDotLt a b) = prettyPrecBinOp prec [op] a b
