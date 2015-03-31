{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.DotLeq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpDotLeq x = OpDotLeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDotLeq x)
instance Hashable  x => Hashable  (OpDotLeq x)
instance ToJSON    x => ToJSON    (OpDotLeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDotLeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpDotLeq x) where
    opLexeme _ = L_DotLeq

instance (TypeOf x, Pretty x) => TypeOf (OpDotLeq x) where
    typeOf (OpDotLeq a b) = sameToSameToBool a b

instance (Pretty x, TypeOf x) => DomainOf (OpDotLeq x) x where
    domainOf op = mkDomainAny ("OpDotLeq:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpDotLeq where
    evaluateOp (OpDotLeq x y) = return $ ConstantBool $ x <= y

instance SimplifyOp OpDotLeq x where
    simplifyOp _ = na "simplifyOp{OpDotLeq}"

instance Pretty x => Pretty (OpDotLeq x) where
    prettyPrec prec op@(OpDotLeq a b) = prettyPrecBinOp prec [op] a b
