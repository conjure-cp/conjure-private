{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Sum where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpSum x = OpSum x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSum x)
instance Hashable  x => Hashable  (OpSum x)
instance ToJSON    x => ToJSON    (OpSum x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSum x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpSum x) where
    typeOf p@(OpSum x) = do
        ty <- typeOf x
        innerTy <- case ty of
            TypeList t -> return t
            TypeMatrix _ t -> return t
            TypeSet t -> return t
            TypeMSet t -> return t
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]
        case innerTy of
            TypeAny | Just [] <- listOut x -> return (TypeInt TagInt)
            TypeInt t | ?typeCheckerMode == RelaxedIntegerTags -> return (TypeInt t)
            TypeInt TagInt -> return (TypeInt TagInt)
            TypeInt t@(TaggedInt _) -> return (TypeInt t)
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]

instance BinaryOperator (OpSum x) where
    opLexeme _ = L_Plus

instance (OpSum x :< x) => SimplifyOp OpSum x where
    simplifyOp (OpSum x)
        | Just xs <- listOut x
        , let filtered = filter (/=0) xs
        , length filtered /= length xs      -- there were 0's
        = case filtered of
            []  -> return 0
            [n] -> return n
            _   -> return $ inject $ OpSum $ fromList filtered
    simplifyOp _ = na "simplifyOp{OpSum}"

instance (Pretty x, ExpressionLike x) => Pretty (OpSum x) where
    prettyPrec prec op@(OpSum x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpSum x) = "sum" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpSum x) where
    varSymBreakingDescription (OpSum x) | Just xs <- listOut x = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpSum")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpSum x) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpSum")
        , ("children", varSymBreakingDescription x)
        ]
