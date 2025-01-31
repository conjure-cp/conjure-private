{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.ToSet where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpToSet x = OpToSet
                    Bool       -- True means we can assume there won't be any duplicates
                    x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpToSet x)
instance Hashable  x => Hashable  (OpToSet x)
instance ToJSON    x => ToJSON    (OpToSet x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToSet x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpToSet x) where
    typeOf p@(OpToSet _ x) = do
        tx <- typeOf x
        case tx of
            TypeRelation is  -> return (TypeSet (TypeTuple is))
            TypePermutation is -> return (TypeSet (TypeTuple [is, is]))
            TypeMSet i       -> return (TypeSet i)
            TypeFunction i j -> return (TypeSet (TypeTuple [i,j]))
            TypeMatrix _ i   -> return (TypeSet i)
            TypeList i       -> return (TypeSet i)
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty tx
                                       ]

instance SimplifyOp OpToSet x where
    simplifyOp _ = na "simplifyOp{OpToSet}"

instance Pretty x => Pretty (OpToSet x) where
    prettyPrec _ (OpToSet _ a) = "toSet" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpToSet x) where
    varSymBreakingDescription (OpToSet b x) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpToSet")
        , ("children", JSON.Array $ V.fromList
            [ toJSON b
            , varSymBreakingDescription x
            ])
        ]
