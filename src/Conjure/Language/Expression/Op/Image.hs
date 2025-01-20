{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Image where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpImage x = OpImage x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpImage x)
instance Hashable  x => Hashable  (OpImage x)
instance ToJSON    x => ToJSON    (OpImage x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpImage x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpImage x) where
    typeOf p@(OpImage f x) = do
        tyF <- typeOf f
        tyX <- typeOf x
        (from, to) <- case tyF of
            TypeFunction from to -> return (from, to)
            TypeSequence      to -> return (TypeInt TagInt, to)
            TypePermutation _    -> return (tyX, tyX)
            _ -> raiseTypeError $ "(function application)" <+> pretty p
        if typesUnify [tyX, from]
            then return to
            else raiseTypeError $ vcat
                [ pretty p
                , "function     :" <+> pretty f
                , "function type:" <+> pretty (TypeFunction from to)
                , "argument     :" <+> pretty x
                , "argument type:" <+> pretty tyX
                ]

instance SimplifyOp OpImage x where
    simplifyOp _ = na "simplifyOp{OpImage}"

instance Pretty x => Pretty (OpImage x) where
    prettyPrec _ (OpImage a b) = "image" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpImage x) where
    varSymBreakingDescription (OpImage a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpImage")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
