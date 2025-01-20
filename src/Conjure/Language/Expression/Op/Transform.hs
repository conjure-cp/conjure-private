{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Conjure.Language.Expression.Op.Transform where

import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Prelude
import Data.Aeson qualified as JSON -- aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V -- vector

data OpTransform x = OpTransform [x] x
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (OpTransform x)

instance (Hashable x) => Hashable (OpTransform x)

instance (ToJSON x) => ToJSON (OpTransform x) where toJSON = genericToJSON jsonOptions

instance (FromJSON x) => FromJSON (OpTransform x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpTransform x) where
  typeOf p@(OpTransform fs x) = do
    fromTos <- forM fs $ \f -> do
      tyF <- typeOf f
      (from, to) <- case tyF of
        TypeFunction from to -> return (from, to)
        TypeSequence to -> return (TypeInt TagInt, to)
        TypePermutation ov -> return (ov, ov)
        _ -> raiseTypeError $ "(transform first argument not a morphism)" <+> pretty p
      return (from, to)
    if typesUnify $ concat [[a, b] | (a, b) <- fromTos]
      then typeOf x
      else
        raiseTypeError
          $ vcat
            [ pretty p,
              "transform morphism not homomorphic!",
              "morphisms    :" <+> vcat (map pretty fs)
            ]

instance SimplifyOp OpTransform x where
  simplifyOp _ = na "simplifyOp{OpTransform}"

instance (Pretty x) => Pretty (OpTransform x) where
  prettyPrec _ (OpTransform a b) = "transform" <> prettyListDoc prParens "," [prettyList prBrackets "," a, pretty b]

instance (VarSymBreakingDescription x) => VarSymBreakingDescription (OpTransform x) where
  varSymBreakingDescription (OpTransform a b) =
    JSON.Object
      $ KM.fromList
        [ ("type", JSON.String "OpTransform"),
          ( "children",
            JSON.Array
              $ V.fromList
                (map varSymBreakingDescription a ++ [varSymBreakingDescription b])
          )
        ]
