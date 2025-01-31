{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Conjure.Language.Expression.Op.PermInverse where

import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Prelude
import Data.Aeson qualified as JSON -- aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V -- vector

newtype OpPermInverse x = OpPermInverse x
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (OpPermInverse x)

instance (Hashable x) => Hashable (OpPermInverse x)

instance (ToJSON x) => ToJSON (OpPermInverse x) where toJSON = genericToJSON jsonOptions

instance (FromJSON x) => FromJSON (OpPermInverse x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpPermInverse x) where
  typeOf p@(OpPermInverse op) = do
    ty <- typeOf op
    case ty of
      TypePermutation {} -> return ty
      _ -> raiseTypeError p

instance SimplifyOp OpPermInverse x where
  simplifyOp _ = na "simplifyOp{OpPermInverse}"

instance (Pretty x) => Pretty (OpPermInverse x) where
  prettyPrec _ (OpPermInverse a) = "permInverse" <> prParens (pretty a)

instance (VarSymBreakingDescription x) => VarSymBreakingDescription (OpPermInverse x) where
  varSymBreakingDescription (OpPermInverse a) =
    JSON.Object $
      KM.fromList
        [ ("type", JSON.String "OpPermInverse"),
          ( "children",
            JSON.Array $
              V.fromList
                [ varSymBreakingDescription a
                ]
          )
        ]
