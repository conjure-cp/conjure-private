{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Min where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpMin x = OpMin x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMin x)
instance Hashable  x => Hashable  (OpMin x)
instance ToJSON    x => ToJSON    (OpMin x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMin x) where parseJSON = genericParseJSON jsonOptions

instance ( TypeOf x, Pretty x
         , Domain () x :< x
         ) => TypeOf (OpMin x) where
    typeOf p@(OpMin x) | Just (dom :: Domain () x) <- project x = do
        ty <- typeOfDomain dom
        case ty of
            TypeInt TagInt -> return ty
            TypeInt TaggedInt{} -> return ty
            TypeInt (TagEnum _) -> return ty
            TypeEnum{} -> return ty
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside min:" <+> pretty ty
                                       ]
    typeOf p@(OpMin x) = do
        ty <- typeOf x
        tyInner <- case ty of
            TypeList tyInner -> return tyInner
            TypeMatrix _ tyInner -> return tyInner
            TypeSet tyInner -> return tyInner
            TypeMSet tyInner -> return tyInner
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside min:" <+> pretty ty
                                       ]
        case tyInner of
            TypeInt TagInt -> return ()
            TypeInt TaggedInt{} -> return () 
            TypeInt (TagEnum _) -> return ()
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside min:" <+> pretty ty
                                       ]
        return tyInner

instance SimplifyOp OpMin x where
    simplifyOp _ = na "simplifyOp{OpMin}"

instance Pretty x => Pretty (OpMin x) where
    prettyPrec _ (OpMin x) = "min" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpMin x) where
    varSymBreakingDescription (OpMin x) | Just xs <- listOut x = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpMin")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpMin x) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpMin")
        , ("children", varSymBreakingDescription x)
        ]
