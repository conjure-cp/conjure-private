{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.TwoBars where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpTwoBars x = OpTwoBars x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTwoBars x)
instance Hashable  x => Hashable  (OpTwoBars x)
instance ToJSON    x => ToJSON    (OpTwoBars x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTwoBars x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, Domain () x :< x) => TypeOf (OpTwoBars x) where
    typeOf p@(OpTwoBars a) = do
        case project a of
            Just (_ :: Domain () x) -> return ()
            Nothing -> do
                ty <- typeOf a
                case ty of
                    TypeInt _       -> return ()
                    TypeList{}      -> return ()
                    TypeMatrix{}    -> return ()
                    TypeSet{}       -> return ()
                    TypeMSet{}      -> return ()
                    TypeFunction{}  -> return ()
                    TypeSequence{}  -> return ()
                    TypeRelation{}  -> return ()
                    TypePartition{} -> return ()
                    TypePermutation{} -> return ()
                    _               -> raiseTypeError $ vcat [ pretty p
                                                             , "Expected an integer or a collection."
                                                             , "But got:" <+> pretty ty
                                                             ]
        return $ TypeInt TagInt

instance SimplifyOp OpTwoBars x where
    simplifyOp _ = na "simplifyOp{OpTwoBars}"

instance Pretty x => Pretty (OpTwoBars x) where
    prettyPrec _ (OpTwoBars a) = "|" <> pretty a <> "|"

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTwoBars x) where
    varSymBreakingDescription (OpTwoBars a) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpTwoBars")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
