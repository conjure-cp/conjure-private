{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.RelationProj where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpRelationProj x = OpRelationProj x [Maybe x]      -- Nothing represents an _
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpRelationProj x)
instance Hashable  x => Hashable  (OpRelationProj x)
instance ToJSON    x => ToJSON    (OpRelationProj x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRelationProj x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpRelationProj x) where
    typeOf p@(OpRelationProj r xs) = do
        tyR <- typeOf r
        case (tyR, xs) of
            (TypeRelation ts', _) -> do
                let xs' = catMaybes xs
                if length xs == length xs'
                    then do -- all Just's
                        let loop [] [] = return TypeBool
                            loop (i:is) (t:ts) = do
                                tyI <- typeOf i
                                if typesUnify [tyI,t]
                                    then loop is ts
                                    else raiseTypeError $ "(relation projection 1)" <+> pretty p
                            loop _ _ = raiseTypeError $ "(relation projection 2)" <+> pretty p
                        loop xs' ts'
                    else do
                        let loop [] [] = return []
                            loop (Nothing:is) (t:ts) = (t:) <$> loop is ts
                            loop (Just i :is) (t:ts) = do
                                tyI <- typeOf i
                                if typesUnify [tyI,t]
                                    then loop is ts
                                    else raiseTypeError $ "(relation projection 3)" <+> pretty p
                            loop _ _ = raiseTypeError $ "(relation projection 4)" <+> pretty p
                        TypeRelation <$> loop xs ts'
            _ -> raiseTypeError $ "(relation projection 5)" <+> vcat [pretty p, pretty tyR]

instance SimplifyOp OpRelationProj x where
    simplifyOp _ = na "simplifyOp{OpRelationProj}"

instance Pretty x => Pretty (OpRelationProj x) where
    prettyPrec _ (OpRelationProj a bs) = pretty a <> prettyList prParens "," (map pr bs)
        where pr Nothing = "_"
              pr (Just b) = pretty b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpRelationProj x) where
    varSymBreakingDescription (OpRelationProj a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpRelationProj")
        , ("children", JSON.Array $ V.fromList
            $ varSymBreakingDescription a
            : map (maybe JSON.Null varSymBreakingDescription) b
          )
        ]
