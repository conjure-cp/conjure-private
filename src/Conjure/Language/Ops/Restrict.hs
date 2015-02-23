{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Restrict where

import Conjure.Prelude
import Conjure.Language.Ops.Common
import Conjure.Process.Enumerate ( enumerateDomain )


data OpRestrict x = OpRestrict x {- the function -} x {- the domain -}
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpRestrict x)
instance Hashable  x => Hashable  (OpRestrict x)
instance ToJSON    x => ToJSON    (OpRestrict x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRestrict x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpRestrict x) where
    typeOf p@(OpRestrict f dom) = do
        TypeFunction from to <- typeOf f
        from'                <- typeOf dom
        if typesUnify [from, from']
            then return (TypeFunction (mostDefined [from', from]) to)
            else raiseTypeError p

instance EvaluateOp OpRestrict where
    evaluateOp (OpRestrict (ConstantAbstract (AbsLitFunction xs)) domX) = do
        dom       <- domainOut domX
        valsInDom <- enumerateDomain (dom :: Domain () Constant)
        return $ ConstantAbstract $ AbsLitFunction $ sortNub
            [ x
            | x@(a,_) <- xs
            , a `elem` valsInDom
            ]
    evaluateOp op = na $ "evaluateOp{OpRestrict}:" <++> pretty (show op)

instance SimplifyOp OpRestrict where
    simplifyOp _ _ = na "simplifyOp{OpRestrict}"

instance Pretty x => Pretty (OpRestrict x) where
    prettyPrec _ (OpRestrict a b) = "restrict" <> prettyList prParens "," [a,b]
