{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.Language.Expression.DomainSizeOf
    ( DomainSizeOf(..)
    , getMaxNumberOfElementsInContainer
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.AdHoc
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Conjure.Language.Lenses
import Conjure.Language.TH
import Conjure.Language.Type

import Conjure.Language.DomainSizeOf
import Conjure.Language.Pretty


instance DomainSizeOf Expression Expression where
    domainSizeOf (DomainReference _ (Just d)) = domainSizeOf d
    domainSizeOf DomainBool = return 2
    domainSizeOf (DomainInt _ [] ) = failDoc "domainSizeOf infinite integer domain"
    domainSizeOf (DomainInt _ [r]) = domainSizeOfRange r
    domainSizeOf (DomainInt _ rs ) = make opSum . fromList <$> mapM domainSizeOfRange rs
    domainSizeOf (DomainIntE _ x) = do
        let
            go (Reference _ (Just (Alias y))) = go y
            go (Comprehension _body gocs) = return $ make opSum $ Comprehension 1 gocs
            go y = bug ("not implemented: domainSizeOf.go:" <+> vcat [pretty y, pretty (show y)])
        go x
    domainSizeOf (DomainEnum n Nothing _) = return $
        let n' = n `mappend` "_EnumSize"
        in  Reference n' (Just (DeclHasRepr Given n' (DomainInt TagInt [])))
    domainSizeOf (DomainUnnamed _ x) = return x
    domainSizeOf (DomainTuple []) = return 1
    domainSizeOf (DomainTuple xs) = make opProduct . fromList <$> mapM domainSizeOf xs
    domainSizeOf (DomainRecord xs) = make opProduct . fromList <$> mapM (domainSizeOf . snd) xs
    domainSizeOf (DomainVariant xs) = make opSum . fromList <$> mapM (domainSizeOf . snd) xs
    domainSizeOf (DomainMatrix index inner) = make opPow <$> domainSizeOf inner <*> domainSizeOf index
    domainSizeOf (DomainSet _ (SetAttr sizeAttr) inner) = do
        innerSize <- domainSizeOf inner
        case sizeAttr of
            SizeAttr_None           -> return (make opPow 2 innerSize)
            SizeAttr_Size size      -> return (nchoosek (make opFactorial) innerSize size)
            SizeAttr_MinSize _      -> return (make opPow 2 innerSize)              -- TODO: can be better
            SizeAttr_MaxSize _      -> return (make opPow 2 innerSize)              -- TODO: can be better
            SizeAttr_MinMaxSize _ _ -> return (make opPow 2 innerSize)              -- TODO: can be better
    domainSizeOf (DomainMSet _ attrs inner) = do
        innerSize <- domainSizeOf inner
        let
            getMaxSize = case attrs of
                MSetAttr (SizeAttr_Size x) _ -> return x
                MSetAttr (SizeAttr_MaxSize x) _ -> return x
                MSetAttr (SizeAttr_MinMaxSize _ x) _ -> return x
                MSetAttr _ (OccurAttr_MaxOccur x) -> return (x * innerSize)
                MSetAttr _ (OccurAttr_MinMaxOccur _ x) -> return (x * innerSize)
                _ -> failDoc ("domainSizeOf.getMaxSize, mset not supported. attributes:" <+> pretty attrs)
            getMaxOccur = case attrs of
                MSetAttr _ (OccurAttr_MaxOccur x) -> return x
                MSetAttr _ (OccurAttr_MinMaxOccur _ x) -> return x
                MSetAttr (SizeAttr_Size x) _ -> return (make opMin $ fromList [x, innerSize])
                MSetAttr (SizeAttr_MaxSize x) _ -> return (make opMin $ fromList [x, innerSize])
                MSetAttr (SizeAttr_MinMaxSize _ x) _ -> return (make opMin $ fromList [x, innerSize])
                _ -> failDoc ("domainSizeOf.getMaxSize, mset not supported. attributes:" <+> pretty attrs)
        maxSize  <- getMaxSize
        maxOccur <- getMaxOccur
        return (make opPow maxOccur maxSize)
    domainSizeOf d@(DomainSequence _ (SequenceAttr sizeAttr jectivityAttr) innerTo) = do
        size <- case sizeAttr of
            SizeAttr_None           -> failDoc ("Infinite domain:" <+> pretty d)
            SizeAttr_Size s         -> return s
            SizeAttr_MinSize _      -> failDoc ("Infinite domain:" <+> pretty d)
            SizeAttr_MaxSize s      -> return s
            SizeAttr_MinMaxSize _ s -> return s
        domainSizeOf $ DomainFunction def (FunctionAttr sizeAttr PartialityAttr_Partial jectivityAttr)
            (DomainInt TagInt [RangeBounded 1 size]) innerTo
    domainSizeOf (DomainFunction _ (FunctionAttr sizeAttr _ _) innerFr innerTo) =
        domainSizeOf $ DomainRelation def (RelationAttr sizeAttr def) [innerFr, innerTo]
    domainSizeOf (DomainRelation _ (RelationAttr sizeAttr _binRelAttr) inners) =
        domainSizeOf (DomainSet def (SetAttr sizeAttr) (DomainTuple inners))
    domainSizeOf (DomainPartition _ a inner) =
        domainSizeOf $ DomainSet def (SetAttr (partsNum  a))
                      $ DomainSet def (SetAttr (partsSize a)) inner
    domainSizeOf (DomainPermutation _ (PermutationAttr sizeAttr) inner) =
        domainSizeOf $ DomainSet def (SetAttr sizeAttr) inner
    domainSizeOf d = bug ("not implemented: domainSizeOf:" <+> vcat [pretty d, pretty (show d)])


domainSizeOfRange :: (Op a :< a, ExpressionLike a, Pretty a, MonadFailDoc m, Num a, Eq a) => Range a -> m a
domainSizeOfRange RangeSingle{} = return 1
domainSizeOfRange (RangeBounded 1 u) = return u
domainSizeOfRange (RangeBounded l u) = return $ make opSum $ fromList [1, make opMinus u l]
domainSizeOfRange r = failDoc ("domainSizeOf infinite range:" <+> pretty r)


getMaxNumberOfElementsInContainer :: Domain () Expression -> Expression
getMaxNumberOfElementsInContainer domain@(DomainSet _ (SetAttr sizeAttr) inner) =
    case (getMaxFrom_SizeAttr sizeAttr, domainSizeOf inner) of
        (Just n, _) -> n
        (_, Just n) -> n
        _           -> bug $ "getMaxNumberOfElementsInContainer, DomainSet:" <+> pretty domain
getMaxNumberOfElementsInContainer domain@(DomainMSet _ (MSetAttr sizeAttr occurAttr) inner) =
    case (getMaxFrom_SizeAttr sizeAttr, getMaxFrom_OccurAttr occurAttr, domainSizeOf inner) of
        (Just n, _     , _     ) -> n
        (_     , Just o, Just n) -> [essence| &o * &n |]
        _                        -> bug $ "getMaxNumberOfElementsInContainer, DomainMSet:" <+> pretty domain
getMaxNumberOfElementsInContainer domain@(DomainSequence _ (SequenceAttr sizeAttr _) _) =
    case getMaxFrom_SizeAttr sizeAttr of
        Just n -> n
        _      -> bug $ "getMaxNumberOfElementsInContainer, DomainSequence:" <+> pretty domain
getMaxNumberOfElementsInContainer domain = bug $ "getMaxNumberOfElementsInContainer:" <+> pretty domain

