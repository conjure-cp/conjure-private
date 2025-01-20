{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Conjure.Language.Domain.AddAttributes
    ( allSupportedAttributes
    , addAttributesToDomain
    , mkMin, mkMax
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Name
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Lenses
import Conjure.Language.Definition
import Conjure.Language.Expression.Op

import Data.List as L ( union )

-- containers
import Data.Set as S ( singleton )


allSupportedAttributes :: [(Name, Int)]
allSupportedAttributes =
    map (,1) [ "size", "minSize", "maxSize"
             , "minOccur", "maxOccur"
             , "numParts", "minNumParts", "maxNumParts"
             , "partSize", "minPartSize", "maxPartSize"
             ] ++
    map (,0) [ "total"
             , "injective", "surjective", "bijective"
             , "regular"
             ] ++
    map (,0) [ "reflexive"
             , "irreflexive"
             , "coreflexive"
             , "symmetric"
             , "antiSymmetric"
             , "aSymmetric"
             , "transitive"
             , "total"
             , "connex"
             , "Euclidean"
             , "serial"
             , "equivalence"
             , "partialOrder"
             ]


addAttributesToDomain
    :: ( MonadFailDoc m
       , Pretty r
       )
    => Domain r Expression
    -> [(AttrName, Maybe Expression)]
    -> m (Domain r Expression)
addAttributesToDomain domain [] = return domain
addAttributesToDomain domain ((attr, val) : rest) = do
    domain' <- addAttributeToDomain domain attr val
    addAttributesToDomain domain' rest


addAttributeToDomain
    :: ( MonadFailDoc m
       , Pretty r
       )
    => Domain r Expression                          -- the input domain
    -> AttrName                                     -- the name of the attribute
    -> Maybe Expression                             -- the value for the attribute
    -> m (Domain r Expression)                      -- the modified domain

addAttributeToDomain d@DomainAny{}       = const $ const $ return d
addAttributeToDomain d@DomainBool{}      = const $ const $ return d
addAttributeToDomain d@DomainIntE{}      = const $ const $ return d
addAttributeToDomain d@DomainInt{}       = const $ const $ return d
addAttributeToDomain d@DomainEnum{}      = const $ const $ return d
addAttributeToDomain d@DomainUnnamed{}   = const $ const $ return d
addAttributeToDomain d@DomainTuple{}     = const $ const $ return d
addAttributeToDomain d@DomainRecord{}    = const $ const $ return d
addAttributeToDomain d@DomainVariant{}   = const $ const $ return d
addAttributeToDomain d@DomainMatrix{}    = const $ const $ return d
addAttributeToDomain d@DomainOp{}        = const $ const $ return d
addAttributeToDomain d@DomainReference{} = const $ const $ return d
addAttributeToDomain d@DomainMetaVar{}   = const $ const $ return d
addAttributeToDomain d@DomainPermutation{} = const $ const $ return d

addAttributeToDomain domain@(DomainSet r (SetAttr sizeAttr) inner) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size s | val == s -> return domain
                SizeAttr_Size{}            -> failDoc $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _                          -> return $ DomainSet r (SetAttr (SizeAttr_Size val)) inner
        AttrName_minSize -> do
            let fails = failDoc $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size s | val == s    -> return domain
                SizeAttr_Size{}               -> fails
                SizeAttr_MinSize minS         -> return $ DomainSet r (SetAttr (SizeAttr_MinSize (mkMax minS val))) inner
                SizeAttr_MaxSize maxS      | val == maxS -> return $ DomainSet r (SetAttr (SizeAttr_Size val)) inner
                SizeAttr_MaxSize maxS         -> return $ DomainSet r (SetAttr (SizeAttr_MinMaxSize val maxS)) inner
                SizeAttr_MinMaxSize _ maxS | val == maxS -> return $ DomainSet r (SetAttr (SizeAttr_Size val)) inner
                SizeAttr_MinMaxSize minS maxS -> return $ DomainSet r (SetAttr (SizeAttr_MinMaxSize (mkMax minS val) maxS)) inner
                SizeAttr_None{}               -> return $ DomainSet r (SetAttr (SizeAttr_MinSize val)) inner
        AttrName_maxSize -> do
            let fails = failDoc $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size s | val == s    -> return domain
                SizeAttr_Size{}               -> fails
                SizeAttr_MinSize minS      | val == minS -> return $ DomainSet r (SetAttr (SizeAttr_Size val)) inner
                SizeAttr_MinSize minS         -> return $ DomainSet r (SetAttr (SizeAttr_MinMaxSize minS val)) inner
                SizeAttr_MaxSize maxS         -> return $ DomainSet r (SetAttr (SizeAttr_MaxSize (mkMin maxS val))) inner
                SizeAttr_MinMaxSize minS _ | val == minS -> return $ DomainSet r (SetAttr (SizeAttr_Size val)) inner
                SizeAttr_MinMaxSize minS maxS -> return $ DomainSet r (SetAttr (SizeAttr_MinMaxSize minS (mkMin maxS val))) inner
                SizeAttr_None{}               -> return $ DomainSet r (SetAttr (SizeAttr_MaxSize val)) inner
        _ ->
            failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater attr Nothing =
            failDoc $ vcat [ "Missing attribute value for" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]

addAttributeToDomain domain@(DomainMSet r (MSetAttr sizeAttr occurAttr) inner) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size s | val == s -> return domain
                SizeAttr_Size{}            -> failDoc $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _                          -> return $ DomainMSet r (MSetAttr (SizeAttr_Size val) occurAttr) inner
        AttrName_minSize -> do
            let fails = failDoc $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size s | val == s    -> return domain
                SizeAttr_Size{}               -> fails
                SizeAttr_MinSize minS         -> return $ DomainMSet r
                                                 (MSetAttr (SizeAttr_MinSize (mkMax minS val))         occurAttr)
                                                 inner
                SizeAttr_MaxSize maxS      | val == maxS -> return $ DomainMSet r
                                                            (MSetAttr (SizeAttr_Size val)              occurAttr)
                                                            inner
                SizeAttr_MaxSize maxS         -> return $ DomainMSet r
                                                 (MSetAttr (SizeAttr_MinMaxSize val maxS)              occurAttr)
                                                 inner
                SizeAttr_MinMaxSize _ maxS | val == maxS -> return $ DomainMSet r
                                                            (MSetAttr (SizeAttr_Size val)              occurAttr)
                                                            inner
                SizeAttr_MinMaxSize minS maxS -> return $ DomainMSet r
                                                 (MSetAttr (SizeAttr_MinMaxSize (mkMax minS val) maxS) occurAttr)
                                                 inner
                SizeAttr_None{}               -> return $ DomainMSet r
                                                 (MSetAttr (SizeAttr_MinSize val)                      occurAttr)
                                                 inner
        AttrName_maxSize -> do
            let fails = failDoc $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size s | val == s    -> return domain
                SizeAttr_Size{}               -> fails
                SizeAttr_MinSize minS      | val == minS -> return $ DomainMSet r
                                                            (MSetAttr (SizeAttr_Size val)              occurAttr)
                                                            inner
                SizeAttr_MinSize minS         -> return $ DomainMSet r
                                                 (MSetAttr (SizeAttr_MinMaxSize minS val)              occurAttr)
                                                 inner
                SizeAttr_MaxSize maxS         -> return $ DomainMSet r
                                                 (MSetAttr (SizeAttr_MaxSize (mkMin maxS val))         occurAttr)
                                                 inner
                SizeAttr_MinMaxSize minS _ | val == minS -> return $ DomainMSet r
                                                            (MSetAttr (SizeAttr_Size val)              occurAttr)
                                                            inner
                SizeAttr_MinMaxSize minS maxS -> return $ DomainMSet r
                                                 (MSetAttr (SizeAttr_MinMaxSize minS (mkMin maxS val)) occurAttr)
                                                 inner
                SizeAttr_None{}               -> return $ DomainMSet r
                                                 (MSetAttr (SizeAttr_MaxSize val)                      occurAttr)
                                                 inner
        AttrName_minOccur ->
            case occurAttr of
                OccurAttr_MinOccur minO         -> return $ DomainMSet r
                                                   (MSetAttr sizeAttr (OccurAttr_MinOccur (mkMax minO val)))
                                                   inner
                OccurAttr_MaxOccur maxO         -> return $ DomainMSet r
                                                   (MSetAttr sizeAttr (OccurAttr_MinMaxOccur val maxO))
                                                   inner
                OccurAttr_MinMaxOccur minO maxO -> return $ DomainMSet r
                                                   (MSetAttr sizeAttr (OccurAttr_MinMaxOccur (mkMax minO val) maxO))
                                                   inner
                OccurAttr_None                  -> return $ DomainMSet r
                                                   (MSetAttr sizeAttr (OccurAttr_MinOccur val))
                                                   inner
        AttrName_maxOccur ->
            case occurAttr of
                OccurAttr_MinOccur minO         -> return $ DomainMSet r
                                                   (MSetAttr sizeAttr (OccurAttr_MinMaxOccur minO val))
                                                   inner
                OccurAttr_MaxOccur maxO         -> return $ DomainMSet r
                                                   (MSetAttr sizeAttr (OccurAttr_MaxOccur (mkMin maxO val)))
                                                   inner
                OccurAttr_MinMaxOccur minO maxO -> return $ DomainMSet r
                                                   (MSetAttr sizeAttr (OccurAttr_MinMaxOccur minO (mkMin maxO val)))
                                                   inner
                OccurAttr_None                  -> return $ DomainMSet r
                                                   (MSetAttr sizeAttr (OccurAttr_MaxOccur val))
                                                   inner
        _ ->
            failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater attr Nothing =
            failDoc $ vcat [ "Missing attribute value for" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]

addAttributeToDomain domain@(DomainFunction r
                            (FunctionAttr sizeAttr partialityAttr jectivityAttr)
                            inF inT) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> failDoc $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_Size val) partialityAttr jectivityAttr)
                                            inF inT
        AttrName_minSize -> do
            let fails = failDoc $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MinSize val) partialityAttr jectivityAttr)
                                            inF inT
                SizeAttr_MaxSize maxS -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MinMaxSize val maxS) partialityAttr jectivityAttr)
                                            inF inT
        AttrName_maxSize -> do
            let fails = failDoc $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MaxSize val) partialityAttr jectivityAttr)
                                            inF inT
                SizeAttr_MinSize minS -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MinMaxSize minS val) partialityAttr jectivityAttr)
                                            inF inT
        _ ->
            failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater "total" Nothing = return $ DomainFunction r
                                            (FunctionAttr sizeAttr PartialityAttr_Total jectivityAttr)
                                            inF inT
    updater "injective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None       -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Injective )
                                            inF inT
            JectivityAttr_Injective  -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Injective )
                                            inF inT
            JectivityAttr_Surjective -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
            JectivityAttr_Bijective  -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
    updater "surjective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None          -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Surjective)
                                            inF inT
            JectivityAttr_Injective     -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
            JectivityAttr_Surjective    -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Surjective)
                                            inF inT
            JectivityAttr_Bijective     -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
    updater "bijective" Nothing = return $ DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective)
                                            inF inT
    updater attr _ =
        failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

addAttributeToDomain domain@(DomainSequence r
                            (SequenceAttr sizeAttr jectivityAttr)
                            inner) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> failDoc $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_Size val) jectivityAttr)
                                            inner
        AttrName_minSize -> do
            let fails = failDoc $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_MinSize val) jectivityAttr)
                                            inner
                SizeAttr_MaxSize maxS -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_MinMaxSize val maxS) jectivityAttr)
                                            inner
        AttrName_maxSize -> do
            let fails = failDoc $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_MaxSize val) jectivityAttr)
                                            inner
                SizeAttr_MinSize minS -> return $ DomainSequence r
                                            (SequenceAttr (SizeAttr_MinMaxSize minS val) jectivityAttr)
                                            inner
        _ ->
            failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater "injective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None       -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Injective )
                                            inner
            JectivityAttr_Injective  -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Injective )
                                            inner
            JectivityAttr_Surjective -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective )
                                            inner
            JectivityAttr_Bijective  -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective )
                                            inner
    updater "surjective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None          -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Surjective)
                                            inner
            JectivityAttr_Injective     -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective )
                                            inner
            JectivityAttr_Surjective    -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Surjective)
                                            inner
            JectivityAttr_Bijective     -> DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective )
                                            inner
    updater "bijective" Nothing = return $ DomainSequence r
                                            (SequenceAttr sizeAttr JectivityAttr_Bijective)
                                            inner
    updater attr _ =
        failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

addAttributeToDomain domain@(DomainRelation r
                            (RelationAttr sizeAttr binRelAttr)
                            inners) = updater where
    supportedBinRel :: [AttrName]
    supportedBinRel =
        [ "reflexive", "irreflexive", "coreflexive"
        , "symmetric", "antiSymmetric", "aSymmetric"
        , "transitive", "total", "connex", "Euclidean"
        , "serial", "equivalence", "partialOrder"
        ]
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> failDoc $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainRelation r (RelationAttr (SizeAttr_Size val) binRelAttr) inners
        AttrName_minSize -> do
            let fails = failDoc $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MinSize val)         binRelAttr)
                                            inners
                SizeAttr_MaxSize maxS -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MinMaxSize val maxS) binRelAttr)
                                            inners
        AttrName_maxSize -> do
            let fails = failDoc $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MaxSize val)         binRelAttr)
                                            inners
                SizeAttr_MinSize minS -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MinMaxSize minS val) binRelAttr)
                                            inners
        _ ->
            failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater attr Nothing | attr `elem` supportedBinRel = case readBinRel attr of
        Nothing ->
            failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
        Just a  -> return $ DomainRelation r
                                (RelationAttr sizeAttr (binRelAttr `mappend` BinaryRelationAttrs (S.singleton a)))
                                inners
    updater attr _ =
            failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]

addAttributeToDomain domain@(DomainPartition r partitionAttr inner) = updater where
    updater attr (Just val) = case attr of

        AttrName_numParts ->
            case partsNum partitionAttr of
                SizeAttr_Size s | val == s -> return domain
                SizeAttr_Size{}            -> failDoc $ "Cannot add a numParts attribute to this domain:" <++> pretty domain
                _                          -> return $ DomainPartition r (partitionAttr { partsNum = SizeAttr_Size val }) inner
        AttrName_minNumParts -> do
            let fails = failDoc $ "Cannot add a minNumParts attribute to this domain:" <++> pretty domain
            case partsNum partitionAttr of
                SizeAttr_Size s | val == s    -> return domain
                SizeAttr_Size{}               -> fails
                SizeAttr_MinSize minS         -> return $ DomainPartition r
                                                 partitionAttr { partsNum = SizeAttr_MinSize (mkMax minS val) }
                                                 inner
                SizeAttr_MaxSize maxS      | val == maxS -> return $ DomainPartition r
                                                            partitionAttr { partsNum = SizeAttr_Size val }
                                                            inner
                SizeAttr_MaxSize maxS         -> return $ DomainPartition r
                                                 partitionAttr { partsNum = SizeAttr_MinMaxSize val maxS }
                                                 inner
                SizeAttr_MinMaxSize _ maxS | val == maxS -> return $ DomainPartition r
                                                            partitionAttr { partsNum = SizeAttr_Size val }
                                                            inner
                SizeAttr_MinMaxSize minS maxS -> return $ DomainPartition r
                                                 partitionAttr { partsNum = SizeAttr_MinMaxSize (mkMax minS val) maxS }
                                                 inner
                SizeAttr_None{}               -> return $ DomainPartition r
                                                 partitionAttr { partsNum = SizeAttr_MinSize val }
                                                 inner
        AttrName_maxNumParts -> do
            let fails = failDoc $ "Cannot add a maxNumParts attribute to this domain:" <++> pretty domain
            case partsNum partitionAttr of
                SizeAttr_Size s | val == s    -> return domain
                SizeAttr_Size{}               -> fails
                SizeAttr_MinSize minS      | val == minS -> return $ DomainPartition r
                                                            partitionAttr { partsNum = SizeAttr_Size val }
                                                            inner
                SizeAttr_MinSize minS         -> return $ DomainPartition r
                                                 partitionAttr { partsNum = SizeAttr_MinMaxSize minS val }
                                                 inner
                SizeAttr_MaxSize maxS         -> return $ DomainPartition r
                                                 partitionAttr { partsNum = SizeAttr_MaxSize (mkMin maxS val) }
                                                 inner
                SizeAttr_MinMaxSize minS _ | val == minS -> return $ DomainPartition r
                                                            partitionAttr { partsNum = SizeAttr_Size val }
                                                            inner
                SizeAttr_MinMaxSize minS maxS -> return $ DomainPartition r
                                                 partitionAttr { partsNum = SizeAttr_MinMaxSize minS (mkMin maxS val) }
                                                 inner
                SizeAttr_None{}               -> return $ DomainPartition r
                                                 partitionAttr { partsNum = SizeAttr_MaxSize val }
                                                 inner

        AttrName_partSize ->
            case partsSize partitionAttr of
                SizeAttr_Size s | val == s -> return domain
                SizeAttr_Size{} -> failDoc $ "Cannot add a partSize attribute to this domain:" <++> pretty domain
                _               -> return $ DomainPartition r (partitionAttr { partsSize = SizeAttr_Size val }) inner
        AttrName_minPartSize -> do
            let fails = failDoc $ "Cannot add a minPartSize attribute to this domain:" <++> pretty domain
            case partsSize partitionAttr of
                SizeAttr_Size s | val == s    -> return domain
                SizeAttr_Size{}               -> fails
                SizeAttr_MinSize minS         -> return $ DomainPartition r
                                                 partitionAttr { partsSize = SizeAttr_MinSize (mkMax minS val) }
                                                 inner
                SizeAttr_MaxSize maxS      | val == maxS -> return $ DomainPartition r
                                                            partitionAttr { partsSize = SizeAttr_Size val }
                                                            inner
                SizeAttr_MaxSize maxS         -> return $ DomainPartition r
                                                 partitionAttr { partsSize = SizeAttr_MinMaxSize val maxS }
                                                 inner
                SizeAttr_MinMaxSize _ maxS | val == maxS -> return $ DomainPartition r
                                                            partitionAttr { partsSize = SizeAttr_Size val }
                                                            inner
                SizeAttr_MinMaxSize minS maxS -> return $ DomainPartition r
                                                 partitionAttr { partsSize = SizeAttr_MinMaxSize (mkMax minS val) maxS }
                                                 inner
                SizeAttr_None{}               -> return $ DomainPartition r
                                                 (partitionAttr { partsSize = SizeAttr_MinSize val })
                                                 inner
        AttrName_maxPartSize -> do
            let fails = failDoc $ "Cannot add a maxPartSize attribute to this domain:" <++> pretty domain
            case partsSize partitionAttr of
                SizeAttr_Size s | val == s    -> return domain
                SizeAttr_Size{}               -> fails
                SizeAttr_MinSize minS      | val == minS -> return $ DomainPartition r
                                                            partitionAttr { partsSize = SizeAttr_Size val }
                                                            inner
                SizeAttr_MinSize minS         -> return $ DomainPartition r
                                                 partitionAttr { partsSize = SizeAttr_MinMaxSize minS val }
                                                 inner
                SizeAttr_MaxSize maxS         -> return $ DomainPartition r
                                                 partitionAttr { partsSize = SizeAttr_MaxSize (mkMin maxS val) }
                                                 inner
                SizeAttr_MinMaxSize minS _ | val == minS -> return $ DomainPartition r
                                                            partitionAttr { partsSize = SizeAttr_Size val }
                                                            inner
                SizeAttr_MinMaxSize minS maxS -> return $ DomainPartition r
                                                 partitionAttr { partsSize = SizeAttr_MinMaxSize minS (mkMin maxS val) }
                                                 inner
                SizeAttr_None{}               -> return $ DomainPartition r
                                                 (partitionAttr { partsSize = SizeAttr_MaxSize val })
                                                 inner

        _ ->
            failDoc $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater AttrName_regular Nothing =
            return $ DomainPartition r (partitionAttr { isRegular  = True }) inner
    updater attr Nothing =
            failDoc $ vcat [ "Missing attribute value for" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]


-- | Make a maximum expression between two expressions.
-- | Two max expressions are merged into one.
-- | The max between a value and a max adds the value to the max (if not present).
-- | If the expressions are the same, no max is made and the value is returned.
mkMax :: Expression -> Expression -> Expression
mkMax (Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es1)))))
      (Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es2)))))
        = make opMax $ fromList $ es1 `L.union` es2
mkMax i m@(Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es)))))
          | i `elem` es = m
          | otherwise   = make opMax $ fromList $ i : es
mkMax m@(Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es))))) i
          | i `elem` es = m
          | otherwise   = make opMax $ fromList $ i : es
mkMax i e | i == e      = e
          | otherwise   = make opMax $ fromList [ i, e ]

-- | Make a minimum expression between two expressions.
-- | Two min expressions are merged into one.
-- | The min between a value and a min adds the value to the min (if not present).
-- | If the expressions are the same, no min is made and the value is returned.
mkMin :: Expression -> Expression -> Expression
mkMin (Op (MkOpMin (OpMin (AbstractLiteral (AbsLitMatrix _ es1)))))
      (Op (MkOpMin (OpMin (AbstractLiteral (AbsLitMatrix _ es2)))))
        = make opMin $ fromList $ es1 `L.union` es2
mkMin i m@(Op (MkOpMin (OpMin (AbstractLiteral (AbsLitMatrix _ es)))))
          | i `elem` es = m
          | otherwise   = make opMin $ fromList $ i : es
mkMin m@(Op (MkOpMin (OpMin (AbstractLiteral (AbsLitMatrix _ es))))) i
          | i `elem` es = m
          | otherwise   = make opMin $ fromList $ i : es
mkMin i e | i == e      = e
          | otherwise   = make opMin $ fromList [ i, e ]
