
module Conjure.Representations.Combined
    ( downD, downC, up
    , downD1, downC1, up1
    , downToX1
    , symmetryOrderingDispatch
    , reprOptions, getStructurals
    , reprsStandardOrderNoLevels, reprsStandardOrder, reprsSparseOrder
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Language.Instantiate
import Conjure.Process.Enumerate ( EnumerateDomain )

import Conjure.Representations.Internal
import Conjure.Representations.Primitive
import Conjure.Representations.Tuple
import Conjure.Representations.Matrix
import Conjure.Representations.Record
import Conjure.Representations.Variant
import Conjure.Representations.Set.Occurrence
import Conjure.Representations.Set.Explicit
import Conjure.Representations.Set.ExplicitVarSizeWithDummy
import Conjure.Representations.Set.ExplicitVarSizeWithMarker
import Conjure.Representations.Set.ExplicitVarSizeWithFlags
import Conjure.Representations.MSet.Occurrence
import Conjure.Representations.MSet.ExplicitWithFlags
import Conjure.Representations.MSet.ExplicitWithRepetition
import Conjure.Representations.Function.Function1D
import Conjure.Representations.Function.Function1DPartial
import Conjure.Representations.Function.FunctionND
import Conjure.Representations.Function.FunctionNDPartial
import Conjure.Representations.Function.FunctionNDPartialDummy
import Conjure.Representations.Function.FunctionAsRelation
import Conjure.Representations.Sequence.ExplicitBounded
import Conjure.Representations.Permutation.PermutationAsFunction
import Conjure.Representations.Relation.RelationAsMatrix
import Conjure.Representations.Relation.RelationAsSet
import Conjure.Representations.Partition.Occurrence
import Conjure.Representations.Partition.PartitionAsSet


-- | Refine (down) a domain, outputting refinement expressions (X) one level (1).
--   The domain is allowed to be at the class level.
downToX1 ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    FindOrGiven -> Name -> DomainX Expression -> m [Expression]
downToX1 forg name domain = rDownToX (dispatch domain) forg name domain

-- | Refine (down) a domain (D), one level (1).
--   The domain is allowed to be at the class level.
downD1 ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, DomainX Expression) -> m (Maybe [(Name, DomainX Expression)])
downD1 (name, domain) = rDownD (dispatch domain) (name, domain)

-- | Refine (down) a domain, together with a constant (C), one level (1).
--   The domain has to be fully instantiated.
downC1 ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, DomainC, Constant) -> m (Maybe [(Name, DomainC, Constant)])
downC1 (name, domain, constant) = rDownC (dispatch domain) (name, domain, constant)


-- | Translate a bunch of low level constants up, one level.
--   The high level domain (i.e. the target domain) has to be given.
--   The domain has to be fully instantiated.
up1 ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, DomainC) -> [(Name, Constant)] -> m (Name, Constant)
up1 (name, domain) ctxt = rUp (dispatch domain) ctxt (name, domain)


-- | Refine (down) a domain (D), all the way.
--   The domain is allowed to be at the class level.
downD ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, DomainX Expression) -> m [(Name, DomainX Expression)]
downD inp@(_, domain) = do
    mout <- rDownD (dispatch domain) inp
    case mout of
        Nothing -> return [inp]
        Just outs -> concatMapM downD outs

-- | Refine (down) a domain, together with a constant (C), all the way.
--   The domain has to be fully instantiated.
downC ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Name, DomainC, Constant) -> m [(Name, DomainC, Constant)]
downC inp0 = do
    let inp1 = case inp0 of (nm, dom, TypedConstant con _) -> (nm, dom, con)
                            _                              -> inp0
    mout <- downC1 inp1
    case mout of
        Nothing -> return [inp0]
        Just outs -> concatMapM downC outs

-- | Translate a bunch of low level constants up, all the way.
--   The high level domain (i.e. the target domain) has to be given.
--   The domain has to be fully instantiated.
up ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    [(Name, Constant)] -> (Name, DomainC) -> m (Name, Constant)
up ctxt (name, highDomain) = do
    toDescend'
        -- :: Maybe [(Name, DomainX x)]
        <- downD1 (name, fmap Constant highDomain)
    case toDescend' of
        Nothing ->
            case lookup name ctxt of
                Nothing -> failDoc $ vcat
                    $ ("No value for:" <+> pretty name)
                    : "Bindings in context:"
                    : prettyContext ctxt
                Just val -> return (name, val)
        Just toDescend -> do
            midConstants
                 :: [(Name, Constant)]
                 <- sequence [ do d' <- instantiateDomain [] d
                                  up ctxt (n, d')
                             | (n, d) <- toDescend
                             ]
            up1 (name, highDomain) midConstants


-- | ...
symmetryOrderingDispatch ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Expression -> m [Expression]) ->
    Expression ->
    DomainX Expression ->
    m Expression
symmetryOrderingDispatch downX1 inp domain =
    rSymmetryOrdering
        (dispatch domain)
        symmetryOrderingDispatch downX1
        inp domain


-- | Combine all known representations into one.
--   Dispatch into the actual implementation of the representation depending on the provided domain.
dispatch ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    Pretty x =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Domain HasRepresentation x -> Representation m
dispatch domain = do
    let nope = bug $ "No representation for the domain:" <+> pretty domain
    case domain of
        DomainBool{}    -> primitive
        DomainIntE{}    -> primitive
        DomainInt{}     -> primitive
        DomainTuple{}   -> tuple
        DomainRecord{}  -> record
        DomainVariant{} -> variant
        DomainMatrix{}  -> matrix downD1 downC1 up1
        DomainSet r _ _ -> case r of
            Set_Occurrence                    -> setOccurrence
            Set_Explicit                      -> setExplicit
            Set_ExplicitVarSizeWithDummy      -> setExplicitVarSizeWithDummy
            Set_ExplicitVarSizeWithMarker     -> setExplicitVarSizeWithMarker
            Set_ExplicitVarSizeWithFlags      -> setExplicitVarSizeWithFlags
            _ -> nope
        DomainMSet r _ _ -> case r of
            MSet_Occurrence                   -> msetOccurrence
            MSet_ExplicitWithFlags            -> msetExplicitWithFlags
            MSet_ExplicitWithRepetition       -> msetExplicitWithRepetition
            _ -> nope
        DomainFunction r _ _ _ -> case r of
            Function_1D                       -> function1D
            Function_1DPartial                -> function1DPartial
            Function_ND                       -> functionND
            Function_NDPartial                -> functionNDPartial
            Function_NDPartialDummy           -> functionNDPartialDummy
            Function_AsRelation{}             -> functionAsRelation dispatch
                                                    (bug "reprOptions inside dispatch")
            _ -> nope
        DomainSequence r _ _ -> case r of
            Sequence_ExplicitBounded          -> sequenceExplicitBounded
            _ -> nope
        DomainRelation r _ _ -> case r of
            Relation_AsMatrix                 -> relationAsMatrix
            Relation_AsSet{}                  -> relationAsSet dispatch
                                                    (bug "reprOptions inside dispatch")
                                                    (bug "useLevels inside dispatch")
            _ -> nope
        DomainPartition r _ _ -> case r of
            Partition_Occurrence              -> partitionOccurrence
            Partition_AsSet{}                 -> partitionAsSet dispatch
                                                    (bug "reprOptions inside dispatch")
                                                    (bug "useLevels inside dispatch")
            _ -> nope
        DomainPermutation r _ _ -> case r of
            Permutation_AsFunction            -> permutationAsFunction dispatch
            _ -> nope
        _ -> nope


type AllRepresentations m = [[Representation m]]


-- | No levels!
reprsStandardOrderNoLevels ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    AllRepresentations m
reprsStandardOrderNoLevels = [concat reprsStandardOrder]


-- | A list of all representations.
--   As a crude measure, implementing levels here.
--   We shouldn't have levels between representations in the long run.
reprsStandardOrder ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    AllRepresentations m
reprsStandardOrder =
    [ [ primitive, tuple, record, variant, matrix downD1 downC1 up1
      , setExplicit, setOccurrence, setExplicitVarSizeWithDummy
      , setExplicitVarSizeWithMarker, setExplicitVarSizeWithFlags
      , msetExplicitWithFlags, msetExplicitWithRepetition, msetOccurrence
      , function1D, function1DPartial, functionND, functionNDPartial, functionNDPartialDummy
      , sequenceExplicitBounded
      , relationAsMatrix
      , partitionAsSet     dispatch (reprOptions reprsStandardOrder) True
      , partitionOccurrence
      , permutationAsFunction dispatch
      ]
    , [ functionAsRelation dispatch (reprOptions reprsStandardOrder)
      , relationAsSet      dispatch (reprOptions reprsStandardOrder) True
      ]
    ]


-- | Sparser representations are to be preferred for parameters.
reprsSparseOrder ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    AllRepresentations m
reprsSparseOrder = map return
    [ primitive, tuple, record, variant, matrix downD1 downC1 up1

    , setExplicit, setExplicitVarSizeWithDummy, setExplicitVarSizeWithMarker
    , setOccurrence, setExplicitVarSizeWithFlags              -- redundant

    , msetExplicitWithFlags
    , msetExplicitWithRepetition, msetOccurrence              -- redundant

    , function1D, functionND
    , functionAsRelation dispatch (reprOptions reprsSparseOrder)
    , function1DPartial, functionNDPartial                    -- redundant
    , functionNDPartialDummy                                  -- redundant

    , sequenceExplicitBounded

    , relationAsSet      dispatch (reprOptions reprsSparseOrder) False
    , relationAsMatrix
    , partitionAsSet     dispatch (reprOptions reprsSparseOrder) False

    , partitionOccurrence                                     -- redundant
    , permutationAsFunction dispatch
    ]


-- | For a domain, produce a list of domains with different representation options.
--   This function should never return an empty list.
reprOptions ::
    Monad m =>
    Functor m =>
    Data x =>
    Pretty x =>
    ExpressionLike x =>
    AllRepresentations m -> Domain () x -> m [Domain HasRepresentation x]
reprOptions reprs (expandDomainReference -> domain) = go reprs
    where
        go [] = return []
        go (reprsThisLevel:reprsNextLevels) = do
            matchesOnThisLevel <- concat <$> sequence [ rCheck r (reprOptions reprs) domain
                                                      | r <- reprsThisLevel
                                                      ]
            if null matchesOnThisLevel
                then go reprsNextLevels
                else return matchesOnThisLevel


-- | For a domain, returns the structural constraints.
--   Makes recursive calls to generate the complete structural constraints.
--   Takes in a function to refine inner guys.
getStructurals ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    (Expression -> m [Expression]) ->
    DomainX Expression ->
    m (Expression -> m [Expression])
getStructurals downX1 domain = rStructural (dispatch domain) (getStructurals downX1) downX1 domain

