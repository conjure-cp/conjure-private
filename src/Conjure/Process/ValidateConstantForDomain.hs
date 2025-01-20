-- {-# OPTIONS_GHC -fmax-pmcheck-iterations=50000000 #-} -- stupid cmdargs

module Conjure.Process.ValidateConstantForDomain ( validateConstantForDomain ) where

import Conjure.Prelude
import Conjure.Language.Constant
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty 
import Conjure.Language.Type
import Conjure.Language.Instantiate ( instantiateExpression )
import Conjure.Process.AttributeAsConstraints ( mkAttributeToConstraint )
import Conjure.Process.Enumerate ( EnumerateDomain, enumerateDomain )

-- containers
import Data.Set as S ( size, size, fromList, toList, toAscList, difference )


-- | Assuming both the value and the domain are normalised
-- TODO: this is incomplete, which means parameter values won't be checked for every property

validateConstantForDomain ::
    forall m r .
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Pretty r =>
    Eq r =>
    Name -> Constant -> Domain r Constant -> m ()

validateConstantForDomain _ (viewConstantBool -> Just _) DomainBool{} = return ()

validateConstantForDomain _ _ (DomainInt _ []) = return ()              -- no restrictions

-- enums, always ok
validateConstantForDomain _ (ConstantEnum (Name ty1) _ _) (DomainInt (TagEnum ty2) _) | ty1 == ty2 = return ()

validateConstantForDomain name c@(viewConstantIntWithTag -> Just (cTag, i)) d@(DomainInt dTag rs) | cTag == dTag =
    let
        intInRange RangeOpen                                          = True
        intInRange (RangeSingle (ConstantInt _ a))                    = i == a
        intInRange (RangeLowerBounded (ConstantInt _ a))              = i >= a
        intInRange (RangeUpperBounded (ConstantInt _ a))              = i <= a
        intInRange (RangeBounded (ConstantInt _ a) (ConstantInt _ b)) = i >= a && i <= b
        intInRange _                                                   = False
    in  unless (any intInRange rs) (constantNotInDomain name c d)

validateConstantForDomain _ (viewConstantIntWithTag -> Just (_, i)) (DomainUnnamed _ (ConstantInt _ a)) | i >= 1 && i <= a = return ()

validateConstantForDomain _ _ (DomainEnum _ Nothing _) = return ()    -- no restrictions
validateConstantForDomain name c d@(DomainEnum _ _ Nothing) =
    failDoc $ vcat [ "validateConstantForDomain: enum not handled"
                , pretty name
                , pretty c
                , pretty d
                ]
validateConstantForDomain name
    c@(viewConstantIntWithTag -> Just (cTag, _))
    d@(DomainEnum _ (Just ranges) (Just mp)) = nested c d $ do
        let
            -- lu :: MonadFailDoc m =>  Name -> m Constant
            lu (ConstantEnum _ _ nm) =
                case lookup nm mp of
                    Nothing -> failDoc $ "No value for:" <+> pretty nm
                    Just v  -> return (ConstantInt cTag v)
            lu (ConstantInt t v) = return (ConstantInt t v)
            lu x = failDoc $ "validateConstantForDomain.lu" <+> pretty x

            -- lu2 :: MonadFailDoc m =>  Range Name -> m (Range Constant)
            lu2 = mapM lu

        rs <- mapM lu2 ranges
        validateConstantForDomain name c (DomainInt cTag rs :: Domain r Constant)

validateConstantForDomain name
    c@(viewConstantTuple -> Just cs)
    d@(DomainTuple ds) = nested c d $ zipWithM_ (validateConstantForDomain name) cs ds

validateConstantForDomain name
    c@(viewConstantRecord -> Just cs)
    d@(DomainRecord (sortOn fst -> ds))
        | map fst cs == map fst ds
            = nested c d $ zipWithM_ (validateConstantForDomain name) (map snd cs) (map snd ds)
        | otherwise
            = constantNotInDomain name c d

validateConstantForDomain name
    c@(viewConstantVariant -> Just (_, n, c'))
    d@(DomainVariant ds)
        | Just d' <- lookup n ds
            = nested c d $ validateConstantForDomain name c' d'
        | otherwise
            = constantNotInDomain name c d

validateConstantForDomain name
    c@(viewConstantMatrix -> Just (cIndex, vals))
    d@(DomainMatrix dIndex dInner) = do
        nested c d $
            mapM_ (\ val -> validateConstantForDomain name val dInner ) vals
        let
            isEmptyIntDomain (DomainInt _ []) = True
            isEmptyIntDomain _ = False
        unless (cIndex == dIndex || isEmptyIntDomain cIndex) $ failDoc $ vcat
            [ "The indices do not match between the value and the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            ]

validateConstantForDomain name
    c@(viewConstantSet -> Just vals)
    d@(DomainSet _ (SetAttr sizeAttr) dInner) = do
        let cardinalityOK = case sizeAttr of
                SizeAttr_None -> True
                SizeAttr_Size (ConstantInt _ s) -> s == genericLength vals
                SizeAttr_MinSize (ConstantInt _ s) -> s <= genericLength vals
                SizeAttr_MaxSize (ConstantInt _ s) -> genericLength vals <= s
                SizeAttr_MinMaxSize (ConstantInt _ smin) (ConstantInt _ smax) ->
                    smin <= genericLength vals && genericLength vals <= smax
                _ -> False
        unless cardinalityOK $ failDoc $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty sizeAttr
            ]
        nested c d $ mapM_ (\ val -> validateConstantForDomain name val dInner ) vals

validateConstantForDomain name
    c@(viewConstantMSet -> Just vals)
    d@(DomainMSet _ (MSetAttr sizeAttr occurAttr) dInner) = do
        let cardinalityOK = case sizeAttr of
                SizeAttr_None -> True
                SizeAttr_Size (ConstantInt _ s) -> s == genericLength vals
                SizeAttr_MinSize (ConstantInt _ s) -> s <= genericLength vals
                SizeAttr_MaxSize (ConstantInt _ s) -> genericLength vals <= s
                SizeAttr_MinMaxSize (ConstantInt _ smin) (ConstantInt _ smax) ->
                    smin <= genericLength vals && genericLength vals <= smax
                _ -> False
        unless cardinalityOK $ failDoc $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty sizeAttr
            ]
        let occurOK = case occurAttr of
                OccurAttr_None -> True
                OccurAttr_MinOccur (ConstantInt _ s) -> and [ s <= occ | (_, occ) <- histogram vals ]
                OccurAttr_MaxOccur (ConstantInt _ s) -> and [ occ <= s | (_, occ) <- histogram vals ]
                OccurAttr_MinMaxOccur (ConstantInt _ smin) (ConstantInt _ smax) ->
                    and [ smin <= occ && occ <= smax | (_, occ) <- histogram vals ]
                _ -> False
        unless occurOK $ failDoc $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty occurAttr
            ]
        nested c d $ mapM_ (\ val -> validateConstantForDomain name val dInner ) vals

validateConstantForDomain name
    c@(viewConstantFunction -> Just vals)
    d@(DomainFunction _ _ dFrom dTo) = nested c d $ do
        mapM_ (\ val -> validateConstantForDomain name (fst val) dFrom) vals
        mapM_ (\ val -> validateConstantForDomain name (snd val) dTo  ) vals

validateConstantForDomain name
    c@(viewConstantSequence -> Just vals)
    d@(DomainSequence _ attr dInner) = do
        case attr of
            SequenceAttr sizeAttr@(SizeAttr_Size (ConstantInt _ s)) _ | s /= genericLength vals ->
                failDoc $ vcat
                    [ "The value is not a member of the domain."
                    , "Value :" <+> pretty c
                    , "Domain:" <+> pretty d
                    , "Reason: Domain attributes are not satisfied."
                    , "Specifically:" <+> pretty sizeAttr
                    ]
            SequenceAttr _ jectivity | jectivity `elem` [JectivityAttr_Surjective, JectivityAttr_Bijective] -> do
                constants <- enumerateDomain (forgetRepr dInner)
                let missing = S.toAscList (S.fromList constants `S.difference` S.fromList vals)
                unless (null missing) $  failDoc $ vcat
                    [ "The value is not a member of the domain."
                    , "Value :" <+> pretty c
                    , "Domain:" <+> pretty d
                    , "Reason: Domain attributes are not satisfied."
                    , "Specifically:" <+> pretty JectivityAttr_Surjective
                    ]
            _ -> return () 
        nested c d $
            mapM_ (\ val -> validateConstantForDomain name val dInner ) vals

validateConstantForDomain name
    c@(viewConstantRelation -> Just valss)
    d@(DomainRelation _ (RelationAttr sizeAttr (BinaryRelationAttrs binRelAttrs)) dInners) = do
        let numValss = genericLength valss
        let cardinalityOK = case sizeAttr of
                SizeAttr_None -> True
                SizeAttr_Size (ConstantInt _ s) -> s == numValss
                SizeAttr_MinSize (ConstantInt _ s) -> s <= numValss
                SizeAttr_MaxSize (ConstantInt _ s) -> numValss <= s
                SizeAttr_MinMaxSize (ConstantInt _ smin) (ConstantInt _ smax) ->
                    smin <= numValss && numValss <= smax
                _ -> False
        unless cardinalityOK $ failDoc $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty sizeAttr
            ]
        when (S.size binRelAttrs > 0 && length dInners /= 2) $ failDoc $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Binary relation attributes cannot be used for this domain."
            , "Specifically:" <+> prettyList id "," (S.toList binRelAttrs)
            ]
        forM_ (S.toList binRelAttrs) $ \ a -> do
            constraint <- mkAttributeToConstraint (fmap Constant d) (binRelToAttrName a) Nothing (Constant c)
            evaluated <- instantiateExpression [] constraint
            case evaluated of
                ConstantBool True -> return ()
                ConstantBool False -> failDoc $ vcat
                    [ "The value is not a member of the domain."
                    , "Value :" <+> pretty c
                    , "Domain:" <+> pretty d
                    , "Reason: Domain attributes are not satisfied."
                    , "Specifically:" <+> pretty a
                    ]
                evaluatedC -> failDoc $ vcat
                    [ "The value is not a member of the domain."
                    , "Value :" <+> pretty c
                    , "Domain:" <+> pretty d
                    , "Reason: Domain attributes are not satisfied."
                    , "Specifically:" <+> pretty a
                    , "Evaluted to:" <+> pretty evaluatedC
                    ]
        nested c d $ forM_ valss $ \ vals ->
            zipWithM_ (validateConstantForDomain name) vals dInners

validateConstantForDomain name
    c@(viewConstantPartition -> Just valss)
    d@(DomainPartition _ _ dInner) = nested c d $
        mapM_ (\ val -> validateConstantForDomain name val dInner ) (concat valss)
validateConstantForDomain name
    c@(viewConstantPermutation -> Just valss)
    d@(DomainPermutation _ _ dInner) = nested c d $
        mapM_ (\ val -> validateConstantForDomain name val dInner ) (concat valss)
validateConstantForDomain name c@(TypedConstant c' _) d = nested c d $ validateConstantForDomain name c' d

validateConstantForDomain name c d = constantNotInDomain name c d


nested ::
    MonadFailDoc m =>
    Pretty c =>
    Pretty d =>
    c -> d -> ExceptT m () -> m ()
nested c d inner = do
    mres <- runExceptT inner
    case mres of
        Right () -> return ()
        Left err ->
            failDoc $ vcat
                [ "The value is not a member of the domain."
                , "Value :" <+> pretty c
                , "Domain:" <+> pretty d
                , "Reason:"
                , nest 4 err
                ]


constantNotInDomain :: (MonadFailDoc m, Pretty r) => Name -> Constant -> Domain r Constant -> m ()
constantNotInDomain n c d = failDoc $ vcat
    [ "The value is not a member of the domain."
    , "Name  :" <+> pretty n
    , "Value :" <+> pretty c
    , "Domain:" <+> pretty d
    ]


