{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Conjure.Rules.DontCare where

import Conjure.Rules.Import
import Conjure.Process.Enumerate ( EnumerateDomain )


rule_Bool :: Rule
rule_Bool = "dontCare-bool" `namedRule` theRule where
    theRule p = do
        x          <- match opDontCare p
        DomainBool <- domainOf x
        return
            ( "dontCare value for bools is false."
            , return $ make opEq x (fromBool False)
            )


rule_Int :: Rule
rule_Int = "dontCare-int" `namedRule` theRule where
    theRule p = do
        x <- match opDontCare p
        xDomain <- domainOf x
        val <- minOfDomain xDomain
        return
            ( "dontCare value for this integer is" <+> pretty val
            , return $ make opEq x val
            )


rule_Unnamed :: Rule
rule_Unnamed = "dontCare-unnamed" `namedRule` theRule where
    theRule p = do
        x <- match opDontCare p
        ty <- typeOf x
        case ty of
            TypeInt (TagUnnamed _) -> return ()
            _ -> na "rule_Unnamed"
        return
            ( "dontCare value for this unnamed integer is 1"
            , return $ make opEq x 1
            )


rule_Tuple :: Rule
rule_Tuple = "dontCare-tuple" `namedRule` theRule where
    theRule p = do
        x           <- match opDontCare p
        TypeTuple{} <- typeOf x
        xs          <- downX1 x
        return
            ( "dontCare handling for tuple"
            , return $ make opAnd $ fromList $ map (make opDontCare) xs
            )


rule_Record :: Rule
rule_Record = "dontCare-record" `namedRule` theRule where
    theRule p = do
        x            <- match opDontCare p
        TypeRecord{} <- typeOf x
        xs           <- downX1 x
        return
            ( "dontCare handling for record"
            , return $ make opAnd $ fromList $ map (make opDontCare) xs
            )


rule_Variant :: Rule
rule_Variant = "dontCare-variant" `namedRule` theRule where
    theRule p = do
        x             <- match opDontCare p
        TypeVariant{} <- typeOf x
        xs            <- downX1 x
        return
            ( "dontCare handling for variant"
            , return $ make opAnd $ fromList $ map (make opDontCare) xs
            )


rule_Matrix :: Rule
rule_Matrix = "dontCare-matrix" `namedRule` theRule where
    theRule p = do
        x                    <- match opDontCare p
        indices <- indexDomainsOf x
        when (null indices) $ na "rule_Matrix"
        return
            ( "dontCare handling for matrix"
            , do
                triplets <- forM indices $ \ index -> do
                    (iPat, i) <- quantifiedVar
                    return (iPat, i, index)
                let gens = [Generator (GenDomainNoRepr iPat index) | (iPat, _, index) <- triplets]
                return $ make opAnd $ Comprehension (make opDontCare (make opMatrixIndexing x [i | (_, i, _) <- triplets])) gens
            )

rule_Permutation :: Rule
rule_Permutation = "dontCare-permutation" `namedRule` theRule where
    theRule p = do 
        x                    <- match opDontCare p
        DomainPermutation _ _ inner <- domainOf x
        return
            ( "dontCare handling for permutation"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| forAll &iPat : &inner . image(&x,&i) = &i |]
            )



rule_Abstract :: Rule
rule_Abstract = "dontCare-abstract" `namedRule` theRule where
    theRule p = do
        x  <- match opDontCare p
        ty <- typeOf x
        case ty of
            TypeSet      {} -> return ()
            TypeMSet     {} -> return ()
            TypeSequence {} -> return ()
            TypeFunction {} -> return ()
            TypeRelation {} -> return ()
            TypePartition{} -> return ()
            _ -> na "not a known abstract domain"
        hasRepresentation x
        xs <- downX1 x
        return
            ( "dontCare handling for an abstract domain"
            , return $ make opAnd $ fromList $ map (make opDontCare) xs
            )


handleDontCares ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m Expression
handleDontCares p =
    case match opDontCare p of
        Nothing -> return p
        Just x -> do
            typX <- typeOf x
            case typX of
                TypeBool -> return (make opEq x (fromBool False))
                TypeInt _ -> do
                    domX <- domainOf x
                    let raiseBug = bug ("dontCare on domain:" <+> pretty domX)
                    let val = case domX of
                            DomainInt _ [] -> raiseBug
                            DomainInt _ (r:_) -> case r of
                                RangeOpen -> raiseBug
                                RangeSingle v -> v
                                RangeLowerBounded v -> v
                                RangeUpperBounded v -> v
                                RangeBounded v _ -> v
                            DomainIntE _ v -> [essence| min(&v) |]
                            _ -> raiseBug
                    return $ make opEq x val
                TypeTuple{} -> do
                    xs  <- downX1 x
                    xs' <- mapM (handleDontCares . make opDontCare) xs
                    return $ make opAnd $ fromList xs'
                TypeRecord{} -> do
                    xs  <- downX1 x
                    xs' <- mapM (handleDontCares . make opDontCare) xs
                    return $ make opAnd $ fromList xs'
                TypeVariant{} -> do
                    xs  <- downX1 x
                    xs' <- mapM (handleDontCares . make opDontCare) xs
                    return $ make opAnd $ fromList xs'
                TypeMatrix{} -> do
                    domX <- domainOf x
                    case domX of
                        DomainMatrix index _ -> do
                            (iPat@(Single nm), _) <- quantifiedVar
                            -- direct name resolution
                            let i = Reference nm (Just (DeclNoRepr Find nm index NoRegion))
                            inner <- handleDontCares [essence| dontCare(&x[&i]) |]
                            return [essence| forAll &iPat : &index . &inner |]
                        _ -> bug ("dontCare on domain, expecting matrix, but got:" <+> pretty domX)
                _ -> do
                    case representationOf x of
                        Nothing -> failDoc "doesn't seem to have a representation, during handleDontCares"
                        Just _  -> do
                            xs  <- downX1 x
                            xs' <- mapM (handleDontCares . make opDontCare) xs
                            return $ make opAnd $ fromList xs'

