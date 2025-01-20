{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}

module Conjure.Representations.Function.FunctionAsRelation ( functionAsRelation ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal


functionAsRelation
    :: forall m . (MonadFailDoc m, NameGen m)
    => (forall x . DispatchFunction m x)
    -> (forall r x . ReprOptionsFunction m r x)
    -> Representation m
functionAsRelation dispatch reprOptions = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck _ dom1@(DomainFunction _ attrs _ _) = do
            dom2 <- outDomain_ dom1
            dom3 <- reprOptions dom2
            return [ DomainFunction (Function_AsRelation r) attrs innerDomainFr innerDomainTo
                   | DomainRelation r _ [innerDomainFr, innerDomainTo] <- dom3
                   ]
        chck _ _ = return []

        outName :: Domain HasRepresentation x -> Name -> Name
        outName = mkOutName Nothing

        outDomain_ :: Pretty x => Domain () x -> m (Domain () x)
        outDomain_ (DomainFunction ()
                    (FunctionAttr sizeAttr _partilityAttr _jectivityAttr)
                    innerDomainFr innerDomainTo) =
            return (DomainRelation () (RelationAttr sizeAttr def) [innerDomainFr, innerDomainTo])
        outDomain_ domain = na $ vcat [ "{outDomain_} FunctionAsRelation"
                                      , "domain:" <+> pretty domain
                                      ]

        outDomain :: Pretty x => Domain HasRepresentation x -> m (Domain HasRepresentation x)
        outDomain (DomainFunction (Function_AsRelation repr)
                    (FunctionAttr sizeAttr _partilityAttr _jectivityAttr)
                    innerDomainFr innerDomainTo) =
            return (DomainRelation repr (RelationAttr sizeAttr def) [innerDomainFr, innerDomainTo])
        outDomain domain = na $ vcat [ "{outDomain} FunctionAsRelation"
                                     , "domain:" <+> pretty domain
                                     ]

        downD :: TypeOf_DownD m
        downD (name, inDom) = do
            outDom <- outDomain inDom
            return $ Just [ ( outName inDom name , outDom ) ]

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1
                inDom@(DomainFunction _
                        (FunctionAttr _ partilityAttr jectivityAttr)
                        innerDomainFr innerDomainTo) = return $ \ func -> do
            refs <- downX1 func
            let
                partialityCons rel =
                    case partilityAttr of
                        PartialityAttr_Partial -> do
                            (iPat, i) <- quantifiedVar
                            (jPat, j) <- quantifiedVar
                            return $ return
                                [essence|
                                    $ enforcing that it is indeed a function
                                    forAll {&iPat, &jPat} subsetEq toSet(&rel) .
                                        &i[1] != &j[1]
                                |]
                        PartialityAttr_Total -> do
                            (iPat, i) <- quantifiedVar
                            (jPat, j) <- quantifiedVar
                            return $ return
                                [essence|
                                    forAll &iPat : &innerDomainFr .
                                        1 =  sum([ 1
                                                | &jPat <- &rel
                                                , &j[1] = &i
                                                ])
                                |]

                jectivityCons rel = case jectivityAttr of
                    JectivityAttr_None       -> return []
                    JectivityAttr_Injective  -> injectiveCons rel
                    JectivityAttr_Surjective -> surjectiveCons rel
                    JectivityAttr_Bijective  -> (++) <$> injectiveCons rel <*> surjectiveCons rel

                injectiveCons rel = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return
                        [essence|
                            and([ &i[1] .< &j[1] -> &i[2] != &j[2]
                                | &iPat <- &rel
                                , &jPat <- &rel
                                ])
                        |]

                surjectiveCons rel = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat in &rel .
                                    &j[2] = &i
                        |]

            case refs of
                [rel] -> do
                    outDom                 <- outDomain inDom
                    innerStructuralConsGen <- f outDom
                    concat <$> sequence
                        [ innerStructuralConsGen rel
                        , partialityCons rel
                        , jectivityCons rel
                        ]
                _ -> na $ vcat [ "{structuralCons} FunctionAsRelation"
                               , pretty inDom
                               ]
        structuralCons _ _ inDom =
            na $ vcat [ "{structuralCons} FunctionAsRelation"
                      , pretty inDom
                      ]

        downC :: TypeOf_DownC m
        downC ( name
              , inDom
              , viewConstantFunction -> Just vals
              ) = do
            outDom <- outDomain inDom
            rDownC
                (dispatch outDom)
                ( outName inDom name
                , outDom
                , ConstantAbstract $ AbsLitRelation $ map (\ (a,b) -> [a,b] ) vals
                )
        downC (name, domain, constant) = na $ vcat [ "{downC} FunctionAsRelation"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainFunction Function_AsRelation{} _ _ _)) =
            case lookup (outName domain name) ctxt of
                Just (viewConstantRelation -> Just  pairs) -> do
                    let pairOut [a,b] = return (a,b)
                        pairOut c = failDoc $ "Expecting a 2-tuple, but got:" <++> prettyList prParens "," c
                    vals <- mapM pairOut pairs
                    return (name, ConstantAbstract (AbsLitFunction vals))
                Nothing -> failDoc $ vcat $
                    [ "(in FunctionAsRelation up)"
                    , "No value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant -> failDoc $ vcat $
                    [ "Incompatible value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    , "Expected a set value, but got:" <++> pretty constant
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ (name, domain) = na $ vcat [ "{up} FunctionAsRelation"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do

            [rel] <- downX1 inp
            Just [(_, relDomain)] <- downD ("SO", domain)
            innerSO downX1 rel relDomain
