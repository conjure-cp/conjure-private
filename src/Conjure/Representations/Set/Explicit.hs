{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Set.Explicit ( setExplicit ) where

-- conjure
import Conjure.Prelude
import Conjure.Language
import Conjure.Representations.Internal


setExplicit :: forall m . (MonadFailDoc m, NameGen m) => Representation m
setExplicit = Representation chck downD structuralCons downC up symmetryOrdering

    where
        
        -- | We can represent any inner domain but set must be fixed size
        chck :: TypeOf_ReprCheck m
        chck f (DomainSet _ attrs@(SetAttr SizeAttr_Size{}) innerDomain) =
            map (DomainSet Set_Explicit attrs) <$> f innerDomain
        chck _ _ = return []

        outName :: Domain HasRepresentation x -> Name -> Name
        outName = mkOutName Nothing

        -- | A 1D matrix of size of set containing innerDomain objects 
        downD :: TypeOf_DownD m
        downD (name, domain@(DomainSet Set_Explicit (SetAttr (SizeAttr_Size size)) innerDomain)) = return $ Just
            [ ( outName domain name
              , DomainMatrix
                  (DomainInt TagInt [RangeBounded 1 size])
                  innerDomain
              ) ]
        downD _ = na "{downD} Explicit"

        -- | Enforce lex ordering of matrix (symmetry breaking) and inner structural constraints of
        -- 'active' elements of inner domain
        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainSet Set_Explicit (SetAttr (SizeAttr_Size size)) innerDomain) = do
            let
                -- | Makes sure i'th value is lex less than (i+1)'th value
                -- a symmetry breaking structural constraint
                ordering m = do
                    (iPat, i) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            forAll &iPat : int(1..&size-1) .
                                &m[&i] .< &m[&i+1]
                        |]

                -- | Enforces structural constraints for the elements of the inner domain
                -- that are in the set. 
                innerStructuralCons m = do
                    (iPat, i) <- quantifiedVarOverDomain [essenceDomain| int(1..&size) |]
                    let activeZone b = [essence| forAll &iPat : int(1..&size) . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &m[&i] |]
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ ref -> do
                refs <- downX1 ref
                case refs of
                    [m] ->
                        concat <$> sequence
                            [ ordering m
                            , innerStructuralCons m
                            ]
                    _ -> na "{structuralCons} Explicit"
        structuralCons _ _ _ = na "{structuralCons} Explicit"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainSet Set_Explicit (SetAttr (SizeAttr_Size size)) innerDomain)
              , viewConstantSet -> Just constants
              ) =
            let outIndexDomain = mkDomainIntB 1 size
            in  return $ Just
                    [ ( outName domain name
                      , DomainMatrix outIndexDomain innerDomain
                      , ConstantAbstract $ AbsLitMatrix outIndexDomain constants
                      ) ]
        downC _ = na "{downC} Explicit"

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainSet Set_Explicit (SetAttr (SizeAttr_Size _)) _)) =
            case lookup (outName domain name) ctxt of
                Nothing -> failDoc $ vcat $
                    [ "(in Set Explicit up)"
                    , "No value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant ->
                    case viewConstantMatrix constant of
                        Just (_, vals) ->
                            return (name, ConstantAbstract (AbsLitSet vals))
                        _ -> failDoc $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName domain name)
                                , "But got:" <+> pretty constant
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
        up _ _ = na "{up} Explicit"

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [inner] <- downX1 inp
            Just [(_, innerDomain)] <- downD ("SO", domain)
            innerSO downX1 inner innerDomain
