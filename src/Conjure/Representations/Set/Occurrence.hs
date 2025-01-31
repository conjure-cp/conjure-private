{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Set.Occurrence ( setOccurrence ) where

-- conjure
import Conjure.Prelude hiding (MonadFail,fail)
import Conjure.Language
import Conjure.Representations.Internal
import Conjure.Representations.Common

import Control.Monad.Fail


setOccurrence :: forall m . (MonadFailDoc m, NameGen m) => Representation m
setOccurrence = Representation chck downD structuralCons downC up symmetryOrdering

    where

        -- | We can only represent Set of Int as occurrence
        chck :: TypeOf_ReprCheck m
        chck f (DomainSet _ attrs innerDomain@DomainInt{}) =
          map (DomainSet Set_Occurrence attrs) <$> f innerDomain
        chck _ _ = return []

        outName :: Domain HasRepresentation x -> Name -> Name
        outName = mkOutName Nothing

        -- | Matrix of Bool indexed by inner domain of set (which must be an int domain)
        downD :: TypeOf_DownD m
        downD (name, domain@(DomainSet Set_Occurrence _attrs innerDomain@DomainInt{})) = return $ Just
            [ ( outName domain name
              , DomainMatrix (forgetRepr innerDomain) DomainBool
              )
            ]
        downD _ = na "{downD} Occurrence"

        -- | Constrain number of trues in matrix to be congruent with cardinality constraint
        structuralCons :: TypeOf_Structural m
        structuralCons _ downX1 (DomainSet Set_Occurrence (SetAttr attrs) innerDomain@DomainInt{}) =
            return $ \ set -> do
                refs <- downX1 set
                case refs of
                    [m] -> do
                        (iPat, i) <- quantifiedVar
                        let cardinality = [essence| sum &iPat : &innerDomain . toInt(&m[&i]) |]
                        return (mkSizeCons attrs cardinality)
                    _ -> na "{structuralCons} Occurrence"
        structuralCons _ _ _ = na "{structuralCons} Occurrence"

        -- | If value is in the set then that value's index maps to a bool
        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainSet Set_Occurrence _attrs innerDomain@(DomainInt t intRanges))
              , viewConstantSet -> Just constants
              ) = do
                innerDomainVals <- valuesInIntDomain intRanges
                return $ Just
                    [ ( outName domain name
                      , DomainMatrix (forgetRepr innerDomain) DomainBool
                      , ConstantAbstract $ AbsLitMatrix (forgetRepr innerDomain)
                          [ ConstantBool isIn
                          | v <- innerDomainVals
                          , let isIn = ConstantInt t v `elem` constants
                          ]
                      )
                    ]
        downC _ = na "{downC} Occurrence"

        -- | Reversal of downC - if innerDom value zips with matrix true then it's in
        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainSet _ _ (DomainInt t intRanges)))=
            case lookup (outName domain name) ctxt of
                Just constantMatrix ->
                    case viewConstantMatrix constantMatrix of
                        Just (_, vals) -> do
                            innerDomainVals <- valuesInIntDomain intRanges
                            return (name, ConstantAbstract $ AbsLitSet
                                            [ ConstantInt t v
                                            | (v,b) <- zip innerDomainVals vals
                                            , viewConstantBool b == Just True
                                            ] )
                        _ -> failDoc $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName domain name)
                                , "But got:" <+> pretty constantMatrix
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                Nothing -> failDoc $ vcat $
                    [ "(in Set Occurrence up)"
                    , "No value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} Occurrence"

        -- produce a [int]
        symmetryOrdering :: (MonadFail m) => TypeOf_SymmetryOrdering m
        symmetryOrdering _innerSO downX1 inp (DomainSet Set_Occurrence _attrs innerDomain) = do
            [m] <- downX1 inp
            (iPat, i) <- quantifiedVar
            return [essence| [ -toInt(&m[&i]) | &iPat : &innerDomain ] |]
        symmetryOrdering _ _ _ _ = na "{symmetryOrdering} Occurrence"
