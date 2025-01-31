{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Representations.Permutation.PermutationAsFunction (permutationAsFunction) where

-- conjure

import Conjure.Language
import Conjure.Language.DomainSizeOf
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Prelude
import Conjure.Process.Enumerate
import Conjure.Representations.Common
import Conjure.Representations.Internal
import Conjure.Util.Permutation

permutationAsFunction ::
  forall m.
  (MonadFailDoc m, NameGen m, EnumerateDomain m) =>
  (forall x. DispatchFunction m x) ->
  Representation m
permutationAsFunction dispatch = Representation chck downD structuralCons downC up symmetryOrdering
  where
    chck :: TypeOf_ReprCheck m
    chck f (DomainPermutation _ s innerDomain)
      | domainCanIndexMatrix innerDomain =
          map (DomainPermutation Permutation_AsFunction s) <$> f innerDomain
    chck _ _ = return []

    outNameF :: Domain HasRepresentation x -> Name -> Name
    outNameF = mkOutName (Just "PermutationFunction_forwards")

    outNameB :: Domain HasRepresentation x -> Name -> Name
    outNameB = mkOutName (Just "PermutationFunction_backwards")

    outDomain :: (DomainSizeOf x x, Pretty x) => Domain HasRepresentation x -> m (Domain HasRepresentation x)
    outDomain (DomainPermutation Permutation_AsFunction _ innerDomain) = do
      s <- domainSizeOf innerDomain
      return
        ( DomainFunction
            Function_1D
            (FunctionAttr (SizeAttr_Size s) PartialityAttr_Total JectivityAttr_Bijective)
            innerDomain
            innerDomain
        )
    outDomain domain =
      na
        $ vcat
          [ "{outDomain} PermutationAsFunction",
            "domain:" <+> pretty domain
          ]

    downD :: TypeOf_DownD m
    downD (name, domain@(DomainPermutation Permutation_AsFunction _ innerDomain))
      | domainCanIndexMatrix innerDomain = do
          m <- domainSizeOf innerDomain
          return
            $ Just
              [ ( outNameF domain name,
                  DomainFunction
                    Function_1D
                    (FunctionAttr (SizeAttr_Size m) PartialityAttr_Total JectivityAttr_Bijective)
                    innerDomain
                    innerDomain
                ),
                ( outNameB domain name,
                  DomainFunction
                    Function_1D
                    (FunctionAttr (SizeAttr_Size m) PartialityAttr_Total JectivityAttr_Bijective)
                    innerDomain
                    innerDomain
                )
              ]
    downD _ = na "{downD} AsFunction"

    structuralCons :: TypeOf_Structural m
    structuralCons f downX1 inDom@(DomainPermutation _ (PermutationAttr s) innerDom) =
      return $ \inpFun -> do
        refs <- downX1 inpFun
        case refs of
          [forw, back] -> do
            outDom <- outDomain inDom
            innerStructuralConsGen <- f outDom
            (iPat, i) <- quantifiedVarOverDomain (forgetRepr innerDom)
            concat
              <$> sequence
                [ innerStructuralConsGen forw,
                  innerStructuralConsGen back,
                  return $ mkSizeCons s [essence| sum([ toInt(&i != image(&forw, &i)) | &iPat : &innerDom ]) |],
                  return [[essence| forAll &iPat : &innerDom . &back(&forw(&i)) = &i |]],
                  return [[essence| forAll &iPat : &innerDom . &forw(&back(&i)) = &i |]]
                ]
          _ ->
            na
              $ vcat
                [ "{structuralCons} PermutationAsFunction",
                  pretty inDom
                ]
    structuralCons _ _ inDom =
      na
        $ vcat
          [ "{structuralCons} PermutationAsFunction",
            pretty inDom
          ]

    downC :: TypeOf_DownC m
    downC
      ( name,
        inDom@(DomainPermutation Permutation_AsFunction _ innerDom),
        ConstantAbstract (AbsLitPermutation vals)
        ) = do
        outDom <- outDomain inDom
        enumDo <- enumerateDomain $ forgetRepr innerDom
        case (fromCycles vals, inverse <$> fromCycles vals) of
          (Right perm1, Right perm2) -> do
            out1 <-
              rDownC
                (dispatch outDom)
                ( outNameF inDom name,
                  outDom,
                  ConstantAbstract $ AbsLitFunction $ zip enumDo (toFunction perm1 <$> enumDo)
                )
            out2 <-
              rDownC
                (dispatch outDom)
                ( outNameB inDom name,
                  outDom,
                  ConstantAbstract $ AbsLitFunction $ zip enumDo (toFunction perm2 <$> enumDo)
                )
            return $ Just (fromMaybe [] out1 ++ fromMaybe [] out2)
          (Left (PermutationError err), _) -> failDoc $ "PermutationError: " <+> stringToDoc err
          (_, Left (PermutationError err)) -> failDoc $ "PermutationError: " <+> stringToDoc err
    downC (name, domain, constant) =
      na
        $ vcat
          [ "{downC} PermutationAsFunction",
            "name:" <+> pretty name,
            "domain:" <+> pretty domain,
            "constant:" <+> pretty constant
          ]

    up :: TypeOf_Up m
    up
      ctxt
      ( name,
        domain@(DomainPermutation Permutation_AsFunction {} _ _)
        ) = do
        case lookup (outNameF domain name) ctxt of
          (Just (ConstantAbstract (AbsLitFunction f))) -> do
            case toCyclesCanonical <$> fromRelation f of
              Right cycles ->
                return (name, ConstantAbstract (AbsLitPermutation cycles))
              Left (PermutationError err) ->
                failDoc
                  $ vcat
                  $ [ "PermutationError: " <+> stringToDoc err,
                      "No value for:" <+> pretty (outNameF domain name),
                      "When working on:" <+> pretty name,
                      "With domain:" <+> pretty domain
                    ]
                  ++ ("Bindings in context:" : prettyContext ctxt)
          _ ->
            failDoc
              $ vcat
              $ [ "No value for:" <+> pretty (outNameF domain name),
                  "When working on:" <+> pretty name,
                  "With domain:" <+> pretty domain
                ]
              ++ ("Bindings in context:" : prettyContext ctxt)
    up _ (name, domain) =
      na
        $ vcat
          [ "{up} PermutationAsFunction",
            "name:" <+> pretty name,
            "domain:" <+> pretty domain
          ]

    symmetryOrdering :: TypeOf_SymmetryOrdering m
    symmetryOrdering innerSO downX1 inp domain = do
      [x, y] <- downX1 inp
      Just [(_, xDomain), (_, yDomain)] <- downD ("SO", domain)
      xs <- innerSO downX1 x xDomain
      ys <- innerSO downX1 y yDomain
      return [essence| concatenate([&xs, &ys]) |]
