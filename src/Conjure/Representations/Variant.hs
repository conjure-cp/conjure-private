{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Variant
    ( variant
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Representations.Internal
import Conjure.Language.ZeroVal ( EnumerateDomain, zeroVal )


variant :: forall m . (MonadFailDoc m, NameGen m, EnumerateDomain m) => Representation m
variant = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainVariant ds) = do
            let names = map fst ds
            outDoms <- sequence <$> mapM (f . snd) ds
            return [ DomainVariant (zip names ds') | ds' <- outDoms ]
        chck _ _ = return []

        mkName name n = mconcat [name, "_", n]

        downD :: TypeOf_DownD m
        downD (name, DomainVariant ds) = return $ Just
            $ (mkName name "_tag", defRepr $ mkDomainIntB 1 (fromInt (genericLength ds)))
            : [ (mkName name n, d)
              | (n,d) <- ds
              ]
        downD _ = na "{downD}"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainVariant ds) = do
            let
                innerStructuralCons which thisIndex thisRef thisDom = do
                    let activeZone b = [essence| &which = &thisIndex -> &b |]
                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f thisDom
                    outs <- innerStructuralConsGen thisRef
                    return (map activeZone outs)

                dontCares which thisIndex thisRef =
                    [essence| &which != &thisIndex -> dontCare(&thisRef) |]

            return $ \ rec -> do
                (which:refs) <- downX1 rec
                concat <$> sequence
                    [ do
                        isc <- innerStructuralCons which (fromInt i) ref dom
                        let dcs = dontCares        which (fromInt i) ref
                        return (dcs:isc)
                    | (i, ref, (_, dom)) <- zip3 [1..] refs ds
                    ]
        structuralCons _ _ _ = na "{structuralCons} variant"

        -- TODO: check if (length ds == length cs)
        downC :: TypeOf_DownC m
        downC (name, DomainVariant ds, ConstantAbstract (AbsLitVariant _ n c)) = do
            let theTag =
                    ( mkName name "_tag"
                    , defRepr $ mkDomainIntB 1 (fromInt (genericLength ds))
                    , case [ fromInt i
                           | (i, (n', _)) <- zip [1..] ds
                           , n == n' ] of
                          [v] -> v
                          _   -> bug "downC variant tag"
                    )
            outs <- forM ds $ \ (n', d) -> do
                        c' <- if n == n'
                                then return c
                                else zeroVal d
                        return (mkName name n', d, c')
            return $ Just (theTag : outs)
        downC (n, d, c) =
            na $ "{downC} variant" <+> vcat
                [ "name  :" <+> pretty n
                , "domain:" <+> pretty d
                , "value :" <+> pretty c
                ]

        up :: TypeOf_Up m
        up ctxt (name, DomainVariant ds) = do
            let dsForgotten = [ (n, defRepr d) | (n,d) <- ds ]
            case lookup (mkName name "_tag") ctxt of
                Just (ConstantInt _ i) ->
                    let iTag = at ds (fromInteger (i-1)) |> fst
                        iName = mkName name iTag
                    in  case lookup iName ctxt of
                            Just val -> return (name, ConstantAbstract $ AbsLitVariant (Just dsForgotten) iTag val)
                            Nothing -> failDoc $ vcat $
                                [ "(in Variant up 1)"
                                , "No value for:" <+> pretty iName
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty (DomainRecord ds)
                                ] ++
                                ("Bindings in context:" : prettyContext ctxt)
                Nothing -> failDoc $ vcat $
                    [ "(in Variant up 2)"
                    , "No value for:" <+> pretty (mkName name "_tag")
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty (DomainRecord ds)
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just val -> failDoc $ vcat $
                    [ "Expecting an integer value for:" <+> pretty (mkName name "_tag")
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty (DomainRecord ds)
                    , "But got:" <+> pretty val
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up}"

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            xs <- downX1 inp
            Just xsDoms' <- downD ("SO", domain)
            let xsDoms = map snd xsDoms'
            soValues <- sequence [ innerSO downX1 x xDom | (x, xDom) <- zip xs xsDoms ]
            return (fromList soValues)
