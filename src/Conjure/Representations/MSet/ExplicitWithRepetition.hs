{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.MSet.ExplicitWithRepetition ( msetExplicitWithRepetition ) where

-- conjure
import Conjure.Prelude
import Conjure.Language
import Conjure.Language.DomainSizeOf
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain )
import Conjure.Representations.Internal
import Conjure.Representations.Common


msetExplicitWithRepetition :: forall m . (MonadFailDoc m, NameGen m, EnumerateDomain m) => Representation m
msetExplicitWithRepetition = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainMSet _ attrs innerDomain) =
            map (DomainMSet MSet_ExplicitWithRepetition attrs) <$> f innerDomain
        chck _ _ = return []

        nameFlag   = mkOutName (Just "Flag")
        nameValues = mkOutName (Just "Values")

        getMaxSize attrs innerDomain = case attrs of
            MSetAttr (SizeAttr_Size x) _ -> return x
            MSetAttr (SizeAttr_MaxSize x) _ -> return x
            MSetAttr (SizeAttr_MinMaxSize _ x) _ -> return x
            MSetAttr _ (OccurAttr_MaxOccur x) -> do y <- domainSizeOf innerDomain ; return (x * y)
            MSetAttr _ (OccurAttr_MinMaxOccur _ x) -> do y <- domainSizeOf innerDomain ; return (x * y)
            _ -> failDoc ("getMaxSize, mset not supported. attributes:" <+> pretty attrs)

        getMinOccur attrs = case attrs of
            MSetAttr _ (OccurAttr_MinOccur x) -> Just x
            MSetAttr _ (OccurAttr_MinMaxOccur x _) -> Just x
            _ -> Nothing

        getMaxOccur attrs = case attrs of
            MSetAttr _ (OccurAttr_MaxOccur x) -> return x
            MSetAttr _ (OccurAttr_MinMaxOccur _ x) -> return x
            _ -> failDoc ("getMaxOccur, mset not supported. attributes:" <+> pretty attrs)

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainMSet _ attrs innerDomain)) = do
            (indexDomain, flagDomain) <-
                case attrs of
                    MSetAttr (SizeAttr_Size size) _ -> do
                        let indexDomain = mkDomainIntB 1 size
                        let flagDomain  = defRepr $ DomainInt TagInt [RangeSingle size]
                        return (indexDomain, flagDomain)
                    _ -> do
                        maxSize <- getMaxSize attrs innerDomain
                        let indexDomain =           mkDomainIntB 1 maxSize
                        let flagDomain  = defRepr $ mkDomainIntB 0 maxSize
                        return (indexDomain, flagDomain)
            return $ Just
                [ ( nameFlag domain name
                  , flagDomain
                  )
                , ( nameValues domain name
                  , DomainMatrix indexDomain innerDomain
                  )
                ]
        downD _ = na "{downD} ExplicitVarSizeWithRepetition"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainMSet MSet_ExplicitWithRepetition attrs@(MSetAttr sizeAttrs _) innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let maxIndex = maxSize
            let
                orderingUpToFlag flag values = do
                    (iPat, i) <- quantifiedVar
                    return $ return -- list
                        [essence|
                            forAll &iPat : int(1..&maxIndex-1) , &i+1 <= &flag . &values[&i] .<= &values[&i+1]
                        |]

                dontCareAfterFlag flag values = do
                    (iPat, i) <- quantifiedVar
                    return $ return -- list
                        [essence|
                            forAll &iPat : int(1..&maxIndex) , &i > &flag . dontCare(&values[&i])
                        |]

                minOccurrenceCons mset = do
                    (iPat, i) <- quantifiedVar
                    return
                        [ [essence|
                            forAll &iPat : &innerDomain .
                                freq(&mset, &i) >= &minOccur
                                  |]
                        | Just minOccur <- [getMinOccur attrs]
                        ]

                maxOccurrenceCons mset flag values = do
                    (iPat, i) <- quantifiedVar
                    return
                        [ [essence|
                            forAll &iPat : int(1..&maxIndex) , &i <= &flag .
                                freq(&mset, &values[&i]) <= &maxOccur_
                                  |]
                        | Just maxOccur_ <- [getMaxOccur attrs]
                        ]

                innerStructuralCons flag values = do
                    (iPat, i) <- quantifiedVarOverDomain [essenceDomain| int(1..&maxIndex) |]
                    let activeZone b = [essence| forAll &iPat : int(1..&maxIndex) , &i <= &flag . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &values[&i] |]
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ mset -> do
                refs <- downX1 mset
                case refs of
                    [flag, values] ->
                        concat <$> sequence
                            [ orderingUpToFlag  flag values
                            , dontCareAfterFlag flag values
                            , minOccurrenceCons mset
                            , maxOccurrenceCons mset flag values
                            , return (mkSizeCons sizeAttrs flag)
                            , innerStructuralCons flag values
                            ]
                    _ -> na "{structuralCons} ExplicitVarSizeWithRepetition"

        structuralCons _ _ _ = na "{structuralCons} ExplicitVarSizeWithRepetition"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainMSet _ attrs innerDomain)
              , viewConstantMSet -> Just constants
              ) = case attrs of
                    MSetAttr (SizeAttr_Size size) _ -> do
                        let indexDomain = mkDomainIntB 1 size
                        let flagDomain  = DomainInt TagInt [RangeSingle size]

                        return $ Just
                            [ ( nameFlag domain name
                              , defRepr flagDomain
                              , ConstantInt TagInt (genericLength constants)
                              )
                            , ( nameValues domain name
                              , DomainMatrix indexDomain innerDomain
                              , ConstantAbstract $ AbsLitMatrix indexDomain constants
                              )
                            ]

                    _ -> do
                        maxSize    <- getMaxSize attrs innerDomain
                        maxSizeInt <-
                            case maxSize of
                                ConstantInt _ x -> return x
                                _ -> failDoc $ vcat
                                        [ "Expecting an integer for the maxSize attribute."
                                        , "But got:" <+> pretty maxSize
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        let indexDomain = mkDomainIntB 1 maxSize
                        let flagDomain  = mkDomainIntB 0 maxSize

                        z <- zeroVal innerDomain
                        let zeroes = replicate (fromInteger (maxSizeInt - genericLength constants)) z

                        return $ Just
                            [ ( nameFlag domain name
                              , defRepr flagDomain
                              , ConstantInt TagInt (genericLength constants)
                              )
                            , ( nameValues domain name
                              , DomainMatrix indexDomain innerDomain
                              , ConstantAbstract $ AbsLitMatrix indexDomain (constants ++ zeroes)
                              )
                            ]

        downC _ = na "{downC} ExplicitVarSizeWithRepetition"

        up :: TypeOf_Up m
        up ctxt (name, domain) =
            case (lookup (nameFlag domain name) ctxt, lookup (nameValues domain name) ctxt) of
                (Just flag, Just constantMatrix) ->
                    case viewConstantInt flag of
                        -- TODO: check if indices match
                        Just flagInt ->
                            case viewConstantMatrix constantMatrix of
                                Just (_, vals) ->
                                    return (name, ConstantAbstract $ AbsLitMSet
                                                    (genericTake flagInt vals) )
                                _ -> failDoc $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty (nameValues domain name)
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        _ -> failDoc $ vcat
                                [ "Expecting an integer literal for:" <+> pretty (nameFlag domain name)
                                , "But got:" <+> pretty flag
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                (Nothing, _) -> failDoc $ vcat $
                    [ "(in MSet ExplicitVarSizeWithRepetition up 1)"
                    , "No value for:" <+> pretty (nameFlag domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> failDoc $ vcat $
                    [ "(in MSet ExplicitVarSizeWithRepetition up 2)"
                    , "No value for:" <+> pretty (nameValues domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [marker, values] <- downX1 inp
            Just [_, (_, DomainMatrix index inner)] <- downD ("SO", domain)
            (iPat, i) <- quantifiedVar
            soValues <- innerSO downX1 [essence| &values[&i] |] inner
            return
                [essence|
                    ( &marker
                    , [ &soValues
                      | &iPat : &index
                      ]
                    )
                |]

