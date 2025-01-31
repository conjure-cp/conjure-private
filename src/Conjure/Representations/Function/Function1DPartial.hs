{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.Function1DPartial ( function1DPartial ) where

-- conjure
import Conjure.Prelude
import Conjure.Language
import Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain )
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues )

-- unordered-containers
import qualified Data.HashMap.Strict as M


function1DPartial :: forall m . (MonadFailDoc m, NameGen m, EnumerateDomain m) => Representation m
function1DPartial = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainFunction _
                    attrs@(FunctionAttr _ PartialityAttr_Partial _)
                    innerDomainFr
                    innerDomainTo) | domainCanIndexMatrix innerDomainFr = do
            innerDomainFr' <- f innerDomainFr
            innerDomainTo' <- f innerDomainTo
            return [ DomainFunction Function_1DPartial attrs fr to
                   | fr <- innerDomainFr'
                   , to <- innerDomainTo'
                   ]
        chck _ _ = return []

        nameFlags  = mkOutName (Just "Flags")
        nameValues = mkOutName (Just "Values")

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainFunction Function_1DPartial
                    (FunctionAttr _ PartialityAttr_Partial _)
                    innerDomainFr
                    innerDomainTo)) | domainCanIndexMatrix innerDomainFr = return $ Just
            [ ( nameFlags domain name
              , DomainMatrix
                  (forgetRepr innerDomainFr)
                  DomainBool
              )
            , ( nameValues domain name
              , DomainMatrix
                  (forgetRepr innerDomainFr)
                  innerDomainTo
              )
            ]
        downD _ = na "{downD} Function1DPartial"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1
            (DomainFunction Function_1DPartial
                (FunctionAttr sizeAttr PartialityAttr_Partial jectivityAttr)
                innerDomainFr
                innerDomainTo) | domainCanIndexMatrix innerDomainFr = do

            let injectiveCons flags values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            and([ &values[&i] != &values[&j]
                                | &iPat : &innerDomainFr
                                , &jPat : &innerDomainFr
                                , &i .< &j
                                , &flags[&i]
                                , &flags[&j]
                                ])
                        |]

            let surjectiveCons flags values = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &flags[&j] /\ &values[&j] = &i
                        |]

            let jectivityCons flags values = case jectivityAttr of
                    JectivityAttr_None       -> return []
                    JectivityAttr_Injective  -> injectiveCons  flags values
                    JectivityAttr_Surjective -> surjectiveCons flags values
                    JectivityAttr_Bijective  -> (++) <$> injectiveCons  flags values
                                                     <*> surjectiveCons flags values

            let cardinality flags = do
                    (iPat, i) <- quantifiedVar
                    return [essence| sum &iPat : &innerDomainFr . toInt(&flags[&i]) |]

            let dontCareInactives flags values = do
                    (iPat, i) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            forAll &iPat : &innerDomainFr . &flags[&i] = false ->
                                dontCare(&values[&i])
                        |]

            let innerStructuralCons flags values = do
                    (iPat, i) <- quantifiedVarOverDomain (forgetRepr innerDomainFr)
                    let activeZone b = [essence| forAll &iPat : &innerDomainFr . &flags[&i] -> &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomainTo

                    let inLoop = [essence| &values[&i] |]
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ func -> do
                refs <- downX1 func
                case refs of
                    [flags,values] ->
                        concat <$> sequence
                            [ jectivityCons     flags values
                            , dontCareInactives flags values
                            , mkSizeCons sizeAttr <$> cardinality flags
                            , innerStructuralCons flags values
                            ]
                    _ -> na "{structuralCons} Function1DPartial"

        structuralCons _ _ _ = na "{structuralCons} Function1DPartial"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainFunction Function_1DPartial
                    (FunctionAttr _ PartialityAttr_Partial _)
                    innerDomainFr
                    innerDomainTo)
              , viewConstantFunction -> Just vals_
              ) | domainCanIndexMatrix innerDomainFr = do
            let vals = M.fromList vals_
            z <- zeroVal innerDomainTo
            froms               <- domainValues innerDomainFr
            (flagsOut, valsOut) <- unzip <$> sequence
                [ val
                | fr <- froms
                , let val = case M.lookup fr vals of
                                Nothing -> return (ConstantBool False, z)
                                Just v  -> return (ConstantBool True , v)
                ]
            return $ Just
                [ ( nameFlags domain name
                  , DomainMatrix
                      (forgetRepr innerDomainFr)
                      DomainBool
                  , ConstantAbstract $ AbsLitMatrix
                      (forgetRepr innerDomainFr)
                      flagsOut
                  )
                , ( nameValues domain name
                  , DomainMatrix
                      (forgetRepr innerDomainFr)
                      innerDomainTo
                  , ConstantAbstract $ AbsLitMatrix
                      (forgetRepr innerDomainFr)
                      valsOut
                  )
                ]
        downC (name, domain, constant) = na $ vcat [ "{downC} Function1DPartial"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]
 
        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainFunction Function_1DPartial
                                (FunctionAttr _ PartialityAttr_Partial _)
                                innerDomainFr _)) =
            case (lookup (nameFlags domain name) ctxt, lookup (nameValues domain name) ctxt) of
                ( Just (ConstantAbstract (AbsLitMatrix _ flagMatrix)) ,
                  Just (ConstantAbstract (AbsLitMatrix _ valuesMatrix)) ) -> do
                    froms          <- domainValues innerDomainFr
                    functionValues <- forM (zip3 flagMatrix froms valuesMatrix) $ \ (flag, from, to) ->
                        case viewConstantBool flag of
                            Just b  -> return $ if b then Just (from,to) else Nothing
                            Nothing -> failDoc $ vcat [ "Expected a boolean, but got:" <++> pretty flag
                                                   , "When working on:" <+> pretty name
                                                   , "With domain:" <+> pretty domain
                                                   ]
                    return ( name, ConstantAbstract $ AbsLitFunction $ catMaybes functionValues )
                (Nothing, _) -> failDoc $ vcat $
                    [ "(in Function1DPartial up 1)"
                    , "No value for:" <+> pretty (nameFlags domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> failDoc $ vcat $
                    [ "(in Function1DPartial up 2)"
                    , "No value for:" <+> pretty (nameValues domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                _ -> failDoc $ vcat $
                    [ "Expected matrix literals for:" <+> pretty (nameFlags domain name)
                                            <+> "and" <+> pretty (nameValues domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} Function1DPartial"

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [flags, values] <- downX1 inp
            Just [_, (_, DomainMatrix innerDomainFr innerDomainTo)] <- downD ("SO", domain)
            (iPat, i) <- quantifiedVar
            soValues <- innerSO downX1 [essence| &values[&i] |] innerDomainTo
            return
                [essence|
                    [ ( -toInt(&flags[&i])
                      , &soValues
                      )
                    | &iPat : &innerDomainFr
                    ]
                |]

