{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.BubbleUp where

import Conjure.Rules.Import


rule_MergeNested :: Rule
rule_MergeNested = "bubble-up-merge-nested" `namedRule` theRule where
    theRule (WithLocals (WithLocals body (DefinednessConstraints locals1)) (DefinednessConstraints locals2)) =
        return
            ( "Merging nested bubbles"
            , return $ WithLocals body (DefinednessConstraints (locals1 ++ locals2))
            )
    theRule (WithLocals (WithLocals body (AuxiliaryVars locals1)) (AuxiliaryVars locals2)) =
        return
            ( "Merging nested bubbles"
            , return $ WithLocals body (AuxiliaryVars (locals1 ++ locals2))
            )
    theRule _ = na "rule_MergeNested"


rule_ToAnd :: Rule
rule_ToAnd = "bubble-to-and" `namedRule` theRule where
    theRule (WithLocals x (AuxiliaryVars [])) = return ("Empty bubble is no bubble", return x)
    theRule (WithLocals x (DefinednessConstraints [])) = return ("Empty bubble is no bubble", return x)
    theRule (WithLocals x (DefinednessConstraints cons))
        | let isTrueCons (Constant (ConstantBool True)) = True
              isTrueCons _ = False
        , all isTrueCons cons
        = return ("Trivially defined", return x)
    theRule (WithLocals x (DefinednessConstraints cons))
        | let isFalseCons (Constant (ConstantBool False)) = True
              isFalseCons _ = False
        , any isFalseCons cons
        , length cons > 1
        = return ( "Trivially undefined"
                 , return (WithLocals x (DefinednessConstraints [Constant (ConstantBool False)]))
                 )
    theRule (WithLocals x (DefinednessConstraints locals@(_:_))) = do
        TypeBool <- typeOf x
        let out = make opAnd $ fromList (x:locals)
        return
            ( "Converting a bubble into a conjunction."
            , return out
            )
    theRule _ = na "rule_BubbleToAnd"


rule_ToMultiply_HeadOfIntComprehension :: Rule
rule_ToMultiply_HeadOfIntComprehension = "bubble-to-multiply-HeadOfIntComprehension" `namedRule` theRule where
    theRule p = do
        (_, _, mk, Comprehension (WithLocals x (DefinednessConstraints cons)) gocs) <- match opReducer p
        TypeInt _ <- typeOf x
        let conjunct = make opAnd (fromList cons)
        let x' = [essence| &x * toInt(&conjunct) |]
        let out = mk $ Comprehension x' gocs
        return
            ( "Converting a bubble into a multiplication."
            , return out
            )


rule_NotBoolYet :: Rule
rule_NotBoolYet = "bubble-up-NotBoolYet" `namedRule` theRule where
    theRule WithLocals{}
        = na "rule_NotBoolYet WithLocals"

    theRule [essence| catchUndef(&x, &_) |]
        | WithLocals _ (DefinednessConstraints _) <- x
        = na "rule_NotBoolYet WithLocals"


    -- if anything in a comprehension is undefined, the whole comprehension is undefined
    -- this is for the non-bool case.
    theRule (Comprehension (WithLocals body (DefinednessConstraints locals@(_:_))) gensOrConds) = do

        ty <- typeOf body
        case ty of
            TypeBool -> na "rule_NotBoolYet"
            _        -> return ()

        forM_ gensOrConds $ \case
            Generator GenDomainHasRepr{} -> return ()
            Generator {}                 -> na "rule_NotBoolYet"        -- no other generators, only GenDomainHasRepr
            Condition {}                 -> return ()
            ComprehensionLetting {}      -> na "rule_NotBoolYet"        -- no lettings

        let localsLifted =
                [ make opAnd $ Comprehension c gensOrConds
                | c <- locals
                ]

        return
            ( "Bubbling up (through comprehension), not reached a relational context yet."
            , return $ WithLocals (Comprehension body gensOrConds) (DefinednessConstraints localsLifted)
            )

    theRule (Comprehension x gensOrConds) = do
        let (gensOrConds', Any changed) = mconcat
                [ case goc of
                    Generator (GenInExpr pat (WithLocals y (DefinednessConstraints cons)))
                        -> (Generator (GenInExpr pat y) : map Condition cons, Any True)
                    _ -> ([goc], Any False)
                | goc <- gensOrConds
                ]
        unless changed (na "rule_NotBoolYet")
        return
            ( "Bubbling up, attached to a generator inside a comprehension"
            , return $ Comprehension x gensOrConds'
            )

    theRule p = do
        let
            f x@(WithLocals y (DefinednessConstraints locals@(_:_))) = do
                ty <- typeOf y
                case ty of
                    TypeBool ->                return x         -- do not bubble-up if it is attached to a bool
                    _        -> tell locals >> return y
            f x = return x
        (p', collected) <- runWriterT (descendM f p)
        when (null collected) $
            na "rule_NotBoolYet doesn't have any bubbly children"
        return
            ( "Bubbling up, not reached a relational context yet."
            , return $ WithLocals p' (DefinednessConstraints collected)
            )


rule_ConditionInsideGeneratorDomain :: Rule
rule_ConditionInsideGeneratorDomain = "bubble-up-ConditionInsideGeneratorDomain" `namedRule` theRule where

    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (goc', newConditions), gocAfter) <- matchFirst gensOrConds $ \case
            Generator (GenDomainHasRepr pat domain@DomainInt{}) -> do
                let
                    f (WithLocals x (DefinednessConstraints cons)) = do
                        tell cons
                        f x
                    f x = return x
                (domain', newConditions) <- runWriterT (transformBiM f domain)
                let goc' = Generator (GenDomainHasRepr pat domain')
                if null newConditions
                    then na "rule_ConditionInsideGeneratorDomain"
                    else return (goc', newConditions)
            _ -> na "rule_ConditionInsideGeneratorDomain"
        return
            ( "Bubbling up definedness constraints inside a generator domain."
            , return $ Comprehension body
                        $  gocBefore
                        ++ [goc']
                        ++ map Condition newConditions
                        ++ gocAfter
            )
    theRule _ = na "rule_ConditionInsideGeneratorDomain"


rule_LiftVars :: Rule
rule_LiftVars = "bubble-up-LiftVars" `namedRule` theRule where

    theRule [essence| catchUndef(&x, &ifUndefVal) |]
        | WithLocals body (DefinednessConstraints cons) <- x = do
            let ifDef = make opAnd (fromList cons)
            return
                ( ""
                , return [essence| [ catchUndef(&body, &ifUndefVal)
                                   , &ifUndefVal
                                   ; int(0..1)
                                   ] [ toInt(!&ifDef) ]
                         |]
                )

    theRule (Comprehension (WithLocals body locals) gensOrConds)
        | and [ case goc of
                    Condition{} -> True
                    ComprehensionLetting{} -> True
                    _ -> False
              | goc <- gensOrConds
              ]                                                 -- if gensOrConds do not contain generators
        = return
            ( "Bubbling up when a comprehension doesn't contain any generators."
            , return $ WithLocals (Comprehension body gensOrConds) locals
            )

    theRule (Comprehension (WithLocals body (AuxiliaryVars locals@(_:_))) gensOrConds) = do

        let decls = [ (nm,dom) | Declaration (FindOrGiven LocalFind nm dom) <- locals ]
        let cons  = concat [ xs | SuchThat xs <- locals ]

        (_conditions, generators) <- fmap mconcat $ forM gensOrConds $ \ goc -> case goc of
            Condition{} -> return ([goc], [])
            ComprehensionLetting{} -> return ([], [goc])
            Generator (GenDomainHasRepr _ _) -> return ([], [goc])
            _ -> na "rule_LiftVars"

        let gensOrConds_dontCare = [ case goc of
                                        Condition c -> Condition (make opNot c)
                                        _ -> goc
                                   | goc <- gensOrConds
                                   ]

        let patRefs = [ Reference patName Nothing | Generator (GenDomainHasRepr patName _domain) <- generators ]
        let indexDomains = [domain | Generator (GenDomainHasRepr _patName domain) <- generators ]

        let upd (Reference nm _) | nm `elem` map fst decls
                = let r = Reference nm Nothing
                  in  make opMatrixIndexing r patRefs
            upd r = r

        let declsLifted =
                [ Declaration (FindOrGiven LocalFind nm domLifted)
                | (nm, dom) <- decls
                , let domLifted = foldr (DomainMatrix . forgetRepr) dom indexDomains
                ]
        let declsLiftedNames =
                [ nm
                | (nm, _dom) <- decls
                ]

        -- traceM $ show $ "declsLifted" <+> vcat (map pretty declsLifted)

        let consLifted =
                [ make opAnd $ Comprehension c gensOrConds
                | c <- transformBi upd cons
                ]

        -- traceM $ show $ "consLifted" <+> vcat (map pretty consLifted)
        -- traceM $ show $ "head lifted 1" <+> pretty body
        -- traceM $ show $ "head lifted 2" <+> pretty (transform upd body)

        let referencesInCons = nub [ r | r@(Reference _ (Just (DeclHasRepr {}))) <- concatMap universe cons]
        let referencesInBody = nub [ r | r@(Reference _ (Just (DeclHasRepr {}))) <- universe body]
        -- traceM $ show $ "referencesInBody" <+> vcat (map pretty referencesInBody)

        let pr :: Name -> String = show . pretty

        -- the name is a prefix of a name that's defined in the decls, but not identical
        let unrefinedInBody = [ name | Reference name _ <- referencesInBody
                                     , or [ pr name `isPrefixOf` pr declName && pr declName /= pr name | declName <- declsLiftedNames ]
                                     ]

        let unrefinedInCons = [ name | Reference name _ <- referencesInCons
                                     , or [ pr name `isPrefixOf` pr declName && pr declName /= pr name | declName <- declsLiftedNames ]
                                     ]

        let dontCareCons = make opAnd $ fromList [ make opDontCare (Reference r Nothing) | r <- declsLiftedNames ]

        -- traceM $ show $ "unrefinedInBody" <+> vcat (map pretty unrefinedInBody)
        -- traceM $ show $ "unrefinedInCons" <+> vcat (map pretty unrefinedInCons)

        when (not (null unrefinedInBody) || not (null unrefinedInCons)) $ na "rule_LiftVars"

        return
            ( "Bubbling up auxiliary variables through a comprehension."
            , return $ WithLocals (make opConcatenate $ fromList [ Comprehension (transform upd body) (transformBi upd gensOrConds)
                                                                 , Comprehension dontCareCons (transformBi upd gensOrConds_dontCare)
                                                                 ])
                                  (AuxiliaryVars (declsLifted ++ [SuchThat consLifted]))
            )

    -- this is to deal with when an AuxiliaryVars expression is in the generator
    theRule (Comprehension body gensOrConds) = do

        (gocBefore, (pat, expr, locals), gocAfter) <- matchFirst gensOrConds $ \case
                Generator (GenInExpr pat (WithLocals expr (AuxiliaryVars locals))) ->
                    return (pat, expr, locals)
                _ -> na "rule_LiftVars"

        return
            ( "Bubbling up auxiliary variables through a comprehension's generator."
            , return $ WithLocals
                (Comprehension body (gocBefore ++ [Generator (GenInExpr pat expr)] ++ gocAfter))
                (AuxiliaryVars locals)
            )

    theRule WithLocals{} = na "rule_LiftVars"
    theRule Reference{} = na "rule_LiftVars"

    -- special handling of AuxiliaryVars-bubbles on the rhs of an implication
    theRule p
        | Just (a, WithLocals b (AuxiliaryVars locals@(_:_))) <- match opImply p
        = do
        let
            decls = [ l | l@(Declaration (FindOrGiven LocalFind _ _)) <- locals ]

            cons  = make opAnd $ fromList $ concat [ xs | SuchThat xs <- locals ]

            dontCares = make opAnd $ fromList [ make opDontCare (Reference nm Nothing)
                                              | Declaration (FindOrGiven LocalFind nm _) <- locals
                                              ]

            p' = [essence| and([ &a -> (&b /\ &cons)
                               , !&a -> &dontCares
                               ])
                         |]

        return
            ( "Bubbling up auxiliary variables, rhs of imply."
            , return $ WithLocals p' (AuxiliaryVars decls)
            )

    theRule p = do
        let
            f (WithLocals y (AuxiliaryVars locals@(_:_))) = do
                tell locals
                return y
            f x = return x
        (p', collected) <- runWriterT (descendM f p)
        when (null collected) $
            na "rule_LiftVars doesn't have any bubbly children"
        return
            ( "Bubbling up auxiliary variables."
            , return $ WithLocals p' (AuxiliaryVars collected)
            )
