{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Sequence where

import Conjure.Rules.Import


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "sequence-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension_Literal"
        (TypeSequence t, elems) <- match sequenceLiteral expr
        let outLiteral = make matrixLiteral
                            (TypeMatrix (TypeInt TagInt) t)
                            (DomainInt TagInt [RangeBounded 1 (fromInt (genericLength elems))])
                            elems
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on sequence literals"
            , do
                (iPat, i) <- quantifiedVar
                let val = [essence| (&i, &outLiteral[&i]) |]
                return $ Comprehension (upd val body)
                         $  gofBefore
                         ++ [Generator (GenDomainNoRepr iPat $ mkDomainIntB 1 (fromInt $ genericLength elems))]
                         ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension_Literal"


rule_Image_Literal_Bool :: Rule
rule_Image_Literal_Bool = "sequence-image-literal-bool" `namedRule` theRule where
    theRule p = do
        (func, arg)                    <- match opImage p
        (TypeSequence TypeBool, elems) <- match sequenceLiteral func
        -- let argIsUndef = make opNot $ make opOr $ fromList
        --         [ [essence| &a = &arg |]
        --         | (a,_) <- elems
        --         ]
        return $
            if null elems
                then
                    ( "Image of empty sequence literal"
                    , return [essence| false |]                          -- undefined is false.
                    )
                else
                    ( "Image of sequence literal"
                    , return $ make opOr $ fromList
                          [ [essence| (&a = &arg) /\ &b |]              -- if this is ever true, the output is true.
                                                                        -- undefined is still false.
                          | (a',b) <- zip allNats elems
                          , let a = fromInt a'
                          ]
                    )


rule_Image_Literal_Int :: Rule
rule_Image_Literal_Int = "sequence-image-literal-int" `namedRule` theRule where
    theRule p = do
        (func, arg)                   <- match opImage p
        (TypeSequence (TypeInt _), elems) <- match sequenceLiteral func
        return
            ( "Image of sequence literal"
            , return $
                let
                    val = make opSum $ fromList
                        -- if this is ever true, the output is the value of b.
                        [ [essence| toInt(&a = &arg) * &b |]
                        | (a',b) <- zip allNats elems
                        , let a = fromInt a'
                        ]
                    len = fromInt $ genericLength elems
                    argIsDef = [essence| &arg <= &len |]
                in
                    WithLocals val (DefinednessConstraints [argIsDef])
            )


rule_Eq_Literal :: Rule
rule_Eq_Literal = "sequence-eq-literal" `namedRule` theRule where
    theRule p = do
        (x,y) <- match opEq p
        xTy <- typeOf x
        yTy <- typeOf y
        case (xTy, yTy) of
            (TypeSequence{}, _) -> return ()
            (_, TypeSequence{}) -> return ()
            _ -> na "rule_Eq_Literal"
        (other, vals) <- case ( match sequenceLiteral x, match matrixLiteral x
                              , match sequenceLiteral y, match matrixLiteral y) of
            (Just (_, vals), _                , _             , _                ) -> return (y, vals)
            (_             , Just (_, _, vals), _             , _                ) -> return (y, vals)
            (_             , _                , Just (_, vals), _                ) -> return (x, vals)
            (_             , _                , _             , Just (_, _, vals)) -> return (x, vals)
            _                 -> na "sequence not empty"
        return
            ( "Horizontal rule for sequence equality, one side empty"
            , do
                let len = fromInt (genericLength vals)
                return $ make opAnd $ fromList
                    $ [essence| |&other| = &len |]
                    : [ [essence| &other(&i) = &val |]
                      | (i_, val) <- zip allNats vals
                      , let i = fromInt i_
                      ]
            )


rule_Eq :: Rule
rule_Eq = "sequence-eq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opEq p
        TypeSequence{} <- typeOf x
        TypeSequence{} <- typeOf y
        case (match sequenceLiteral x, match sequenceLiteral y) of
            (Just (_, []), _) -> na "Sequence{rule_Eq}: one side empty"
            (_, Just (_, [])) -> na "Sequence{rule_Eq}: one side empty"
            _            -> return ()
        return
            ( "Horizontal rule for sequence equality"
            , do
                 (iPat, i) <- quantifiedVar
                 return
                     [essence|
                         (forAll &iPat in &x . &y(&i[1]) = &i[2])
                             /\
                         (forAll &iPat in &y . &x(&i[1]) = &i[2])
                             /\
                         |&x| = |&y|
                     |]
            )


rule_Eq_Comprehension :: Rule
rule_Eq_Comprehension = "sequence-eq-comprehension" `namedRule` theRule where
    theRule p = do
        (x, y@(Comprehension _ goc)) <- do
            (x,y) <- match opEq p
            case x of
                Comprehension{} -> return (y, x)       -- swap if x is a Comprehension
                _               -> return (x, y)
        TypeSequence{} <- typeOf x
        return
            ( "Horizontal rule for sequence equality, with a comprehension on the rhs"
            , do
                (iPat, i) <- quantifiedVar
                let cardinality = Comprehension 1 goc
                return
                    [essence|
                        |&x| = sum(&cardinality) /\
                        and([ &y[&i[1]] = &i[2]
                            | &iPat <- &x
                            ])
                    |]
            )


rule_Neq :: Rule
rule_Neq = "sequence-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeSequence{} <- typeOf x
        TypeSequence{} <- typeOf y
        return
            ( "Horizontal rule for sequence dis-equality"
            , do
                 (iPat, i) <- quantifiedVar
                 return
                     [essence|
                         (exists &iPat in &x . !(&i in &y))
                         \/
                         (exists &iPat in &y . !(&i in &x))
                     |]
            )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "sequence-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opSubsetEq p
        TypeSequence{} <- typeOf x
        TypeSequence{} <- typeOf y
        return
            ( "Horizontal rule for sequence subsetEq"
            , do
                 (iPat, i) <- quantifiedVar
                 return
                     [essence|
                         (forAll &iPat in &x . &y(&i[1]) = &i[2])
                             /\
                         defined(&x) subsetEq defined(&y)
                     |]
            )


rule_Subset :: Rule
rule_Subset = "sequence-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] = do
        TypeSequence{} <- typeOf a
        TypeSequence{} <- typeOf b
        return
            ( "Horizontal rule for sequence subset"
            , return [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "sequence-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypeSequence{} <- typeOf a
        TypeSequence{} <- typeOf b
        return
            ( "Horizontal rule for sequence supset"
            , return [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "sequence-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypeSequence{} <- typeOf a
        TypeSequence{} <- typeOf b
        return
            ( "Horizontal rule for sequence supsetEq"
            , return [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


rule_Comprehension_PreImage :: Rule
rule_Comprehension_PreImage = "sequence-preImage" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_PreImage"
        (func, img) <- match opPreImage expr
        TypeSequence{} <- typeOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the preImage of a sequence"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &j[1] |]
                return $ Comprehension
                        (upd val body)
                        $  gofBefore
                        ++ [ Generator (GenInExpr jPat func)
                           , Condition [essence| &j[2] = &img |]
                           ]
                        ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension_PreImage"


rule_Card :: Rule
rule_Card = "sequence-cardinality" `namedRule` theRule where
    theRule [essence| |&s| |] = do
        TypeSequence{} <- typeOf s
        dom <- domainOf s
        return
            ( "Horizontal rule for sequence cardinality."
            , case dom of
                DomainSequence _ (SequenceAttr (SizeAttr_Size n) _) _
                    -> return n
                DomainSequence _ (SequenceAttr _ jectivity) inner
                    | jectivity `elem` [JectivityAttr_Surjective, JectivityAttr_Bijective]
                    -> domainSizeOf inner
                _ -> do
                    (iPat, _) <- quantifiedVar
                    return [essence| sum &iPat in &s . 1 |]
            )
    theRule _ = na "rule_Card"


rule_Comprehension_Defined :: Rule
rule_Comprehension_Defined = "sequence-defined" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Defined"
        s                                            <- match opDefined expr
        DomainSequence _ (SequenceAttr sizeAttr _) _ <- domainOf s
        maxSize <- case sizeAttr of
                    SizeAttr_Size x -> return x
                    SizeAttr_MaxSize x -> return x
                    SizeAttr_MinMaxSize _ x -> return x
                    _ -> failDoc "rule_Comprehension_Defined maxSize"
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over defined(f)"
            , do
                (jPat, j) <- quantifiedVar
                let val = j
                return $ Comprehension
                            (upd val body)
                            $  gofBefore
                            ++ [ Generator (GenDomainNoRepr jPat $ mkDomainIntB 1 maxSize)
                               , Condition [essence| &j <= |&s| |]
                               ]
                            ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension_Defined"


-- | TODO: This may allow repetitions.
rule_Comprehension_Range :: Rule
rule_Comprehension_Range = "sequence-range" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_PreImage"
        func <- match opRange expr
        TypeSequence{} <- typeOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over range(f)"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &j[2] |]
                return $ Comprehension
                            (upd val body)
                            $  gofBefore
                            ++ [ Generator (GenInExpr jPat func) ]
                            ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension_Range"


rule_In :: Rule
rule_In = "sequence-in" `namedRule` theRule where
    theRule [essence| &x in &f |] = do
        TypeSequence{} <- typeOf f
        return
            ( "Sequence membership to sequence image."
            , return [essence| &f(&x[1]) = &x[2] |]
            )
    theRule _ = na "rule_In"


rule_Restrict_Image :: Rule
rule_Restrict_Image = "sequence-restrict-image" `namedRule` theRule where
    theRule p = do
        (func', arg) <- match opImage p
        (func , dom) <- match opRestrict func'
        TypeSequence{} <- typeOf func
        return
            ( "Sequence image on a restricted sequence."
            , do
                (iPat, i) <- quantifiedVar
                let bob = [essence| exists &iPat : &dom . &i = &arg |]
                return $ WithLocals (make opImage func arg) (DefinednessConstraints [bob])
            )


rule_Restrict_Comprehension :: Rule
rule_Restrict_Comprehension = "sequence-restrict-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (iPat, iPatName, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr iPat@(Single iPatName) expr) -> return (iPat, iPatName, expr)
            _ -> na "rule_Comprehension_PreImage"
        (func, dom) <- match opRestrict expr
        TypeSequence{} <- typeOf func
        return
            ( "Mapping over restrict(func, dom)"
            , do
                (jPat, j) <- quantifiedVar
                let i = Reference iPatName Nothing
                return $ Comprehension body
                            $  gofBefore
                            ++ [ Generator (GenInExpr iPat func)
                               , Condition [essence| exists &jPat : &dom . &j = &i[1] |]
                               ]
                            ++ gofAfter
            )
    theRule _ = na "rule_Restrict_Comprehension"



rule_Image_Bool :: Rule
rule_Image_Bool = "sequence-image-bool" `namedRule` theRule where
    theRule Reference{} = na "rule_Image_Bool"
    theRule p = do
        let
            onChildren
                :: MonadState (Maybe (Expression, Expression)) m
                => Expression
                -> m (Expression -> Expression)
            onChildren ch = do
                let
                    try = do
                        (func, arg) <- match opImage ch
                        case match opRestrict func of
                            Nothing -> return ()
                            Just{}  -> na "rule_Image_Bool"         -- do not use this rule for restricted sequences
                        case match opTransform func of
                            Nothing -> na "rule_Image_Bool"          -- only use this rule for transformed sequences
                            Just{}  -> return ()
                        TypeSequence TypeBool <- typeOf func
                        return (func, arg)
                case try of
                    Nothing -> return (const ch)        -- do not failDoc if a child is not of proper form
                    Just (func, arg) -> do              -- just return it back unchanged
                        seenBefore <- gets id
                        case seenBefore of
                            Nothing -> do
                                modify $ const $ Just (func, arg)
                                return id
                            Just{}  ->
                                return (const ch)

        let (children_, gen) = uniplate p
        (genChildren, mFunc) <- runStateT (mapM onChildren children_) Nothing
        let
            mkP :: Expression -> Expression
            mkP new = gen $ fmap ($ new) genChildren
        (func, arg) <- maybe (na "rule_Image_Bool") return mFunc        -- Nothing signifies no relevant children
        return
            ( "Sequence image, bool."
            , do
                (iPat, i) <- quantifiedVar
                return $ mkP $ make opOr $ Comprehension [essence| &i[2] |]
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
            )


rule_Image_Int :: Rule
rule_Image_Int = "sequence-image-int" `namedRule` theRule where
    theRule Reference{} = na "rule_Image_Int"
    theRule p = do
        let
            onChildren
                :: MonadState (Maybe (Expression, Expression)) m
                => Expression
                -> m (Expression -> Expression)
            onChildren ch = do
                let
                    try = do
                        (func, arg) <- match opImage ch
                        case match opRestrict func of
                            Nothing -> return ()
                            Just{}  -> na "rule_Image_Int"          -- do not use this rule for restricted sequences
                        case match opTransform func of
                            Nothing -> na "rule_Image_Int"          -- only use this rule for transformed sequences
                            Just{}  -> return ()
                        TypeSequence (TypeInt _) <- typeOf func
                        return (func, arg)
                case try of
                    Nothing -> return (const ch)        -- do not failDoc if a child is not of proper form
                    Just (func, arg) -> do              -- just return it back unchanged
                        seenBefore <- gets id
                        case seenBefore of
                            Nothing -> do
                                modify $ const $ Just (func, arg)
                                return id
                            Just{}  ->
                                return (const ch)

        let (children_, gen) = uniplate p
        (genChildren, mFunc) <- runStateT (mapM onChildren children_) Nothing
        let
            mkP :: Expression -> Expression
            mkP new = gen $ fmap ($ new) genChildren
        (func, arg) <- maybe (na "rule_Image_Int") return mFunc         -- Nothing signifies no relevant children
        return
            ( "Sequence image, int."
            , do
                (iPat, i) <- quantifiedVar
                let val = make opSum $ Comprehension [essence| &i[2] |]
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
                    isDefined = [essence| &arg in defined(&func) |]
                return $ mkP $ WithLocals val (DefinednessConstraints [isDefined])
            )


rule_Comprehension_Image :: Rule
rule_Comprehension_Image = "sequence-image-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Image"
        (mkModifier, expr2) <- match opModifier expr
        (func, arg) <- match opImage expr2
        TypeSequence{} <- typeOf func
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Image_Bool"         -- do not use this rule for restricted sequences
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the image of a sequence"
            , do
                (iPat, i) <- quantifiedVar
                (jPat, j) <- quantifiedVar
                return $ Comprehension
                        (upd j body)
                        $  gofBefore
                        ++ [ Generator (GenInExpr iPat (mkModifier func))
                           , Condition [essence| &i[1] = &arg |]
                           , Generator (GenInExpr jPat [essence| &i[2] |])
                           ]
                        ++ transformBi (upd j) gofAfter
            )
    theRule _ = na "rule_Comprehension_Image"


rule_Substring :: Rule
rule_Substring = "substring" `namedRule` theRule where
    theRule [essence| &a substring &b |] = do
        TypeSequence{} <- typeOf a
        TypeSequence{} <- typeOf b

        DomainSequence _ (SequenceAttr aSizeAttr _) _ <- domainOf a
        aMaxSize <- case aSizeAttr of
                    SizeAttr_Size x -> return x
                    SizeAttr_MaxSize x -> return x
                    SizeAttr_MinMaxSize _ x -> return x
                    _ -> failDoc "rule_Substring maxSize"

        DomainSequence _ (SequenceAttr bSizeAttr _) _ <- domainOf b
        bMaxSize <- case bSizeAttr of
                    SizeAttr_Size x -> return x
                    SizeAttr_MaxSize x -> return x
                    SizeAttr_MinMaxSize _ x -> return x
                    _ -> failDoc "rule_Substring maxSize"

        let maxSize = [essence| max([&aMaxSize, &bMaxSize]) |]

        return
            ( "Horizontal rule for substring on 2 sequences"
            , do
                (iPat, i) <- quantifiedVar
                (jPat, j) <- quantifiedVar
                return $ make opOr $ Comprehension
                        (make opAnd $ Comprehension
                            [essence| &j[2] = image(&b, &i + &j[1]) |]
                            [ Generator (GenInExpr jPat a)
                            ]
                        )
                        [ Generator (GenDomainNoRepr iPat $ mkDomainIntB 0 [essence| &maxSize - 1 |])]
            )
    theRule _ = na "rule_Substring"


rule_Subsequence :: Rule
rule_Subsequence = "subsequence" `namedRule` theRule where
    theRule [essence| &a subsequence &b |] = do
        TypeSequence{} <- typeOf a
        TypeSequence{} <- typeOf b

        DomainSequence _ (SequenceAttr aSizeAttr _) _ <- domainOf a
        aMaxSize <- case aSizeAttr of
                    SizeAttr_Size x -> return x
                    SizeAttr_MaxSize x -> return x
                    SizeAttr_MinMaxSize _ x -> return x
                    _ -> failDoc "rule_Subsequence maxSize"

        DomainSequence _ (SequenceAttr bSizeAttr _) _ <- domainOf b
        bMaxSize <- case bSizeAttr of
                    SizeAttr_Size x -> return x
                    SizeAttr_MaxSize x -> return x
                    SizeAttr_MinMaxSize _ x -> return x
                    _ -> failDoc "rule_Subsequence maxSize"

        -- for each value in a, find an index into b such that these indices are in increasing order
        -- when there are multiple mappings that produce the same "a" (i.e. when there are duplicates in b)
        -- this is does not functionally define the aux variable
        return
            ( "Horizontal rule for subsequence on 2 sequences"
            , do
                (auxName, aux) <- auxiliaryVar
                (iPat, i) <- quantifiedVar
                return $ WithLocals
                        [essence|
                            and([ &i[2] = image(&b, image(&aux, &i[1]))
                                | &iPat <- &a
                                ])
                        |]
                        (AuxiliaryVars
                            [ Declaration (FindOrGiven LocalFind auxName
                                    (DomainSequence def (SequenceAttr aSizeAttr def) (mkDomainIntB 1 bMaxSize)))
                            , SuchThat
                                [ [essence| and([ image(&aux, &i-1) < image(&aux, &i)
                                                | &iPat : int(2..&aMaxSize)
                                                , &i <= |&aux|
                                                ])
                                  |]
                                , [essence| |&a| = |&aux|
                                  |]
                                ]
                            ])
            )
    theRule _ = na "rule_Subsequence"
