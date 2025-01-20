{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Function where

import Conjure.Rules.Import
import Conjure.Rules.Definition

-- uniplate
import Data.Generics.Uniplate.Zipper as Zipper ( up, hole )


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "function-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension_Literal"
        (TypeFunction fr to, elems) <- match functionLiteral expr
        let outLiteral = make matrixLiteral
                            (TypeMatrix (TypeInt TagInt) (TypeTuple [fr,to]))
                            (DomainInt TagInt [RangeBounded 1 (fromInt (genericLength elems))])
                            [ AbstractLiteral (AbsLitTuple [a,b])
                            | (a,b) <- elems
                            ]
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on function literals"
            , do
                (iPat, i) <- quantifiedVar
                return $ Comprehension (upd i body)
                    $  gocBefore
                    ++ [Generator (GenInExpr iPat outLiteral)]
                    ++ transformBi (upd i) gocAfter
            )
    theRule _ = na "rule_Comprehension_Literal"


rule_Eq :: Rule
rule_Eq = "function-eq" `namedRule` theRule where
    theRule [essence| &x = &y |] = do
        case x of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        case y of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return
            ( "Horizontal rule for function equality"
            , do
                (iPat, i) <- quantifiedVar
                return [essence|
                            (forAll &iPat in &x . &y(&i[1]) = &i[2])
                                /\
                            (forAll &iPat in &y . &x(&i[1]) = &i[2])
                       |]
            )
    theRule _ = na "rule_Eq"


rule_Neq :: Rule
rule_Neq = "function-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        case x of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        case y of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return
            ( "Horizontal rule for function dis-equality"
            , do
                (iPat, i) <- quantifiedVar
                return [essence|
                            (exists &iPat in &x . !(&i in &y))
                            \/
                            (exists &iPat in &y . !(&i in &x))
                       |]
            )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "function-subsetEq" `namedRule` theRule where
    theRule [essence| &x subsetEq &y |] = do
        case x of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        case y of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return
            ( "Horizontal rule for function subsetEq"
            , do
                (iPat, i) <- quantifiedVar
                return [essence|
                            (forAll &iPat in &x . &y(&i[1]) = &i[2])
                       |]
            )
    theRule _ = na "rule_SubsetEq"


rule_Subset :: Rule
rule_Subset = "function-subset" `namedRule` theRule where
    theRule [essence| &x subset &y |] = do
        case x of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        case y of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return
            ( "Horizontal rule for function subset"
            , return [essence| &x subsetEq &y /\ &x != &y |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "function-supset" `namedRule` theRule where
    theRule [essence| &x supset &y |] = do
        case x of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        case y of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return
            ( "Horizontal rule for function supset"
            , return [essence| &y subset &x |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "function-subsetEq" `namedRule` theRule where
    theRule [essence| &x supsetEq &y |] = do
        case x of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        case y of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return
            ( "Horizontal rule for function supsetEq"
            , return [essence| &y subsetEq &x |]
            )
    theRule _ = na "rule_SupsetEq"


rule_Inverse :: Rule
rule_Inverse = "function-inverse" `namedRule` theRule where
    theRule [essence| inverse(&a, &b) |] = do
        case a of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        case b of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        return
            ( "Horizontal rule for function inverse"
            , do
                (iPat, i) <- quantifiedVar
                return
                    [essence|
                        (forAll &iPat in &a . &b(&i[2]) = &i[1])
                            /\
                        (forAll &iPat in &b . &a(&i[2]) = &i[1])
                    |]
            )
    theRule _ = na "rule_Inverse"


rule_Comprehension_PreImage :: Rule
rule_Comprehension_PreImage = "function-preImage" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet] expr)
            _ -> na "rule_Comprehension_PreImage"
        (func, img) <- match opPreImage expr
        TypeFunction{} <- typeOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the preImage of a function"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &j[1] |]
                return $
                    Comprehension
                        (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenInExpr jPat func)
                           , Condition [essence| &j[2] = &img |]
                           ]
                        ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_PreImage"


rule_Card :: Rule
rule_Card = "function-cardinality" `namedRule` theRule where
    theRule [essence| |&f| |] = do
        TypeFunction{} <- typeOf f
        dom <- domainOf f
        return
            ( "Function cardinality"
            , case dom of
                DomainFunction _ (FunctionAttr (SizeAttr_Size n) _ _) _ _
                    -> return n
                DomainFunction _ (FunctionAttr _ _ jectivity) _ innerTo
                    | jectivity `elem` [JectivityAttr_Surjective, JectivityAttr_Bijective]
                    -> domainSizeOf innerTo
                DomainFunction _ (FunctionAttr _ PartialityAttr_Total _) innerFr _
                    -> domainSizeOf innerFr
                _ -> return [essence| |toSet(&f)| |]
            )
    theRule _ = na "rule_Card"


rule_Comprehension_Defined :: Rule
rule_Comprehension_Defined = "function-defined" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Defined"
        func <- match opDefined expr
        TypeFunction{} <- typeOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over defined(f)"
            , do
                (iPat, i) <- quantifiedVar
                let i1 = [essence| &i[1] |]
                return $
                    Comprehension
                        (upd i1 body)
                        $  gocBefore
                        ++ [ Generator (GenInExpr iPat func) ]
                        ++ transformBi (upd i1) gocAfter
            )
    theRule _ = na "rule_Comprehension_Defined"


rule_Comprehension_Range :: Rule
rule_Comprehension_Range = "function-range" `Rule` theRule where

    theRule z p = do
        should <- shouldRemoveDuplicates z
        if should
            then theRule_shouldRemoveDuplicates p
            else theRule_noRemoveDuplicates p

    -- keep going up, until finding a quantifier
    -- when found, return whether this quantifier requires us to remove duplicates or not
    -- if none exists, do not apply the rule.
    -- (or maybe we should call bug right ahead, it can't be anything else.)
    shouldRemoveDuplicates z0 =
        case Zipper.up z0 of
            Nothing -> na "rule_Comprehension_Range shouldRemoveDuplicates 1"
            Just z -> do
                let h = Zipper.hole z
                case ( match opAnd h, match opOr h, match opSum h
                     , match opMin h, match opMax h ) of
                    (Just{}, _, _, _, _) -> return False
                    (_, Just{}, _, _, _) -> return False
                    (_, _, Just{}, _, _) -> return True
                    (_, _, _, Just{}, _) -> return False
                    (_, _, _, _, Just{}) -> return False
                    _                    -> na "rule_Comprehension_Range shouldRemoveDuplicates 2"
                                            -- case Zipper.up z of
                                            --     Nothing -> na "queryQ"
                                            --     Just u  -> queryQ u

    theRule_shouldRemoveDuplicates (Comprehension body gensOrConds) = do

        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Range"
        func <- match opRange expr
        TypeFunction{} <- typeOf func
        DomainFunction _ attrs _domFr domTo <- domainOf func
        let upd val old = lambdaToFunction pat old val
        let
            isInjective =
                case attrs of
                    FunctionAttr _ _ JectivityAttr_Injective -> True
                    FunctionAttr _ _ JectivityAttr_Bijective -> True
                    _ -> False

            -- the range is already alldiff
            caseInjective = do
                (iPat, i) <- quantifiedVar
                let i2 = [essence| &i[2] |]
                return $
                    Comprehension
                        (upd i2 body)
                        $  gocBefore
                        ++ [ Generator (GenInExpr iPat func) ]
                        ++ transformBi (upd i2) gocAfter

            -- this is the expensive case: introduce an aux set for the range to make it alldiff
            caseNonInjective = do
                (auxName, aux) <- auxiliaryVar
                (jPat, j) <- quantifiedVar
                (kPat, k) <- quantifiedVar
                (lPat, l) <- quantifiedVar
                let k2 = [essence| &k[2] |]
                let l2 = [essence| &l[2] |]
                return $ WithLocals
                    (Comprehension
                        (upd j body)
                        $  gocBefore
                        ++ [ Generator (GenInExpr jPat aux) ]
                        ++ transformBi (upd j) gocAfter)
                    (AuxiliaryVars
                        [ Declaration (FindOrGiven LocalFind auxName (DomainSet def def (forgetRepr domTo)))
                        , SuchThat
                            [ make opAnd $ Comprehension
                                [essence| &k2 in &aux |]
                                [ Generator (GenInExpr kPat func) ]
                            , make opAnd $
                                Comprehension
                                    (make opOr $ Comprehension
                                        [essence| &l2 = &k |]
                                        [ Generator (GenInExpr lPat func) ]
                                    )
                                    [ Generator (GenInExpr kPat aux) ]
                            ]
                        ])

        when isInjective $
            unless (null [ () | DomainAny{} <- universe domTo ]) $
                na "Cannot compute the domain of range(f)"

        return
            [ RuleResult
                { ruleResultDescr = "Mapping over range(f)"
                , ruleResultType  = ExpressionRefinement
                , ruleResult      = if isInjective
                                        then caseInjective
                                        else caseNonInjective
                , ruleResultHook  = Nothing
                } ]
    theRule_shouldRemoveDuplicates _ = na "rule_Comprehension_Range"

    theRule_noRemoveDuplicates (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Range"
        func <- match opRange expr
        TypeFunction{} <- typeOf func
        let upd val old = lambdaToFunction pat old val
        return
            [ RuleResult
                { ruleResultDescr = "Mapping over range(f)"
                , ruleResultType  = ExpressionRefinement
                , ruleResult      = do
                    (iPat, i) <- quantifiedVar
                    let i2 = [essence| &i[2] |]
                    return $ Comprehension (upd i2 body)
                                $  gocBefore
                                ++ [Generator (GenInExpr iPat func)]
                                ++ transformBi (upd i2) gocAfter
                , ruleResultHook  = Nothing
                } ]
    theRule_noRemoveDuplicates _ = na "rule_Comprehension_Range"


-- TODO: What about duplicates for sum, product, etc?
rule_Param_DefinedRange :: Rule
rule_Param_DefinedRange = "param-DefinedRange-of-function" `namedRule` theRule where
    theRule p = do
        unless (categoryOf p == CatParameter) $ na "rule_Param_DefinedRange"
        (_reducerType, _, mk, p2) <- match opReducer p
        (index, f) <- case p2 of
            [essence| defined(&f) |] -> return (1, f)
            [essence| range(&f)   |] -> return (2, f)
            _ -> na "rule_Param_DefinedRange"
        return
            ( "rule_Param_DefinedRange"
            , do
                (iPat, i) <- quantifiedVar
                return $ mk $ [essence| [ &i[&index] | &iPat <- &f ] |]
            )


rule_Comprehension_Defined_Size :: Rule
rule_Comprehension_Defined_Size = "function-defined-size" `namedRule` theRule where
    theRule [essence| size(defined(&func), &n) |] = do
        DomainFunction _ _ domFr _domTo <- domainOf func
        return
            ( "size(defined(func), n)"
            , do
                (auxName, aux) <- auxiliaryVar
                (kPat, k) <- quantifiedVar
                (lPat, l) <- quantifiedVar
                let k1 = [essence| &k[1] |]
                let l1 = [essence| &l[1] |]
                return $ WithLocals
                    (fromBool True)
                    (AuxiliaryVars
                        [ Declaration (FindOrGiven LocalFind auxName
                                (DomainSet def (SetAttr (SizeAttr_Size n)) (forgetRepr domFr)))
                        , SuchThat
                            [ make opAnd $ Comprehension
                                [essence| &k1 in &aux |]
                                [ Generator (GenInExpr kPat func) ]
                            , make opAnd $
                                Comprehension
                                    (make opOr $ Comprehension
                                        [essence| &l1 = &k |]
                                        [ Generator (GenInExpr lPat func) ]
                                    )
                                    [ Generator (GenInExpr kPat aux) ]
                            ]
                        ])
            )
    theRule _ = na "rule_Comprehension_Defined_Size"


rule_Comprehension_Range_Size :: Rule
rule_Comprehension_Range_Size = "function-range-size" `namedRule` theRule where
    theRule [essence| size(range(&func), &n) |] = do
        DomainFunction _ _ _domFr domTo <- domainOf func
        return
            ( "size(range(func), n)"
            , do
                (auxName, aux) <- auxiliaryVar
                (kPat, k) <- quantifiedVar
                (lPat, l) <- quantifiedVar
                let k2 = [essence| &k[2] |]
                let l2 = [essence| &l[2] |]
                return $ WithLocals
                    (fromBool True)
                    (AuxiliaryVars
                        [ Declaration (FindOrGiven LocalFind auxName
                                (DomainSet def (SetAttr (SizeAttr_Size n)) (forgetRepr domTo)))
                        , SuchThat
                            [ make opAnd $ Comprehension
                                [essence| &k2 in &aux |]
                                [ Generator (GenInExpr kPat func) ]
                            , make opAnd $
                                Comprehension
                                    (make opOr $ Comprehension
                                        [essence| &l2 = &k |]
                                        [ Generator (GenInExpr lPat func) ]
                                    )
                                    [ Generator (GenInExpr kPat aux) ]
                            ]
                        ])
            )
    theRule _ = na "rule_Comprehension_Range_Size"


rule_In :: Rule
rule_In = "function-in" `namedRule` theRule where
    theRule [essence| &x in &f |] = do
        TypeFunction{} <- typeOf f
        return
            ( "Function membership to function image."
            , return [essence| &f(&x[1]) = &x[2] |]
            )
    theRule _ = na "rule_In"


rule_Restrict_Image :: Rule
rule_Restrict_Image = "function-restrict-image" `namedRule` theRule where
    theRule p = do
        (func', arg) <- match opImage p
        (func , dom) <- match opRestrict func'
        TypeFunction{} <- typeOf func
        return
            ( "Function image on a restricted function."
            , do
                (iPat, i) <- quantifiedVar
                let bob = [essence| exists &iPat : &dom . &i = &arg |]
                return $ WithLocals (make opImage func arg) (DefinednessConstraints [bob])
            )


rule_Restrict_Comprehension :: Rule
rule_Restrict_Comprehension = "function-restrict-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (iPat, iPatName, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr iPat@(Single iPatName) expr) -> return (iPat, iPatName, expr)
            _ -> na "rule_Restrict_Comprehension"
        (func, dom) <- match opRestrict expr
        TypeFunction{} <- typeOf func
        return
            ( "Mapping over restrict(func, dom)"
            , do
                (jPat, j) <- quantifiedVar
                let i = Reference iPatName Nothing
                return $ Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr iPat func)
                       , Condition [essence| exists &jPat : &dom . &j = &i[1] |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Restrict_Comprehension"


--   image(f,x) can be nasty for non-total functions.
--   1.   if f is a total function, it can readily be replaced by a set expression.
--   2.1. if f isn't total, and if the return type is right, it will always end up as a generator for a comprehension.
--      a vertical rule is needed for such cases.
--   2.2. if the return type is not "right", i.e. it is a bool or an int, i.e. sth we cannot quantify over,
--        the vertical rule is harder.


-- | f(x) : bool ~~> or([ b | (a,b) <- f, a = x])
rule_Image_Bool :: Rule
rule_Image_Bool = "function-image-bool" `namedRule` theRule where
    theRule p = do
        (func, arg) <- match opImage p
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Image_Bool"         -- do not use this rule for restricted functions
        TypeFunction _ TypeBool <- typeOf func
        return
            ( "Function image, bool."
            , do
                (iPat, i) <- quantifiedVar
                return $ make opOr $ Comprehension [essence| &i[2] |]
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
            )


-- | f(x)[i] : bool ~~> or([ b[i] | (a,b) <- f, a = x])             "matrix indexing"
rule_Image_BoolMatrixIndexed :: Rule
rule_Image_BoolMatrixIndexed = "function-image-BoolMatrixIndexed" `namedRule` theRule where
    theRule p = do
        (matrix, indices)                      <- match opMatrixIndexing p
        (func, arg)                            <- match opImage matrix
        TypeFunction _ (TypeMatrix _ TypeBool) <- typeOf func
        return
            ( "Function image, matrix of bool."
            , do
                (iPat, i) <- quantifiedVar
                let i2 = make opMatrixIndexing [essence| &i[2] |] indices
                return $ make opOr $ Comprehension i2
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
            )


-- | f(x)[i] : bool ~~> or([ b[i] | (a,b) <- f, a = x])             "tuple indexing"
rule_Image_BoolTupleIndexed :: Rule
rule_Image_BoolTupleIndexed = "function-image-BoolTupleIndexed" `namedRule` theRule where
    theRule p = do
        (matrix, index)               <- match opIndexing p
        (func, arg)                   <- match opImage matrix
        TypeFunction _ (TypeTuple ts) <- typeOf func
        iInt                          <- match constantInt index
        case atMay ts (fromInteger (iInt-1)) of
            Just TypeBool -> return ()
            _             -> na "rule_Image_BoolTupleIndexed"
        return
            ( "Function image, tuple of bool."
            , do
                (iPat, i) <- quantifiedVar
                let i2 = make opIndexing [essence| &i[2] |] index
                return $ make opOr $ Comprehension i2
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
            )


-- | f(x) : int ~~> sum([ b | (a,b) <- f, a = x])
rule_Image_Int :: Rule
rule_Image_Int = "function-image-int" `namedRule` theRule where
    theRule p = do
        (func, arg) <- match opImage p
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Image_Int"          -- do not use this rule for restricted functions
        TypeFunction _ (TypeInt _) <- typeOf func
        return
            ( "Function image, int."
            , do
                (iPat, i) <- quantifiedVar
                let val = make opSum $ Comprehension [essence| &i[2] |]
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
                let isDefined = [essence| &arg in defined(&func) |]
                return $ WithLocals val (DefinednessConstraints [isDefined])
            )


-- | f(x)[i] : int ~~> sum([ b[i] | (a,b) <- f, a = x])             "matrix indexing"
rule_Image_IntMatrixIndexed :: Rule
rule_Image_IntMatrixIndexed = "function-image-IntMatrixIndexed" `namedRule` theRule where
    theRule p = do
        (matrix, indices)                      <- match opMatrixIndexing p
        (func, arg)                            <- match opImage matrix
        TypeFunction _ (TypeMatrix _ (TypeInt _))  <- typeOf func
        return
            ( "Function image, matrix of int."
            , do
                (iPat, i) <- quantifiedVar
                let i2 = make opMatrixIndexing [essence| &i[2] |] indices
                let val = make opSum $ Comprehension i2
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
                let isDefined = [essence| &arg in defined(&func) |]
                return $ WithLocals val (DefinednessConstraints [isDefined])
            )


-- | f(x)[i] : int ~~> sum([ b[i] | (a,b) <- f, a = x])             "tuple indexing"
rule_Image_IntTupleIndexed :: Rule
rule_Image_IntTupleIndexed = "function-image-IntTupleIndexed" `namedRule` theRule where
    theRule p = do
        (matrix, index)               <- match opIndexing p
        (func, arg)                   <- match opImage matrix
        TypeFunction _ (TypeTuple ts) <- typeOf func
        iInt                          <- match constantInt index
        case atMay ts (fromInteger (iInt-1)) of
            Just (TypeInt _) -> return ()
            _            -> na "rule_Image_IntTupleIndexed"
        return
            ( "Function image, tuple of int."
            , do
                (iPat, i) <- quantifiedVar
                let i2 = make opIndexing [essence| &i[2] |] index
                let val = make opSum $ Comprehension i2
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
                let isDefined = [essence| &arg in defined(&func) |]
                return $ WithLocals val (DefinednessConstraints [isDefined])
            )


-- | [ ..i.. | i <- f(x), ..i.. ] ~~>
--   [ ..j.. | (a,b) <- f, a = i, j <- b, ..j.. ]
rule_Comprehension_Image :: Rule
rule_Comprehension_Image = "function-image-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Image"
        (mkModifier, expr2) <- match opModifier expr
        (func, arg) <- match opImage expr2
        TypeFunction{} <- typeOf func
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Comprehension_Image"         -- do not use this rule for restricted functions
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the image of a function"
            , do
                (iPat, i) <- quantifiedVar
                (jPat, j) <- quantifiedVar
                return $ Comprehension
                    (upd j body)
                    $  gocBefore
                    ++ [ Generator (GenInExpr iPat func)
                       , Condition [essence| &i[1] = &arg |]
                       , Generator (GenInExpr jPat (mkModifier [essence| &i[2] |]))
                       ]
                    ++ transformBi (upd j) gocAfter
            )
    theRule _ = na "rule_Comprehension_Image"


-- | [ ..i.. | i <- imageSet(f,x), ..i.. ] ~~>
--   [ ..b.. | (a,b) <- f, a = i, ..b.. ]
rule_Comprehension_ImageSet :: Rule
rule_Comprehension_ImageSet = "function-imageSet-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_ImageSet"
        (mkModifier, expr2) <- match opModifierNoP expr
        (func, arg) <- match opImageSet expr2
        TypeFunction{} <- typeOf func
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Comprehension_ImageSet"         -- do not use this rule for restricted functions
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the imageSet of a function"
            , do
                (iPat, i) <- quantifiedVar
                return $ Comprehension
                    (upd [essence| &i[2] |] body)
                    $  gocBefore
                    ++ [ Generator (GenInExpr iPat (mkModifier func))
                       , Condition [essence| &i[1] = &arg |]
                       ]
                    ++ transformBi (upd [essence| &i[2] |]) gocAfter
            )
    theRule _ = na "rule_Comprehension_ImageSet"


-- | f(x) <=lex m ~~> and([ b <=lex m | (a,b) <- f, a = x])
rule_Image_Matrix_LexLhs :: Rule
rule_Image_Matrix_LexLhs = "function-image-matrix-lexlhs" `namedRule` theRule where
    theRule p = do
        (mkLex, (lhs,rhs)) <- match opLex p
        (func, arg) <- match opImage lhs
        TypeFunction{} <- typeOf func
        return
            ( "Function image, matrix as an argument to a lex operator."
            , do
                (iPat, i) <- quantifiedVar
                let val = make opAnd $ Comprehension (mkLex [essence| &i[2] |] rhs)
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
                let isDefined = [essence| &arg in defined(&func) |]
                return $ WithLocals val (DefinednessConstraints [isDefined])
            )


-- | f(x) <=lex m ~~> and([ b <=lex m | (a,b) <- f, a = x])
rule_Image_Matrix_LexRhs :: Rule
rule_Image_Matrix_LexRhs = "function-image-matrix-lexrhs" `namedRule` theRule where
    theRule p = do
        (mkLex, (lhs,rhs)) <- match opLex p
        (func, arg) <- match opImage rhs
        TypeFunction{} <- typeOf func
        return
            ( "Function image, matrix as an argument to a lex operator."
            , do
                (iPat, i) <- quantifiedVar
                let val = make opAnd $ Comprehension (mkLex lhs [essence| &i[2] |])
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
                let isDefined = [essence| &arg in defined(&func) |]
                return $ WithLocals val (DefinednessConstraints [isDefined])
            )


rule_Defined_Intersect :: Rule
rule_Defined_Intersect = "function-Defined-intersect" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_Defined_Intersect"
        f       <- match opDefined expr
        (x, y)  <- match opIntersect f
        tx      <- typeOf x
        case tx of
            TypeFunction{} -> return ()
            _              -> failDoc "type incompatibility in intersect operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for function intersection"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat (make opDefined x))
                       , Condition [essence| (&i, image(&x,&i)) in &y |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Defined_Intersect"


rule_DefinedOrRange_Union :: Rule
rule_DefinedOrRange_Union = "function-DefinedOrRange-union" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_DefinedOrRange_Union"
        (mk, f) <- match opDefinedOrRange expr
        (x, y)  <- match opUnion f
        tx      <- typeOf x
        case tx of
            TypeFunction{} -> return ()
            _              -> failDoc "type incompatibility in union operator"
        let mkx = mk x
        let mky = mk y
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for function union"
            , return $ make opFlatten $ AbstractLiteral $ AbsLitMatrix
                (DomainInt TagInt [RangeBounded 1 2])
                [ Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat mkx) ]
                    ++ gocAfter
                , Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat mky)
                       , Condition [essence| !(&i in &mkx) |]
                       ]
                    ++ gocAfter
                ]
            )
    theRule _ = na "rule_DefinedOrRange_Union"


rule_DefinedOrRange_Difference :: Rule
rule_DefinedOrRange_Difference = "function-DefinedOrRange-difference" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_DefinedOrRange_Difference"
        (mk, f) <- match opDefinedOrRange expr
        (x, y)  <- match opMinus f
        tx      <- typeOf x
        case tx of
            TypeFunction{} -> return ()
            _              -> failDoc "type incompatibility in difference operator"
        let mkx = mk x
        let mky = mk y
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for function difference"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat mkx)
                       , Condition [essence| !(&i in &mky) |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_DefinedOrRange_Difference"


