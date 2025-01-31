{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Matrix where

import Conjure.Rules.Import
import Conjure.Rules.Definition ( RuleResult(..), QuestionType(..) )
import Conjure.Rules.Vertical.Tuple ( decomposeLexLt, decomposeLexLeq  )

-- uniplate
import Data.Generics.Uniplate.Zipper ( hole )
import Data.Generics.Uniplate.Zipper as Zipper ( up )


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = Rule "matrix-comprehension-literal" theRule where
    theRule z (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        (_, _index, elems) <- match matrixLiteral expr
        notInsideMinMax z
        tyInner <- typeOf body
        let ty = TypeMatrix (TypeInt TagInt) tyInner
        return
            [ RuleResult
                { ruleResultDescr = "Vertical rule for matrix-comprehension on matrix literal"
                , ruleResultType  = ExpressionRefinement
                , ruleResultHook  = Nothing
                , ruleResult      = return $
                    case elems of
                        []   -> make matrixLiteral ty (mkDomainIntB 1 0) []
                        [el] -> Comprehension body
                                    $  gocBefore
                                    ++ [ComprehensionLetting pat el]
                                    ++ gocAfter
                        _    -> make opConcatenate $ AbstractLiteral $ AbsLitMatrix
                                    (mkDomainIntB 1 (fromInt $ genericLength elems))
                                    [ Comprehension body
                                        $  gocBefore
                                        ++ [ComprehensionLetting pat el]
                                        ++ gocAfter
                                    | el <- elems
                                    ]
                } ]
    theRule _ _ = na "rule_Comprehension_Literal"

    notInsideMinMax z0 =
        case Zipper.up z0 of
            Nothing -> na "rule_Comprehension_Literal 1"
            Just z1 -> do
                let h = hole z1
                case match opReducer h of
                    Just (_, False, _, _) -> na "rule_Comprehension_Literal"
                    Just (_, True , _, _) -> return ()
                    Nothing               -> case Zipper.up z1 of
                                                Nothing -> return ()
                                                Just u  -> notInsideMinMax u


rule_Comprehension :: Rule
rule_Comprehension = "matrix-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat expr) -> return (pat, expr)
            _ -> na "rule_Comprehension"
        case match matrixLiteral expr of
            Just{} -> na "rule_Comprehension" -- this case is handled in rule_Comprehension_Literal
            Nothing -> return ()
        indexDom:_ <- indexDomainsOf expr
        case indexDom of
            DomainAny{} -> na "rule_Comprehension"
            DomainInt _ [RangeLowerBounded _] -> na "rule_Comprehension"
            _ -> return ()
        return
            ( "Comprehension on a matrix"
            , do
                (iPat, i) <- quantifiedVar
                return $ Comprehension body
                            $ gocBefore
                            ++ [ Generator (GenDomainNoRepr iPat indexDom)
                               , ComprehensionLetting pat [essence| &expr[&i] |]
                               ]
                            ++ gocAfter
            )
    theRule _ = na "rule_Comprehension"


rule_Comprehension_Flatten :: Rule
rule_Comprehension_Flatten = "matrix-comprehension-flatten" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Flatten"
        m <- match opFlatten expr
        indexDoms <- indexDomainsOf m
        forM_ indexDoms $ \case
            DomainAny{} -> na "rule_Comprehension_Flatten"
            _ -> return ()
        when (null indexDoms) $ na "rule_Comprehension_Flatten"
        return
            ( "Comprehension on a matrix flatten"
            , do
                (gens, is) <- unzip <$> sequence
                                [ do
                                    (iPat, i) <- quantifiedVar
                                    return (Generator (GenDomainNoRepr iPat d), i)
                                | d <- indexDoms
                                ]
                return $ Comprehension body
                            $  gocBefore
                            ++ gens
                            ++ [ ComprehensionLetting pat (make opMatrixIndexing m is) ]
                            ++ gocAfter
            )
    theRule _ = na "rule_Comprehension_Flatten"


-- | input:  [ i | i <- m[j] ] with m = [a,b,c]
--   output: [ [ i | i <- a ]
--           , [ i | i <- b ]
--           , [ i | i <- c ]
--           ][j]
--   namely: [ [ m[k]
--             | k : int(1..3)
--             ]
--           , [ i | i <- b ]
--           , [ i | i <- c ]
--           ][j]

rule_ModifierAroundIndexedMatrixLiteral :: Rule
rule_ModifierAroundIndexedMatrixLiteral = "modifier-around-indexed-matrix-literal" `namedRule` theRule where
    theRule p = do
        (mkM, p2)         <- match opModifier p
        (matrix, indices) <- match opMatrixIndexing p2
        case match opMatrixIndexing p of
            Nothing -> return ()
            Just{}  -> na "rule_ModifierAroundIndexedMatrixLiteral, no modifier"
        let
            fullyMatrixLiteral 0 _ = return True
            fullyMatrixLiteral n m =
                case match matrixLiteral m of
                    Nothing            -> return False
                    Just (_, _, elems) -> and <$> mapM (fullyMatrixLiteral (n-1)) elems
        True <- fullyMatrixLiteral (length indices) matrix
        return
            ( "Pushing a modifier inwards, through a matrix literal"
            , do
                matrix' <- onMatrixLiteral Nothing (return . mkM) matrix
                return $ make opMatrixIndexing matrix' indices
            )


rule_Comprehension_LiteralIndexed :: Rule
rule_Comprehension_LiteralIndexed = "matrix-comprehension-literal-indexed" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_LiteralIndexed"
        tyExpr             <- typeOf expr
        (matrix, indices)  <- match opMatrixIndexing expr
        (_, _index, elems) <- match matrixLiteral matrix
        return
            ( "Vertical rule for matrix-comprehension on matrix literal"
            , case indices of
                [] -> bug "rule_Comprehension_LiteralIndexed indices=[]"
                [index] ->
                    return $ make opFlatten $ AbstractLiteral $ AbsLitMatrix
                        (mkDomainIntB 1 (fromInt $ genericLength elems))
                        [ Comprehension body
                            $  gocBefore
                            ++ [ Generator (GenInExpr pat el)
                               , Condition [essence| &num = &index |]
                               ]
                            ++ gocAfter
                        | (num', el') <- zip [1..] elems
                        , let num = fromInt num'
                        -- let's not lose the type information for empty collections
                        , let el = if emptyCollectionX el'
                                    then case el' of Typed{} -> el'
                                                     _       -> Typed el' tyExpr
                                    else el'
                        ]
                (index:rest) ->
                    return $ make opFlatten $ AbstractLiteral $ AbsLitMatrix
                        (mkDomainIntB 1 (fromInt $ genericLength elems))
                        [ Comprehension body
                            $  gocBefore
                            ++ [ Generator (GenInExpr pat (make opMatrixIndexing el rest))
                               , Condition [essence| &num = &index |]
                               ]
                            ++ gocAfter
                        | (num', el') <- zip [1..] elems
                        , let num = fromInt num'
                        -- let's not lose the type information for empty collections
                        , let el = if emptyCollectionX el'
                                    then case el' of Typed{} -> el'
                                                     _       -> Typed el' tyExpr
                                    else el'
                        ]
            )
    theRule _ = na "rule_Comprehension_LiteralIndexed"


rule_Comprehension_ToSet_Matrix :: Rule
rule_Comprehension_ToSet_Matrix = "matrix-toSet-matrixInside" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_ToSet"
        matrix       <- match opToSet expr
        TypeMatrix{} <- typeOf matrix
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for comprehension over matrix-toSet, matrix inside"
            , do
                 (iPat, i) <- quantifiedVar
                 let val  = make opIndexing i 1
                 let over = make opHist matrix
                 return $ Comprehension (upd val body)
                         $  gocBefore
                         ++ [Generator (GenInExpr iPat over)]
                         ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_ToSet"


rule_Comprehension_ToSet_List :: Rule
rule_Comprehension_ToSet_List = "matrix-toSet-listInside" `namedRule` theRule where
    theRule p = do
        -- we cannot assume that the list is duplicate-free
        (False, Comprehension body gensOrConds) <- match opToSetWithFlag p
        bodyDomain <- domainOf body
        let auxDomain = DomainSet () (SetAttr SizeAttr_None) bodyDomain
        return
            ( "Vertical rule for comprehension over matrix-toSet, list inside"
            , do
                (auxName, aux) <- auxiliaryVar
                (iPat, i) <- quantifiedVar
                return $ WithLocals aux $
                    AuxiliaryVars
                        [ Declaration (FindOrGiven LocalFind auxName auxDomain)
                        , SuchThat
                            -- forAll i in list . i in aux
                            [ make opAnd $ Comprehension
                                [essence| &body in &aux |]
                                gensOrConds
                            -- forAll i in aux . exists j in list . i = j
                            , make opAnd $ Comprehension
                                (make opOr (Comprehension [essence| &i = &body |] gensOrConds))
                                [Generator (GenInExpr iPat aux)]
                            ]
                        ]
            )


rule_Comprehension_ToSet_List_DuplicateFree :: Rule
rule_Comprehension_ToSet_List_DuplicateFree = "matrix-toSet-listInside-nodups" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToMSet] expr)
            _ -> na "rule_Comprehension_ToSet"
        -- we *can* assume that the list is duplicate-free!
        (True, list) <- match opToSetWithFlag expr
        TypeList{} <- typeOf list
        return
            ( "Vertical rule for comprehension over matrix-toSet, list inside, assumed no duplicates"
            , return $ Comprehension body
                     $  gocBefore
                     ++ [Generator (GenInExpr pat list)]
                     ++ gocAfter
            )
    theRule _ = na "rule_Comprehension_ToSet_List_DuplicateFree"


-- [ i | ... , i <- [ j | ... j ... ], ... i ... ]
-- [ j | ... , ... j ..., ... j ... ]
rule_Comprehension_Nested :: Rule
rule_Comprehension_Nested = "matrix-comprehension-nested" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \case
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToMSet] expr)
            _ -> na "rule_Comprehension_Nested"
        let
            extract (isAlias -> Just x) = extract x
            extract (Comprehension innerBody innerGocs) = return (innerBody, innerGocs)
            extract _ = na "rule_Comprehension_Nested"
        (innerBody, innerGocs) <- extract expr
        let upd val old = lambdaToFunction pat old val
        let
            -- update the quantified variable names inside innerBody&innerGocs_ here,
            -- because they may be shadowed.
            updateQuantified innerBody_ innerGocs_ = do
                let olds = concatMap collectOldQuantifiers innerGocs_
                if null olds
                    then return (innerBody_, innerGocs_)
                    else do
                        oldnews <- forM olds $ \ old -> do
                            (Single new, _) <- quantifiedVar
                            return (old, new)
                        let
                            f :: Name -> Name
                            f nm = fromMaybe nm (lookup nm oldnews)
                        return (transformBi f (innerBody_, innerGocs_))

            collectOldQuantifiers = \case
                Generator (GenDomainNoRepr  pt _) -> universeBi pt
                Generator (GenDomainHasRepr nm _) -> [nm]
                Generator (GenInExpr        pt _) -> universeBi pt
                Condition _                       -> []
                ComprehensionLetting pt _         -> universeBi pt

        (innerBody', innerGocs') <- updateQuantified innerBody innerGocs
        return
            ( "Nested matrix comprehension"
            , return $ Comprehension (upd innerBody' body)
                         $  gocBefore
                         ++ innerGocs'
                         ++ transformBi (upd innerBody') gocAfter
            )
    theRule _ = na "rule_Comprehension_Nested"


rule_Comprehension_Hist :: Rule
rule_Comprehension_Hist = "matrix-hist" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Hist"
        matrix               <- match opHist expr
        TypeMatrix{}         <- typeOf matrix
        index:_              <- indexDomainsOf matrix
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for comprehension over matrix-hist"
            , do
                 (iPat, i) <- quantifiedVar
                 (jPat, j) <- quantifiedVar
                 let value = [essence| &matrix[&i] |]
                     -- if this is the left-most occurrence of value
                     -- count all
                     -- otherwise, 0
                     count = [essence|
                         sum([ 1                            $ number of occurrences of this value in the matrix
                             | &jPat : &index
                             , &matrix[&i] = &matrix[&j]
                             ])
                     |]
                     val   = AbstractLiteral $ AbsLitTuple [value, count]
                     appearsBefore = [essence|
                         or([ &matrix[&j] = &matrix[&i]
                            | &jPat : &index
                            , &j < &i
                            ])
                    |]
                 return $ Comprehension (upd val body)
                         $  gocBefore
                         ++ [ Generator (GenDomainNoRepr iPat index)
                            , Condition [essence| ! &appearsBefore |]
                            ]
                         ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_Hist"


rule_Matrix_Eq :: Rule
rule_Matrix_Eq = "matrix-eq" `namedRule` theRule where
    theRule p = do
        (x,y)                 <- match opEq p
        TypeMatrix{}          <- typeOf x        -- TODO: check if x and y have the same arity
        TypeMatrix{}          <- typeOf y
        indexX:_              <- indexDomainsOf x
        indexY:_              <- indexDomainsOf y
        return
            ( "Horizontal rule for matrix ="
            , do
                 (iPat, i) <- quantifiedVar
                 (jPat, j) <- quantifiedVar
                 -- avoid generating the index equality constraint, if the indices are literally the same
                 if indexX == indexY
                     then
                         return [essence|
                             (forAll &iPat : &indexX . &x[&i] = &y[&i])
                                        |]
                    else
                         return [essence|
                             (forAll &iPat : &indexX . &x[&i] = &y[&i])
                             /\
                             (forAll &iPat : &indexX . exists &jPat : &indexY . &i = &j)
                             /\
                             (forAll &iPat : &indexY . exists &jPat : &indexX . &i = &j)
                                        |]
            )


rule_Matrix_Neq :: Rule
rule_Matrix_Neq = "matrix-neq" `namedRule` theRule where
    theRule p = do
        (x,y)                 <- match opNeq p
        TypeMatrix{}          <- typeOf x        -- TODO: check if x and y have the same arity
        TypeMatrix{}          <- typeOf y
        indexX:_              <- indexDomainsOf x
        indexY:_              <- indexDomainsOf y
        return
            ( "Horizontal rule for matrix !="
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence|
                     (exists &iPat : &indexX . &x[&i] != &y[&i])
                     \/
                     (exists &iPat : &indexY . &x[&i] != &y[&i])
                                |]
            )


rule_Matrix_Lt_Primitive :: Rule
rule_Matrix_Lt_Primitive = "matrix-Lt-primitive" `namedRule` theRule where
    theRule p = do
        (x,y)           <- case (match opLt p, match opTildeLt p) of
                                (Just a, _) -> return a
                                (_, Just a) -> return a
                                _ -> na "rule_Matrix_Lt_Primitive"
        tx <- typeOf x        -- TODO: check if x and y have the same arity
        ty <- typeOf y
        unless (matrixNumDims tx > 0 && isPrimitiveType tx) $ failDoc ("not a primitive type:" <+> pretty tx)
        unless (matrixNumDims ty > 0 && isPrimitiveType ty) $ failDoc ("not a primitive type:" <+> pretty ty)
        let x' = flattenIfNeeded (matrixNumDims tx) x
        let y' = flattenIfNeeded (matrixNumDims ty) y
        return
            ( "Horizontal rule for matrix <"
            , return [essence| &x' .< &y' |]
            )


rule_Matrix_Leq_Primitive :: Rule
rule_Matrix_Leq_Primitive = "matrix-Leq-primitive" `namedRule` theRule where
    theRule p = do
        (x,y)           <- case (match opLeq p, match opTildeLeq p) of
                                (Just a, _) -> return a
                                (_, Just a) -> return a
                                _ -> na "rule_Matrix_Leq_Primitive"
        tx <- typeOf x        -- TODO: check if x and y have the same arity
        ty <- typeOf y
        unless (matrixNumDims tx > 0 && isPrimitiveType tx) $ failDoc ("not a primitive type:" <+> pretty tx)
        unless (matrixNumDims ty > 0 && isPrimitiveType ty) $ failDoc ("not a primitive type:" <+> pretty ty)
        let x' = flattenIfNeeded (matrixNumDims tx) x
        let y' = flattenIfNeeded (matrixNumDims ty) y
        return
            ( "Horizontal rule for matrix <="
            , return [essence| &x' .<= &y' |]
            )


rule_Matrix_Lt_Decompose :: Rule
rule_Matrix_Lt_Decompose = "matrix-Lt-tuple" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opLt p
        tx@TypeMatrix{} <- typeOf x     -- TODO: check matrix index & tuple arity
        ty@TypeMatrix{} <- typeOf y
        when (isPrimitiveType tx) $ failDoc ("this is a primitive type:" <+> pretty tx)
        when (isPrimitiveType ty) $ failDoc ("this is a primitive type:" <+> pretty ty)
        xs              <- downX1 x
        ys              <- downX1 y
        return
            ( "Horizontal rule for matrix <, decomposing"
            , return $ decomposeLexLt p xs ys
            )


rule_Matrix_Leq_Decompose :: Rule
rule_Matrix_Leq_Decompose = "matrix-Leq-tuple" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opLeq p
        tx@TypeMatrix{} <- typeOf x     -- TODO: check matrix index & tuple arity
        ty@TypeMatrix{} <- typeOf y
        when (isPrimitiveType tx) $ failDoc ("this is a primitive type:" <+> pretty tx)
        when (isPrimitiveType ty) $ failDoc ("this is a primitive type:" <+> pretty ty)
        xs              <- downX1 x
        ys              <- downX1 y
        return
            ( "Horizontal rule for matrix <=, decomposing"
            , return $ decomposeLexLeq p xs ys
            )

rule_Comprehension_SingletonDomain :: Rule
rule_Comprehension_SingletonDomain = "matrix-comprehension-singleton-domain" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, singleVal), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenDomainHasRepr patName (singletonDomainInt -> Just a)) -> return (Single patName, a)
            _ -> na "rule_Comprehension_SingletonDomain"
        let upd val old = lambdaToFunction pat old val
        return
            ( "Removing matrix-comprehension of a singleton int domain"
            , return $
                if null gocBefore && null gocAfter
                    then AbstractLiteral $ AbsLitMatrix (mkDomainIntB 1 1) [upd singleVal body]
                    else Comprehension (upd singleVal body)
                         $  gocBefore
                         ++ transformBi (upd singleVal) gocAfter
            )
    theRule _ = na "rule_Comprehension_SingletonDomain"


rule_Comprehension_Singleton :: Rule
rule_Comprehension_Singleton = "matrix-comprehension-singleton" `namedRule` theRule where
    theRule p = do
        (_, _, _mkQuan, AbstractLiteral (AbsLitMatrix _ [singleVal])) <- match opReducer p
        return
            ( "Removing quantifier of a single item"
            , return singleVal
            )


rule_Concatenate_Singleton :: Rule
rule_Concatenate_Singleton = "matrix-concatenate-singleton" `namedRule` theRule where
    theRule p = do
        AbstractLiteral (AbsLitMatrix _ [singleVal]) <- match opConcatenate p
        return
            ( "Removing concatenate of a single item"
            , return singleVal
            )


rule_MatrixIndexing :: Rule
rule_MatrixIndexing = "matrix-indexing" `namedRule` theRule where
    theRule p = do
        (matrix, indexer)              <- match opIndexing p
        (_, DomainInt _ ranges, elems) <- match matrixLiteral matrix
        indexInts                      <- rangesInts ranges
        indexerInt                     <- intOut "rule_MatrixIndexing" indexer
        if length indexInts == length elems
            then
                case lookup indexerInt (zip indexInts elems) of
                    Nothing -> na "rule_MatrixIndexing"
                    Just v  ->
                        return
                            ( "Matrix indexing"
                            , return v
                            )
            else na "rule_MatrixIndexing"


rule_IndexingIdentical :: Rule
rule_IndexingIdentical = "matrix-indexing-identical" `namedRule` theRule where
    theRule p = do
        (matrix, indexer)                     <- match opIndexing p
        (_, indexDomain, firstElem:restElems) <- match matrixLiteral matrix
        indexerDomain <- domainOf indexer
        if indexDomain == forgetRepr indexerDomain && all (firstElem==) restElems
            then return
                    ( "rule_IndexingIdentical"
                    , return firstElem
                    )
            else na "rule_IndexingIdentical"


rule_ExpandSlices :: Rule
rule_ExpandSlices = "matrix-expand-slices" `namedRule` theRule where
    theRule p = do
        (m, is) <- match opMatrixIndexingSlicing p
        indexDoms <- indexDomainsOf m
        unless (length is == length indexDoms) $ na "rule_ExpandSlices"
        (is', gocs) <- unzip <$> sequence
            [ case index of
                Left i -> -- indexing
                    return
                        ( Left i
                        , []
                        )
                Right (mLowerBound, mUpperBound) -> do -- slicing
                    (jPat, j) <- quantifiedVar
                    return
                        ( Left j
                        , concat [ [ Generator (GenDomainNoRepr jPat indexDom) ]
                                 , [ Condition [essence| &j >= &lb |] | Just lb <- [mLowerBound] ]
                                 , [ Condition [essence| &j <= &ub |] | Just ub <- [mUpperBound] ]
                                 ]
                        )
            | (index, indexDom) <- zip is indexDoms
            ]
        when (null gocs) $ na "rule_ExpandSlices: this was all indexing and no slicing"
        return ( "Expanding a matrix slice"
               , return $ Comprehension (make opMatrixIndexingSlicing m is') (concat gocs)
               )


-- freq(matrix,arg) ~~> sum([ toInt(arg = i) | i in matrix ])
rule_Freq :: Rule
rule_Freq = "matrix-freq" `namedRule` theRule where
    theRule p = do
        (m, arg) <- match opFreq p
        TypeMatrix{}  <- typeOf m
        [indexDom] <- indexDomainsOf m
        return
            ( "Horizontal rule for matrix-freq."
            , do
                (iPat, i) <- quantifiedVar
                let mis = (make opMatrixIndexing m [i])
                return $ make opSum $ Comprehension [essence| toInt(&mis = &arg) |]
                            [Generator (GenDomainNoRepr iPat indexDom)]
            )

