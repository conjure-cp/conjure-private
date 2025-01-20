{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations
    ( downD, downC, up
    , downD1, downC1, up1
    , downToX1
    , reprOptions, getStructurals
    , symmetryOrdering
    , reprsStandardOrderNoLevels, reprsStandardOrder, reprsSparseOrder
    , downX1
    , downX
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Process.Enumerate
import Conjure.Compute.DomainOf
import Conjure.Representations.Combined


-- | Refine (down) an expression (X), one level (1).
downX1 ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m [Expression]
downX1 (Constant x) = onConstant x
downX1 (AbstractLiteral x) = onAbstractLiteral x
downX1 (Reference x (Just refTo)) = onReference x refTo
downX1 (Op x) = onOp x
downX1 (Comprehension body stmts) = do
    xs <- downX1 body
    return [Comprehension x stmts | x <- xs]
downX1 x@WithLocals{} = failDoc ("downX1:" <++> pretty (show x))
downX1 x = bug ("downX1:" <++> pretty (show x))


-- | Refine (down) an expression (X), all the way.
downX ::
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m [Expression]
downX x = do
    res <- runMaybeT $ downX1 x
    case res of
        Nothing -> return [x]
        Just [] -> return [x]
        Just xs -> concatMapM downX xs


onConstant ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Constant -> m [Expression]
onConstant (ConstantAbstract (AbsLitTuple xs)) = return (map Constant xs)
onConstant (ConstantAbstract (AbsLitRecord xs)) = return (map (Constant . snd) xs)
onConstant (ConstantAbstract (AbsLitVariant (Just t) n x))
    | Just i <- elemIndex n (map fst t)
    , let iExpr = fromInt (fromIntegral (i+1))
    = return $ iExpr : [ if n == n'
                            then Constant x
                            else ExpressionMetaVar "zeroVal for variant"
                       | (n',_) <- t ]
onConstant (ConstantAbstract (AbsLitMatrix index xs)) = do
    yss <- mapM (downX1 . Constant) xs
    let indexX = fmap Constant index
    return [ AbstractLiteral (AbsLitMatrix indexX ys) | ys <- transpose yss ]
onConstant (TypedConstant c _) = onConstant c
onConstant x = bug ("downX1.onConstant:" <++> pretty (show x))

onAbstractLiteral ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    AbstractLiteral Expression -> m [Expression]
onAbstractLiteral (AbsLitTuple xs) = return xs
onAbstractLiteral (AbsLitRecord xs) = return (map snd xs)
onAbstractLiteral (AbsLitVariant (Just t) n x)
    | Just i <- elemIndex n (map fst t)
    , let iExpr = fromInt (fromIntegral (i+1))
    = return $ iExpr : [ if n == n'
                            then x
                            else ExpressionMetaVar "zeroVal for variant"
                       | (n',_) <- t ]
onAbstractLiteral (AbsLitMatrix index xs) = do
    yss <- mapM downX1 xs
    return [ AbstractLiteral (AbsLitMatrix index ys) | ys <- transpose yss ]
onAbstractLiteral x = bug ("downX1.onAbstractLiteral:" <++> pretty (show x))

onReference ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Name -> ReferenceTo -> m [Expression]
onReference nm refTo =
    case refTo of
        Alias x                   -> downX1 x
        InComprehension{}         -> failDoc ("downX1.onReference.InComprehension:" <++> pretty (show nm))
        DeclNoRepr{}              -> failDoc ("downX1.onReference.DeclNoRepr:"      <++> pretty (show nm))
        DeclHasRepr forg _ domain -> downToX1 forg nm domain
        RecordField{}             -> failDoc ("downX1.onReference.RecordField:"     <++> pretty (show nm))
        VariantField{}            -> failDoc ("downX1.onReference.VariantField:"    <++> pretty (show nm))

onOp ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Op Expression -> m [Expression]
onOp p@(MkOpIndexing (OpIndexing m i)) = do
    ty <- typeOf m
    case ty of
        TypeMatrix{} -> return ()
        TypeList{}   -> return ()
        _ -> failDoc $ "[onOp, not a TypeMatrix or TypeList]" <+> vcat [pretty ty, pretty p]
    xs <- downX1 m
    let iIndexed x = Op (MkOpIndexing (OpIndexing x i))
    return (map iIndexed xs)
onOp (MkOpImage (OpImage (match functionLiteral -> Just (_, xs)) a)) | length xs > 0 = do
    vals <- forM xs $ \ (_, value) -> do
        ys <- downX1 value
        return ys
    let keys = map fst xs
    let outs = map (zip keys) (transpose vals)
    return [ Op $ MkOpImage $ OpImage (AbstractLiteral (AbsLitFunction out)) a
           | out <- outs ]
onOp op = failDoc ("downX1.onOp:" <++> pretty op)



symmetryOrdering ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m Expression
symmetryOrdering inp' = do
   let constBool (ConstantBool True) = ConstantInt TagInt 1
       constBool (ConstantBool False) = ConstantInt TagInt 0
       constBool x = x
       inp = transformBi constBool inp'
   ta <- typeOf inp
   case ta of
     TypeBool -> return [essence| [-toInt(&inp)] |]
     TypeInt{} -> return [essence| [&inp] |]
     TypeList TypeInt{} -> return inp
     TypeMatrix TypeInt{} TypeInt{} -> return inp
     _ ->
       case inp of
            -- Constant x -> so_onConstant x
            -- AbstractLiteral _ -> return inp

            Constant ConstantBool{} -> return [essence| -toInt(&inp) |]

            Constant (ConstantAbstract x) -> do
                case x of
                    AbsLitTuple xs -> do
                        soVals <- mapM symmetryOrdering (Constant <$> xs)
                        return $ fromList soVals
                    AbsLitMatrix _ xs -> do
                        soVals <- mapM symmetryOrdering (Constant <$> xs)
                        return $ fromList soVals
                    _ -> na ("symmetryOrdering: AbstractLiteral:" <++> pretty (show inp) <++> pretty inp)

            AbstractLiteral x -> do
                case x of
                    AbsLitTuple xs -> do
                        soVals <- mapM symmetryOrdering xs
                        return $ AbstractLiteral $ AbsLitTuple soVals
                    AbsLitMatrix d xs -> do
                        soVals <- mapM symmetryOrdering xs
                        return $ AbstractLiteral $ AbsLitMatrix d soVals
                    _ -> na ("symmetryOrdering: AbstractLiteral:" <++> pretty (show inp) <++> pretty inp)

            Reference _ (Just refTo) -> do
                case refTo of
                    Alias x                        -> symmetryOrdering x
                    InComprehension{}              -> na ("symmetryOrdering.InComprehension:" <++> pretty (show inp))
                    DeclNoRepr{}                   -> na ("symmetryOrdering.DeclNoRepr:"      <++> pretty (show inp))
                    DeclHasRepr _forg _name domain -> symmetryOrderingDispatch downX1 inp domain
                    RecordField{}                  -> na ("symmetryOrdering.RecordField:"     <++> pretty (show inp))
                    VariantField{}                 -> na ("symmetryOrdering.VariantField:"    <++> pretty (show inp))

            Op op -> case op of
                MkOpIndexing (OpIndexing m _) -> do
                    ty <- typeOf m
                    case ty of
                        TypeMatrix{} -> return ()
                        TypeList{}   -> return ()
                        _ -> na $ "[symmetryOrdering.onOp, not a TypeMatrix or TypeList]" <+> vcat [pretty ty, pretty op]
                    mDom <- domainOfR m
                    case mDom of
                        DomainMatrix _ domainInner -> symmetryOrderingDispatch downX1 inp domainInner
                        _ -> na ("symmetryOrdering, not DomainMatrix:" <++> pretty (show op))
                MkOpImage (OpImage p x) -> do
                    so <- symmetryOrdering x
                    return [essence| image(&p, &so) |]
                _ -> na ("symmetryOrdering, no OpIndexing:" <++> pretty (show op))

            Comprehension body stmts -> do
                xs <- symmetryOrdering body
                return $ make opFlatten $ Comprehension xs stmts

            _ -> na ("symmetryOrdering:" <++> pretty (show inp) <++> pretty inp)
