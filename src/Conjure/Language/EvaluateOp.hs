module Conjure.Language.EvaluateOp ( EvaluateOp(..) ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Util.Permutation
import Conjure.Language
import Conjure.Process.Enumerate ( EnumerateDomain, enumerateInConstant )
import Conjure.Compute.DomainOf ( domainOf )
import Conjure.Language.DomainSizeOf ( domainSizeOf )
import Conjure.Process.AttributeAsConstraints ( mkAttributeToConstraint )
import {-# SOURCE #-} Conjure.Language.Instantiate ( instantiateExpression )    
import {-# SOURCE #-} Conjure.Process.ValidateConstantForDomain ( validateConstantForDomain )

import qualified Data.Semigroup as SG

-- | Assume: the input is already normalised.
--   Make sure the output is normalised.
class EvaluateOp op where
    evaluateOp :: 
        MonadFailDoc m =>
        NameGen m =>
        EnumerateDomain m =>
        (?typeCheckerMode :: TypeCheckerMode) =>
        op Constant -> m Constant

instance EvaluateOp OpActive where
    evaluateOp (OpActive (viewConstantVariant -> Just (_, n1, _)) n2) = return $ fromBool $ n1 == n2
    evaluateOp op = na $ "evaluateOp{OpActive}:" <++> pretty (show op)

instance EvaluateOp OpAllDiff where
    evaluateOp (OpAllDiff (viewConstantMatrix -> Just (_, vals))) =
        return $ ConstantBool $ length vals == length (sortNub vals)
    evaluateOp op = na $ "evaluateOp{OpAllDiff}:" <++> pretty (show op)

instance EvaluateOp OpAllDiffExcept where
    evaluateOp (OpAllDiffExcept (viewConstantMatrix -> Just (_, vals)) i@(viewConstantInt -> Just n)) = do
        TypeInt t <- typeOf i
        let vals' = filter (ConstantInt t n/=) vals
        return $ ConstantBool $ length vals' == length (sortNub vals')
    evaluateOp op = na $ "evaluateOp{OpAllDiffExcept}:" <++> pretty (show op)

instance EvaluateOp OpAnd where
    evaluateOp (OpAnd x) = ConstantBool . and <$> boolsOut x

instance EvaluateOp OpApart where
    evaluateOp (OpApart _ ConstantUndefined{}) = return (fromBool False)
    evaluateOp (OpApart (viewConstantSet -> Just ys) (viewConstantPartition -> Just xss)) =
        return $ ConstantBool $ and
                    [ -- the items in `ys` do not appear together in the partition
                      not $ or [ and [ y `elem` xs | y <- ys ]
                               | xs <- xss
                               ]
                      -- the items in `ys` appear somewhere in the partition
                    , and [ y `elem` concat xss | y <- ys ]
                    ]
    evaluateOp op = na $ "evaluateOp{OpApart}:" <++> pretty (show op)

instance EvaluateOp OpAttributeAsConstraint where
    evaluateOp (OpAttributeAsConstraint x attrName attrVal) = do
        dom <- domainOf x
        constraint <- mkAttributeToConstraint dom attrName (fmap Constant attrVal) (Constant x)
        evaluated <- instantiateExpression [] constraint
        return evaluated

instance EvaluateOp OpCatchUndef where
    evaluateOp (OpCatchUndef ConstantUndefined{} d) = return d
    evaluateOp (OpCatchUndef x _) = return x

instance EvaluateOp OpDefined where
    evaluateOp p | any isUndef (childrenBi p) = do
        ty <- typeOf p
        return $ mkUndef ty $ "Has undefined children:" <+> pretty p
    evaluateOp (OpDefined (viewConstantFunction -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub $ map fst xs
    evaluateOp op = na $ "evaluateOp{OpDefined}:" <++> pretty (show op)

instance EvaluateOp OpDiv where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpDiv x y)
        | y /= 0    = ConstantInt TagInt <$> (div <$> intOut "div x" x <*> intOut "div y" y)
        | otherwise = return $ mkUndef (TypeInt TagInt) $ "division by zero:" <+> pretty p

instance EvaluateOp OpDontCare where
    evaluateOp op = na $ "evaluateOp{OpDontcare}:" <++> pretty (show op)

instance EvaluateOp OpDotLeq where
    evaluateOp (OpDotLeq x y) = return $ ConstantBool $ x <= y

instance EvaluateOp OpDotLt where
    evaluateOp (OpDotLt x y) = return $ ConstantBool $ x < y

instance EvaluateOp OpEq where
    evaluateOp (OpEq ConstantUndefined{} _) = return $ fromBool False
    evaluateOp (OpEq _ ConstantUndefined{}) = return $ fromBool False
    evaluateOp (OpEq (TypedConstant x _) y) = evaluateOp (OpEq x y)
    evaluateOp (OpEq x (TypedConstant y _)) = evaluateOp (OpEq x y)
    evaluateOp (OpEq x y) = return $ ConstantBool $ x == y

instance EvaluateOp OpCompose where
    evaluateOp (OpCompose (viewConstantPermutation -> Just gss)
                             (viewConstantPermutation -> Just hss)) = do
       case (fromCycles gss, fromCycles hss) of
         (Right g, Right h) ->
           return $ ConstantAbstract $ AbsLitPermutation $ toCycles $ g SG.<> h
         (Left e, _) -> failDoc $ "evaluateOp{OpCompose}" <++> pretty (show e)
         (_, Left e) -> failDoc $ "evaluateOp{OpCompose}" <++> pretty (show e)
    evaluateOp op = na $ "evaluateOp{OpCompose}:" <++> pretty (show op)


instance EvaluateOp OpFactorial where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpFactorial x) = ConstantInt TagInt . product . enumFromTo 1 <$> intOut "factorial" x

instance EvaluateOp OpFlatten where
    evaluateOp (OpFlatten Nothing m) = do
        let flat (viewConstantMatrix -> Just (_, xs)) = concatMap flat xs
            flat c = [c]
        let flattened = flat m
        return (ConstantAbstract $ AbsLitMatrix
                    (DomainInt TagInt [RangeBounded 1 (fromInt (genericLength flattened))])
                    flattened)
    evaluateOp (OpFlatten (Just n) m) = do
        let flat lvl c | lvl < 0 = return [c]
            flat lvl (viewConstantMatrix -> Just (_, xs)) = concatMapM (flat (lvl-1)) xs
            flat _ _ = failDoc $ "Cannot flatten" <+> pretty n <+> "levels."
        flattened <- flat n m
        return (ConstantAbstract $ AbsLitMatrix
                    (DomainInt TagInt [RangeBounded 1 (fromInt (genericLength flattened))])
                    flattened)

instance EvaluateOp OpFreq where
    evaluateOp (OpFreq (viewConstantMSet -> Just cs) c) = return $ (ConstantInt TagInt) $ sum [ 1 | i <- cs, c == i ]
    evaluateOp (OpFreq (viewConstantMatrix -> Just (_, cs)) c) = return $ (ConstantInt TagInt) $ sum [ 1 | i <- cs, c == i ]
    evaluateOp op = na $ "evaluateOp{OpFreq}:" <++> pretty (show op)

instance EvaluateOp OpGeq where
    evaluateOp (OpGeq x y) = return $ ConstantBool $ x >= y

instance EvaluateOp OpGt where
    evaluateOp (OpGt x y) = return $ ConstantBool $ x > y

instance EvaluateOp OpHist where
    evaluateOp (OpHist (viewConstantMSet -> Just cs)) = return $ ConstantAbstract $ AbsLitMatrix
        (DomainInt TagInt [RangeBounded 1 (fromInt $ genericLength $ histogram cs)])
        [ ConstantAbstract $ AbsLitTuple [e, ConstantInt TagInt n] | (e, n) <- histogram cs ]
    evaluateOp (OpHist (viewConstantMatrix -> Just (_, cs))) = return $ ConstantAbstract $ AbsLitMatrix
        (DomainInt TagInt [RangeBounded 1 (fromInt $ genericLength $ histogram cs)])
        [ ConstantAbstract $ AbsLitTuple [e, ConstantInt TagInt n] | (e, n) <- histogram cs ]
    evaluateOp op = na $ "evaluateOp{OpHist}:" <++> pretty (show op)

instance EvaluateOp OpIff where
    evaluateOp (OpIff (ConstantBool x) (ConstantBool y)) = return $ ConstantBool $ x == y
    evaluateOp _ = na "evaluateOp{OpIff}"

instance EvaluateOp OpImage where
    evaluateOp (OpImage f@(viewConstantFunction -> Just xs) a) =
        case [ y | (x,y) <- xs, a == x ] of
            [y] -> return y
            []  -> do
                TypeFunction _ tyTo <- typeOf f
                return $ mkUndef tyTo $ vcat
                    [ "Function is not defined at this point:" <+> pretty a
                    , "Function value:" <+> pretty f
                    ]
            _   -> do
                TypeFunction _ tyTo <- typeOf f
                return $ mkUndef tyTo $ vcat
                    [ "Function is multiply defined at this point:" <+> pretty a
                    , "Function value:" <+> pretty f
                    ]
    evaluateOp (OpImage f@(viewConstantSequence -> Just xs) a) =
        case [ y | (x,y) <- zip allNats xs, a == fromInt x ] of
            [y] -> return y
            []  -> do
                TypeSequence tyTo <- typeOf f
                return $ mkUndef tyTo $ vcat
                    [ "Sequence is not defined at this point:" <+> pretty a
                    , "Sequence value:" <+> pretty f
                    ]
            _   -> do
                TypeSequence tyTo <- typeOf f
                return $ mkUndef tyTo $ vcat
                    [ "Sequence is multiply defined at this point:" <+> pretty a
                    , "Sequence value:" <+> pretty f
                    ]
    evaluateOp (OpImage f@(viewConstantPermutation -> Just _) a) = do
        permVals <- enumerateInConstant f
        case [ y | ConstantAbstract (AbsLitTuple [x,y]) <- permVals, a == x ] of
            [y] -> return y
            []  -> return a -- permutations map things to themselves by default
            _   -> do
                TypePermutation tyTo <- typeOf f
                return $ mkUndef tyTo $ vcat
                    [ "Permutation is multiply defined at this point:" <+> pretty a
                    , "Permutation value:" <+> pretty f
                    ]
    evaluateOp op = na $ "evaluateOp{OpImage}:" <++> pretty (show op)

instance EvaluateOp OpImageSet where
    evaluateOp (OpImageSet f@(viewConstantFunction -> Just xs) a) = do
        TypeFunction _ tyTo <- typeOf f
        case [ y | (x,y) <- xs, a == x ] of
            [y] -> return $ ConstantAbstract $ AbsLitSet [y]
            _   -> return $ TypedConstant (ConstantAbstract $ AbsLitSet []) (TypeSet tyTo)
    evaluateOp (OpImageSet f@(viewConstantSequence -> Just xs) a) = do
        TypeSequence tyTo <- typeOf f
        case [ y | (x,y) <- zip allNats xs, a == fromInt x ] of
            [y] -> return $ ConstantAbstract $ AbsLitSet [y]
            _   -> return $ TypedConstant (ConstantAbstract $ AbsLitSet []) (TypeSet tyTo)
    evaluateOp op = na $ "evaluateOp{OpImageSet}:" <++> pretty (show op)

instance EvaluateOp OpImply where
    evaluateOp (OpImply x y) = ConstantBool <$> ((<=) <$> boolOut x <*> boolOut y)

instance EvaluateOp OpIn where
    evaluateOp (OpIn c (viewConstantSet      -> Just cs)) = return $ ConstantBool $ elem c cs
    evaluateOp (OpIn c (viewConstantMSet     -> Just cs)) = return $ ConstantBool $ elem c cs
    evaluateOp (OpIn c (viewConstantFunction -> Just cs)) =
        return $ ConstantBool $ elem c $ map (\ (i,j) -> ConstantAbstract $ AbsLitTuple [i,j] ) cs
    evaluateOp op@(OpIn (viewConstantTuple -> Just [a,b]) (viewConstantPermutation -> Just xss)) =
        case fromCycles xss of
            Right p -> do
                let f = toFunction p
                return $ ConstantBool $ f a == b
            _ -> na $ "evaluateOp{OpIn}:" <++> pretty (show op)
    evaluateOp (OpIn c (viewConstantRelation -> Just cs)) =
        return $ ConstantBool $ elem c $ map (ConstantAbstract . AbsLitTuple) cs
    evaluateOp op = na $ "evaluateOp{OpIn}:" <++> pretty (show op)

instance EvaluateOp OpIndexing where
    evaluateOp p@(OpIndexing m i) | isUndef i = do
        ty   <- typeOf m
        tyTo <- case ty of TypeMatrix _ tyTo -> return tyTo
                           TypeList tyTo     -> return tyTo
                           _ -> failDoc "evaluateOp{OpIndexing}"
        return $ mkUndef tyTo $ "Has undefined children (index):" <+> pretty p
    evaluateOp (OpIndexing m@(viewConstantMatrix -> Just (DomainInt _ index, vals)) (ConstantInt _ x)) = do
            ty   <- typeOf m
            tyTo <- case ty of TypeMatrix _ tyTo -> return tyTo
                               TypeList tyTo     -> return tyTo
                               _ -> bug "evaluateOp{OpIndexing}"
            indexVals <- valuesInIntDomain index
            case [ v | (i, v) <- zip indexVals vals, i == x ] of
                [v] -> return v
                []  -> return $ mkUndef tyTo $ vcat
                        [ "Matrix is not defined at this point:" <+> pretty x
                        , "Matrix value:" <+> pretty m
                        ]
                _   -> return $ mkUndef tyTo $ vcat
                        [ "Matrix is multiply defined at this point:" <+> pretty x
                        , "Matrix value:" <+> pretty m
                        ]
    evaluateOp (OpIndexing (viewConstantTuple -> Just vals) (ConstantInt _ x)) =
        return (at vals (fromInteger (x-1)))
    evaluateOp rec@(OpIndexing (viewConstantRecord -> Just vals) (ConstantField name _)) =
        case lookup name vals of
            Nothing -> failDoc $ vcat
                    [ "Record doesn't have a member with this name:" <+> pretty name
                    , "Record:" <+> pretty rec
                    ]
            Just val -> return val
    evaluateOp var@(OpIndexing (viewConstantVariant -> Just (_, name', x)) (ConstantField name ty)) =
        if name == name'
            then return x
            else return $ mkUndef ty $ vcat
                    [ "Variant isn't set to a member with this name:" <+> pretty name
                    , "Variant:" <+> pretty var
                    ]
    evaluateOp op = na $ "evaluateOp{OpIndexing}:" <++> pretty (show op)

instance EvaluateOp OpIntersect where
    evaluateOp p | any isUndef (childrenBi p) = do
        ty <- typeOf p
        return $ mkUndef ty $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpIntersect (viewConstantSet -> Just as) (viewConstantSet -> Just bs)) = do
        ty <- typeOf p
        let outs = sortNub [ i | i <- as, i `elem` bs]
        return $ TypedConstant (ConstantAbstract $ AbsLitSet outs) ty
    evaluateOp p@(OpIntersect (viewConstantMSet -> Just as) (viewConstantMSet -> Just bs)) = do
        ty <- typeOf p
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
            outs =
                [ replicate (fromInteger (min countA countB)) e
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
        return $ TypedConstant (ConstantAbstract $ AbsLitMSet $ concat outs) ty
    evaluateOp p@(OpIntersect (viewConstantFunction -> Just as) (viewConstantFunction -> Just bs)) = do
        ty <- typeOf p
        let outs = sortNub [ i | i <- as, i `elem` bs]
        return $ TypedConstant (ConstantAbstract $ AbsLitFunction outs) ty
    evaluateOp p@(OpIntersect (viewConstantRelation -> Just as) (viewConstantRelation -> Just bs)) = do
        ty <- typeOf p
        let outs = sortNub [ i | i <- as, i `elem` bs]
        return $ TypedConstant (ConstantAbstract $ AbsLitRelation outs) ty
    evaluateOp op = na $ "evaluateOp{OpIntersect}:" <++> pretty (show op)

instance EvaluateOp OpInverse where
    evaluateOp (OpInverse (viewConstantFunction -> Just xs) (viewConstantFunction -> Just ys)) =
        return $ ConstantBool $ and $ concat [ [ (j,i) `elem` ys | (i,j) <- xs ]
                                             , [ (j,i) `elem` xs | (i,j) <- ys ]
                                             ]
    evaluateOp op@(OpInverse (viewConstantPermutation -> Just xss) (viewConstantPermutation -> Just yss)) =
        case (fromCycles xss, fromCycles yss) of
            (Right px, Right py) -> return $ ConstantBool $ px == inverse py
            _ -> na $ "evaluateOp{OpInverse}:" <++> pretty (show op)
    evaluateOp op = na $ "evaluateOp{OpInverse}:" <++> pretty (show op)

instance EvaluateOp OpLeq where
    evaluateOp (OpLeq x y) = return $ ConstantBool $ x <= y

instance EvaluateOp OpLexLeq where
    evaluateOp (OpLexLeq (viewConstantMatrix -> Just (_, xs)) (viewConstantMatrix -> Just (_, ys))) =
        return $ ConstantBool $ xs <= ys
    evaluateOp op = na $ "evaluateOp{OpLexLeq}:" <++> pretty (show op)

instance EvaluateOp OpLexLt where
    evaluateOp (OpLexLt (viewConstantMatrix -> Just (_, xs)) (viewConstantMatrix -> Just (_, ys))) =
        return $ ConstantBool $ xs < ys
    evaluateOp op = na $ "evaluateOp{OpLexLt}:" <++> pretty (show op)

instance EvaluateOp OpLt where
    evaluateOp (OpLt x y) = return $ ConstantBool $ x < y

instance EvaluateOp OpMakeTable where
    evaluateOp op = na $ "evaluateOp{OpMakeTable}:" <++> pretty (show op)

instance EvaluateOp OpMax where
    evaluateOp p | any isUndef (childrenBi p) =
            return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpMax x)
        | Just xs <- listOut x
        , any isUndef xs =
            return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpMax (DomainInConstant DomainBool)) = return (ConstantBool True)
    evaluateOp (OpMax (DomainInConstant (DomainInt t rs))) = do
        is <- rangesInts rs
        return $ if null is
            then mkUndef (TypeInt TagInt) "Empty collection in max"
            else ConstantInt t (maximum is)
    evaluateOp (OpMax coll@(viewConstantMatrix -> Just (_, xs))) =
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in max"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt t -> do
                        is <- concatMapM (intsOut "OpMax 1") xs
                        return $ ConstantInt t (maximum is)
                    _ -> na "evaluateOp{OpMax}"
    evaluateOp (OpMax coll@(viewConstantSet -> Just xs)) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in max"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt t -> do
                        is <- concatMapM (intsOut "OpMax 1") xs
                        return $ ConstantInt t (maximum is)
                    _ -> na "evaluateOp{OpMax}"
    evaluateOp (OpMax coll@(viewConstantMSet -> Just xs)) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in max"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt t -> do
                        is <- concatMapM (intsOut "OpMax 1") xs
                        return $ ConstantInt t (maximum is)
                    _ -> na "evaluateOp{OpMax}"
    evaluateOp _ = na "evaluateOp{OpMax}"

instance EvaluateOp OpMin where
    evaluateOp p | any isUndef (childrenBi p) =
            return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpMin x)
        | Just xs <- listOut x
        , any isUndef xs =
            return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpMin (DomainInConstant DomainBool)) = return (ConstantBool False)
    evaluateOp (OpMin (DomainInConstant (DomainInt t rs))) = do
        is <- rangesInts rs
        return $ if null is
            then mkUndef (TypeInt TagInt) "Empty collection in min"
            else ConstantInt t (minimum is)
    evaluateOp (OpMin coll@(viewConstantMatrix -> Just (_, xs))) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in min"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt t -> do
                        is <- concatMapM (intsOut "OpMin 1") xs
                        return $ ConstantInt t (minimum is)
                    _ -> na "evaluateOp{OpMin}"
    evaluateOp (OpMin coll@(viewConstantSet -> Just xs)) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in min"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt t -> do
                        is <- concatMapM (intsOut "OpMin 1") xs
                        return $ ConstantInt t (minimum is)
                    _ -> na "evaluateOp{OpMin}"
    evaluateOp (OpMin coll@(viewConstantMSet -> Just xs)) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in min"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt t -> do
                        is <- concatMapM (intsOut "OpMin 1") xs
                        return $ ConstantInt t (minimum is)
                    _ -> na "evaluateOp{OpMin}"
    evaluateOp op = na $ "evaluateOp{OpMin}" <+> pretty (show op)

instance EvaluateOp OpMinus where
    evaluateOp p | any isUndef (childrenBi p) = do
        ty <- typeOf p
        return $ mkUndef ty $ "Has undefined children:" <+> pretty p
    evaluateOp (OpMinus (ConstantInt t a) (ConstantInt _ b))
      = return $ ConstantInt t (a - b)
    evaluateOp (OpMinus (viewConstantSet -> Just as) (viewConstantSet -> Just bs)) = do
        let outs =
                [ a
                | a <- as
                , a `notElem` bs
                ]
        return $ ConstantAbstract $ AbsLitSet outs
    evaluateOp (OpMinus (viewConstantMSet -> Just as) (viewConstantMSet -> Just bs)) = do
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
            outs =
                [ replicate (fromInteger (countA - countB)) e
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
        return $ ConstantAbstract $ AbsLitMSet $ concat outs
    evaluateOp (OpMinus (viewConstantFunction -> Just as) (viewConstantFunction -> Just bs)) = do
        let outs =
                [ a
                | a <- as
                , a `notElem` bs
                ]
        return $ ConstantAbstract $ AbsLitFunction outs
    evaluateOp (OpMinus (viewConstantRelation -> Just as) (viewConstantRelation -> Just bs)) = do
        let outs =
                [ a
                | a <- as
                , a `notElem` bs
                ]
        return $ ConstantAbstract $ AbsLitRelation outs
    evaluateOp op = na $ "evaluateOp{OpMinus}:" <++> pretty (show op)

instance EvaluateOp OpMod where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpMod x y)
        | y /= 0    = ConstantInt TagInt <$> (mod <$> intOut "mod x" x <*> intOut "mod y" y)
        | otherwise = return $ mkUndef (TypeInt TagInt) $ "modulo zero:" <+> pretty p

instance EvaluateOp OpNegate where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpNegate x) = ConstantInt TagInt . negate <$> intOut "OpNegate" x

instance EvaluateOp OpNeq where
    evaluateOp (OpNeq ConstantUndefined{} _) = return $ fromBool False
    evaluateOp (OpNeq _ ConstantUndefined{}) = return $ fromBool False
    evaluateOp (OpNeq x y) = do
        out <- evaluateOp (OpEq x y)
        evaluateOp (OpNot out)

instance EvaluateOp OpNot where
    evaluateOp (OpNot x) = ConstantBool . not <$> boolOut x

instance EvaluateOp OpOr where
    evaluateOp (OpOr x) = ConstantBool . or <$> boolsOut x

instance EvaluateOp OpParticipants where
    evaluateOp (OpParticipants (viewConstantPartition -> Just xss)) =
        return $ ConstantAbstract $ AbsLitSet $ sort $ concat xss
    evaluateOp op = na $ "evaluateOp{OpParticipants}:" <++> pretty (show op)

instance EvaluateOp OpParts where
    evaluateOp (OpParts (viewConstantPartition -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet $ map (ConstantAbstract . AbsLitSet) xs
    evaluateOp op = na $ "evaluateOp{OpParts}:" <++> pretty (show op)

instance EvaluateOp OpParty where
    evaluateOp op@(OpParty x p@(viewConstantPartition -> Just xss)) = do
        TypePartition tyInner <- typeOf p
        let
            outSet = [ xs
                     | xs <- xss
                     , x `elem` xs
                     ]
        case outSet of
            [s] -> return $ ConstantAbstract (AbsLitSet s)
            []  -> return $ TypedConstant (ConstantAbstract (AbsLitSet [])) (TypeSet tyInner)
            _   -> return $ mkUndef (TypeSet tyInner) $ "Element found in multiple parts of the partition:"
                                                                                                <++> pretty op
    evaluateOp op = na $ "evaluateOp{OpParty}:" <++> pretty (show op)

instance EvaluateOp OpPermInverse where
    evaluateOp (OpPermInverse (viewConstantPermutation -> Just xss))
        | Right perm <- fromCycles xss
        = return $ ConstantAbstract $ AbsLitPermutation $ toCyclesCanonical $ inverse perm
    evaluateOp op = na $ "evaluateOp{OpPermInverse}:" <++> pretty (show op)

instance EvaluateOp OpPow where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpPow x y)
        | y >= 0    = ConstantInt TagInt <$> ((^) <$> intOut "pow x" x <*> intOut "pow y" y)
        | otherwise = return $ mkUndef (TypeInt TagInt) $ "negative exponent:" <+> pretty p

instance EvaluateOp OpPowerSet where
    evaluateOp (OpPowerSet (viewConstantSet -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet ys
            | ys <- subsequences (sortBy ordTildeLt (sortNub xs)) ]
    evaluateOp op = na $ "evaluateOp{OpPowerSet}:" <++> pretty (show op)

instance EvaluateOp OpPred where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpPred (ConstantBool _)) = return (ConstantBool False)          -- True --> False
                                                                                -- False --> undef, hence False
    evaluateOp (OpPred (ConstantInt TagInt x)) = return (ConstantInt TagInt (pred x))
    evaluateOp (OpPred (ConstantInt (TagEnum t) x))
        = return (ConstantInt (TagEnum t) (pred x))
    evaluateOp op = na $ "evaluateOp{OpPred}" <+> pretty (show op)

instance EvaluateOp OpPreImage where
    evaluateOp (OpPreImage (viewConstantFunction -> Just xs) a) =
        return $ ConstantAbstract $ AbsLitSet [ x | (x,y) <- xs, a == y ]
    evaluateOp (OpPreImage (viewConstantSequence -> Just xs) a) =
        return $ ConstantAbstract $ AbsLitSet [ x | (n,y) <- zip allNats xs
                                                  , let x = ConstantInt TagInt n
                                                  , a == y ]
    evaluateOp op = na $ "evaluateOp{OpPreImage}:" <++> pretty (show op)

instance EvaluateOp OpProduct where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpProduct x)
        | Just xs <- listOut x
        , any isUndef xs =
            return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpProduct x) = ConstantInt TagInt . product <$> intsOut "OpProduct" x

instance EvaluateOp OpRange where
    evaluateOp p | any isUndef (childrenBi p) = do
        ty <- typeOf p
        return $ mkUndef ty $ "Has undefined children:" <+> pretty p
    evaluateOp (OpRange (viewConstantFunction -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub $ map snd xs
    evaluateOp op = na $ "evaluateOp{OpRange}:" <++> pretty (show op)

instance EvaluateOp OpRelationProj where
    evaluateOp (OpRelationProj (viewConstantRelation -> Just xss) mas) = do
        let mas' = catMaybes mas
        if length mas == length mas'
            then -- all Just's
                return $ ConstantBool $ mas' `elem` xss
            else
                return $ ConstantAbstract $ AbsLitRelation
                    [ xsProject
                    | xs <- xss
                    , let xsProject   = [ x
                                        | (x, Nothing) <- zip xs mas
                                        ]
                    , let xsCondition = [ x == y
                                        | (x, Just y ) <- zip xs mas
                                        ]
                    , and xsCondition
                    ]
    -- leave the OpImage evaluator in -- it is just easier
    evaluateOp (OpRelationProj f@(viewConstantFunction -> Just _) [Just arg]) =
        evaluateOp (OpImage f arg)
    evaluateOp (OpRelationProj f@(viewConstantSequence -> Just _) [Just arg]) =
        evaluateOp (OpImage f arg)
    evaluateOp op = na $ "evaluateOp{OpRelationProj}:" <++> pretty (show op)

instance EvaluateOp OpRestrict where
    evaluateOp (OpRestrict (viewConstantFunction -> Just xs) domX) = do
        dom     <- domainOut domX
        outVals <- concatForM xs $ \case
            x@(a, _) -> do
                mres <- runExceptT $ validateConstantForDomain "<in memory>" a (dom :: Domain () Constant)
                case mres of
                    Left {} -> return []
                    Right{} -> return [x]
        return $ ConstantAbstract $ AbsLitFunction $ sortNub outVals
    evaluateOp op = na $ "evaluateOp{OpRestrict}:" <++> pretty (show op)

instance EvaluateOp OpSlicing where
    evaluateOp (OpSlicing (viewConstantMatrix -> Just (DomainInt n index, vals)) lb ub)
      = do
        indexVals <- valuesInIntDomain index
        outVals   <- fmap catMaybes $ forM (zip indexVals vals)
                     $ \ (thisIndex, thisVal) ->
                         case lb of
                             Just (ConstantInt cn lower)
                               | cn == n && lower > thisIndex -> return Nothing
                             _ -> case ub of
                                    Just (ConstantInt cn upper)
                                      | cn == n && upper < thisIndex -> return Nothing
                                    _ -> return $ Just (thisIndex, thisVal)
        let outDomain = DomainInt n $ map (RangeSingle . (ConstantInt n) . fst) outVals
        return $ ConstantAbstract $ AbsLitMatrix outDomain (map snd outVals)
    evaluateOp op = na $ "evaluateOp{OpSlicing}:" <++> pretty (show op)

instance EvaluateOp OpSubsequence where
    evaluateOp (OpSubsequence
        (viewConstantSequence -> Just xs)
        (viewConstantSequence -> Just ys)) =
            return $ fromBool $
                or [ and (zipWith (==) xs zs)
                   | zs <- subsequences ys
                   , length zs >= length xs
                   ]
    evaluateOp op = na $ "evaluateOp{OpSubsequence}:" <++> pretty (show op)

instance EvaluateOp OpSubset where
    evaluateOp (OpSubset a b) = do
        x <- evaluateOp (OpSubsetEq a b)
        y <- evaluateOp (OpNeq a b)
        evaluateOp (OpAnd (fromList [x,y]))

instance EvaluateOp OpSubsetEq where
    evaluateOp (OpSubsetEq (viewConstantSet -> Just as) (viewConstantSet -> Just bs)) =
        return $ ConstantBool $ all (`elem` bs) as
    evaluateOp (OpSubsetEq (viewConstantMSet -> Just as) (viewConstantMSet -> Just bs)) =
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
        in return $ ConstantBool $ and
            [ countA <= countB
            | e <- allElems
            , let countA = fromMaybe 0 (e `lookup` asHist)
            , let countB = fromMaybe 0 (e `lookup` bsHist)
            ]
    evaluateOp (OpSubsetEq (viewConstantFunction -> Just as) (viewConstantFunction -> Just bs)) =
        return $ ConstantBool $ all (`elem` bs) as
    evaluateOp (OpSubsetEq (viewConstantRelation -> Just as) (viewConstantRelation -> Just bs)) =
        return $ ConstantBool $ all (`elem` bs) as
    evaluateOp op = na $ "evaluateOp{OpSubsetEq}:" <++> pretty (show op)

instance EvaluateOp OpSubstring where
    evaluateOp (OpSubstring
        (viewConstantSequence -> Just xs)
        (viewConstantSequence -> Just ys)) =
            return $ fromBool $
                or [ and (zipWith (==) xs zs)
                   | zs <- tails ys
                   , length zs >= length xs
                   ]
    evaluateOp op = na $ "evaluateOp{OpSubstring}:" <++> pretty (show op)

instance EvaluateOp OpSucc where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpSucc (ConstantBool False)) = return (ConstantBool True)
    evaluateOp (OpSucc (ConstantBool True )) = return (ConstantBool False)          -- undef
    evaluateOp (OpSucc (ConstantInt TagInt x)) = return (ConstantInt TagInt (succ x))
    evaluateOp (OpSucc (ConstantInt (TagEnum t) x))
        = return (ConstantInt (TagEnum t) (succ x))
    evaluateOp op = na $ "evaluateOp{OpSucc}" <+> pretty (show op)

instance EvaluateOp OpSum where
    evaluateOp p | any isUndef (childrenBi p) =
            return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpSum x)
        | Just xs <- listOut x
        , any isUndef xs =
            return $ mkUndef (TypeInt TagInt) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpSum x) = ConstantInt TagInt . sum <$> intsOut "OpSum" x

instance EvaluateOp OpSupset where
    evaluateOp (OpSupset a b) = evaluateOp (OpSubset b a)

instance EvaluateOp OpSupsetEq where
    evaluateOp (OpSupsetEq a b) = evaluateOp (OpSubsetEq b a)

instance EvaluateOp OpTable where
    evaluateOp (OpTable rows table) = do
        rows' <- intsOut "OpTable-rows" rows
        table' <- intsOut2D "OpTable-table" table
        return $ ConstantBool $ rows' `elem` table'

instance EvaluateOp OpGCC where
    evaluateOp op@OpGCC{} = na $ "evaluateOp{OpGCC}" <+> pretty op

instance EvaluateOp OpAtLeast where
    evaluateOp (OpAtLeast (intsOut "" -> Just vars)
                          (intsOut "" -> Just bounds)
                          (intsOut "" -> Just vals)) = do
        return $ ConstantBool $ and [ sum [1 | x <- vars, x == val] >= bound
                                    | (bound, val) <- zip bounds vals
                                    ]
    evaluateOp op@OpAtLeast{} = na $ "evaluateOp{OpAtLeast}" <+> pretty op

instance EvaluateOp OpAtMost where
    evaluateOp (OpAtMost (intsOut "" -> Just vars)
                         (intsOut "" -> Just bounds)
                         (intsOut "" -> Just vals)) = do
        return $ ConstantBool $ and [ sum [1 | x <- vars, x == val] <= bound
                                    | (bound, val) <- zip bounds vals
                                    ]
    evaluateOp op@OpAtMost{} = na $ "evaluateOp{OpAtMost}" <+> pretty op

instance EvaluateOp OpTildeLeq where
    evaluateOp (OpTildeLeq x y) = do
        flag1 <- evaluateOp (OpEq x y)
        flag2 <- evaluateOp (OpTildeLt x y)
        evaluateOp $ OpOr $ fromList [flag1, flag2]

instance EvaluateOp OpTildeLt where
    evaluateOp (OpTildeLt x y) = return $ ConstantBool $ tildeLt x y

instance EvaluateOp OpTogether where
    evaluateOp (OpTogether _ ConstantUndefined{}) = return (fromBool False)
    evaluateOp (OpTogether (viewConstantSet -> Just ys) (viewConstantPartition -> Just xss)) =
        return $ ConstantBool $ or
            [ and [ y `elem` xs | y <- ys ]
            | xs <- xss
            ]
    evaluateOp op = na $ "evaluateOp{OpTogether}:" <++> pretty (show op)

instance EvaluateOp OpToInt where
    evaluateOp (OpToInt (ConstantBool False)) = return (ConstantInt TagInt 0)
    evaluateOp (OpToInt (ConstantBool True )) = return (ConstantInt TagInt 1)
    evaluateOp (OpToInt ConstantUndefined{})  = return (ConstantInt TagInt 0)
    evaluateOp op = na $ "evaluateOp{OpToInt}:" <++> pretty (show op)

instance EvaluateOp OpToMSet where
    evaluateOp (OpToMSet (viewConstantSet -> Just xs)) =
        return $ ConstantAbstract $ AbsLitMSet xs
    evaluateOp (OpToMSet (viewConstantMSet -> Just xs)) =
        return $ ConstantAbstract $ AbsLitMSet xs
    evaluateOp (OpToMSet (viewConstantFunction -> Just xs)) =
        return $ ConstantAbstract $ AbsLitMSet [ConstantAbstract $ AbsLitTuple [a,b] | (a,b) <- xs]
    evaluateOp (OpToMSet (viewConstantRelation -> Just xs)) =
        return $ ConstantAbstract $ AbsLitMSet $ map (ConstantAbstract . AbsLitTuple) xs
    evaluateOp op = na $ "evaluateOp{OpToMSet}:" <++> pretty (show op)

instance EvaluateOp OpToRelation where
    evaluateOp (OpToRelation (viewConstantFunction -> Just xs)) =
        return $ ConstantAbstract $ AbsLitRelation $ sortNub [ [a,b] | (a,b) <- xs ]
    evaluateOp op = na $ "evaluateOp{OpToRelation}:" <++> pretty (show op)

instance EvaluateOp OpToSet where
    evaluateOp (OpToSet _ (viewConstantMatrix -> Just (_, xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub xs
    evaluateOp (OpToSet _ (viewConstantSet -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub xs
    evaluateOp (OpToSet _ (viewConstantMSet -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub xs
    evaluateOp (OpToSet _ (viewConstantFunction -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub [ConstantAbstract $ AbsLitTuple [a,b] | (a,b) <- xs]
    evaluateOp (OpToSet _ (viewConstantRelation -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub $ map (ConstantAbstract . AbsLitTuple) xs
    evaluateOp (OpToSet _ (viewConstantPermutation -> Just xs)) =
        case toFunction <$> fromCycles xs of
           Left (PermutationError e) -> na $ "evaluateOp{OpToSet}:" <++> pretty e 
           Right fn -> return $ ConstantAbstract $ AbsLitSet $ (ConstantAbstract . AbsLitTuple) <$> ((\x -> [x, fn x]) <$> join xs)
    evaluateOp op = na $ "evaluateOp{OpToSet}:" <++> pretty (show op)

instance EvaluateOp OpTransform where
    evaluateOp op = na $ "evaluateOp{OpTransform}:" <++> pretty (show op)

instance EvaluateOp OpTrue where
    evaluateOp _ = return (fromBool True)

instance EvaluateOp OpTwoBars where
    evaluateOp (OpTwoBars x) =
        case x of
            -- absolute value
            ConstantInt _ y                         -> return $ ConstantInt TagInt $ abs y

            -- cardinality of a constant
            (viewConstantMatrix    -> Just (_, xs)) -> return $ ConstantInt TagInt $ genericLength                    xs
            (viewConstantSet       -> Just xs)      -> return $ ConstantInt TagInt $ genericLength $ sortNub          xs
            (viewConstantMSet      -> Just xs)      -> return $ ConstantInt TagInt $ genericLength                    xs
            (viewConstantFunction  -> Just xs)      -> return $ ConstantInt TagInt $ genericLength $ sortNub          xs
            (viewConstantSequence  -> Just xs)      -> return $ ConstantInt TagInt $ genericLength                    xs
            (viewConstantRelation  -> Just xs)      -> return $ ConstantInt TagInt $ genericLength $ sortNub          xs
            (viewConstantPartition -> Just xs)      -> return $ ConstantInt TagInt $ genericLength $ sortNub $ concat xs
            (viewConstantPermutation -> Just xs)    -> return $ ConstantInt TagInt $ genericLength $ sortNub $ concat xs

            -- cardinality of a domain
            DomainInConstant (DomainInt _ rs) -> ConstantInt TagInt . genericLength <$> rangesInts rs
            DomainInConstant dom            -> runNameGen () $ domainSizeOf dom
            _ -> na $ "evaluateOp OpTwoBars" <+> pretty (show x)

instance EvaluateOp OpUnion where
    evaluateOp p | any isUndef (childrenBi p) = do
        ty <- typeOf p
        return $ mkUndef ty $ "Has undefined children:" <+> pretty p
    evaluateOp (OpUnion (viewConstantSet -> Just as) (viewConstantSet -> Just bs)) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub (as ++ bs)
    evaluateOp (OpUnion (viewConstantMSet -> Just as) (viewConstantMSet -> Just bs)) =
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
        in
            return $ ConstantAbstract $ AbsLitMSet $ concat
                [ replicate (fromInteger (max countA countB)) e
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
    -- TODO: what if the same thing is mapped to two different values? undefined behaviour?
    evaluateOp (OpUnion (viewConstantFunction -> Just as) (viewConstantFunction -> Just bs)) =
        return $ ConstantAbstract $ AbsLitFunction $ sortNub (as ++ bs)
    evaluateOp (OpUnion (viewConstantRelation -> Just as) (viewConstantRelation -> Just bs)) =
        return $ ConstantAbstract $ AbsLitRelation $ sortNub (as ++ bs)
    evaluateOp op = na $ "evaluateOp{OpUnion}:" <++> pretty (show op)

instance EvaluateOp OpXor where
    evaluateOp (OpXor x) = ConstantBool . xor <$> boolsOut x
        where xor xs = odd (length [ () | True <- xs ])

instance EvaluateOp OpQuickPermutationOrder where
    evaluateOp op = na $ "evaluateOp{OpQuickPermutationOrder}:" <++> pretty (show op)

boolsOut :: MonadFailDoc m => Constant -> m [Bool]
boolsOut (viewConstantMatrix -> Just (_, cs)) = concatMapM boolsOut cs
boolsOut b = return <$> boolOut b

intsOut :: MonadFailDoc m => Doc -> Constant -> m [Integer]
intsOut doc (viewConstantMatrix -> Just (_, cs)) = concatMapM (intsOut doc) cs
intsOut doc (viewConstantSet -> Just cs) = concatMapM (intsOut doc) cs
intsOut doc (viewConstantMSet -> Just cs) = concatMapM (intsOut doc) cs
intsOut doc b = return <$> intOut ("intsOut" <+> doc) b

intsOut2D :: MonadFailDoc m => Doc -> Constant -> m [[Integer]]
intsOut2D doc (viewConstantMatrix -> Just (_, cs)) = mapM (intsOut doc) cs
intsOut2D doc (viewConstantSet -> Just cs) = mapM (intsOut doc) cs
intsOut2D doc (viewConstantMSet -> Just cs) = mapM (intsOut doc) cs
intsOut2D doc _ = failDoc ("intsOut2D" <+> doc)

tildeLt :: Constant -> Constant -> Bool
tildeLt = tilLt
    where
        freq :: Eq a => a -> [a] -> Int
        freq i xs = sum [ 1 | j <- xs , i == j ]

        tupleE (i,j) = ConstantAbstract $ AbsLitTuple [i,j]

        tilLt :: Constant -> Constant -> Bool
        tilLt (ConstantBool a) (ConstantBool b) = a < b
        tilLt (ConstantInt TagInt a) (ConstantInt TagInt b) = a < b
        tilLt (ConstantInt (TagEnum an) a) (ConstantInt (TagEnum bn) b)
              | an == bn = a < b
        tilLt (viewConstantTuple -> Just [])
              (viewConstantTuple -> Just []) = False
        tilLt (viewConstantTuple -> Just (a:as))
              (viewConstantTuple -> Just (b:bs)) =
                  if tilLt a b
                      then True
                      else a == b &&
                           tilLt (ConstantAbstract $ AbsLitTuple as)
                                 (ConstantAbstract $ AbsLitTuple bs)
        tilLt (viewConstantSet -> Just as)
              (viewConstantSet -> Just bs) =
            or [ and [ freq i as < freq i bs
                     , and [ if tilLt j i
                                 then freq j as == freq j bs
                                 else True
                           | j <- cs
                           ]
                     ]
               | let cs = sortNub (as ++ bs)
               , i <- cs
               ]
        tilLt (viewConstantMSet -> Just as)
              (viewConstantMSet -> Just bs) =
            or [ and [ freq i as < freq i bs
                     , and [ if tilLt j i
                                 then freq j as == freq j bs
                                 else True
                           | j <- cs
                           ]
                     ]
               | let cs = as ++ bs
               , i <- cs
               ]
        tilLt (viewConstantFunction -> Just as')
              (viewConstantFunction -> Just bs') =
            or [ and [ freq i as < freq i bs
                     , and [ if tilLt j i
                                 then freq j as == freq j bs
                                 else True
                           | j <- cs
                           ]
                     ]
               | let as = map tupleE as'
               , let bs = map tupleE bs'
               , let cs = as ++ bs
               , i <- cs
               ]
        tilLt (viewConstantRelation -> Just as')
              (viewConstantRelation -> Just bs') =
            or [ and [ freq i as < freq i bs
                     , and [ if tilLt j i
                                 then freq j as == freq j bs
                                 else True
                           | j <- cs
                           ]
                     ]
               | let as = map (ConstantAbstract . AbsLitTuple) as'
               , let bs = map (ConstantAbstract . AbsLitTuple) bs'
               , let cs = as ++ bs
               , i <- cs
               ]
        tilLt (viewConstantPartition -> Just as')
              (viewConstantPartition -> Just bs') =
            or [ and [ freq i as < freq i bs
                     , and [ if tilLt j i
                                 then freq j as == freq j bs
                                 else True
                           | j <- cs
                           ]
                     ]
               | let as = map (ConstantAbstract . AbsLitSet) as'
               , let bs = map (ConstantAbstract . AbsLitSet) bs'
               , let cs = as ++ bs
               , i <- cs
               ]
        tilLt a b = a < b

ordTildeLt :: Constant -> Constant -> Ordering
ordTildeLt x y =
    case (tildeLt x y, tildeLt y x) of
        (True, _) -> LT
        (_, True) -> GT
        _         -> EQ


instance EvaluateOp Op where
    evaluateOp (MkOpActive x) = evaluateOp x
    evaluateOp (MkOpCompose x) = evaluateOp x
    evaluateOp (MkOpAllDiff x) = evaluateOp x
    evaluateOp (MkOpAllDiffExcept x) = evaluateOp x
    evaluateOp (MkOpAnd x) = evaluateOp x
    evaluateOp (MkOpApart x) = evaluateOp x
    evaluateOp (MkOpAtLeast x) = evaluateOp x
    evaluateOp (MkOpAtMost x) = evaluateOp x
    evaluateOp (MkOpAttributeAsConstraint x) = evaluateOp x
    evaluateOp (MkOpCatchUndef x) = evaluateOp x
    evaluateOp (MkOpDefined x) = evaluateOp x
    evaluateOp (MkOpDiv x) = evaluateOp x
    evaluateOp (MkOpDontCare x) = evaluateOp x
    evaluateOp (MkOpDotLeq x) = evaluateOp x
    evaluateOp (MkOpDotLt x) = evaluateOp x
    evaluateOp (MkOpEq x) = evaluateOp x
    evaluateOp (MkOpFactorial x) = evaluateOp x
    evaluateOp (MkOpFlatten x) = evaluateOp x
    evaluateOp (MkOpFreq x) = evaluateOp x
    evaluateOp (MkOpGCC x) = evaluateOp x
    evaluateOp (MkOpGeq x) = evaluateOp x
    evaluateOp (MkOpGt x) = evaluateOp x
    evaluateOp (MkOpHist x) = evaluateOp x
    evaluateOp (MkOpIff x) = evaluateOp x
    evaluateOp (MkOpImage x) = evaluateOp x
    evaluateOp (MkOpImageSet x) = evaluateOp x
    evaluateOp (MkOpImply x) = evaluateOp x
    evaluateOp (MkOpIn x) = evaluateOp x
    evaluateOp (MkOpIndexing x) = evaluateOp x
    evaluateOp (MkOpIntersect x) = evaluateOp x
    evaluateOp (MkOpInverse x) = evaluateOp x
    evaluateOp (MkOpLeq x) = evaluateOp x
    evaluateOp (MkOpLexLeq x) = evaluateOp x
    evaluateOp (MkOpLexLt x) = evaluateOp x
    evaluateOp (MkOpLt x) = evaluateOp x
    evaluateOp (MkOpMakeTable x) = evaluateOp x
    evaluateOp (MkOpMax x) = evaluateOp x
    evaluateOp (MkOpMin x) = evaluateOp x
    evaluateOp (MkOpMinus x) = evaluateOp x
    evaluateOp (MkOpMod x) = evaluateOp x
    evaluateOp (MkOpNegate x) = evaluateOp x
    evaluateOp (MkOpNeq x) = evaluateOp x
    evaluateOp (MkOpNot x) = evaluateOp x
    evaluateOp (MkOpOr x) = evaluateOp x
    evaluateOp (MkOpParticipants x) = evaluateOp x
    evaluateOp (MkOpParts x) = evaluateOp x
    evaluateOp (MkOpParty x) = evaluateOp x
    evaluateOp (MkOpPermInverse x) = evaluateOp x
    evaluateOp (MkOpPow x) = evaluateOp x
    evaluateOp (MkOpPowerSet x) = evaluateOp x
    evaluateOp (MkOpPred x) = evaluateOp x
    evaluateOp (MkOpPreImage x) = evaluateOp x
    evaluateOp (MkOpProduct x) = evaluateOp x
    evaluateOp (MkOpRange x) = evaluateOp x
    evaluateOp (MkOpRelationProj x) = evaluateOp x
    evaluateOp (MkOpRestrict x) = evaluateOp x
    evaluateOp (MkOpSlicing x) = evaluateOp x
    evaluateOp (MkOpSubsequence x) = evaluateOp x
    evaluateOp (MkOpSubset x) = evaluateOp x
    evaluateOp (MkOpSubsetEq x) = evaluateOp x
    evaluateOp (MkOpSubstring x) = evaluateOp x
    evaluateOp (MkOpSucc x) = evaluateOp x
    evaluateOp (MkOpSum x) = evaluateOp x
    evaluateOp (MkOpSupset x) = evaluateOp x
    evaluateOp (MkOpSupsetEq x) = evaluateOp x
    evaluateOp (MkOpTable x) = evaluateOp x
    evaluateOp (MkOpTildeLeq x) = evaluateOp x
    evaluateOp (MkOpTildeLt x) = evaluateOp x
    evaluateOp (MkOpTogether x) = evaluateOp x
    evaluateOp (MkOpToInt x) = evaluateOp x
    evaluateOp (MkOpToMSet x) = evaluateOp x
    evaluateOp (MkOpToRelation x) = evaluateOp x
    evaluateOp (MkOpToSet x) = evaluateOp x
    evaluateOp (MkOpTransform x) = evaluateOp x
    evaluateOp (MkOpTrue x) = evaluateOp x
    evaluateOp (MkOpTwoBars x) = evaluateOp x
    evaluateOp (MkOpUnion x) = evaluateOp x
    evaluateOp (MkOpXor x) = evaluateOp x
    evaluateOp (MkOpQuickPermutationOrder x) = evaluateOp x
