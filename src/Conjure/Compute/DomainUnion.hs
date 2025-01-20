{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Conjure.Compute.DomainUnion
  ( domainUnion,
    domainUnions,
  )
where

-- conjure

import Conjure.Bug
import Conjure.Language.AdHoc
import Conjure.Language.Domain
import Conjure.Language.Expression.Op
import Conjure.Language.Lenses
import Conjure.Language.Name
import Conjure.Language.Pretty
import Conjure.Language.Type
import Conjure.Prelude
import Data.List as L (union)
-- containers
import Data.Set as S (union)

class DomainUnion a where
  domainUnion :: (Applicative m, Monad m) => a -> a -> m a

domainUnions ::
  ( Applicative m,
    Monad m,
    Pretty r,
    Default r,
    Eq x,
    Pretty x,
    ExpressionLike x,
    Op x :< x
  ) =>
  [Domain r x] ->
  m (Domain r x)
domainUnions [] = return $ DomainAny "domainUnions []" TypeAny
domainUnions [a] = return a
domainUnions (a : as) = do b <- domainUnions as; domainUnion a b

instance
  ( Eq x,
    ExpressionLike x,
    Op x :< x,
    Pretty x,
    Pretty r,
    Default r
  ) =>
  DomainUnion (Domain r x)
  where
  domainUnion d@(DomainReference nm1 _) (DomainReference nm2 _) | nm1 == nm2 = return d
  domainUnion d@(DomainInt (TagUnnamed nm1) _) (DomainReference (Name nm2) _) | nm1 == nm2 = return d
  domainUnion (DomainReference (Name nm2) _) d@(DomainInt (TagUnnamed nm1) _) | nm1 == nm2 = return d
  domainUnion DomainAny {} d = return d
  domainUnion d DomainAny {} = return d
  domainUnion DomainBool DomainBool = return DomainBool
  domainUnion (DomainInt t r1) (DomainInt _ r2) =
    return $ DomainInt t (r1 `L.union` r2)
  domainUnion (DomainTuple []) d@DomainTuple {} = return d
  domainUnion d@DomainTuple {} (DomainTuple []) = return d
  domainUnion (DomainTuple xs) (DomainTuple ys)
    | length xs == length ys =
        DomainTuple <$> zipWithM domainUnion xs ys
  domainUnion d1@(DomainRecord xs) d2@(DomainRecord ys)
    | length xs == length ys =
        DomainRecord
          <$> sequence
            [ case mdomY of
                Just domY -> do
                  domZ <- domainUnion domX domY
                  return (nm, domZ)
                Nothing -> bug $ vcat ["Domain.domainUnion", pretty d1, pretty d2]
              | (nm, domX) <- xs,
                let mdomY = lookup nm ys
            ]
  domainUnion (DomainMatrix x1 x2) (DomainMatrix y1 y2) =
    DomainMatrix <$> domainUnion x1 y1 <*> domainUnion x2 y2
  domainUnion (DomainSet _ xA x) (DomainSet _ yA y) =
    DomainSet def <$> domainUnion xA yA <*> domainUnion x y
  domainUnion (DomainMSet _ xA x) (DomainMSet _ yA y) =
    DomainMSet def <$> domainUnion xA yA <*> domainUnion x y
  domainUnion (DomainFunction _ xA x1 x2) (DomainFunction _ yA y1 y2) =
    DomainFunction def <$> domainUnion xA yA <*> domainUnion x1 y1 <*> domainUnion x2 y2
  domainUnion (DomainSequence _ xA x) (DomainSequence _ yA y) =
    DomainSequence def <$> domainUnion xA yA <*> domainUnion x y
  domainUnion (DomainRelation _ _ []) d@DomainRelation {} = return d
  domainUnion d@DomainRelation {} (DomainRelation _ _ []) = return d
  domainUnion (DomainRelation _ xA xs) (DomainRelation _ yA ys)
    | length xs == length ys =
        DomainRelation def <$> domainUnion xA yA <*> zipWithM domainUnion xs ys
  domainUnion (DomainPartition _ xA x) (DomainPartition _ yA y) =
    DomainPartition def <$> domainUnion xA yA <*> domainUnion x y
  domainUnion (DomainPermutation _ xA x) (DomainPermutation _ yA y) =
    DomainPermutation def <$> domainUnion xA yA <*> domainUnion x y
  domainUnion d1 d2 = bug $ vcat ["Domain.domainUnion", pretty (show d1), pretty (show d2)]

instance
  ( ExpressionLike x,
    Op x :< x,
    Pretty x
  ) =>
  DomainUnion (SetAttr x)
  where
  domainUnion (SetAttr a) (SetAttr b) = SetAttr <$> domainUnion a b

instance
  ( ExpressionLike x,
    Op x :< x,
    Pretty x,
    Eq x
  ) =>
  DomainUnion (PermutationAttr x)
  where
  domainUnion (PermutationAttr a) (PermutationAttr b)
    | a == b = return (PermutationAttr a)
    | otherwise = bug "DomainUnion PermutationAttr"

instance
  ( ExpressionLike x,
    Op x :< x,
    Pretty x
  ) =>
  DomainUnion (SizeAttr x)
  where
  domainUnion SizeAttr_None s = return s
  domainUnion s SizeAttr_None = return s
  domainUnion a b =
    return
      $ SizeAttr_MinMaxSize
        (make opMin (fromList [minA, minB]))
        (make opMax (fromList [maxA, maxB]))
    where
      (minA, maxA) = getMinMax a
      (minB, maxB) = getMinMax b
      getMinMax p = case p of
        SizeAttr_None -> bug "Monoid SizeAttr"
        SizeAttr_Size x -> (x, x)
        SizeAttr_MinSize x -> (x, x)
        SizeAttr_MaxSize x -> (x, x)
        SizeAttr_MinMaxSize x y -> (x, y)

instance
  ( ExpressionLike x,
    Op x :< x,
    Pretty x
  ) =>
  DomainUnion (MSetAttr x)
  where
  domainUnion (MSetAttr a1 a2) (MSetAttr b1 b2) = MSetAttr <$> domainUnion a1 b1 <*> domainUnion a2 b2

instance
  ( ExpressionLike x,
    Op x :< x,
    Pretty x
  ) =>
  DomainUnion (OccurAttr x)
  where
  domainUnion OccurAttr_None s = return s
  domainUnion s OccurAttr_None = return s
  domainUnion a b =
    return
      $ OccurAttr_MinMaxOccur
        (make opMin (fromList [minA, minB]))
        (make opMax (fromList [maxA, maxB]))
    where
      (minA, maxA) = getMinMax a
      (minB, maxB) = getMinMax b
      getMinMax p = case p of
        OccurAttr_None -> bug "Monoid OccurAttr"
        OccurAttr_MinOccur x -> (x, x)
        OccurAttr_MaxOccur x -> (x, x)
        OccurAttr_MinMaxOccur x y -> (x, y)

instance
  ( ExpressionLike x,
    Op x :< x,
    Pretty x
  ) =>
  DomainUnion (FunctionAttr x)
  where
  domainUnion (FunctionAttr a1 a2 a3) (FunctionAttr b1 b2 b3) =
    FunctionAttr <$> domainUnion a1 b1 <*> domainUnion a2 b2 <*> domainUnion a3 b3

instance DomainUnion PartialityAttr where
  domainUnion PartialityAttr_Partial _ = return PartialityAttr_Partial
  domainUnion _ PartialityAttr_Partial = return PartialityAttr_Partial
  domainUnion PartialityAttr_Total PartialityAttr_Total = return PartialityAttr_Total

instance DomainUnion JectivityAttr where
  domainUnion x y | x == y = return x
  domainUnion _ _ = return JectivityAttr_None

instance
  ( ExpressionLike x,
    Op x :< x,
    Pretty x
  ) =>
  DomainUnion (SequenceAttr x)
  where
  domainUnion (SequenceAttr a1 a2) (SequenceAttr b1 b2) =
    SequenceAttr <$> domainUnion a1 b1 <*> domainUnion a2 b2

instance
  ( ExpressionLike x,
    Op x :< x,
    Pretty x
  ) =>
  DomainUnion (RelationAttr x)
  where
  domainUnion (RelationAttr a1 a2) (RelationAttr b1 b2) =
    RelationAttr <$> domainUnion a1 b1 <*> domainUnion a2 b2

instance DomainUnion BinaryRelationAttrs where
  domainUnion (BinaryRelationAttrs a) (BinaryRelationAttrs b) =
    return $ BinaryRelationAttrs (S.union a b)

instance
  ( ExpressionLike x,
    Op x :< x,
    Pretty x
  ) =>
  DomainUnion (PartitionAttr x)
  where
  domainUnion (PartitionAttr a1 a2 a3) (PartitionAttr b1 b2 b3) =
    PartitionAttr <$> domainUnion a1 b1 <*> domainUnion a2 b2 <*> pure (a3 || b3)
