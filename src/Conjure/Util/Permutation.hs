-- | Safely handle permutations with permutation-safe.
-- Conversion between standard permutation forms.
-- Helpful errors are given when trying to construct permutations with invalid data.
module Conjure.Util.Permutation
  ( -- * The Permutation Type
    Permutation (),

    -- ** Error Type
    PermutationError (..),

    -- ** Smart Constructors
    fromCycles,
    fromRelation,
    fromTwoLineForm,

    -- ** Accessors
    toFunction,
    toCycles,
    toCyclesCanonical,
    toRelation,
    toTwoLineForm,
    permutedPoints,

    -- * Permutation Utilities
    inverse,
    size,
    (^^^),

    completedCycles
  )
where

import Conjure.Prelude
import Control.Monad.State.Strict ( State, put )
import Data.Semigroup ( (<>) )

--------------------------Safe Permutation Type----------------------------------------

-- | The Permutation constructor is for internal use only.
-- To construct a permutation use any of the smart constructors.
newtype Permutation a = Permutation [(a, a)] deriving (Show)

-- | Equality tests that permutations contain the same permuted values.
instance (Eq a) => Eq (Permutation a) where
  (==) (Permutation l) (Permutation r) = and [e `elem` r | e <- l]

-- | Permutations compose as a semigroup in the same way they would compose if you composed them as functions.
instance (Eq a) => Semigroup (Permutation a) where
  (<>) pl@(Permutation l) pr@(Permutation r) =
    let flatten z = (z >>= (\(x, y) -> [x, y]))
        elemsofp = nub $ flatten l ++ flatten r
        permfunc = toFunction pl . toFunction pr
     in case fromRelation (zip elemsofp (permfunc <$> elemsofp)) of
          Left _ ->
            error $
              "Data.Permutation.Semigroup: this should only happen "
                ++ "if you didn't use a smart constructor AND created a "
                ++ "Permutation object that is not in fact a permutation.\n"
                ++ "If you did use a smart constructor and you see this "
                ++ "error then please submit a bug report with a minimal "
                ++ "failing example."
          Right p -> p

-- | The Monoid identity is the identity permutation. The Monoid plus is semigroup composition.
instance (Eq a) => Monoid (Permutation a) where
  mempty = Permutation []
  mappend = (<>)

--------------------------Error Type--------------------------------------------------

-- | There may be an error detailing why the permutation computation has failed.
newtype PermutationError = PermutationError String
  deriving (Eq, Show)

-- | Create a Permutation from disjoint cycles.
fromCycles :: (Eq a) => [[a]] -> Either PermutationError (Permutation a)
fromCycles c =
  if length (join c) /= length (nub $ join c)
    then
      Left $
        PermutationError
          "Data.Permutation.fromCycles: Cycles contain a duplicate element"
    else Right $ Permutation $ c >>= cycleToTuples
  where
    cycleToTuples :: [a] -> [(a, a)]
    cycleToTuples [] = []
    cycleToTuples [_] = []
    cycleToTuples l = zip (cycle l) (drop 1 l ++ [head l])

-- | Create a permutation from a relation a*a.
-- Only non identity mappings need be specified
-- (e.g. fromRelation [(1,2),(2,1),(3,3)] == fromRelation [(1,2),(2,1)]).
fromRelation :: (Eq a) => [(a, a)] -> Either PermutationError (Permutation a)
fromRelation r =
  let perm = Permutation $ filter (uncurry (/=)) r
   in if isBijective $ Permutation r
        then Right perm
        else
          Left $
            PermutationError
              "Data.Permutation.fromRelation: The relation is not bijective"

-- | Create a permutation from two line form.
--
-- >   fromTwoLineForm ([2,4,7,3]
-- >                   ,[7,3,2,4])
fromTwoLineForm :: (Eq a) => ([a], [a]) -> Either PermutationError (Permutation a)
fromTwoLineForm (t, b) =
  if length t /= length b
    then
      Left $
        PermutationError
          "Data.Permutation.fromTwoLineForm: The top and bottom lines have different length"
    else fromRelation (zip t b)

--------------------------Permutation Utilities----------------------------------------

-- | Gets the permutation as a function.
toFunction :: (Eq a) => Permutation a -> (a -> a)
toFunction (Permutation p) v = fromMaybe v (lookup v p)

-- | Convert the permutation to cycle form.
toCycles :: (Eq a) => Permutation a -> [[a]]
toCycles (Permutation p) = evalState go ([], [], p)
  where
    go :: (Eq a) => CycleFinder a [[a]]
    go = do
      cf <- cyclesFound
      if cf
        then returnCycles
        else do
          wc <- workingCycle
          case wc of
            [] -> startNewCycle >> go
            _  -> nextCycleElem >> go

-- | Convert the permutation to canonical cycle form.
toCyclesCanonical :: (Eq a, Ord a) => Permutation a -> [[a]]
toCyclesCanonical (Permutation p) =
  toCycles (Permutation $ sortBy (\x y -> fst x `compare` fst y) p)

-- | Given a lower and upper bound on a and a permutation within this range
-- returns a two line for representation.
toRelation ::
  (Eq a, Enum a) =>
  -- | Lower bound
  a ->
  -- | Upper bound
  a ->
  Permutation a ->
  Either PermutationError [(a, a)]
toRelation from to p@(Permutation pe) =
  let maybep = zip [from .. to] (toFunction p <$> [from .. to])
   in if length [from .. to] == length ([from .. to] \\ (fst <$> pe)) + length pe
        then Right maybep
        else
          Left $
            PermutationError
              "Data.Permutation.toRelation: the range used does not cover the permuted elements"

-- | Given a lower and upper bound on a and a permutation within this range
-- returns a two line for representation.
toTwoLineForm ::
  (Eq a, Enum a) =>
  -- | Lower bound
  a ->
  -- | Upper bound
  a ->
  Permutation a ->
  Either PermutationError ([a], [a])
toTwoLineForm from to p =
  case toRelation from to p of
    Left _ ->
      Left $
        PermutationError
          "Data.Permutation.toTwoLineForm: the range used does not cover the permuted elements"
    Right rf -> Right $ unzip rf

-- | Gets the list of points that are acted on by the permutation.
permutedPoints :: Permutation a -> [a]
permutedPoints (Permutation p) = fst <$> p

-- | Power of a permutation.
(^^^) :: (Eq a) => Permutation a -> Int -> Permutation a
(^^^) _ 0 = Permutation []
(^^^) p 1 = p
(^^^) p n = p <> (p ^^^ (n - 1))

-- | Gets the size of the permutation. The number of elements not mapped to themselves.
size :: Permutation a -> Int
size (Permutation p) = length p

-- | Gets the inverse of the permutation.
inverse :: Permutation a -> Permutation a
inverse (Permutation p) = Permutation $ (\(x, y) -> (y, x)) <$> p

--------------------------Bijectivity Checking-----------------------------------------

-- | Equality is required to assess inner relation bijectivity.
isBijective :: (Eq a) => Permutation a -> Bool
isBijective (Permutation p) =
  let (l, r) = unzip p
   in (length (nub l) == length (nub r))
        && (length (nub l) == length l)
        && null (l \\ r)

-------------------------CycleFinder Monad---------------------------------------------

-- | A builder monad for finding cycles.
type CycleFinder a = State ([a], [[a]], [(a, a)])

-- | The current cycle we are working on.
workingCycle :: CycleFinder a [a]
workingCycle = do
  (w, _, _) <- get
  return w

-- | Gets the completed cycles.
completedCycles :: CycleFinder a [[a]]
completedCycles = do
  (_, c, _) <- get
  return c

-- | Finds the element that a maps on to.
mapsOnto :: (Eq a) => a -> CycleFinder a a
mapsOnto i = do
  (_, _, m) <- get
  case lookup i m of
    Nothing ->
      error $
        "Data.Permutation.toCycles: this should only happen "
          ++ "if you didn't use a smart constructor AND created a "
          ++ "Permutation object that is not in fact a permutation.\n"
          ++ "If you did use a smart constructor and you see this "
          ++ "error then please submit a bug report with a minimal "
          ++ "failing example."
    Just so -> return so

-- | The exit condition - whether we have found all the cycles.
cyclesFound :: (Eq a) => CycleFinder a Bool
cyclesFound = do
  (w, _, m) <- get
  return (null w && null m)

-- | Returns the cycles.
returnCycles :: CycleFinder a [[a]]
returnCycles = do
  (_, c, _) <- get
  return $ reverse c

-- | Start finding a new cycle.
startNewCycle :: CycleFinder a ()
startNewCycle = do
  got <- get
  case got of
    (_, c, (a, b) : m) -> put ([a, b], c, m)
    _ -> error "Data.Permutation.Internal.startNewCycle: this should never happen"

-- | Adds the next element of the cycle.
nextCycleElem :: (Eq a) => CycleFinder a ()
nextCycleElem = do
  (w, c, m) <- get
  let w_last = last w
  next <- mapsOnto w_last
  let filt = filter (/= (w_last, next)) m
  if next == head w
    then put ([], w : c, filt)
    else put (w ++ [next], c, filt)
