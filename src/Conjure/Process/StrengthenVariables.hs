{-
 - Module      : Conjure.Process.StrengthenVariables
 - Description : Strengthen variables using type- and domain-inference.
 - Copyright   : Billy Brown 2017
 - License     : BSD3
 
 Processing step that attempts to strengthen variables at the Essence class level, using methods described in the "Reformulating Essence Specifications for Robustness" paper.
-}

module Conjure.Process.StrengthenVariables
  (
    strengthenVariables
  ) where

import Data.List ( delete, intersect, union )
import Data.Maybe ( mapMaybe )

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )
-- These two are needed together
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.DomainSizeOf ( domainSizeOf )
import Conjure.Compute.DomainOf ( domainOf )

-- | Strengthen the variables in a model using type- and domain-inference.
strengthenVariables :: (MonadFail m, MonadLog m, MonadUserError m)
                    => Model -> m Model
strengthenVariables = runNameGen . (resolveNames >=> core)
  where core :: (MonadFail m, MonadLog m, NameGen m) => Model -> m Model
        core model = do model' <- foldM folder model [ functionAttributes
                                                     , functionConstraints
                                                     , setAttributes
                                                     ]
                        if model == model'
                           then return model'
                           else core model'
        -- Apply the function to every statement and fold it into the model
        folder :: (MonadFail m, MonadLog m)
               => Model -> (Model -> Declaration -> m Model) -> m Model
        folder m@Model { mStatements = stmts } f = foldM (applyToDecl f) m stmts
        -- Apply the function only to declarations
        applyToDecl f m (Declaration d) = f m d
        applyToDecl _ m _               = return m

-- | Update a declaration in a model.
updateDeclaration :: Declaration  -- ^ Old declaration to be removed.
                  -> Declaration  -- ^ New declaration to be inserted.
                  -> Model        -- ^ Model to be updated.
                  -> Model        -- ^ Updated model.
updateDeclaration d d' m@Model { mStatements = stmts }
  | d == d'   = m
  | otherwise = m { mStatements = Declaration d' : delete (Declaration d) stmts }

-- | Merge a list of constraints into a model.
mergeConstraints :: Model         -- ^ Model to be updated.
                 -> [Expression]  -- ^ Constraints to merge into the model.
                 -> Model         -- ^ Updated model with new constraints.
mergeConstraints m@Model { mStatements = stmts } cs
  = m { mStatements = map mergeConstraints' stmts }
  where mergeConstraints' (SuchThat cs') = SuchThat $ cs' `union` cs
        mergeConstraints' s              = s

-- | Make the attributes of function variables as constrictive as possible.
functionAttributes :: (MonadFail m, MonadLog m)
                   => Model       -- ^ Model as context.
                   -> Declaration -- ^ Statement to constrain.
                   -> m Model     -- ^ Possibly updated model.
functionAttributes m f@(FindOrGiven forg n d@DomainFunction{}) = do
  d' <- constrainFunctionDomain n d m
  let f' = FindOrGiven forg n d'
  return $ updateDeclaration f f' m
functionAttributes m _ = return m

-- | Constrain the domain of a function given the context of a model.
constrainFunctionDomain :: (MonadFail m, MonadLog m)
                        => Name                     -- ^ Name of the function.
                        -> Domain () Expression     -- ^ Current domain of the function.
                        -> Model                    -- ^ Context of the model.
                        -> m (Domain () Expression) -- ^ Possibly modified domain.
constrainFunctionDomain n d@DomainFunction{} m
  = surjectiveIsTotalBijective d  >>=
    totalInjectiveIsBijective     >>=
    definedForAllIsTotal n (suchThat m)
constrainFunctionDomain _ d _ = return d

-- | Extract the such that expressions from a model.
suchThat :: Model -> [Expression]
suchThat = foldr fromSuchThat [] . mStatements
  where fromSuchThat (SuchThat es) a = a `union` es
        fromSuchThat _             a = a

-- | If a function is surjective or bijective and its domain and codomain
--   are of equal size, then it is total and bijective.
surjectiveIsTotalBijective :: (MonadFail m, MonadLog m)
                           => Domain () Expression      -- ^ Domain of the function.
                           -> m (Domain () Expression)  -- ^ Possibly modified domain.
surjectiveIsTotalBijective d@(DomainFunction r (FunctionAttr s PartialityAttr_Partial j) from to)
  | j == JectivityAttr_Surjective || j == JectivityAttr_Bijective = do
    (fromSize, toSize) <- functionDomainSizes from to
    if fromSize == toSize
       then return $ DomainFunction r (FunctionAttr s PartialityAttr_Total JectivityAttr_Bijective) from to
       else return d
surjectiveIsTotalBijective d = return d

-- | If a function is total and injective, and its domain and codomain
--   are of equal size, then it is bijective.
totalInjectiveIsBijective :: (MonadFail m, MonadLog m)
                          => Domain () Expression
                          -> m (Domain () Expression)
totalInjectiveIsBijective d@(DomainFunction r (FunctionAttr s p@PartialityAttr_Total JectivityAttr_Injective) from to) = do
  (fromSize, toSize) <- functionDomainSizes from to
  if fromSize == toSize
     then return $ DomainFunction r (FunctionAttr s p JectivityAttr_Bijective) from to
     else return d
totalInjectiveIsBijective d = return d

-- | Calculate the sizes of the domain and codomain of a function.
functionDomainSizes :: (MonadFail m)
                    => Domain () Expression       -- ^ The function's domain.
                    -> Domain () Expression       -- ^ The function's codomain.
                    -> m (Expression, Expression) -- ^ The sizes of the two.
functionDomainSizes from to = do
  fromSize <- domainSizeOf from
  toSize   <- domainSizeOf to
  return (fromSize, toSize)

-- | If a function is defined for all values in its domain, then it is total.
definedForAllIsTotal :: (MonadFail m, MonadLog m)
                     => Name                      -- ^ Name of the function.
                     -> [Expression]              -- ^ Such that constraints.
                     -> Domain () Expression      -- ^ Domain of the function.
                     -> m (Domain () Expression)  -- ^ Possibly modified domain.
definedForAllIsTotal n cs (DomainFunction r (FunctionAttr s PartialityAttr_Partial j) from to)
  | any definedForAll cs
    = return $ DomainFunction r (FunctionAttr s PartialityAttr_Total j) from to
  where
        -- Is there at least one forAll expression that uses the variable?
        definedForAll (Op (MkOpAnd (OpAnd (Comprehension e gorcs))))
          = functionCalledWithParam e n gorcs from && -- the function in question is called with the correct generated parameter
            hasNoImplications e &&                    -- no implications to ignore domain values
            hasNoConditions gorcs                     -- no conditions to ignore domain values
        definedForAll _ = False
        -- Make sure that there are no implications in an expression
        hasNoImplications = not . any isImplies . universe
        isImplies (Op (MkOpImply OpImply{})) = True
        isImplies _                          = False
        -- Make sure that there are no conditions in a list of generators or conditions
        hasNoConditions = not . any isCondition
        isCondition Condition{} = True
        isCondition _           = False
definedForAllIsTotal _ _ d = return d

-- | Determine whether the given function is called with a value generated from its domain.
functionCalledWithParam :: Expression             -- ^ Expression being checked for the function call.
                        -> Name                   -- ^ Name of the function being called.
                        -> [GeneratorOrCondition] -- ^ Generated variables and conditions of the comprehension.
                        -> Domain () Expression   -- ^ Domain of the function.
                        -> Bool                   -- ^ Whether or not the function is called with a value generated from the domain.
functionCalledWithParam e n gorcs from = let funCalls = filter isFunctionCall $ universe e
                                             in any (functionCallsWithParam generatedVariables) funCalls
  where
        -- Get the names of the generated variables
        generatedVariables = mapMaybe getGeneratedName $ filter isGenerator gorcs
        isGenerator (Generator GenDomainNoRepr{}) = True
        isGenerator _                             = False
        getGeneratedName (Generator (GenDomainNoRepr (Single n') _)) = Just n'
        getGeneratedName _                                           = Nothing
        isFunctionCall (Op (MkOpRelationProj OpRelationProj{})) = True
        isFunctionCall _                                        = False
        -- Determine whether the function is called with a generated parameter
        functionCallsWithParam ps (Op (MkOpRelationProj (OpRelationProj (Reference n' _) args)))
          | n' == n = not $ null $ ps `intersect` mapMaybe getArgName (filter domainArg args)
        functionCallsWithParam _  _ = False
        domainArg (Just (Reference _ d)) = d `domainEquals` from
        domainArg _                      = False
        domainEquals (Just (DeclNoRepr _ _ d1 _)) d2 = d1 == d2
        domainEquals _                            _  = False
        getArgName (Just (Reference n' _)) = Just n'
        getArgName _                       = Nothing

-- | Add constraints that apply to function variables.
functionConstraints :: (MonadFail m, MonadLog m, NameGen m)
                    => Model        -- ^ Model as context.
                    -> Declaration  -- ^ Statement to constrain.
                    -> m Model      -- ^ Possibly updated constraints.
functionConstraints m f = addFunctionConstraints f m >>=
                          return . mergeConstraints m

-- | Add constraints on a function.
addFunctionConstraints :: (MonadFail m, MonadLog m, NameGen m)
                       => Declaration     -- ^ Function being constrained.
                       -> Model           -- ^ Model for context.
                       -> m [Expression]  -- ^ New constraints to add to the model.
addFunctionConstraints (FindOrGiven forg n d@(DomainFunction _ (FunctionAttr _ PartialityAttr_Total _) _ _)) _
  = return [ functionRangeGeqOne (mkReference forg n d)
           ]
addFunctionConstraints (FindOrGiven forg n d@(DomainSet _ _ f)) _
  = case f of
         -- Make sure that the nested function is total.
         DomainFunction _ (FunctionAttr _ PartialityAttr_Total _) _ _
           -> let name = "addFunctionConstraints_name"
                  setRef = mkReference forg n d
                  in return [ mkForAll [ name ] [ setRef ]
                              (functionRangeGeqOne $ mkReference forg name f)
                            ]
         _ -> return []
addFunctionConstraints _ _ = return []

-- | Expression constraining the range of a function to be >= 1.
functionRangeGeqOne :: Expression -- ^ Reference to the function
                    -> Expression -- ^ Generated constraint.
functionRangeGeqOne r
  = Op (MkOpGeq (OpGeq
      -- size of
      (Op (MkOpTwoBars (OpTwoBars
        -- range of function
        (Op (MkOpRange (OpRange r))))))
      (Constant (ConstantInt 1)))) 

-- | Make a reference to a given variable.
mkReference :: FindOrGiven -> Name -> Domain () Expression -> Expression
mkReference forg n d = Reference n (Just $ DeclNoRepr forg n d NoRegion)

-- | Make a forAll expression and place an expression inside it.
mkForAll :: [Name]        -- ^ Names to be used for the generated variables.
         -> [Expression]  -- ^ References to the domains for generating variables.
         -> Expression    -- ^ Expression to place in the forAll.
         -> Expression    -- ^ Generated forAll expression.
mkForAll ns rs e = let mkGen n r = Generator $ GenInExpr (Single n) r
                       gs = zipWith mkGen ns rs
                       in Op (MkOpAnd (OpAnd (Comprehension e gs)))

-- | Make the attributes of a set as constrictive as possible.
setAttributes :: (MonadFail m, MonadLog m, NameGen m)
              => Model        -- ^ Model as context.
              -> Declaration  -- ^ Statement to constrain.
              -> m Model      -- ^ Possibly updated model.
setAttributes m f@(FindOrGiven forg n d@DomainSet{}) = do
  d' <- setSizeFromConstraint n d m
  let f' = FindOrGiven forg n d'
  return $ updateDeclaration f f' m
setAttributes m _ = return m

-- | Constrain the size of a set from constraints on it.
setSizeFromConstraint :: (MonadFail m, MonadLog m, NameGen m)
                      => Name                     -- ^ Name of the set being worked on.
                      -> Domain () Expression     -- ^ Domain of the set to possibly modify.
                      -> Model                    -- ^ Model for context.
                      -> m (Domain () Expression) -- ^ Possibly modified set domain.
setSizeFromConstraint n d = foldM subsetMinSize d . filter isSubsetOfN . concatMap universe . suchThat
  where isSubsetOfN (Op (MkOpSubset   (OpSubset   _ (Reference n' _)))) = n == n'
        isSubsetOfN (Op (MkOpSubsetEq (OpSubsetEq _ (Reference n' _)))) = n == n'
        isSubsetOfN _ = False
        subsetMinSize d' (Op (MkOpSubset   (OpSubset   l _))) = minSizeFromFunction d' l
        subsetMinSize d' (Op (MkOpSubsetEq (OpSubsetEq l _))) = minSizeFromFunction d' l
        subsetMinSize d' _ = return d'

-- | Set the minimum size of a set based on it being a superset of the range of a total function
minSizeFromFunction :: (MonadFail m, MonadLog m, NameGen m)
                    => Domain () Expression     -- ^ Domain of the set for which to change to minimum size.
                    -> Expression               -- ^ Expression from which the minimum size is being inferred.
                    -> m (Domain () Expression) -- ^ Set domain with a possible change of its minimum size.
minSizeFromFunction d (Op (MkOpRange (OpRange r))) = do
  f <- getFunDom r
  case f of
       DomainFunction _ (FunctionAttr _ PartialityAttr_Total _) _ _
         -> return $ setSetMinSize (Constant $ ConstantInt 1) d
       _ -> return d
minSizeFromFunction d _ = return d

-- | Look for a function domain, allowing it to be generated in a comprehension.
getFunDom :: (MonadFail m, MonadLog m, NameGen m)
          => Expression
          -> m (Domain () Expression)
getFunDom (Reference _ (Just (DeclNoRepr _ _ d@DomainFunction{} _))) = return d
getFunDom (Reference _ (Just (InComprehension (GenInExpr _ e))))     = getFunDom e
getFunDom (Reference _ (Just (DeclNoRepr _ _ d@DomainSet{} _)))      = domFromSet d
  where domFromSet (DomainSet _ _ d') = return d'
        domFromSet d'                 = return d'
getFunDom e = domainOf e

-- | Set the minSize attribute of a set.
setSetMinSize :: Expression           -- ^ New minimum size to apply.
              -> Domain () Expression -- ^ Set domain to modify.
              -> Domain () Expression -- ^ Possibly modified set domain.
setSetMinSize n (DomainSet r (SetAttr s) d) = DomainSet r (minSetSize s) d
  where minSetSize :: SizeAttr Expression -> SetAttr Expression
        minSetSize SizeAttr_None                   = SetAttr $ SizeAttr_MinSize n
        minSetSize (SizeAttr_MinSize    s')        = SetAttr $ SizeAttr_MinSize $ mkMax n s'
        minSetSize (SizeAttr_MaxSize    s')        = SetAttr $ SizeAttr_MinMaxSize n s'
        minSetSize (SizeAttr_MinMaxSize minS maxS) = SetAttr $ SizeAttr_MinMaxSize (mkMax n minS) maxS
        minSetSize a                               = SetAttr a
setSetMinSize _ d = d

-- | Make a maximum expression between two expressions.
--   Two max expressions are merged into one.
--   The max between a value and a max adds the value to the max (if not present).
--   If the expressions are the same, no max is made and the value is returned.
mkMax :: Expression -> Expression -> Expression
mkMax (Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es1)))))
      (Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es2)))))
        = make opMax $ fromList $ es1 `union` es2
mkMax i m@(Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es)))))
          | i `elem` es = m
          | otherwise   = make opMax $ fromList $ i : es
mkMax m@(Op (MkOpMax (OpMax (AbstractLiteral (AbsLitMatrix _ es))))) i
          | i `elem` es = m
          | otherwise   = make opMax $ fromList $ i : es
mkMax i e | i == e      = e
          | otherwise   = make opMax $ fromList [ i, e ]
