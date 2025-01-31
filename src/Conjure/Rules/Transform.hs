{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Transform (rules_Transform) where

import Conjure.Rules.Import

-- import Conjure.Rules.Vertical.Variant (onTagged)

rules_Transform :: [Rule]
rules_Transform =
  [ rule_Transform_DotLess_matrix,
    rule_Transform_DotLess_function,
    rule_Transform_DotLess_set,
    rule_Transform_DotLess_relation,
    -- rule_Transform_Sequence_Literal,
    rule_Transform_FunctionImage,
    rule_Transform_Tuple,
    rule_Transform_Functorially,
    rule_Transform_Comprehension,
    rule_Transform_Product_Types,
    rule_Transform_Matrix,
    rule_Transform_Partition,
    rule_Transform_Sequence,
    rule_Transform_Sequence_Defined,
    rule_Transformed_Indexing,
    rule_Lift_Transformed_Indexing,
    rule_Transform_Indexing,
    rule_TransformToImage,
    rule_Transform_Unifying
    -- rule_Transform_Variant_Literal,
    -- rule_Transform_Variant_Eq,
    -- rule_Transform_Variant_Neq,
    -- rule_Transform_Variant_Lt,
    -- rule_Transform_Variant_Leq,
    -- rule_Transformed_Variant_Index,
    -- rule_Transformed_Variant_Active
  ]

rule_Transform_DotLess_matrix :: Rule
rule_Transform_DotLess_matrix = "transform-dotless" `namedRule` theRule
  where
    theRule p
      | Just (x, rhs) <- match opDotLeq p <|> match opDotLt p,
        Just (ps, y) <- match opTransform rhs,
        x == y = do
          let mk = case match opDotLeq p of Just _ -> make opDotLeq; Nothing -> make opDotLt
          TypeMatrix {} <- typeOf x
          -- traceM $ show $ "rule_Transform_DotLess 1" <+> pretty x
          xIndices <- indexDomainsOf x
          -- traceM $ show $ "rule_Transform_DotLess 2" <+> vcat (map pretty xIndices)
          case xIndices of
            [xInd] ->
              return
                ( "",
                  do
                    (iPat, i) <- quantifiedVar
                    let transformed_i = make opTransform (map (make opPermInverse) ps) i
                    let transformed_x_i = make opTransform ps [essence| &x[&transformed_i] |]
                    return $ mk x [essence| [ &transformed_x_i | &iPat : &xInd ] |]
                )
            [xInd1, xInd2] ->
              return
                ( "",
                  do
                    (iPat1, i1) <- quantifiedVar
                    (iPat2, i2) <- quantifiedVar
                    let transformed_i1 = make opTransform (map (make opPermInverse) ps) i1
                    let transformed_i2 = make opTransform (map (make opPermInverse) ps) i2
                    let transformed_x_i1_i2 = make opTransform ps [essence| &x[&transformed_i1, &transformed_i2] |]
                    return
                      $ mk
                        [essence| [ &x[&i1, &i2] | &iPat1 : &xInd1 , &iPat2 : &xInd2 ] |]
                        [essence| [ &transformed_x_i1_i2 | &iPat1 : &xInd1 , &iPat2 : &xInd2 ] |]
                )
            _ -> na "rule_Transform_DotLess"
    theRule _ = na "rule_Transform_DotLess"

rule_Transform_DotLess_function :: Rule
rule_Transform_DotLess_function = "transform-dotless-function" `namedRule` theRule
  where
    theRule p
      | Just (x, rhs) <- match opDotLeq p <|> match opDotLt p,
        Just (ps, y) <- match opTransform rhs,
        x == y = do
          let mk :: Expression -> Expression -> Expression = case match opDotLeq p of Just _ -> make opDotLeq; Nothing -> make opDotLt
          TypeFunction {} <- typeOf x
          -- traceM $ show $ "rule_Transform_DotLess 1 x" <+> pretty x
          domain_x@(DomainFunction _ _ _fr _to) <- domainOf x
          -- traceM $ show $ "rule_Transform_DotLess 2 fr" <+> pretty fr
          -- traceM $ show $ "rule_Transform_DotLess 2 to" <+> pretty to

          return
            ( "",
              do
                (auxName, x') <- auxiliaryVar
                (iPat, i) <- quantifiedVar

                let lhs1_inner = make opTransform ps [essence| &i[1] |]
                let lhs1 = make opImage x' lhs1_inner
                let rhs1 = make opTransform ps [essence| &i[2] |]

                let lhs2_inner = make opTransform (map (make opPermInverse) ps) [essence| &i[1] |]
                let lhs2 = make opImage x lhs2_inner
                let rhs2 = make opTransform (map (make opPermInverse) ps) [essence| &i[2] |]

                return
                  $ WithLocals
                    (mk x x')
                    ( AuxiliaryVars
                        [ Declaration (FindOrGiven LocalFind auxName domain_x),
                          SuchThat
                            [ [essence| forAll &iPat in &x . &lhs1 = &rhs1 |],
                              [essence| forAll &iPat in &x' . &lhs2 = &rhs2 |]
                            ]
                        ]
                    )
            )
    -- na ""

    -- x : function T --> U
    -- x' : function T --> U
    -- such that forAll (t,u) in x . x'(transform(ps, t)) = transform(ps, u)
    -- such that forAll (t,u) in x' . x(transform(permInverse(ps), t)) = transform(permInverse(ps), u)

    -- x: set/mset/func...
    -- forAll i : innerDomainOf(x) . INSIDE-LHS = INSIDE-RHS

    -- INSIDE-LHS
    -- set: i in x
    -- mset: freq(i, x)
    -- function: i in x -- same as (x[i[0]] = i[1])
    -- partition: i in parts(x)

    -- INSIDE-RHS
    -- set: transform(ps, i) in x'
    -- mset: freq(transform(ps, i), x')
    -- function: transform(ps, i) in x'
    -- relation: same as func
    -- partition: transform(ps, i) in parts(x')

    -- x, x' : set of T
    -- set: forAll i :  . i in x <-> transform(ps, i) in x'
    -- mset: forAll i : T . i in x <-> transform(ps, i) in x'

    -- such that forAll t in x . transform(ps, t) in x'
    -- such that forAll t in x' . transform(permInverse(ps), t) in x

    -- x, x' : mset of T
    -- such that forAll t in x . freq(transform(ps, t), x') = freq(t, x)
    -- such that forAll t in x' . freq(transform(permInverse(ps), t), x) = freq(t, x')

    -- x, x' : relation (A,B,C)
    -- such that forAll entry in x . transform(ps, entry) in x'
    -- such that forAll entry in x' . transform(permInverse(ps), entry) in x

    -- x, x' : partition of set of T
    -- such that forAll i1, i2 : set of T . together({i1, i2}, x) <-> together({transform(ps, x), transform(ps, y)}, x')

    theRule _ = na "rule_Transform_DotLess"

rule_Transform_DotLess_set :: Rule
rule_Transform_DotLess_set = "transform-dotless-set" `namedRule` theRule
  where
    theRule p
      | Just (x, rhs) <- match opDotLeq p <|> match opDotLt p,
        Just (ps, y) <- match opTransform rhs,
        x == y = do
          let mk :: Expression -> Expression -> Expression = case match opDotLeq p of Just _ -> make opDotLeq; Nothing -> make opDotLt
          TypeSet {} <- typeOf x
          domain_x@DomainSet {} <- domainOf x

          return
            ( "",
              do
                (auxName, x') <- auxiliaryVar
                (iPat, i) <- quantifiedVar

                let transform_i = make opTransform ps i
                let transform_i' = make opTransform (map (make opPermInverse) ps) i

                return
                  $ WithLocals
                    (mk x x')
                    ( AuxiliaryVars
                        [ Declaration (FindOrGiven LocalFind auxName domain_x),
                          SuchThat
                            [ [essence| forAll &iPat in &x . &transform_i in &x' |],
                              [essence| forAll &iPat in &x' . &transform_i' in &x |]
                            ]
                        ]
                    )
            )
    theRule _ = na "rule_Transform_DotLess"

rule_Transform_DotLess_relation :: Rule
rule_Transform_DotLess_relation = "transform-dotless-relation" `namedRule` theRule
  where
    theRule p
      | Just (x, rhs) <- match opDotLeq p <|> match opDotLt p,
        Just (ps, y) <- match opTransform rhs,
        x == y = do
          let mk :: Expression -> Expression -> Expression = case match opDotLeq p of Just _ -> make opDotLeq; Nothing -> make opDotLt
          TypeRelation {} <- typeOf x
          domain_x@DomainRelation {} <- domainOf x

          return
            ( "",
              do
                (auxName, x') <- auxiliaryVar
                (iPat, i) <- quantifiedVar

                let transform_i = make opTransform ps i
                let transform_i' = make opTransform (map (make opPermInverse) ps) i

                return
                  $ WithLocals
                    (mk x x')
                    ( AuxiliaryVars
                        [ Declaration (FindOrGiven LocalFind auxName domain_x),
                          SuchThat
                            [ [essence| forAll &iPat in &x . &transform_i in &x' |],
                              [essence| forAll &iPat in &x' . &transform_i' in &x |]
                            ]
                        ]
                    )
            )
    theRule _ = na "rule_Transform_DotLess"

rule_Transform_DotLess_rest :: Rule
rule_Transform_DotLess_rest = "transform-dotless" `namedRule` theRule
  where
    theRule p
      | Just (x, rhs) <- match opDotLeq p <|> match opDotLt p,
        Just (ps, y) <- match opTransform rhs,
        x == y = do
          let mk = case match opDotLeq p of Just _ -> make opDotLeq; Nothing -> make opDotLt
          TypeFunction {} <- typeOf x
          -- traceM $ show $ "rule_Transform_DotLess 1" <+> pretty x
          xIndices <- indexDomainsOf x
          -- traceM $ show $ "rule_Transform_DotLess 2" <+> vcat (map pretty xIndices)
          case xIndices of
            [xInd] ->
              -- x .<= transform(ps, x)

              -- x .<= [ transform(ps, x[transform(permInverse(ps), i)]) | i : indexOf(x) ]

              -- x : function T --> U
              -- x' : function T --> U
              -- such that forAll (t,u) in x . x'(transform(ps, t)) = transform(ps, u)
              -- such that forAll (t,u) in x' . x(transform(permInverse(ps), t)) = transform(permInverse(ps), u)

              -- x: set/mset/func...
              -- forAll i : innerDomainOf(x) . INSIDE-LHS = INSIDE-RHS

              -- INSIDE-LHS
              -- set: i in x
              -- mset: freq(i, x)
              -- function: i in x -- same as (x[i[0]] = i[1])
              -- partition: i in parts(x)

              -- INSIDE-RHS
              -- set: transform(ps, i) in x'
              -- mset: freq(transform(ps, i), x')
              -- function: transform(ps, i) in x'
              -- relation: same as func
              -- partition: transform(ps, i) in parts(x')

              -- x, x' : set of T
              -- set: forAll i :  . i in x <-> transform(ps, i) in x'
              -- mset: forAll i : T . i in x <-> transform(ps, i) in x'

              -- such that forAll t in x . transform(ps, t) in x'
              -- such that forAll t in x' . transform(permInverse(ps), t) in x

              -- x, x' : mset of T
              -- such that forAll t in x . freq(transform(ps, t), x') = freq(t, x)
              -- such that forAll t in x' . freq(transform(permInverse(ps), t), x) = freq(t, x')

              -- x, x' : relation (A,B,C)
              -- such that forAll entry in x . transform(ps, entry) in x'
              -- such that forAll entry in x' . transform(permInverse(ps), entry) in x

              -- x, x' : partition of set of T
              -- such that forAll i1, i2 : set of T . together({i1, i2}, x) <-> together({transform(ps, x), transform(ps, y)}, x')

              return
                ( "",
                  do
                    (iPat, i) <- quantifiedVar
                    let transformed_i = make opTransform (map (make opPermInverse) ps) i
                    let transformed_x_i = make opTransform ps [essence| &x[&transformed_i] |]
                    return $ mk x [essence| [ &transformed_x_i | &iPat : &xInd ] |]
                )
            [xInd1, xInd2] ->
              return
                ( "",
                  do
                    (iPat1, i1) <- quantifiedVar
                    (iPat2, i2) <- quantifiedVar
                    let transformed_i1 = make opTransform (map (make opPermInverse) ps) i1
                    let transformed_i2 = make opTransform (map (make opPermInverse) ps) i2
                    let transformed_x_i1_i2 = make opTransform ps [essence| &x[&transformed_i1, &transformed_i2] |]
                    return
                      $ mk
                        [essence| [ &x[&i1, &i2] | &iPat1 : &xInd1 , &iPat2 : &xInd2 ] |]
                        [essence| [ &transformed_x_i1_i2 | &iPat1 : &xInd1 , &iPat2 : &xInd2 ] |]
                )
            _ -> na "rule_Transform_DotLess"
    theRule _ = na "rule_Transform_DotLess"

-- transform(p, x)[i] ~~> transform(p, x[transform(permInverse(p), i)])
-- transform(p, f)[x] ~~> transform(p, f[transform(permInverse(p), x)])
-- image(transform(p, f), x) ~~> transform(p, image(f, transform(permInverse(p), x)))
rule_Transform_FunctionImage :: Rule
rule_Transform_FunctionImage = "transform-function-image" `namedRule` theRule
  where
    theRule [essence| image(transform([&p], &f), &x)  |] = do
      return ("", return [essence| transform([&p], image(&f, transform([permInverse(&p)], &x))) |])
    theRule _ = na "rule_Transform_FunctionImage"

-- transform(p, x)[i] ~~> transform(p, x[transform(permInverse(p), i)])
-- transform(p, f)[x] ~~> transform(p, f[transform(permInverse(p), x)])
-- image(transform(p, f), x) ~~> transform(p, image(f, transform(permInverse(p), x)))
rule_Transform_Tuple :: Rule
rule_Transform_Tuple = "transform-tuple" `namedRule` theRule
  where
    theRule p
      | Just (ps, tup) <- match opTransform p,
        Just (TypeTuple tup_types) <- typeOf tup =
          return
            ( "",
              return
                $ AbstractLiteral
                $ AbsLitTuple
                  [ make opTransform ps [essence| &tup[&i] |]
                    | iInt <- take (length tup_types) allNats,
                      let i = fromInt iInt
                  ]
            )
    theRule _ = na "rule_Transform_Tuple"

rule_Transform_Functorially :: Rule
rule_Transform_Functorially = "transform-functorially" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) ->
          return (pat, matchDefs [opToSet, opToMSet] expr)
        _ -> na "rule_Transform_Functorially"
      (morphisms, y) <- match opTransform x
      return
        ( "Horizontal rule for transform of functorially",
          do
            (dPat, d) <- quantifiedVar
            return
              ( Comprehension body
                  $ gocBefore
                  ++ [Generator (GenInExpr dPat y)]
                  ++ ( ComprehensionLetting
                         (Single pat)
                         (make opTransform morphisms d)
                         : gocAfter
                     )
              )
        )
    theRule _ = na "rule_Transform_Functorially"

rule_Transform_Comprehension :: Rule
rule_Transform_Comprehension = "transform-comprehension" `namedRule` theRule
  where
    theRule x = do
      ([morphism], cmp@(Comprehension body gensOrConds)) <- match opTransform x
      ty <- typeOf cmp
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then
          return
            ( "Horizontal rule for transform comprehension",
              do
                gox <- mapM (transformOverGenOrCond morphism) gensOrConds
                return $ Comprehension [essence| transform([&morphism], &body) |] (join gox)
            )
        else na "rule_Transform_Comprehension"
    transformOverGenOrCond m (Generator g) = transformOverGenerator m g
    transformOverGenOrCond m (Condition e) =
      return [Condition [essence| transform([&m], &e) |]]
    transformOverGenOrCond m (ComprehensionLetting pat e) =
      return [ComprehensionLetting pat [essence| transform([&m], &e) |]]

    transformOverGenerator m (GenDomainHasRepr a d) = do
      (Single nm, n) <- quantifiedVarOverDomain $ forgetRepr d
      return
        [ Generator (GenDomainHasRepr nm d),
          ComprehensionLetting (Single a) [essence| transform([&m], &n) |]
        ]
    transformOverGenerator m (GenInExpr a e) =
      return [Generator (GenInExpr a [essence| transform([&m], &e) |])]
    transformOverGenerator m (GenDomainNoRepr absPat d) = do
      (rPat, ns) <- clonePattern absPat
      return
        $ Generator (GenDomainNoRepr rPat d)
        : ( ( \(pat, exp) ->
                ComprehensionLetting (Single pat) [essence| transform([&m], &exp) |]
            )
              <$> ns
          )

    clonePattern (Single name) = do
      (nPat, n) <- quantifiedVar
      return (nPat, [(name, n)])
    clonePattern (AbsPatTuple pats) = do
      rec <- mapM clonePattern pats
      return
        ( AbsPatTuple $ fst <$> rec,
          snd =<< rec
        )
    clonePattern (AbsPatMatrix pats) = do
      rec <- mapM clonePattern pats
      return
        ( AbsPatMatrix $ fst <$> rec,
          snd =<< rec
        )
    clonePattern (AbsPatSet pats) = do
      rec <- mapM clonePattern pats
      return
        ( AbsPatSet $ fst <$> rec,
          snd =<< rec
        )
    clonePattern _ =
      bug "rule_Transform_Comprehension: clonePattern: unsupported Abstract Pattern"

rule_Transform_Product_Types :: Rule
rule_Transform_Product_Types = "transform-product-types" `namedRule` theRule
  where
    theRule [essence| transform([&morphism], &i) |] = do
      inn <- morphing =<< typeOf morphism
      ti <- typeOf i
      if let ?typeCheckerMode = StronglyTyped in ti `containsProductType` inn
        then case ti of
          (TypeTuple tint) -> do
            let tupleIndexTransform indx =
                  let indexexpr = Constant (ConstantInt TagInt indx)
                   in [essence| transform([&morphism], &i[&indexexpr]) |]
                tupleExpression =
                  AbstractLiteral
                    $ AbsLitTuple (tupleIndexTransform <$> [1 .. (fromIntegral $ length tint)])
            return
              ( "Horizontal rule for transform of tuple",
                return tupleExpression
              )
          (TypeRecord namet) -> do
            let recordIndexTransform indx =
                  let indexexpr =
                        Reference (fst indx)
                          $ Just
                          $ uncurry RecordField indx
                   in (fst indx, [essence| transform([&morphism], &i[&indexexpr]) |])
                recordExpression =
                  AbstractLiteral
                    $ AbsLitRecord
                    $ recordIndexTransform
                    <$> namet
            return
              ( "Horizontal rule for transform of record",
                return recordExpression
              )
          _ -> bug "rule_Transform_Product_Types this is a bug"
        else na "rule_Transform_Product_Types"
    theRule _ = na "rule_Transform_Product_Types"

rule_Transform_Matrix :: Rule
rule_Transform_Matrix = "transform-matrix" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, exp), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
        _ -> na "rule_Transform_Matrix"
      ([morphism], matexp) <- match opTransform exp
      DomainMatrix domIndx _ <- domainOf matexp
      ty <- typeOf matexp
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then
          return
            ( "Horizontal rule for transform matrix in comprehension generator",
              do
                (dPat, d) <- quantifiedVar
                (Single mName, m) <- quantifiedVar
                (Single iName, i) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [Generator (GenDomainNoRepr dPat (forgetRepr domIndx))]
                      ++ [ComprehensionLetting (Single iName) [essence| transform([&morphism], &d) |]]
                      ++ [ComprehensionLetting (Single mName) [essence| &matexp[&i] |]]
                      ++ [ComprehensionLetting (Single pat) [essence| transform([&morphism], &m) |]]
                      ++ gocAfter
                  )
            )
        else na "rule_Transform_Matrix"
    theRule _ = na "rule_Transform_Matrix"

rule_Transform_Partition :: Rule
rule_Transform_Partition = "transform-partition" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
        _ -> na "rule_Transform_Partition"
      z <- match opParts x
      ([morphism], y) <- match opTransform z
      ty <- typeOf y
      case ty of TypePartition {} -> return (); _ -> na "only applies to partitions"
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transform of partition",
              do
                (dPat, d) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [Generator (GenInExpr dPat [essence| parts(&y) |])]
                      ++ (ComprehensionLetting (Single pat) [essence| transform([&morphism], &d) |] : gocAfter)
                  )
            )
        else na "rule_Transform_Partition"
    theRule _ = na "rule_Transform_Partition"

rule_Transform_Sequence :: Rule
rule_Transform_Sequence = "transform-sequence" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) ->
          return (pat, matchDefs [opToSet, opToMSet] expr)
        _ -> na "rule_Transform_Sequence"
      ([morphism], y) <- match opTransform x
      ty <- typeOf y
      case ty of TypeSequence {} -> return (); _ -> na "only applies to sequences"
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transform of sequence",
              do
                (dPat, d) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [Generator (GenInExpr dPat y)]
                      ++ ( ComprehensionLetting
                             (Single pat)
                             [essence| (&d[1], transform([&morphism], &d[2])) |]
                             : gocAfter
                         )
                  )
            )
        else na "rule_Transform_Sequence"
    theRule _ = na "rule_Transform_Sequence"

rule_Transform_Sequence_Defined :: Rule
rule_Transform_Sequence_Defined = "transform-sequence-defined" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr pat@Single {} expr) ->
          return (pat, matchDefs [opToSet, opToMSet] expr)
        _ -> na "rule_Transform_Sequence_Defined"
      defi <- match opDefined x
      ([morphism], y) <- match opTransform defi
      ty <- typeOf y
      case ty of TypeSequence {} -> return (); _ -> na "only applies to sequences"
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transform of sequence defined",
              do
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [Generator (GenInExpr pat [essence| defined(&y) |])]
                      ++ gocAfter
                  )
            )
        else na "rule_Transform_Sequence_Defined"
    theRule _ = na "rule_Transform_Sequence_Defined"

rule_Transformed_Indexing :: Rule
rule_Transformed_Indexing = "transformed-indexing" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, exp), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
        _ -> na "rule_Transformed_Indexing"
      (matexp, indexer) <- match opIndexing exp
      TypeMatrix {} <- typeOf matexp
      ([morphism], mat) <- match opTransform matexp
      ty <- typeOf mat
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transformed indexing",
              do
                (Single mName, m) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [ComprehensionLetting (Single mName) [essence| &matexp[&indexer] |]]
                      ++ [ComprehensionLetting (Single pat) [essence| transform([&morphism], &m) |]]
                      ++ gocAfter
                  )
            )
        else na "rule_Transformed_Indexing"
    theRule _ = na "rule_Transformed_Indexing"

rule_Lift_Transformed_Indexing :: Rule
rule_Lift_Transformed_Indexing = "lift-transformed-indexing" `namedRule` theRule
  where
    theRule [essence| transform([&p], &x)[&i] |] = do
      TypePermutation {} <- typeOf p
      return
        ( "transformed indexing",
          return [essence| transform([&p], &x[transform([permInverse(&p)], &i)]) |]
        )
    theRule _ = na "rule_Lift_Transformed_Indexing"

rule_Transform_Indexing :: Rule
rule_Transform_Indexing = "transform-indexing" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr pat expr) -> return (pat, expr)
        _ -> na "rule_Transform_Indexing"
      ([morphism], matexp) <- match opTransform expr
      (mat, indexer) <- match opIndexing matexp
      TypeMatrix {} <- typeOf mat
      ty <- typeOf mat
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transform indexing",
              do
                (Single mName, m) <- quantifiedVar
                (Single iName, i) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [ComprehensionLetting (Single iName) [essence| transform([&morphism], &indexer) |]]
                      ++ [ComprehensionLetting (Single mName) [essence| &mat[&i] |]]
                      ++ [Generator (GenInExpr pat [essence| transform([&morphism], &m) |])]
                      ++ gocAfter
                  )
            )
        else na "rule_Transform_Indexing"
    theRule _ = na "rule_Transform_Indexing"

rule_TransformToImage :: Rule
rule_TransformToImage = "transform-to-image" `namedRule` theRule
  where
    -- transform([f], i) ~~> image(f, i) if the types match
    theRule [essence| transform([&morphism], &i) |] = do
      inner <- morphing =<< typeOf morphism
      typeI <- typeOf i
      if typesUnify [inner, typeI]
        then
          return
            ( "Horizontal rule for transform unifying",
              return [essence| image(&morphism, &i) |]
            )
        else na "rule_Transform_Unifying"
    theRule _ = na "rule_Transform_Unifying"

rule_Transform_Unifying :: Rule
rule_Transform_Unifying = "transform-unifying" `namedRule` theRule
  where
    -- drop transforms that do not apply
    theRule p | Just (morphisms :: [Expression], i) <- match opTransform p = do
      typeI <- typeOf i
      morphisms' <- fmap catMaybes $ forM morphisms $ \morphism -> do
        inner <- morphing =<< typeOf morphism
        if containsType typeI inner
          then return (Just morphism)
          else return Nothing
      if length morphisms' == length morphisms
        then na "rule_Transform_Unifying" -- didn't drop anything
        else
          if null morphisms'
            then
              return
                ( "Horizontal rule for transform unifying -- none of them apply",
                  return i
                )
            else
              return
                ( "Horizontal rule for transform unifying -- some of them apply",
                  return $ make opTransform morphisms' i
                )
    theRule _ = na "rule_Transform_Unifying"

-- rule_Transform_Sequence_Literal :: Rule
-- rule_Transform_Sequence_Literal = "transform-sequence-literal" `namedRule` theRule
--   where
--     theRule p = do
--       _ <- match opTransform p
--       let (x, rx) = matchManyTransforms p
--       TypeSequence {} <- typeOf x
--       (_, as) <- match sequenceLiteral x
--       return
--         ( "Horizontal rule for transform sequence literal",
--           return $ AbstractLiteral $ AbsLitSequence $ rx <$> as
--         )

-- rule_Transform_Variant_Literal :: Rule
-- rule_Transform_Variant_Literal = "transform-variant-literal" `namedRule` theRule
--   where
--     theRule p = do
--       _ <- match opTransform p
--       let (x, rx) = matchManyTransforms p
--       case x of
--         AbstractLiteral (AbsLitVariant d n a) ->
--           return
--             ( "Horizontal rule for transform variant literal",
--               return $ AbstractLiteral $ AbsLitVariant d n $ rx a
--             )
--         _ -> na "rule_Transform_Variant_Literal"

-- atLeastOneTransform :: (MonadFailDoc m) => (Expression, Expression) -> m ()
-- atLeastOneTransform (l, r) = do
--   case (match opTransform l, match opTransform r) of
--     (Nothing, Nothing) -> na "no transforms on either side"
--     _ -> return ()

-- matchManyTransforms ::
--   Expression ->
--   (Expression, Expression -> Expression)
-- matchManyTransforms exp =
--   case match opTransform exp of
--     Nothing -> (exp, id)
--     Just ([morphism], so) ->
--       let (nexp, ntrans) = matchManyTransforms so
--        in ( nexp,
--             \x -> let nx = ntrans x in [essence| transform([&morphism], &nx) |]
--           )
--     _ -> bug "matchManyTransforms"

-- rule_Transform_Variant_Eq :: Rule
-- rule_Transform_Variant_Eq = "transform-variant-eq" `namedRule` theRule
--   where
--     theRule p = do
--       (l, r) <- match opEq p
--       atLeastOneTransform (l, r)
--       let (x, rx) = matchManyTransforms l
--       let (y, ry) = matchManyTransforms r
--       TypeVariant {} <- typeOf x
--       TypeVariant {} <- typeOf y
--       (xWhich : xs) <- downX1 x
--       (yWhich : ys) <- downX1 y
--       return
--         ( "Vertical rule for right transformed variant equality",
--           return
--             $ make opAnd
--             $ fromList
--               [ [essence| &xWhich = &yWhich |],
--                 onTagged (make opEq) xWhich (rx <$> xs) (ry <$> ys)
--               ]
--         )

-- rule_Transform_Variant_Neq :: Rule
-- rule_Transform_Variant_Neq = "transform-variant-neq" `namedRule` theRule
--   where
--     theRule p = do
--       (l, r) <- match opNeq p
--       atLeastOneTransform (l, r)
--       let (x, rx) = matchManyTransforms l
--       let (y, ry) = matchManyTransforms r
--       TypeVariant {} <- typeOf x
--       TypeVariant {} <- typeOf y
--       (xWhich : xs) <- downX1 x
--       (yWhich : ys) <- downX1 y
--       return
--         ( "Vertical rule for right transformed variant nequality",
--           return
--             $ make opOr
--             $ fromList
--               [ [essence| &xWhich != &yWhich |],
--                 make opAnd
--                   $ fromList
--                     [ [essence| &xWhich = &yWhich |],
--                       onTagged (make opNeq) xWhich (rx <$> xs) (ry <$> ys)
--                     ]
--               ]
--         )

-- rule_Transform_Variant_Lt :: Rule
-- rule_Transform_Variant_Lt = "transform-variant-lt" `namedRule` theRule
--   where
--     theRule p = do
--       (l, r) <- match opLt p
--       atLeastOneTransform (l, r)
--       let (x, rx) = matchManyTransforms l
--       let (y, ry) = matchManyTransforms r
--       TypeVariant {} <- typeOf x
--       TypeVariant {} <- typeOf y
--       (xWhich : xs) <- downX1 x
--       (yWhich : ys) <- downX1 y
--       return
--         ( "Vertical rule for right transformed variant less than",
--           return
--             $ make opOr
--             $ fromList
--               [ [essence| &xWhich < &yWhich |],
--                 make opAnd
--                   $ fromList
--                     [ [essence| &xWhich = &yWhich |],
--                       onTagged (make opLt) xWhich (rx <$> xs) (ry <$> ys)
--                     ]
--               ]
--         )

-- rule_Transform_Variant_Leq :: Rule
-- rule_Transform_Variant_Leq = "transform-variant-leq" `namedRule` theRule
--   where
--     theRule p = do
--       (l, r) <- match opLeq p
--       atLeastOneTransform (l, r)
--       let (x, rx) = matchManyTransforms l
--       let (y, ry) = matchManyTransforms r
--       TypeVariant {} <- typeOf x
--       TypeVariant {} <- typeOf y
--       (xWhich : xs) <- downX1 x
--       (yWhich : ys) <- downX1 y
--       return
--         ( "Vertical rule for right transformed variant less than eq",
--           return
--             $ make opOr
--             $ fromList
--               [ [essence| &xWhich < &yWhich |],
--                 make opAnd
--                   $ fromList
--                     [ [essence| &xWhich = &yWhich |],
--                       onTagged (make opLeq) xWhich (rx <$> xs) (ry <$> ys)
--                     ]
--               ]
--         )

-- rule_Transformed_Variant_Index :: Rule
-- rule_Transformed_Variant_Index = "transformed-variant-index" `namedRule` theRule
--   where
--     theRule p = do
--       (l, arg) <- match opIndexing p
--       atLeastOneTransform (l, l)
--       let (x, rx) = matchManyTransforms l
--       TypeVariant ds <- typeOf x
--       (xWhich : xs) <- downX1 x
--       name <- nameOut arg
--       argInt <-
--         case elemIndex name (map fst ds) of
--           Nothing -> failDoc "Variant indexing, not a member of the type."
--           Just argInt -> return argInt
--       return
--         ( "Variant indexing on:" <+> pretty p,
--           return
--             $ WithLocals
--               (rx (atNote "Variant indexing" xs argInt))
--               ( DefinednessConstraints
--                   [ [essence| &xWhich = &argInt2 |]
--                     | let argInt2 = fromInt (fromIntegral (argInt + 1))
--                   ]
--               )
--         )

-- rule_Transformed_Variant_Active :: Rule
-- rule_Transformed_Variant_Active = "transformed-variant-active" `namedRule` theRule
--   where
--     theRule p = do
--       (l, name) <- match opActive p
--       atLeastOneTransform (l, l)
--       let (x, _) = matchManyTransforms l
--       TypeVariant ds <- typeOf x
--       (xWhich : _) <- downX1 x
--       argInt <- case elemIndex name (map fst ds) of
--         Nothing -> failDoc "Variant indexing, not a member of the type."
--         Just argInt -> return $ fromInt $ fromIntegral $ argInt + 1
--       return
--         ( "Variant active on:" <+> pretty p,
--           return [essence| &xWhich = &argInt |]
--         )
