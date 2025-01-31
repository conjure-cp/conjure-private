{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Permutation where

import Conjure.Rules.Import
import Conjure.Util.Permutation (fromCycles, size, toCycles, toFunction)

rule_Cardinality_Literal :: Rule
rule_Cardinality_Literal = "permutation-cardinality-literal" `namedRule` theRule
  where
    theRule p' = do
      p <- match opTwoBars p'
      (TypePermutation _, elems) <- match permutationLiteral p
      let i' = Constant . ConstantInt TagInt . fromIntegral . size <$> fromCycles elems
      case i' of
        Left er -> failDoc $ "Permutation literal invalid." <++> stringToDoc (show er)
        Right i ->
          return
            ( "Horizontal rule for permutation cardinality, AsFunction representation.",
              do
                return [essence| &i |]
            )

rule_Defined_Literal :: Rule
rule_Defined_Literal = "permutation-defined-literal" `namedRule` theRule
  where
    theRule p' = do
      p <- match opDefined p'
      (TypePermutation _, elems) <- match permutationLiteral p
      let i' = AbstractLiteral . AbsLitSet . nub . join . toCycles <$> fromCycles elems
      case i' of
        Left er -> failDoc $ "Permutation literal invalid." <++> stringToDoc (show er)
        Right i ->
          return
            ( "Horizontal rule for permutation defined, AsFunction representation.",
              do
                return [essence| &i |]
            )

rule_Equality :: Rule
rule_Equality = "permutation-equality" `namedRule` theRule
  where
    theRule e = do
      (p, q) <- match opEq e
      TypePermutation {} <- typeOf p
      TypePermutation {} <- typeOf q
      return
        ( "Horizontal rule for permutation equality",
          return [essence| toSet(&p) = toSet(&q) |]
        )

rule_Disequality :: Rule
rule_Disequality = "permutation-disequality" `namedRule` theRule
  where
    theRule e = do
      (p, q) <- match opNeq e
      TypePermutation {} <- typeOf p
      TypePermutation {} <- typeOf q
      return
        ( "Horizontal rule for permutation disequality",
          return [essence| toSet(&p) != toSet(&q) |]
        )

rule_Comprehension :: Rule
rule_Comprehension = "permutation-comprehension" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, perm), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr pat expr) -> return (pat, matchDefs [opToSet] expr)
        _ -> na "rule_Comprehension"
      (TypePermutation inner, elems) <- match permutationLiteral perm
      DomainPermutation _ _ innerD <- domainOf perm
      let f' = toFunction <$> fromCycles elems
      case f' of
        Left er -> failDoc $ "Permutation literal invalid." <++> stringToDoc (show er)
        Right f -> do
          let outLiteral =
                make
                  matrixLiteral
                  (TypeMatrix (TypeInt TagInt) (TypeTuple [inner, inner]))
                  innerD
                  [ AbstractLiteral
                      ( AbsLitTuple
                          [ de,
                            f de
                          ]
                      )
                    | de <- join elems
                  ]
          return
            ( "Horizontal rule for permutation-comprehension",
              do
                return
                  $ Comprehension body
                  $ gocBefore
                  ++ [ Generator (GenInExpr pat [essence| &outLiteral|])
                     ]
                  ++ gocAfter
            )
    theRule _ = na "rule_Comprehension"

rule_In :: Rule
rule_In = "permutation-in" `namedRule` theRule
  where
    theRule p = do
      (x, s) <- match opIn p
      TypePermutation {} <- typeOf s
      -- do not apply this rule to quantified variables
      -- or else we might miss the opportunity to apply a more specific vertical rule
      when (referenceToComprehensionVar s) $ na "rule_In"
      return
        ( "Horizontal rule for permutation-in.",
          do
            (iPat, i) <- quantifiedVar
            return [essence| exists &iPat in &s . &i = &x |]
        )

rule_Permutation_Inverse :: Rule
rule_Permutation_Inverse = "permutation-inverse" `namedRule` theRule
  where
    theRule [essence| inverse(&p1, &p2)|] = do
      TypePermutation {} <- typeOf p1
      TypePermutation {} <- typeOf p2
      return
        ( "Horizontal rule for permutation-inverse",
          do
            (iPat, i) <- quantifiedVar
            return
              [essence|
                        (forAll &iPat in &p1 . image(&p2,&i[2]) = &i[1])
                            /\
                        (forAll &iPat in &p2 . image(&p1,&i[2]) = &i[1])
                      |]
        )
    theRule _ = na "rule_Permutation_Inverse"

rule_Compose_Image :: Rule
rule_Compose_Image = "permutation-compose-image" `namedRule` theRule
  where
    theRule [essence| image(compose(&g, &h),&i) |] = do
      TypePermutation innerG <- typeOf g
      TypePermutation innerH <- typeOf g
      typeI <- typeOf i
      if let ?typeCheckerMode = StronglyTyped in typesUnify [innerG, innerH, typeI]
        then
          return
            ( "Horizontal rule for image of permutation composition",
              do
                return [essence| image(&g, image(&h,&i)) |]
            )
        else na "rule_Compose_Image"
    theRule _ = na "rule_Compose_Image"

rule_Image_Literal :: Rule
rule_Image_Literal = "permutation-image-literal" `namedRule` theRule
  where
    theRule [essence| image(&p, &i) |] = do
      (TypePermutation inner, elems) <- match permutationLiteral p
      typeI <- typeOf i
      let f' = toFunction <$> fromCycles elems
      case f' of
        Left er -> failDoc $ "Permutation literal invalid." <++> stringToDoc (show er)
        Right f -> do
          if let ?typeCheckerMode = StronglyTyped in typesUnify [inner, typeI]
            then do
              let srtdel = sort (join elems)
                  indexr =
                    (\x -> [essence| sum(&x) |])
                      ( fromList
                          ( (\(n, q) -> [essence| toInt(&q = &i) * &n |])
                              <$> zip [1 ..] srtdel
                          )
                      )
                  matIdx =
                    mkDomainIntB
                      (fromInt 0)
                      (fromInt (fromIntegral (length srtdel)))
                  matLit =
                    make
                      matrixLiteral
                      (TypeMatrix (TypeInt TagInt) inner)
                      matIdx
                      ([essence| &i |] : (f <$> srtdel))
              iDomain <- domainOf i
              minval <- minOfDomain iDomain
              return
                ( "Horizontal rule for permutation literal application to a single value (image), AsFunction representation",
                  do
                    return [essence| catchUndef(&matLit[&indexr], &minval) |]
                )
            else failDoc "Permutation applied to a type its inner does not unify with"
    theRule _ = na "rule_Image_Literal"
