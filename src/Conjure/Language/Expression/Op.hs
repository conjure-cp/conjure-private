module Conjure.Language.Expression.Op
    ( module Conjure.Language.Expression.Op.Internal.Generated
    , module Conjure.Language.Expression.Op.Internal.Common
    , mkBinOp, mkOp
    , OpType (..)
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.AdHoc
import Conjure.Language.Pretty
import Conjure.Language.Lexer
import Conjure.Language.Expression.Op.Internal.Common ( simplifyOp
                                                      , Fixity(..), operators, functionals
                                                      , EssenceOperatorParsingDescr(..) )
import Conjure.Language.Expression.Op.Internal.Generated



mkBinOp :: (Op x :< x, ExpressionLike x) => Text -> x -> x -> x
mkBinOp op a b =
    case textToLexeme op of
        Nothing -> bug ("Unknown binary operator:" <+> pretty op)
        Just l  ->
            let
                f = case l of
                    L_Plus        -> \ x y -> inject $ MkOpSum         $ OpSum       $ fromList [x,y]
                    L_Minus       -> \ x y -> inject $ MkOpMinus       $ OpMinus       x y
                    L_Times       -> \ x y -> inject $ MkOpProduct     $ OpProduct $ fromList [x,y]
                    L_Div         -> \ x y -> inject $ MkOpDiv         $ OpDiv         x y
                    L_Mod         -> \ x y -> inject $ MkOpMod         $ OpMod         x y
                    L_Pow         -> \ x y -> inject $ MkOpPow         $ OpPow         x y
                    L_Eq          -> \ x y -> inject $ MkOpEq          $ OpEq          x y
                    L_Neq         -> \ x y -> inject $ MkOpNeq         $ OpNeq         x y
                    L_Lt          -> \ x y -> inject $ MkOpLt          $ OpLt          x y
                    L_Leq         -> \ x y -> inject $ MkOpLeq         $ OpLeq         x y
                    L_Gt          -> \ x y -> inject $ MkOpGt          $ OpGt          x y
                    L_Geq         -> \ x y -> inject $ MkOpGeq         $ OpGeq         x y
                    L_in          -> \ x y -> inject $ MkOpIn          $ OpIn          x y
                    L_And         -> \ x y -> inject $ MkOpAnd         $ OpAnd     $ fromList [x,y]
                    L_Or          -> \ x y -> inject $ MkOpOr          $ OpOr      $ fromList [x,y]
                    L_Imply       -> \ x y -> inject $ MkOpImply       $ OpImply       x y
                    L_Iff         -> \ x y -> inject $ MkOpIff         $ OpIff         x y
                    L_subset      -> \ x y -> inject $ MkOpSubset      $ OpSubset      x y
                    L_subsetEq    -> \ x y -> inject $ MkOpSubsetEq    $ OpSubsetEq    x y
                    L_supset      -> \ x y -> inject $ MkOpSupset      $ OpSupset      x y
                    L_supsetEq    -> \ x y -> inject $ MkOpSupsetEq    $ OpSupsetEq    x y
                    L_subsequence -> \ x y -> inject $ MkOpSubsequence $ OpSubsequence x y
                    L_substring   -> \ x y -> inject $ MkOpSubstring   $ OpSubstring   x y
                    L_intersect   -> \ x y -> inject $ MkOpIntersect   $ OpIntersect   x y
                    L_union       -> \ x y -> inject $ MkOpUnion       $ OpUnion       x y
                    L_LexLt       -> \ x y -> inject $ MkOpLexLt       $ OpLexLt       x y
                    L_LexLeq      -> \ x y -> inject $ MkOpLexLeq      $ OpLexLeq      x y
                    L_LexGt       -> \ x y -> inject $ MkOpLexLt       $ OpLexLt       y x
                    L_LexGeq      -> \ x y -> inject $ MkOpLexLeq      $ OpLexLeq      y x
                    L_DotLt       -> \ x y -> inject $ MkOpDotLt       $ OpDotLt       x y
                    L_DotLeq      -> \ x y -> inject $ MkOpDotLeq      $ OpDotLeq      x y
                    L_DotGt       -> \ x y -> inject $ MkOpDotLt       $ OpDotLt       y x
                    L_DotGeq      -> \ x y -> inject $ MkOpDotLeq      $ OpDotLeq      y x
                    L_TildeLt     -> \ x y -> inject $ MkOpTildeLt     $ OpTildeLt     x y
                    L_TildeLeq    -> \ x y -> inject $ MkOpTildeLeq    $ OpTildeLeq    x y
                    L_TildeGt     -> \ x y -> inject $ MkOpTildeLt     $ OpTildeLt     y x
                    L_TildeGeq    -> \ x y -> inject $ MkOpTildeLeq    $ OpTildeLeq    y x
                    _ -> bug ("Unknown lexeme for binary operator:" <+> pretty (show l))
            in
                f a b

data OpType
    = PrefixOp Lexeme
    | FactorialOp
    | TwoBarOp
    | FunctionOp Lexeme


mkOp :: (Op x :< x, ReferenceContainer x, ExpressionLike x) => OpType -> [x] -> x
mkOp op xs = case op of
  PrefixOp lex -> case lex of
        L_ExclamationMark -> inject $ MkOpNot       $ OpNot     (arg xs 0 "not")
        L_Minus    -> inject $ MkOpNegate    $ OpNegate  (arg xs 0 "negate")
        _ -> bug $ "Unexpected Prefix operator :" <+> pretty (show lex)
  FactorialOp -> inject $ MkOpFactorial    $ OpFactorial    (arg xs 0 "factorial")
  TwoBarOp -> inject $ MkOpTwoBars   $ OpTwoBars (arg xs 0 "twoBars")
  FunctionOp lex -> case lex of
            L_fAnd       -> inject $ MkOpAnd       $ OpAnd     (arg xs 0 "and")
            L_fOr       -> inject $ MkOpOr        $ OpOr      (arg xs 0 "or")
            L_fXor       -> inject $ MkOpXor       $ OpXor     (arg xs 0 "xor")
            L_Sum       -> inject $ MkOpSum       $ OpSum     (arg xs 0 "sum")
            L_Product   -> inject $ MkOpProduct   $ OpProduct (arg xs 0 "product")
            -- _     -> opImage (fromName (Name op)) xs
            L_true         -> inject $ MkOpTrue         $ OpTrue         (arg xs 0 "true")
            L_toInt        -> inject $ MkOpToInt        $ OpToInt        (arg xs 0 "toInt")
            L_makeTable    -> inject $ MkOpMakeTable    $ OpMakeTable    (arg xs 0 "makeTable")
            L_table        -> inject $ MkOpTable        $ OpTable        (arg xs 0 "table") (arg xs 1 "table")
            L_gcc          -> inject $ MkOpGCC          $ OpGCC          (arg xs 0 "gcc") (arg xs 1 "gcc") (arg xs 2 "gcc")
            L_atleast      -> inject $ MkOpAtLeast      $ OpAtLeast      (arg xs 0 "atleast") (arg xs 1 "atleast") (arg xs 2 "atleast")
            L_atmost       -> inject $ MkOpAtMost       $ OpAtMost       (arg xs 0 "atmost" ) (arg xs 1 "atmost" ) (arg xs 2 "atmost" )
            L_defined      -> inject $ MkOpDefined      $ OpDefined      (arg xs 0 "defined")
            L_range        -> inject $ MkOpRange        $ OpRange        (arg xs 0 "range")
            L_restrict     -> inject $ MkOpRestrict     $ OpRestrict     (arg xs 0 "restrict") (arg xs 1 "restrict")
            L_allDiff      -> inject $ MkOpAllDiff      $ OpAllDiff      (arg xs 0 "allDiff")
            L_alldifferent_except -> inject $ MkOpAllDiffExcept $ OpAllDiffExcept
                                                                         (arg xs 0 "allDiffExcept")
                                                                         (arg xs 1 "allDiffExcept")
            L_catchUndef   -> inject $ MkOpCatchUndef   $ OpCatchUndef   (arg xs 0 "catchUndef")
                                                                         (arg xs 1 "catchUndef")
            L_quickPermutationOrder -> inject $ MkOpQuickPermutationOrder $ OpQuickPermutationOrder (arg xs 0 "quickPermutationOrder" |> listOut |> fromMaybe (bug "")) (arg xs 1 "quickPermutationOrder")
            L_dontCare     -> inject $ MkOpDontCare     $ OpDontCare     (arg xs 0 "dontCare")
            L_toSet        -> inject $ MkOpToSet        $ OpToSet        False (arg xs 0 "toSet")
            L_toMSet       -> inject $ MkOpToMSet       $ OpToMSet       (arg xs 0 "toMSet")
            L_toRelation   -> inject $ MkOpToRelation   $ OpToRelation   (arg xs 0 "toRelation")
            L_max          -> inject $ MkOpMax          $ OpMax          (arg xs 0 "max")
            L_min          -> inject $ MkOpMin          $ OpMin          (arg xs 0 "min")
            L_image        -> inject $ MkOpImage        $ OpImage        (arg xs 0 "image")
                                                                         (arg xs 1 "image")
            L_transform    -> inject $ MkOpTransform    $ OpTransform    (arg xs 0 "transform" |> listOut |> fromMaybe (bug ""))
                                                                         (arg xs 1 "transform")

            L_imageSet     -> inject $ MkOpImageSet     $ OpImageSet     (arg xs 0 "imageSet")
                                                                         (arg xs 1 "imageSet")
            L_preImage     -> inject $ MkOpPreImage     $ OpPreImage     (arg xs 0 "preImage")
                                                                         (arg xs 1 "preImage")
            L_inverse      -> inject $ MkOpInverse      $ OpInverse      (arg xs 0 "inverse")
                                                                         (arg xs 1 "inverse")
            L_freq         -> inject $ MkOpFreq         $ OpFreq         (arg xs 0 "freq")
                                                                         (arg xs 1 "freq")
            L_hist         -> inject $ MkOpHist         $ OpHist         (arg xs 0 "hist")
            L_parts        -> inject $ MkOpParts        $ OpParts        (arg xs 0 "parts")
            L_together     -> inject $ MkOpTogether     $ OpTogether     (arg xs 0 "together")
                                                                         (arg xs 1 "together")
            L_apart        -> inject $ MkOpApart        $ OpApart        (arg xs 0 "apart")
                                                                         (arg xs 1 "apart")
            L_party        -> inject $ MkOpParty        $ OpParty        (arg xs 0 "party")
                                                                         (arg xs 1 "party")
            L_permInverse  -> inject $ MkOpPermInverse  $ OpPermInverse  (arg xs 0 "permInverse")
            L_participants -> inject $ MkOpParticipants $ OpParticipants (arg xs 0 "participants")
            L_compose      -> inject $ MkOpCompose      $ OpCompose      (arg xs 0 "compose")
                                                                         (arg xs 1 "compose")

            L_active       -> inject $ MkOpActive       $ OpActive       (arg xs 0 "active")
                                                                         (arg xs 1 "active" |> nameOut |> fromMaybe (bug "active"))
            L_pred         -> inject $ MkOpPred         $ OpPred         (arg xs 0 "pred")
            L_succ         -> inject $ MkOpSucc         $ OpSucc         (arg xs 0 "succ")
            L_factorial    -> inject $ MkOpFactorial    $ OpFactorial    (arg xs 0 "factorial")
            L_powerSet     -> inject $ MkOpPowerSet     $ OpPowerSet     (arg xs 0 "powerSet")
            L_concatenate  -> inject $ MkOpFlatten      $ OpFlatten      (Just 1)
                                                                         (arg xs 0 "concatenate")
            L_flatten      ->
                 case xs of
                     [m]   -> inject $ MkOpFlatten      $ OpFlatten      Nothing  m
                     [n,m] ->
                          let n' = fromInteger $ fromMaybe (bug "The 1st argument of flatten has to be a constant integer.") (intOut "flatten" n)
                          in  inject $ MkOpFlatten      $ OpFlatten      (Just n') m
                     _     -> bug "flatten takes 1 or 2 arguments."
            _ -> bug ("Unknown lexeme for function type operator:" <+> pretty (show lex))


arg :: [a] -> Int -> Doc -> a
arg xs n op =
    case atMay xs n of
        Nothing -> bug ("Missing argument" <+> pretty (n+1) <+> "for operator" <+> op)
        Just v  -> v