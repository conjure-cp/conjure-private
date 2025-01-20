
module Conjure.Language.Expression.Op.Internal.Common
    ( module X

    , SimplifyOp(..)
    , BinaryOperator(..)

    , prettyPrecBinOp
    , Fixity(..), operators, functionals
    , overloadedFunctionals
    , quantifiers
    , EssenceOperatorParsingDescr(..)

    , raiseTypeError

    , intToInt
    , intToIntToInt
    , intToIntToIntStrict
    , boolToBoolToBool
    , sameToSameToBool
    , sameToSameToSame
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name as X
import Conjure.Language.AbstractLiteral as X
import Conjure.Language.Constant as X
import Conjure.Language.Type as X
import Conjure.Language.Domain as X
import Conjure.Language.TypeOf as X
import Conjure.Language.Pretty as X
import Conjure.Language.AdHoc as X
import Conjure.Language.Lexemes as X (lexemeFaceDoc)
import Conjure.Language.Lexer as X ( Lexeme(..), textToLexeme )


class SimplifyOp op x where
    simplifyOp ::
        MonadFailDoc m =>
        Eq x =>
        Num x =>
        ExpressionLike x =>
        op x ->     -- the input
        m x         -- the simplified output (or failure if it cannot be simplified)

class BinaryOperator op where
    opLexeme :: proxy op -> Lexeme

-- | just the operator not the arguments
opPretty :: BinaryOperator op => proxy op -> Doc
opPretty = lexemeFaceDoc . opLexeme

opFixityPrec :: BinaryOperator op => proxy op -> (Fixity, Int)
opFixityPrec op =
    case [ (f,p) | (BinaryOp l f, p) <- operators, l == opLexeme op ] of
        [x] -> x
        _ -> bug "opFixityPrec"

prettyPrecBinOp :: (BinaryOperator op, Pretty x) => Int -> proxy op -> x -> x -> Doc
prettyPrecBinOp envPrec op a b =
    let
        (fixity, prec) = opFixityPrec op
    in
        case fixity of
            FLeft  -> parensIf (envPrec > prec) $ fsep [ prettyPrec  prec    a
                                                       , opPretty op
                                                       , prettyPrec (prec+1) b
                                                       ]
            FNone  -> parensIf (envPrec > prec) $ fsep [ prettyPrec (prec+1) a
                                                       , opPretty op
                                                       , prettyPrec (prec+1) b
                                                       ]
            FRight -> parensIf (envPrec > prec) $ fsep [ prettyPrec (prec+1) a
                                                       , opPretty op
                                                       , prettyPrec  prec    b
                                                       ]

intToInt :: (MonadFailDoc m, TypeOf a, Pretty p, ?typeCheckerMode :: TypeCheckerMode) => p -> a -> m Type
intToInt p a = do
    tya <- typeOf a
    case tya of
        TypeInt t -> return (TypeInt t)
        _         -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            , "Argument expected to be an int, but it is:" <++> pretty tya
            ]


intToIntToInt :: (MonadFailDoc m, TypeOf a, Pretty p, ?typeCheckerMode :: TypeCheckerMode) => p -> a -> a -> m Type
intToIntToInt p a b = do
    tya <- typeOf a
    tyb <- typeOf b
    case (tya, tyb) of
        (TypeInt{}, TypeInt{}) ->
            if typeUnify tya tyb
                then return $ mostDefined [tya, tyb]
                else failDoc $ vcat
                        [ "When type checking:" <+> pretty p
                        , "Types do not unify:" <++> pretty tya 
                        ]
        (_, TypeInt{})         -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            ,  "First argument expected to be an int, but it is:" <++> pretty tya
            ]
        _                      -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            , "Second argument expected to be an int, but it is:" <++> pretty tyb
            ]

intToIntToIntStrict :: (MonadFailDoc m, TypeOf a, Pretty p, ?typeCheckerMode :: TypeCheckerMode) => p -> a -> a -> m Type
intToIntToIntStrict p a b = do
    tya <- typeOf a
    tyb <- typeOf b
    case (tya, tyb) of
        (TypeInt TagInt, TypeInt TagInt) ->
            if typeUnify tya tyb
                then return $ mostDefined [tya, tyb]
                else failDoc $ vcat
                        [ "When type checking:" <+> pretty p
                        , "Types do not unify:" <++> pretty tya 
                        ]
        (TypeInt TaggedInt{}, TypeInt TaggedInt{}) ->
            if typeUnify tya tyb
                then return $ mostDefined [tya, tyb]
                else failDoc $ vcat
                        [ "When type checking:" <+> pretty p
                        , "Types do not unify:" <++> pretty tya 
                        ]
        (_, TypeInt{})         -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            ,  "First argument expected to be an int, but it is:" <++> pretty tya
            ]
        _                      -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            , "Second argument expected to be an int, but it is:" <++> pretty tyb
            ]



boolToBoolToBool :: (MonadFailDoc m, TypeOf a, Pretty p, ?typeCheckerMode :: TypeCheckerMode) => p -> a -> a -> m Type
boolToBoolToBool p a b = do
    tya <- typeOf a
    tyb <- typeOf b
    case (tya, tyb) of
        (TypeBool, TypeBool) -> return TypeBool
        (_, TypeBool)        -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            ,  "First argument expected to be a bool, but it is:" <++> pretty tya
            ]
        _                    -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            , "Second argument expected to be a bool, but it is:" <++> pretty tyb
            ]


-- if acceptableTypes is null, use checkType
-- if acceptableTypes is not null, either one of these is true or checkType
sameToSameToBool
    :: ( MonadFailDoc m, ?typeCheckerMode :: TypeCheckerMode
       , TypeOf a, Pretty a, Pretty p
       ) => p -> a -> a -> [Type] -> (Type -> Bool) -> m Type
sameToSameToBool p a b acceptableTypes checkType = do
    tyA <- typeOf a
    tyB <- typeOf b
    let tyAB = mostDefined [tyA,tyB]
    let allowed = if null acceptableTypes
                    then checkType tyAB
                    else checkType tyAB || any (typeUnify tyAB) acceptableTypes
    case (tyA `typeUnify` tyB, allowed) of
        (True, True) -> return TypeBool
        (False, _) -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            , "Cannot unify the types of the following."
            , "lhs        :" <+> pretty a
            , "type of lhs:" <+> pretty tyA
            , "rhs        :" <+> pretty b
            , "type of rhs:" <+> pretty tyB
            ]
        (_, False) -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            , "Arguments have unsupported types."
            , "lhs        :" <+> pretty a
            , "type of lhs:" <+> pretty tyA
            , "rhs        :" <+> pretty b
            , "type of rhs:" <+> pretty tyB
            ]

-- See sameToSameToBool
sameToSameToSame
    :: ( MonadFailDoc m
       , ?typeCheckerMode :: TypeCheckerMode
       , TypeOf a, Pretty a, Pretty p
       ) => p -> a -> a -> [Type] -> (Type -> Bool) -> m Type
sameToSameToSame p a b acceptableTypes checkType = do
    tyA <- typeOf a
    tyB <- typeOf b
    let tyAB = mostDefined [tyA,tyB]
    let allowed = if null acceptableTypes
                    then checkType tyAB
                    else checkType tyAB || any (typeUnify tyAB) acceptableTypes
    case (tyA `typeUnify` tyB, allowed) of
        (True, True) -> return tyAB
        (False, _) -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            , "Cannot unify the types of the following."
            , "lhs        :" <+> pretty a
            , "type of lhs:" <+> pretty tyA
            , "rhs        :" <+> pretty b
            , "type of rhs:" <+> pretty tyB
            ]
        (_, False) -> failDoc $ vcat
            [ "When type checking:" <+> pretty p
            , "Arguments have unsupported types."
            , "lhs        :" <+> pretty a
            , "type of lhs:" <+> pretty tyA
            , "rhs        :" <+> pretty b
            , "type of rhs:" <+> pretty tyB
            ]


data Fixity = FNone | FLeft | FRight
    deriving Show

data EssenceOperatorParsingDescr = BinaryOp Lexeme Fixity | UnaryPrefix Lexeme

operators :: [(EssenceOperatorParsingDescr, Int)]
operators =
    [ ( BinaryOp L_Plus         FLeft   ,  600 )
    , ( BinaryOp L_Minus        FLeft   ,  600 )
    , ( BinaryOp L_Times        FLeft   ,  700 )
    , ( BinaryOp L_Div          FLeft   ,  700 )
    , ( BinaryOp L_Mod          FLeft   ,  700 )
    , ( BinaryOp L_Pow          FRight  , 2001 )
    , ( BinaryOp L_Lt           FNone   ,  400 )
    , ( BinaryOp L_Leq          FNone   ,  400 )
    , ( BinaryOp L_Gt           FNone   ,  400 )
    , ( BinaryOp L_Geq          FNone   ,  400 )
    , ( BinaryOp L_Neq          FNone   ,  400 )
    , ( BinaryOp L_Eq           FNone   ,  400 )
    , ( BinaryOp L_Or           FLeft   ,  110 )
    , ( BinaryOp L_And          FLeft   ,  120 )
    , ( BinaryOp L_Imply        FNone   ,   50 )
    , ( BinaryOp L_Iff          FNone   ,   50 )
    , ( BinaryOp L_union        FLeft   ,  600 )
    , ( BinaryOp L_intersect    FLeft   ,  700 )
    , ( BinaryOp L_subset       FNone   ,  400 )
    , ( BinaryOp L_subsetEq     FNone   ,  400 )
    , ( BinaryOp L_supset       FNone   ,  400 )
    , ( BinaryOp L_supsetEq     FNone   ,  400 )
    , ( BinaryOp L_subsequence  FNone   ,  400 )
    , ( BinaryOp L_substring    FNone   ,  400 )
    , ( BinaryOp L_in           FNone   ,  550 )
    , ( BinaryOp L_HasRepr      FNone   ,   10 )
    , ( BinaryOp L_HasType      FNone   ,   10 )
    , ( BinaryOp L_HasDomain    FNone   ,   10 )
    , ( BinaryOp L_LexLt        FNone   ,  400 )
    , ( BinaryOp L_LexLeq       FNone   ,  400 )
    , ( BinaryOp L_LexGt        FNone   ,  400 )
    , ( BinaryOp L_LexGeq       FNone   ,  400 )
    , ( BinaryOp L_DotLt        FNone   ,  400 )
    , ( BinaryOp L_DotLeq       FNone   ,  400 )
    , ( BinaryOp L_DotGt        FNone   ,  400 )
    , ( BinaryOp L_DotGeq       FNone   ,  400 )
    , ( BinaryOp L_TildeLt      FNone   ,  400 )
    , ( BinaryOp L_TildeLeq     FNone   ,  400 )
    , ( BinaryOp L_TildeGt      FNone   ,  400 )
    , ( BinaryOp L_TildeGeq     FNone   ,  400 )
    , ( UnaryPrefix L_Minus           , 2000 )
    , ( UnaryPrefix L_ExclamationMark , 2000 )
    ]
--Functional operators that clash with other constructs so require no spaces
overloadedFunctionals :: [Lexeme]
overloadedFunctionals = [
    L_true,
    L_Sum,
    L_Product
    ]

functionals :: [Lexeme]
functionals =
    [ L_toInt
    , L_makeTable
    , L_table
    , L_min
    , L_max
    , L_allDiff
    , L_alldifferent_except
    , L_compose
    , L_gcc
    , L_atleast
    , L_atmost
    , L_catchUndef
    , L_quickPermutationOrder
    , L_dontCare
    , L_hist
    , L_factorial

    , L_toSet
    , L_toMSet
    , L_toRelation
    , L_defined
    , L_range
    , L_restrict
    , L_image
    , L_imageSet
    , L_preImage
    , L_inverse
    , L_together
    , L_apart
    , L_party
    , L_permInverse
    , L_participants
    , L_parts
    , L_image
    , L_freq
    , L_toInt
    , L_flatten
    , L_concatenate
    , L_normIndices
    , L_indices
    , L_inverse

    , L_transform
    , L_true

    , L_fAnd
    , L_fOr
    , L_Sum
    , L_Product
    , L_fXor

    , L_active

    , L_pred
    , L_succ

    , L_powerSet

    ]

quantifiers :: [Lexeme]
quantifiers = [
    L_ForAll,
    L_Exists,
    L_Product,
    L_Sum
    ]

raiseTypeError :: MonadFailDoc m => Pretty a => a -> m b
raiseTypeError p = failDoc ("Type error in" <+> pretty p)
