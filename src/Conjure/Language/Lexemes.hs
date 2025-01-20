{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Conjure.Language.Lexemes where

import Conjure.Prelude
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T



data Lexeme
    = LIntLiteral Integer
    | LMissingIntLiteral   --helper for missing symbol
    | LIdentifier T.Text
    | LMissingIdentifier --helper for missing symbol
    | LMetaVar T.Text
    | LUnexpected T.Text
    | LMissingMetaVar --helper for missing symbol
    -- general
    | L_be
    | L_from
    | L_of
    | L_domain

    | L_language
    | L_dim
    | L_find
    | L_given
    | L_letting
    | L_where
    | L_such
    | L_that
    | L_minimising
    | L_maximising
    | L_branching
    | L_on
    | L_heuristic

    -- type: boolean
    | L_bool
    | L_false
    | L_true

    -- type: integer
    | L_int

    -- creating a new type
    | L_new
    | L_type
    | L_enum

    -- type tuple
    | L_tuple

    -- type record
    | L_record

    -- type variant
    | L_variant
    | L_active

    -- type: matrix
    | L_matrix
    | L_indexed
    | L_by

    -- type set
    | L_set
    | L_size
    | L_minSize
    | L_maxSize

    -- type: mset
    | L_mset
    | L_minOccur
    | L_maxOccur

    -- type: function
    | L_function
    | L_total
    | L_partial
    | L_injective
    | L_surjective
    | L_bijective

    -- type: sequence
    | L_sequence

    -- type: relation
    | L_relation
    | L_reflexive
    | L_irreflexive
    | L_coreflexive
    | L_symmetric
    | L_antiSymmetric
    | L_aSymmetric
    | L_transitive
    | L_connex
    | L_Euclidean
    | L_serial
    | L_equivalence
    | L_partialOrder
    | L_linearOrder
    | L_weakOrder
    | L_preOrder
    | L_strictPartialOrder
    | L_leftTotal
    | L_rightTotal

    -- type: partition
    | L_partition
    | L_regular
    | L_numMoved
    | L_minNumMoved
    | L_maxNumMoved
    | L_partSize
    | L_minPartSize
    | L_maxPartSize
    | L_numParts
    | L_minNumParts
    | L_maxNumParts

    -- type: permutation
    | L_permutation
    | L_compose

    -- operators, page 21 of the holy paper
    | L_union
    | L_intersect
    | L_subset
    | L_subsetEq
    | L_supset
    | L_supsetEq
    | L_in
    | L_max
    | L_min
    | L_toSet
    | L_toMSet
    | L_toRelation
    | L_defined
    | L_range
    | L_restrict
    | L_image
    | L_imageSet
    | L_preImage
    | L_inverse
    | L_together
    | L_apart
    | L_party
    | L_permInverse
    | L_participants
    | L_parts
    | L_freq
    | L_hist

    | L_toInt
    | L_makeTable
    | L_table

    -- global constraints
    | L_allDiff
    | L_alldifferent_except
    | L_gcc
    | L_atleast
    | L_atmost

    | L_dontCare

    | L_catchUndef

    | L_quickPermutationOrder

    -- matrix only operators
    | L_flatten
    | L_concatenate
    | L_normIndices

    -- in the rule language
    -- | L_lambda
    -- | L_quantifier
    -- | L_representation

    -- arithmetic operators

    | L_Plus                --    +           -- sum, infix : (int,int) -> int
    | L_Minus               --    -           -- (subtraction, infix : (int,int) -> int) OR (unary minus : int -> int)
    | L_Times               --    *           -- multiplication, infix : (int,int) -> int
    | L_Div                 --    /           -- integer division, infix
    | L_Mod                 --    %           -- modulo, infix
    | L_Pow                 --    **          -- exponentiation, infix : (int,int) -> int
    | L_factorial

    -- equality

    | L_Eq                  --    =           -- equals, infix.
    | L_Neq                 --    !=          -- not-equals, infix

    -- comparison

    | L_Lt                  --    <           -- less-than, infix.
    | L_Leq                 --    <=          -- less-than-or-eq, infix.
    | L_Gt                  --    >           -- greater-than, infix.
    | L_Geq                 --    >=          -- greater-than-or-eq, infix.

    -- logical operators

    | L_And                 --    /\          -- logical-and, infix
    | L_Or                  --    \/          -- logical-or, infix.
    | L_Imply               --    ->          -- implication, infix
    | L_Iff                 --    <->         -- iff, infix.
    | L_Not                 --    !           -- negation, prefix
    | L_ExclamationMark     -- for poth L_Factorial and L_ExclamationMark

    -- the function arrow

    | L_LongArrow           --    -->         -- function domains and constants

    -- in rule language

    | L_Colon               --    :           -- has-domain, infix, (expr,domain) -> bool. also does pattern matching.
    | L_DoubleColon         --    ::          -- has-type, infix, (expr,type) -> bool. also does pattern matching.
    | L_At                  --    @           -- bubble operator.

    -- lex operators

    | L_LexGeq              --    >=lex
    | L_LexGt               --    >lex
    | L_LexLt               --    <=lex
    | L_LexLeq              --    <lex

    -- for "abs" and "card"
    | L_Bar                 --    |

    -- attaching a type to an expression
    | L_BackTick            --    `

    --Quantifiers

    | L_ForAll
    | L_Exists
    | L_Sum
    | L_Product
    | L_fXor

    | L_fAnd
    | L_fOr


    -- others
    | L_Dot
    | L_DoubleDot
    | L_Comma
    | L_SemiColon

    | L_OpenParen
    | L_CloseParen
    | L_OpenBracket
    | L_CloseBracket
    | L_OpenCurly
    | L_CloseCurly

    | L_Newline
    | L_Carriage
    | L_Space
    | L_Tab

    | L_SquigglyArrow
    | L_CaseSeparator

    | L_HasRepr
    | L_HasType
    | L_HasDomain
    | L_indices

    | L_DotLt
    | L_DotLeq
    | L_DotGt
    | L_DotGeq

    | L_TildeLt
    | L_TildeLeq
    | L_TildeGt
    | L_TildeGeq

    | L_LeftArrow

    | L_subsequence
    | L_substring
    | L_powerSet

    | L_pred
    | L_succ

    -- type functional
    | L_transform

    -- helper
    | L_Missing MissingStructuralElements
    | L_EOF
    | L_SpecialCase

    deriving (Eq, Ord, Show,Data,Generic) --Generic

instance Hashable Lexeme

data MissingStructuralElements = MissingExpression | MissingDomain | MissingUnknown
    deriving (Eq, Ord, Data,Generic) --Generic
instance Show MissingStructuralElements where
    show MissingExpression = "Expression"
    show MissingDomain = "Domain"
    show MissingUnknown = "Unknown"

instance Hashable MissingStructuralElements

lexemes :: [(T.Text, Lexeme)]
lexemes = sortBy (flip (comparing (T.length . fst))) $ map swap
    [ ( L_be         , "be"         )
    , ( L_from       , "from"       )
    , ( L_of         , "of"         )
    , ( L_domain     , "domain"     )
    , ( L_language   , "language"   )
    , ( L_dim        , "dim"        )
    , ( L_find       , "find"       )
    , ( L_given      , "given"      )
    , ( L_letting    , "letting"    )
    , ( L_where      , "where"      )
    , ( L_such       , "such"       )
    , ( L_that       , "that"       )
    , ( L_minimising , "minimising" )
    , ( L_maximising , "maximising" )
    , ( L_minimising , "minimizing" )
    , ( L_maximising , "maximizing" )
    , ( L_branching  , "branching"  )
    , ( L_on         , "on"         )
    , ( L_heuristic  , "heuristic"  )

    , ( L_bool, "bool" )
    , ( L_false, "false" )
    , ( L_true, "true" )
    , ( L_int, "int" )
    , ( L_new, "new" )
    , ( L_type, "type" )
    , ( L_enum, "enum" )
    , ( L_tuple, "tuple" )
    , ( L_record, "record" )
    , ( L_variant, "variant" )
    , ( L_active, "active" )
    , ( L_matrix, "matrix" )
    , ( L_indexed, "indexed" )
    , ( L_by, "by" )
    , ( L_set, "set" )
    , ( L_size, "size" )
    , ( L_minSize, "minSize" )
    , ( L_maxSize, "maxSize" )
    , ( L_mset, "mset" )
    , ( L_minOccur, "minOccur" )
    , ( L_maxOccur, "maxOccur" )
    , ( L_function, "function" )
    , ( L_total, "total" )
    , ( L_partial, "partial" )
    , ( L_injective, "injective" )
    , ( L_surjective, "surjective" )
    , ( L_bijective, "bijective" )
    , ( L_sequence, "sequence" )
    , ( L_permutation, "permutation" )
    , ( L_relation, "relation")
    , ( L_reflexive, "reflexive")
    , ( L_irreflexive, "irreflexive")
    , ( L_coreflexive, "coreflexive")
    , ( L_symmetric, "symmetric")
    , ( L_antiSymmetric, "antiSymmetric")
    , ( L_aSymmetric, "aSymmetric")
    , ( L_transitive, "transitive")
    , ( L_connex, "connex")
    , ( L_Euclidean, "Euclidean")
    , ( L_serial, "serial")
    , ( L_equivalence, "equivalence")
    , ( L_partialOrder, "partialOrder")
    , ( L_linearOrder , "linearOrder")
    , ( L_weakOrder , "weakOrder")
    , ( L_preOrder , "preOrder")
    , ( L_strictPartialOrder , "strictPartialOrder")
    , ( L_leftTotal , "leftTotal")
    , ( L_rightTotal , "rightTotal")
    , ( L_partition, "partition" )
    , ( L_regular, "regular" )
    , ( L_numMoved, "numMoved" )
    , ( L_minNumMoved, "minNumMoved" )
    , ( L_maxNumMoved, "maxNumMoved" )
    , ( L_partSize, "partSize" )
    , ( L_minPartSize, "minPartSize" )
    , ( L_maxPartSize, "maxPartSize" )
    , ( L_numParts, "numParts" )
    , ( L_minNumParts, "minNumParts" )
    , ( L_maxNumParts, "maxNumParts" )
    , ( L_union, "union" )
    , ( L_intersect, "intersect" )
    , ( L_subset, "subset" )
    , ( L_subsetEq, "subsetEq" )
    , ( L_supset, "supset" )
    , ( L_supsetEq, "supsetEq" )
    , ( L_in, "in" )
    , ( L_max, "max" )
    , ( L_min, "min" )
    , ( L_toSet, "toSet" )
    , ( L_toMSet, "toMSet" )
    , ( L_toRelation, "toRelation" )
    , ( L_defined, "defined" )
    , ( L_range, "range" )
    , ( L_restrict, "restrict" )
    , ( L_image, "image" )
    , ( L_imageSet, "imageSet" )
    , ( L_preImage, "preImage" )
    , ( L_inverse, "inverse" )
    , ( L_together, "together" )
    , ( L_apart, "apart" )
    , ( L_party, "party" )
    , ( L_permInverse, "permInverse" )
    , ( L_participants, "participants" )
    , ( L_parts, "parts" )
    , ( L_freq, "freq" )
    , ( L_hist, "hist" )
    , ( L_toInt, "toInt" )
    , ( L_makeTable, "makeTable" )
    , ( L_table, "table" )

    , ( L_compose, "compose" )


    , ( L_allDiff, "allDiff" )
    , ( L_alldifferent_except, "alldifferent_except" )
    , ( L_gcc, "gcc" )
    , ( L_atleast, "atleast" )
    , ( L_atmost, "atmost" )

    , ( L_dontCare, "dontCare" )
    , ( L_catchUndef, "catchUndef" )

    , ( L_quickPermutationOrder, "quickPermutationOrder" )

    , ( L_flatten, "flatten" )
    , ( L_concatenate, "concatenate" )
    , ( L_normIndices, "normIndices" )
    -- , ( L_lambda, "lambda" )
    -- , ( L_quantifier, "quantifier" )
    -- , ( L_representation, "representation" )

    , ( L_ForAll            , "forAll"     )
    , ( L_Exists           , "exists"     )
    , ( L_Sum           , "sum"     )
    , ( L_Product           , "product"     )
    , ( L_Not           , "not"     )
    , ( L_fXor           , "xor"     )
    , ( L_fAnd           , "and"     )
    , ( L_fOr           , "or"     )

    , ( L_Plus            , "+"     )
    , ( L_Minus           , "-"     )
    , ( L_Times           , "*"     )
    , ( L_Div             , "/"     )
    , ( L_Mod             , "%"     )
    , ( L_Pow             , "**"    )
    , ( L_factorial       , "factorial" )
    , ( L_Eq              , "="     )
    , ( L_Neq             , "!="    )
    , ( L_Lt              , "<"     )
    , ( L_Leq             , "<="    )
    , ( L_Gt              , ">"     )
    , ( L_Geq             , ">="    )
    , ( L_And             , "/\\"   )
    , ( L_Or              , "\\/"   )
    , ( L_Imply           , "->"    )
    , ( L_Iff             , "<->"   )
    , ( L_ExclamationMark , "!"     )
    , ( L_LongArrow       , "-->"   )
    , ( L_Colon           , ":"     )
    , ( L_DoubleColon     , "::"    )
    , ( L_At              , "@"     )
    , ( L_LexGeq          , ">=lex" )
    , ( L_LexGt           , ">lex"  )
    , ( L_LexLeq          , "<=lex" )
    , ( L_LexLt           , "<lex"  )
    , ( L_Bar             , "|"     )
    , ( L_BackTick        , "`"     )
    , ( L_Dot             , "."     )
    , ( L_DoubleDot       , ".."    )
    , ( L_Comma           , ","     )
    , ( L_SemiColon       , ";"     )
    , ( L_OpenParen       , "("     )
    , ( L_CloseParen      , ")"     )
    , ( L_OpenBracket     , "["     )
    , ( L_CloseBracket    , "]"     )
    , ( L_OpenCurly       , "{"     )
    , ( L_CloseCurly      , "}"     )

    , ( L_Newline         , "\n"    )
    , ( L_Carriage        , "\r"    )
    , ( L_Space           , " "     )
    , ( L_Tab             , "\t"    )

    , ( L_SquigglyArrow   , "~~>"   )
    , ( L_CaseSeparator   , "***"   )

    , ( L_HasRepr         , "hasRepr"   )
    , ( L_HasType         , "hasType"   )
    , ( L_HasDomain       , "hasDomain" )
    , ( L_indices         , "indices"   )

    , ( L_DotLt           , ".<"    )
    , ( L_DotLeq          , ".<="   )
    , ( L_DotGt           , ".>"    )
    , ( L_DotGeq          , ".>="   )

    , ( L_TildeLt         , "~<"    )
    , ( L_TildeLeq        , "~<="   )
    , ( L_TildeGt         , "~>"    )
    , ( L_TildeGeq        , "~>="   )

    , ( L_LeftArrow       , "<-"   )

    , ( L_subsequence     , "subsequence"  )
    , ( L_substring       , "substring"    )
    , ( L_powerSet        , "powerSet"     )

    , ( L_pred, "pred" )
    , ( L_succ, "succ" )


    , ( L_transform, "transform")

    , ( L_SpecialCase, "?#")
    ]

textToLexeme :: Text -> Maybe Lexeme
textToLexeme t = M.lookup t mapTextToLexeme

mapTextToLexeme :: M.HashMap T.Text Lexeme
mapTextToLexeme = M.fromList lexemes

mapLexemeToText :: M.HashMap Lexeme T.Text
mapLexemeToText = M.fromList $ map swap lexemes

lexemeFace :: Lexeme -> String
lexemeFace L_Newline = "new line"
lexemeFace L_Carriage = "\\r"
lexemeFace L_Space   = "space character"
lexemeFace L_Tab     = "tab character"
lexemeFace (LIntLiteral i) = show i
lexemeFace (LIdentifier i) = T.unpack i
-- lexemeFace (LComment    i) = Pr.text (T.unpack i)
lexemeFace l =
    case M.lookup l mapLexemeToText of
        Nothing ->  (show l)
        Just t  ->  (T.unpack t)

lexemeFaceDoc :: Lexeme -> Doc
lexemeFaceDoc = stringToDoc . lexemeFace

lexemeText :: Lexeme -> T.Text
lexemeText (LIdentifier t) =  t
lexemeText l = fromMaybe (T.pack $ show l) (M.lookup l mapLexemeToText)

--Categories
functionAttributes :: [Lexeme]
functionAttributes = [L_injective,L_size]



