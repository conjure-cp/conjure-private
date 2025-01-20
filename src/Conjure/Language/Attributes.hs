module Conjure.Language.Attributes where

import Conjure.Language.Domain (BinaryRelationAttr (..))
import Conjure.Language.Expression.Op.Internal.Common (Lexeme (..))
import Conjure.Prelude
import Data.Map (Map)
import Data.Map.Strict qualified as M

type Attr = (Lexeme, Bool)

mapFrom :: [[Attr]] -> Map Lexeme Bool
mapFrom attrs = M.fromList $ concat attrs

setValidAttrs :: Map Lexeme Bool
setValidAttrs = mapFrom [sizeyAttrs]

msetValidAttrs :: Map Lexeme Bool
msetValidAttrs = mapFrom [sizeyAttrs, occursAttrs]

permValidAttrs :: Map Lexeme Bool
permValidAttrs = mapFrom [permAttrs]

funAttrs :: Map Lexeme Bool
funAttrs = mapFrom [sizeyAttrs, jectivityAttrs, totalityAttrs]

seqAttrs :: Map Lexeme Bool
seqAttrs = mapFrom [sizeyAttrs, jectivityAttrs]

relAttrs :: Map Lexeme Bool
relAttrs = mapFrom [sizeyAttrs, binRelAttrs, totalityAttrs]

partitionAttrs :: Map Lexeme Bool
partitionAttrs = mapFrom [sizeyAttrs, partSizeAttrs, partNumAttrs, regularity]

sizeyAttrs :: [Attr]
sizeyAttrs =
  [ (L_size, True),
    (L_maxSize, True),
    (L_minSize, True)
  ]

permAttrs :: [Attr]
permAttrs =
  [ (L_numMoved, True),
    (L_minNumMoved, True),
    (L_maxNumMoved, True)
  ]

occursAttrs :: [Attr]
occursAttrs =
  [ (L_minOccur, True),
    (L_maxOccur, True)
  ]

partNumAttrs :: [Attr]
partNumAttrs =
  [ (L_numParts, True),
    (L_maxNumParts, True),
    (L_minNumParts, True)
  ]

partSizeAttrs :: [(Lexeme, Bool)]
partSizeAttrs =
  [ (L_partSize, True),
    (L_minPartSize, True),
    (L_maxPartSize, True)
  ]

jectivityAttrs :: [(Lexeme, Bool)]
jectivityAttrs =
  [ (L_injective, False),
    (L_bijective, False),
    (L_surjective, False)
  ]

binRelAttrs :: [(Lexeme, Bool)]
binRelAttrs =
  [ (L_reflexive, False),
    (L_irreflexive, False),
    (L_coreflexive, False),
    (L_symmetric, False),
    (L_antiSymmetric, False),
    (L_aSymmetric, False),
    (L_transitive, False),
    (L_total, False),
    (L_connex, False),
    (L_Euclidean, False),
    (L_serial, False),
    (L_equivalence, False),
    (L_partialOrder, False),
    (L_linearOrder, False),
    (L_weakOrder, False),
    (L_preOrder, False),
    (L_strictPartialOrder, False),
    (L_leftTotal, False),
    (L_rightTotal, False)
  ]

lexemeToBinRel :: Lexeme -> Maybe BinaryRelationAttr
lexemeToBinRel L_reflexive = Just BinRelAttr_Reflexive
lexemeToBinRel L_irreflexive = Just BinRelAttr_Irreflexive
lexemeToBinRel L_coreflexive = Just BinRelAttr_Coreflexive
lexemeToBinRel L_symmetric = Just BinRelAttr_Symmetric
lexemeToBinRel L_antiSymmetric = Just BinRelAttr_AntiSymmetric
lexemeToBinRel L_aSymmetric = Just BinRelAttr_ASymmetric
lexemeToBinRel L_transitive = Just BinRelAttr_Transitive
lexemeToBinRel L_total = Just BinRelAttr_Total
lexemeToBinRel L_connex = Just BinRelAttr_Connex
lexemeToBinRel L_Euclidean = Just BinRelAttr_Euclidean
lexemeToBinRel L_serial = Just BinRelAttr_Serial
lexemeToBinRel L_equivalence = Just BinRelAttr_Equivalence
lexemeToBinRel L_partialOrder = Just BinRelAttr_PartialOrder
lexemeToBinRel L_linearOrder = Just BinRelAttr_LinearOrder
lexemeToBinRel L_weakOrder = Just BinRelAttr_WeakOrder
lexemeToBinRel L_preOrder = Just BinRelAttr_PreOrder
lexemeToBinRel L_strictPartialOrder = Just BinRelAttr_StrictPartialOrder
lexemeToBinRel L_leftTotal = Just BinRelAttr_LeftTotal
lexemeToBinRel L_rightTotal = Just BinRelAttr_RightTotal
lexemeToBinRel _ = Nothing

totalityAttrs :: [Attr]
totalityAttrs = [(L_total, False)]

regularity :: [Attr]
regularity = [(L_regular, False)]

allAttributLexemes :: [Lexeme]
allAttributLexemes =
  concatMap
    (map fst)
    [ sizeyAttrs,
      jectivityAttrs,
      occursAttrs,
      partNumAttrs,
      partSizeAttrs,
      binRelAttrs,
      totalityAttrs,
      regularity,
      permAttrs
    ]