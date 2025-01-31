{-# LANGUAGE DeriveDataTypeable #-}
module Conjure.Language.AST.Reformer (HighLevelTree(..),HLTree(..),flatten,flattenSeq,contains,filterContaining,TreeItemLinks(..),ListItemClasses(..)) where

import Conjure.Language.AST.Syntax
import Conjure.Language.Lexer (ETok (..), trueStart, sourcePosAfter)
import Conjure.Prelude


import Data.Semigroup ((<>))
import qualified Data.Sequence as S
import Text.Megaparsec (SourcePos (SourcePos))




-- class HighLevelTree a where
--     makeTree :: HighLevelTree a => a -> S.Seq ETok
flatten :: HighLevelTree a => a -> [ETok]
flatten x = case makeTree x of
    HLNone -> []
    HLTagged _ xs -> concatMap flatten xs
    HLLeaf t -> [t]
flattenSeq :: HighLevelTree a => a -> S.Seq ETok
flattenSeq = S.fromList . flatten

instance HighLevelTree HLTree where
    makeTree = id

instance HighLevelTree StatementNode where
    makeTree x = case x of
        DeclarationStatement dsn -> makeTree dsn
        BranchingStatement bsn -> makeTree bsn
        SuchThatStatement stsn -> makeTree stsn
        WhereStatement wsn -> makeTree wsn
        ObjectiveStatement osn -> makeTree osn
        HeuristicStatement l1 ex -> makeTree l1 <> makeTree ex
        UnexpectedToken tok -> makeTree tok

instance HighLevelTree DeclarationStatementNode where
    makeTree x = case x of
        FindStatement f fsn -> makeTree f <> makeTree fsn
        GivenStatement g gsn -> makeTree g <> makeTree gsn
        LettingStatement t lsn -> makeTree t <> makeTree lsn


instance HighLevelTree LettingStatementNode where 
    makeTree (LettingStatementNode a b c) = mconcat[ makeTree a, makeTree b, makeTree c]

instance HighLevelTree LettingAssignmentNode where
    makeTree x = case x of
        LettingExpr d ->  makeTree d
        LettingDomain d e -> makeTree d <> makeTree e
        LettingEnum d e f g -> mconcat [makeTree d, makeTree e, makeTree f, makeTree g]
        LettingUnnamed d e f g h -> mconcat [makeTree d, makeTree e, makeTree f, makeTree g, makeTree h]

instance HighLevelTree FindStatementNode where
    makeTree (FindStatementNode a b c) = mconcat  [makeTree a, makeTree b, makeTree c]

instance HighLevelTree GivenStatementNode where
    makeTree x = case x of
        GivenStatementNode a b c -> mconcat [makeTree a, makeTree b, makeTree c]
        GivenEnumNode a b c d -> mconcat [makeTree a, makeTree b, makeTree c, makeTree d]



instance HighLevelTree BranchingStatementNode where
    makeTree (BranchingStatementNode lt lt' ln) = mconcat [makeTree lt, makeTree lt', makeTree ln]


instance HighLevelTree SuchThatStatementNode where
    makeTree (SuchThatStatementNode l1 l2 l3) =  makeTree l1 <> makeTree l2 <> makeTree l3
instance HighLevelTree WhereStatementNode where
    makeTree (WhereStatementNode l1 l2) =  makeTree l1 <> makeTree l2
instance HighLevelTree ObjectiveStatementNode where
    makeTree x =  case x of
        ObjectiveMin lt en -> makeTree lt <> makeTree en
        ObjectiveMax lt en -> makeTree lt <> makeTree en




instance HighLevelTree ExpressionNode where
    makeTree x = HLTagged (TIExpression x) $ case x of
        Literal ln -> [makeTree ln]
        IdentifierNode nn -> [makeTree nn]
        MetaVarExpr tk -> [makeTree tk]
        QuantificationExpr qen -> [makeTree qen]
        OperatorExpressionNode oen -> [makeTree oen]
        ParenExpression pen ->[makeTree pen]
        AbsExpression pen ->  [makeTree pen]
        DomainExpression dex -> [makeTree dex]
        FunctionalApplicationNode lt ln ->  [makeTree lt ,makeTree ln]
        SpecialCase nd -> [makeTree nd]
        AttributeAsConstriant l1 exprs -> [makeTree l1 , makeTree exprs]
        MissingExpressionNode e ->  [makeTree e]

instance HighLevelTree SpecialCaseNode where 
    makeTree x = case x of 
        ExprWithDecls l1 en l2 sns l3 -> mconcat [makeTree l1,makeTree en,makeTree l2, makeTree sns , makeTree l3]


instance HighLevelTree DomainExpressionNode where
    makeTree (DomainExpressionNode a b c) = makeTree a <> makeTree b <> makeTree c
instance HighLevelTree QuantificationExpressionNode where
    makeTree (QuantificationExpressionNode a b c d e f) = mconcat [
        makeTree a, makeTree b, makeTree c, makeTree d, makeTree e, makeTree f]

instance HighLevelTree QuantificationOverNode where
    makeTree x = case x of
      QuantifiedSubsetOfNode a b -> makeTree a <> makeTree b
      QuantifiedMemberOfNode a b -> makeTree a <> makeTree b
      QuantifiedDomainNode a -> makeTree a

instance HighLevelTree OverDomainNode where
    makeTree (OverDomainNode a b) = makeTree a <> makeTree b

instance HighLevelTree QuanticationGuard where
    makeTree (QuanticationGuard a b ) = makeTree a <> makeTree b

instance HighLevelTree AbstractPatternNode where
    makeTree x = case x of
      AbstractIdentifier nn -> makeTree nn
      AbstractMetaVar lt -> makeTree lt
      AbstractPatternTuple a b -> makeTree a <> makeTree b
      AbstractPatternMatrix ln -> makeTree ln
      AbstractPatternSet ln -> makeTree ln
instance HighLevelTree QuantificationPattern where
    makeTree (QuantificationPattern en) = makeTree en

instance HighLevelTree LiteralNode where
    makeTree x = case x of
        IntLiteral lt _ -> makeTree lt -- TODO: should the tag be represented here?
        BoolLiteral lt -> makeTree lt
        MatrixLiteral mln -> makeTree mln
        TupleLiteralNode lt -> makeTree lt
        TupleLiteralNodeShort st -> makeTree st
        RecordLiteral lt ln -> makeTree lt <> makeTree ln
        VariantLiteral lt ln -> makeTree lt <> makeTree ln
        SetLiteral ln -> makeTree ln
        MSetLiteral lt ln -> makeTree lt <> makeTree ln
        FunctionLiteral lt ln -> makeTree lt <> makeTree ln
        SequenceLiteral lt ln -> makeTree lt <> makeTree ln
        PermutationLiteral lt ln -> makeTree lt <> makeTree ln
        RelationLiteral lt ln -> makeTree lt <> makeTree ln
        PartitionLiteral lt ln -> makeTree lt <> makeTree ln

instance HighLevelTree PermutationElemNode where
    makeTree (PermutationElemNode ln) = makeTree ln

instance HighLevelTree PartitionElemNode where
    makeTree (PartitionElemNode ln) = makeTree ln

instance HighLevelTree RelationElemNode where
    makeTree x = case x of
        RelationElemNodeLabeled lt -> makeTree lt
        RelationElemNodeShort st -> makeTree st

instance HighLevelTree ArrowPairNode where
    makeTree (ArrowPairNode a b c) = mconcat [makeTree a, makeTree b, makeTree c]

instance HighLevelTree RecordMemberNode where
    makeTree (RecordMemberNode nn lt en) =  mconcat [makeTree nn, makeTree lt, makeTree en]
instance HighLevelTree LongTuple where
    makeTree (LongTuple a b) =  makeTree a <> makeTree b

instance HighLevelTree ShortTuple where
    makeTree (ShortTuple a) = makeTree a

instance HighLevelTree MatrixLiteralNode where
    makeTree ( MatrixLiteralNode a b c d e) = mconcat
            [ makeTree a
            , makeTree b
            , makeTree c
            , makeTree d
            , makeTree e
            ]

instance HighLevelTree ComprehensionNode where
    makeTree (ComprehensionNode a b) = makeTree a <> makeTree b
instance HighLevelTree ComprehensionExpressionNode where
    makeTree (ComprehensionExpressionNode a b c d e) =
        mconcat
            [ makeTree a
            , makeTree b
            , makeTree c
            , makeTree d
            , makeTree e
            ]

instance HighLevelTree ComprehensionBodyNode where
    makeTree x =  case x of
        CompBodyCondition en -> makeTree en
        CompBodyDomain a b c -> makeTree a <> makeTree b <> makeTree c
        CompBodyGenExpr a b c -> makeTree a <> makeTree b <> makeTree c
        CompBodyLettingNode a b c d -> mconcat [makeTree a, makeTree b, makeTree c, makeTree d]

instance HighLevelTree OperatorExpressionNode where
    makeTree x =  case x of
        PostfixOpNode en pon -> makeTree en <> makeTree pon
        PrefixOpNode lt en -> makeTree lt <> makeTree en
        BinaryOpNode en lt en' -> mconcat [makeTree en, makeTree lt, makeTree en']

instance HighLevelTree PostfixOpNode where
    makeTree x =  case x of
        IndexedNode l -> makeTree l
        OpFactorial lt -> makeTree lt
        ApplicationNode ln -> makeTree ln
        ExplicitDomain l1 l2 dom l3 -> mconcat  [makeTree l1,makeTree l2,makeTree dom,makeTree l3]




instance HighLevelTree DomainNode where
    makeTree x = HLTagged (TIDomain x) $ case x of
        ParenDomainNode a b c -> [makeTree a, makeTree b, makeTree c]
        BoolDomainNode lt -> [makeTree lt]
        RangedIntDomainNode lt ln _maybe_tag -> [makeTree lt,makeTree ln] -- TODO: should the tag be represented here?
        MetaVarDomain a -> [makeTree a]
        RangedEnumNode nn ln -> [makeTree nn ,  makeTree ln]
        -- EnumDomainNode nn -> makeTree nn
        ShortTupleDomainNode ln -> [makeTree ln]
        TupleDomainNode lt ln -> [makeTree lt , makeTree ln]
        RecordDomainNode lt ln -> [makeTree lt , makeTree ln]
        VariantDomainNode lt ln -> [makeTree lt , makeTree ln]
        MatrixDomainNode a m_ib b c d -> [makeTree a ,makeTree m_ib, makeTree b , makeTree c , makeTree d]
        SetDomainNode a b c d -> [makeTree a , makeTree  b , makeTree c , makeTree d]
        MSetDomainNode a b c d -> [makeTree a , makeTree  b, makeTree c , makeTree d]
        FunctionDomainNode a b c d e -> [makeTree a , makeTree  b , makeTree c , makeTree d,makeTree e]
        SequenceDomainNode a b c d -> [makeTree a , makeTree  b , makeTree c , makeTree d]
        PermutationDomainNode a b c d -> [makeTree a , makeTree  b , makeTree c , makeTree d]
        RelationDomainNode a b c d -> [makeTree a , makeTree b , makeTree c , makeTree d]
        PartitionDomainNode a b c d -> [makeTree a , makeTree b , makeTree c , makeTree d]
        MissingDomainNode m -> [makeTree m]

instance HighLevelTree IndexedByNode where
    makeTree (IndexedByNode a b ) = makeTree a <> makeTree b


instance HighLevelTree a => HighLevelTree (Maybe a) where
  makeTree = maybe mempty makeTree 

instance HighLevelTree AttributeNode where
    makeTree x =  case x of
        NamedAttributeNode nn m_e -> makeTree nn <> makeTree m_e
        -- NamedExpressionAttribute nn en -> makeTree nn <> makeTree en

instance HighLevelTree RangeNode where
    makeTree x =  case x of
        SingleRangeNode en -> makeTree en
        OpenRangeNode ddn -> makeTree ddn
        RightUnboundedRangeNode en ddn -> makeTree en <> makeTree ddn
        LeftUnboundedRangeNode ddn en -> makeTree ddn <> makeTree en
        BoundedRangeNode en ddn en' -> mconcat [makeTree en, makeTree ddn, makeTree en']

-- instance HighLevelTree DoubleDotNode where
--     makeTree (DoubleDotNode a b) =  makeTree a <> makeTree b

instance HighLevelTree NamedDomainNode where
    makeTree (NameDomainNode a Nothing) = makeTree a
    makeTree (NameDomainNode a (Just (b,c))) = mconcat [makeTree a,makeTree b,makeTree c]

instance HighLevelTree NameNode where
    makeTree (NameNode n) =  makeTree n
    makeTree (MissingNameNode n) =  makeTree n

instance HighLevelTree NameNodeS where
    makeTree (NameNodeS n) =  makeTree n
instance HighLevelTree ParenExpressionNode where
    makeTree (ParenExpressionNode a b c) =  makeTree a <> makeTree b <> makeTree c





instance HighLevelTree b => HighLevelTree (Sequence b) where
    makeTree (Seq es) = mconcat $ map makeTree es

instance HighLevelTree b => HighLevelTree (SeqElem b) where
    makeTree (SeqElem v s) =  makeTree v <> makeTree s
    makeTree (MissingSeqElem v s) =  makeTree v <> makeTree s
instance HighLevelTree b => HighLevelTree [b] where
    makeTree = HLTagged TIGeneral . map makeTree 

type TreeTag = ListItemClasses
data HLTree 
    = HLTagged TreeItemLinks [HLTree]
    | HLLeaf ETok
    | HLNone
    deriving (Show,Data,Typeable)

instance Semigroup HLTree where
    HLNone <> a = a
    a <> HLNone = a
    HLTagged TIGeneral xs <> a = HLTagged TIGeneral (xs++[a])
    a <> HLTagged TIGeneral xs = HLTagged TIGeneral $ a:xs
    a <> b = HLTagged TIGeneral [a,b]

instance Monoid HLTree where
    mempty = HLNone

taggedSeq :: HighLevelTree a => TreeTag -> Sequence a -> HLTree
taggedSeq s (Seq els) = HLTagged (TIList s) $ makeTree <$> els 
taggedList :: HighLevelTree a => TreeTag -> ListNode a -> HLTree
taggedList s (ListNode a b c) = HLTagged TIGeneral $ makeTree a : taggedSeq s b  : [makeTree c]

-- Tag types for nodes, mainly used to guide completions
data ListItemClasses 
    = ICAttribute
    | ICExpression
    | ICDomain
    | ICRange
    | ICIdentifier
    | ICStatement
    deriving (Show,Data,Ord,Eq)

-- Embed the actual syntax portion into the tree, in case needed
data TreeItemLinks
    = TIExpression ExpressionNode
    | TIDomain DomainNode
    | TIList ListItemClasses
    | TIGeneral
    deriving (Show,Data)
instance Eq TreeItemLinks where
    TIGeneral == TIGeneral = True
    _ == _ = False
instance HighLevelTree (ListNode ExpressionNode) where
    makeTree = taggedList ICExpression
instance HighLevelTree (ListNode NameNode) where
    makeTree = taggedList ICIdentifier
instance HighLevelTree (ListNode DomainNode) where
    makeTree = taggedList ICDomain

instance HighLevelTree (ListNode RangeNode) where
    makeTree = taggedList ICRange

instance HighLevelTree (ListNode AttributeNode) where
    makeTree = taggedList ICAttribute
instance HighLevelTree (ListNode RecordMemberNode) where
    makeTree = taggedList ICIdentifier
instance HighLevelTree (ListNode ArrowPairNode) where
    makeTree = taggedList ICIdentifier
instance HighLevelTree (ListNode RelationElemNode) where
    makeTree = taggedList ICIdentifier
instance HighLevelTree (ListNode PermutationElemNode) where
    makeTree = taggedList ICIdentifier
instance HighLevelTree (ListNode PartitionElemNode) where
    makeTree = taggedList ICIdentifier
instance HighLevelTree (ListNode NamedDomainNode) where
    makeTree = taggedList ICIdentifier

instance HighLevelTree (ListNode AbstractPatternNode) where
    makeTree = taggedList ICIdentifier
class HighLevelTree a where
    makeTree :: a -> HLTree

instance HighLevelTree LToken where
    makeTree (RealToken a) = makeTree a
    makeTree (SkippedToken t) = HLLeaf t
    makeTree (MissingToken m) = HLLeaf m
instance HighLevelTree SToken where
    makeTree (StrictToken ts t) =  HLTagged TIGeneral $ (HLLeaf <$> ts) ++ [HLLeaf t] 

instance HighLevelTree ProgramTree where
  makeTree (ProgramTree Nothing sts cln) = HLTagged TIGeneral $ (HLTagged (TIList ICStatement) $ makeTree <$> sts) : [makeTree cln] 
  makeTree (ProgramTree (Just lv) sts cln) = HLTagged TIGeneral $ [makeTree lv] ++ (makeTree <$> sts) ++ [makeTree cln] 

instance HighLevelTree LangVersionNode where
    makeTree (LangVersionNode a b c) = HLTagged TIGeneral $ makeTree a : makeTree b : [makeTree c]


-- getContainers :: HLTree -> Int -> Int -> HLTree
-- getContainers HLNone r c = HLNone


bounds :: ETok -> SourcePos -> Bool
bounds t (SourcePos _ r c)= let
    (SourcePos _ rl cl,SourcePos _ rr cr) = (trueStart t,sourcePosAfter  t)
        in r >= rl && c >= cl && r <= rr && c <= cr

-- inBounds :: SourcePos -> HLTree -> Bool
-- inBounds (SourcePos _ r c) t 
--     | null $ flatten t = False
--     | otherwise = let 
--         (SourcePos _ rl cl,SourcePos _ rr cr) = bounds t
--         in r >= rl && c >= cl && r <= rr && c <= cr

contains :: SourcePos -> HLTree -> Bool
contains p t = case t of 
    HLNone -> False
    HLLeaf e -> bounds e p
    HLTagged _ xs -> any (contains p) xs
    -- HLGeneral xs -> any (contains p) xs
    -- HLList _ xs -> any (contains p ) xs

filterContaining :: SourcePos -> HLTree -> [HLTree]
filterContaining _ HLNone = []
filterContaining p n@(HLLeaf _) = [n |contains p n]
filterContaining p (HLTagged t xs) = let cs = [x | x <-xs,contains p x]
                                        in HLTagged t cs : concatMap (filterContaining p) cs