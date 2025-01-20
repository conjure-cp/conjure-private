{-# LANGUAGE DeriveDataTypeable #-}
module Conjure.Language.AST.Syntax  where
import Data.Data
import Conjure.Language.Lexer (ETok(..), prettySplitComments)
import Conjure.Prelude hiding (Doc, group,Data,Typeable)

import Prettyprinter 

import Prettyprinter.Render.Text (renderStrict)


data LToken
    = RealToken SToken
    | MissingToken ETok
    | SkippedToken ETok
    deriving (Eq, Ord, Show, Data)

data SToken
    = StrictToken [ETok] ETok
    deriving (Eq , Ord, Show, Data)
instance Null SToken where
    isMissing = const False
instance Pretty SToken where
    pretty (StrictToken _ r) = pretty r
makeStrict :: ETok -> LToken
makeStrict = RealToken . StrictToken [] 
instance Pretty LToken where
    pretty (SkippedToken e) = pretty e
    pretty (RealToken r) = pretty r
    pretty _ = emptyDoc

instance Null LToken where
    isMissing (MissingToken _) = True
    isMissing _ = False


data ProgramTree = ProgramTree
    { langVersionInfo :: Maybe LangVersionNode
    , statements :: [StatementNode]
    , eofToken :: SToken
    }
    deriving (Show, Data ,Typeable)

instance Pretty ProgramTree where
    pretty (ProgramTree l s e) =
        vcat
            [ maybe "language Essence 1.3" pretty l <> line
            , vcat $ map pretty s
            , pretty e
            ]

data LangVersionNode = LangVersionNode SToken NameNode (Sequence SToken)
    deriving (Show, Data)
instance Pretty LangVersionNode where
    pretty (LangVersionNode t n ns) = pretty t <+> pretty n <+> pretty ns

data StatementNode
    = DeclarationStatement DeclarationStatementNode
    | BranchingStatement BranchingStatementNode
    | SuchThatStatement SuchThatStatementNode
    | WhereStatement WhereStatementNode
    | ObjectiveStatement ObjectiveStatementNode
    | HeuristicStatement SToken ExpressionNode
    | UnexpectedToken LToken
    deriving (Show, Data , Typeable)
instance Pretty StatementNode where
    pretty x = case x of
        DeclarationStatement dsn -> pretty dsn
        BranchingStatement bsn -> pretty bsn
        SuchThatStatement stsn -> pretty stsn
        WhereStatement wsn -> pretty wsn
        ObjectiveStatement osn -> pretty osn
        HeuristicStatement lt en -> pretty lt <+> pretty en
        UnexpectedToken _ -> emptyDoc

data SuchThatStatementNode
    = SuchThatStatementNode
        SToken -- Such
        LToken -- That
        (Sequence ExpressionNode) -- constraints
    deriving (Show, Data)

instance Pretty SuchThatStatementNode where
    pretty (SuchThatStatementNode l1 l2 es) = topLevelPretty [RealToken l1, l2] (pretty es)

data WhereStatementNode
    = WhereStatementNode
        SToken -- where
        (Sequence ExpressionNode) -- expresssions
    deriving (Show, Data)

instance Pretty WhereStatementNode where
    pretty (WhereStatementNode w se) = topLevelPretty [RealToken w] (pretty se)

data ObjectiveStatementNode
    = ObjectiveMin SToken ExpressionNode
    | ObjectiveMax SToken ExpressionNode
    deriving (Show, Data)
instance Pretty ObjectiveStatementNode where
    pretty x = case x of
        ObjectiveMin lt en -> pretty lt <+> pretty en
        ObjectiveMax lt en -> pretty lt <+> pretty en

-- Declaration statements
data DeclarationStatementNode
    = FindStatement SToken (Sequence FindStatementNode)
    | GivenStatement SToken (Sequence GivenStatementNode)
    | LettingStatement SToken (Sequence LettingStatementNode)
    deriving (Show, Data, Typeable)

instance Pretty DeclarationStatementNode where
    pretty x = case x of
        FindStatement lt se -> topLevelPretty [RealToken lt] (pretty se)
        GivenStatement lt se -> topLevelPretty [RealToken lt] (pretty se)
        LettingStatement lt se -> topLevelPretty [RealToken lt] (pretty se)
data FindStatementNode
    = FindStatementNode
        (Sequence NameNode) -- names
        LToken -- colon
        DomainNode -- domain
    deriving (Show, Data)
instance Pretty FindStatementNode where
    pretty (FindStatementNode names col dom) = pretty names <+> pretty col <+> pretty dom
instance Null FindStatementNode where
    isMissing (FindStatementNode n l d) = isMissing n && isMissing l && isMissing d
data GivenStatementNode
    = GivenStatementNode
        (Sequence NameNode) -- name
        LToken -- colon
        DomainNode -- domain
    | GivenEnumNode
        (Sequence NameNode)
        LToken -- new
        LToken -- type
        LToken -- enum
    deriving (Show, Data)
instance Pretty GivenStatementNode where
    pretty g = case g of
        GivenStatementNode se lt dn -> pretty se <+> pretty lt <+> pretty dn
        GivenEnumNode se lt lt' lt2 -> pretty se <+> pretty lt <+> pretty lt' <+> pretty lt2

instance Null GivenStatementNode where
    isMissing (GivenStatementNode l t d) = isMissing l && isMissing t && isMissing d
    isMissing (GivenEnumNode l a b c) = isMissing l && isMissing a && isMissing b && isMissing c

data LettingStatementNode
    = LettingStatementNode
        (Sequence NameNode)
        LToken --
        LettingAssignmentNode
    deriving (Show, Data)
instance Pretty LettingStatementNode where
    pretty (LettingStatementNode ns be assign) = pretty ns <+> pretty be <+> pretty assign

instance Null LettingStatementNode where
    isMissing (LettingStatementNode l t a) = isMissing l && isMissing t && isMissing a
data LettingAssignmentNode
    = LettingExpr
        ExpressionNode
    | LettingDomain
        SToken -- domain
        DomainNode
    | LettingEnum
        LToken -- lNew
        LToken -- lType
        LToken -- lEnum
        (ListNode NameNode) -- nameList
    | LettingUnnamed
        LToken -- lNew
        LToken -- lType
        LToken -- lOf
        LToken -- lSize
        ExpressionNode -- expr
    deriving (Show, Data)

instance Pretty LettingAssignmentNode where
    pretty a = case a of
        LettingExpr en -> pretty en
        LettingDomain lt dn -> pretty lt <+> pretty dn
        LettingEnum lt lt' lt2 ln -> pretty lt <+> pretty lt' <+> pretty lt2 <+> pretty ln
        LettingUnnamed lt lt' lt2 lt3 en -> pretty lt <+> pretty lt' <+> pretty lt2 <+> pretty lt3 <+> pretty en
instance Null LettingAssignmentNode where
    isMissing x = case x of
        LettingExpr en -> isMissing en
        LettingDomain lt dn -> isMissing lt && isMissing dn
        LettingEnum l1 l2 l3 ln -> all isMissing [l1, l2, l3] && isMissing ln
        LettingUnnamed l1 l2 l3 l4 en -> all isMissing [l1, l2, l3, l4] && isMissing en

-- Branching on

data BranchingStatementNode
    = BranchingStatementNode
        SToken
        LToken
        (ListNode ExpressionNode)
    deriving (Show, Data)

instance Pretty BranchingStatementNode where
    pretty (BranchingStatementNode br o exs) = pretty br <+> pretty o <+> pretty exs

-- Domains

type MAttributes = Maybe (ListNode AttributeNode)

data DomainNode
    = ParenDomainNode SToken DomainNode LToken
    | BoolDomainNode SToken
    | RangedIntDomainNode SToken (Maybe (ListNode RangeNode)) (Maybe ETok) -- the IntTag
    | RangedEnumNode NameNodeS (Maybe (ListNode RangeNode))
    | MetaVarDomain SToken
    | ShortTupleDomainNode (ListNode DomainNode)
    | TupleDomainNode SToken (ListNode DomainNode)
    | RecordDomainNode SToken (ListNode NamedDomainNode)
    | VariantDomainNode SToken (ListNode NamedDomainNode)
    | MatrixDomainNode SToken (Maybe IndexedByNode) (ListNode DomainNode) LToken DomainNode
    | SetDomainNode SToken MAttributes LToken DomainNode
    | MSetDomainNode SToken MAttributes LToken DomainNode
    | FunctionDomainNode SToken MAttributes DomainNode LToken DomainNode
    | SequenceDomainNode SToken MAttributes LToken DomainNode
    | PermutationDomainNode SToken MAttributes LToken DomainNode
    | RelationDomainNode SToken MAttributes LToken (ListNode DomainNode)
    | PartitionDomainNode SToken MAttributes LToken DomainNode
    | MissingDomainNode LToken
    deriving (Show, Data)

instance Pretty DomainNode where
    pretty x = case x of
        ParenDomainNode op dom cl -> pretty op <> pretty dom <> pretty cl
        BoolDomainNode lt -> pretty lt
        RangedIntDomainNode lt m_ln Nothing -> pretty lt <> pretty m_ln
        RangedIntDomainNode lt m_ln (Just tag) -> pretty lt <> ":" <> pretty tag <> pretty m_ln
        RangedEnumNode nn m_ln -> pretty nn <> pretty m_ln
        MetaVarDomain lt -> pretty lt
        ShortTupleDomainNode ln -> pretty ln
        TupleDomainNode lt ln -> pretty lt <> pretty ln
        RecordDomainNode lt ln -> pretty lt <> pretty ln
        VariantDomainNode lt ln -> pretty lt <> pretty ln
        MatrixDomainNode lt m_ibn ln lt' dn ->
            pretty lt
                <+> pretty m_ibn
                <+> pretty ln
                <+> pretty lt'
                <+> pretty dn
        SetDomainNode lt m_ln lt' dn -> pretty lt <+> pretty m_ln <+> pretty lt' <+> pretty dn
        MSetDomainNode lt m_ln lt' dn -> pretty lt <+> pretty m_ln <+> pretty lt' <+> pretty dn
        FunctionDomainNode lt m_ln dn lt' dn' -> pretty lt <+> pretty m_ln <+> pretty dn <+> pretty lt' <+> pretty dn'
        SequenceDomainNode lt m_ln lt' dn -> pretty lt <+> pretty m_ln <+> pretty lt' <+> pretty dn
        PermutationDomainNode lt m_ln lt' dn -> pretty lt <+> pretty m_ln <+> pretty lt' <+> pretty dn
        RelationDomainNode lt m_ln lt' ln -> pretty lt <+> pretty m_ln <+> pretty lt' <+> pretty ln
        PartitionDomainNode lt m_ln lt' dn -> pretty lt <+> pretty m_ln <+> pretty lt' <+> pretty dn
        MissingDomainNode _ -> emptyDoc
instance Null DomainNode where
    isMissing (MissingDomainNode{}) = True
    isMissing _ = False

data IndexedByNode = IndexedByNode LToken LToken
    deriving (Show, Data)
instance Pretty IndexedByNode where
    pretty (IndexedByNode a b) = pretty a <+> pretty b
data RangeNode
    = SingleRangeNode ExpressionNode
    | OpenRangeNode DoubleDotNode
    | RightUnboundedRangeNode ExpressionNode DoubleDotNode
    | LeftUnboundedRangeNode DoubleDotNode ExpressionNode
    | BoundedRangeNode ExpressionNode DoubleDotNode ExpressionNode
    deriving (Show, Data)

instance Pretty RangeNode where
    pretty x = case x of
        SingleRangeNode en -> pretty en
        OpenRangeNode lt -> pretty lt
        RightUnboundedRangeNode en lt -> pretty en <> pretty lt
        LeftUnboundedRangeNode lt en -> pretty lt <> pretty en
        BoundedRangeNode en lt en' -> pretty en <> pretty lt <> pretty en'
instance Null RangeNode where
    isMissing (SingleRangeNode e) = isMissing e
    isMissing _ = False

type DoubleDotNode = SToken

-- data DoubleDotNode = DoubleDotNode LToken LToken deriving (Show, Data)

data AttributeNode
    = NamedAttributeNode SToken (Maybe ExpressionNode)
    deriving (Show, Data)
instance Pretty AttributeNode where
    pretty (NamedAttributeNode a m_e) = pretty a <+> pretty m_e

instance Null AttributeNode where
    isMissing _ = False


data NamedDomainNode = NameDomainNode NameNode (Maybe (LToken, DomainNode))
    deriving (Show, Data)
instance Pretty NamedDomainNode where
    pretty (NameDomainNode nn Nothing) = pretty nn
    pretty (NameDomainNode nn (Just (e, d))) = pretty nn <> pretty e <> pretty d

instance Null NamedDomainNode where
    isMissing (NameDomainNode (a) Nothing) = isMissing a
    isMissing (NameDomainNode (a) (Just (b, c))) = isMissing a && isMissing b && isMissing c

-- Common Statements
data NameNodeS = NameNodeS SToken 
    deriving (Show, Data)
instance Pretty NameNodeS where
    pretty (NameNodeS n) = pretty n

instance Null NameNodeS where
    isMissing = const False
data NameNode = NameNode NameNodeS | MissingNameNode LToken
    deriving (Show, Data)


instance Pretty NameNode where
    pretty (NameNode n) = pretty n
    pretty _ = emptyDoc

instance Null NameNode where
    isMissing (NameNode _) = False
    isMissing (MissingNameNode _) = True

-- Expressions
data ExpressionNode
    = Literal LiteralNode
    | IdentifierNode NameNodeS
    | MetaVarExpr SToken
    | QuantificationExpr QuantificationExpressionNode
    | OperatorExpressionNode OperatorExpressionNode
    | DomainExpression DomainExpressionNode
    | ParenExpression ParenExpressionNode
    | AbsExpression ParenExpressionNode
    | FunctionalApplicationNode SToken (ListNode ExpressionNode)
    | AttributeAsConstriant SToken (ListNode ExpressionNode)
    | MissingExpressionNode LToken
    | SpecialCase SpecialCaseNode
    deriving (Show, Data)

instance Pretty ExpressionNode where
    pretty x = case x of
        Literal ln -> pretty ln
        IdentifierNode nn -> pretty nn
        MetaVarExpr lt -> pretty lt
        QuantificationExpr qen -> pretty qen
        OperatorExpressionNode oen -> pretty oen
        DomainExpression den -> pretty den
        ParenExpression (ParenExpressionNode l e r) -> pretty l <> pretty e <> pretty r
        AbsExpression (ParenExpressionNode l e r) -> pretty l <> pretty e <> pretty r
        FunctionalApplicationNode lt ln -> pretty lt <> pretty ln
        AttributeAsConstriant lt ln -> pretty lt <> pretty ln
        MissingExpressionNode _ -> emptyDoc
        SpecialCase scn -> pretty scn
instance Null ExpressionNode where
    isMissing (MissingExpressionNode _) = True
    isMissing _ = False

data SpecialCaseNode = ExprWithDecls SToken ExpressionNode SToken [StatementNode] SToken
    deriving (Show, Data)
instance Pretty SpecialCaseNode where
    pretty x = case x of
        ExprWithDecls lt en lt' sns lt2 -> group $ cat [pretty lt, pretty en, pretty lt', pretty sns, pretty lt2]

data DomainExpressionNode
    = DomainExpressionNode LToken DomainNode LToken
    deriving (Show, Data)
instance Pretty DomainExpressionNode where
    pretty (DomainExpressionNode l d r) = pretty l <> pretty d <> pretty r
data ParenExpressionNode = ParenExpressionNode LToken ExpressionNode LToken
    deriving (Show, Data)

newtype ShortTuple = ShortTuple (ListNode ExpressionNode) deriving (Show, Data)
instance Pretty ShortTuple where
    pretty (ShortTuple exps) = pretty exps
instance Null ShortTuple where
    isMissing (ShortTuple ls) = isMissing ls

data LongTuple = LongTuple SToken (ListNode ExpressionNode) deriving (Show, Data)
instance Pretty LongTuple where
    pretty (LongTuple t exps) = pretty t <> pretty exps

instance Null LongTuple where
    isMissing (LongTuple s ls) = isMissing s && isMissing ls

-- Literals
data LiteralNode
    = IntLiteral SToken (Maybe ETok) -- the IntTag
    | BoolLiteral SToken
    | MatrixLiteral MatrixLiteralNode
    | TupleLiteralNode LongTuple
    | TupleLiteralNodeShort ShortTuple
    | RecordLiteral SToken (ListNode RecordMemberNode)
    | VariantLiteral SToken (ListNode RecordMemberNode) -- catch later
    | SetLiteral (ListNode ExpressionNode)
    | MSetLiteral SToken (ListNode ExpressionNode)
    | FunctionLiteral SToken (ListNode ArrowPairNode)
    | SequenceLiteral SToken (ListNode ExpressionNode)
    | PermutationLiteral SToken (ListNode PermutationElemNode)
    | RelationLiteral SToken (ListNode RelationElemNode)
    | PartitionLiteral SToken (ListNode PartitionElemNode)
    deriving (Show, Data)

instance Pretty LiteralNode where
    pretty l = case l of
        IntLiteral lt Nothing -> pretty lt
        IntLiteral lt (Just tag) -> pretty lt <> ":" <> pretty tag
        BoolLiteral lt -> pretty lt
        MatrixLiteral mln -> pretty mln
        TupleLiteralNode lt -> pretty lt
        TupleLiteralNodeShort st -> pretty st
        RecordLiteral lt ln -> pretty lt <> pretty ln
        VariantLiteral lt ln -> pretty lt <> pretty ln
        SetLiteral ln -> pretty ln
        MSetLiteral lt ln -> pretty lt <> pretty ln
        FunctionLiteral lt ln -> pretty lt <> pretty ln
        SequenceLiteral lt ln -> pretty lt <> pretty ln
        PermutationLiteral lt ln  -> pretty lt <> pretty ln
        RelationLiteral lt ln -> pretty lt <> pretty ln
        PartitionLiteral lt ln -> pretty lt <> pretty ln

data MatrixLiteralNode
    = MatrixLiteralNode
        LToken -- openBracket
        (Sequence ExpressionNode)
        (Maybe OverDomainNode) -- explicitDomain
        (Maybe ComprehensionNode) -- compBody
        LToken -- close
    deriving (Show, Data)

instance Pretty MatrixLiteralNode where
    pretty (MatrixLiteralNode bl es d c br) =
        group $
            align (cat (pretty bl : prettyElems es ++ catMaybes ((pretty <$> d) : comps) ++ [pretty br]))
      where
        comps = case c of
            Nothing -> []
            Just (ComprehensionNode l seq) -> pure <$> pretty l : prettyElems seq

data ComprehensionNode
    = ComprehensionNode
        SToken
        (Sequence ComprehensionBodyNode)
    deriving (Show, Data)

instance Pretty ComprehensionNode where
    pretty (ComprehensionNode bar es) = align $ pretty bar <+> pretty es

data RecordMemberNode = RecordMemberNode NameNode LToken ExpressionNode
    deriving (Show, Data)
instance Pretty RecordMemberNode where
    pretty (RecordMemberNode n t e) = pretty n <> pretty t <> pretty e

instance Null RecordMemberNode where
    isMissing (RecordMemberNode n t e) = isMissing n && isMissing t && isMissing e

data ArrowPairNode = ArrowPairNode ExpressionNode LToken ExpressionNode
    deriving (Show, Data)
instance Pretty ArrowPairNode where
    pretty (ArrowPairNode l a r) = pretty l <> pretty a <> pretty r
instance Null ArrowPairNode where
    isMissing (ArrowPairNode l a b) = isMissing l && isMissing a && isMissing b

data RelationElemNode
    = RelationElemNodeLabeled LongTuple
    | RelationElemNodeShort ShortTuple
    deriving (Show, Data)
instance Pretty RelationElemNode where
    pretty x = case x of
        RelationElemNodeLabeled lt -> pretty lt
        RelationElemNodeShort st -> pretty st
instance Null RelationElemNode where
    isMissing (RelationElemNodeLabeled lt) = isMissing lt
    isMissing (RelationElemNodeShort st) = isMissing st

newtype PermutationElemNode = PermutationElemNode (ListNode ExpressionNode)
    deriving (Show, Data)
instance Pretty PermutationElemNode where
    pretty (PermutationElemNode l) = pretty l
instance Null PermutationElemNode where
    isMissing (PermutationElemNode l) = isMissing l

newtype PartitionElemNode = PartitionElemNode (ListNode ExpressionNode)
    deriving (Show, Data)
instance Pretty PartitionElemNode where
    pretty (PartitionElemNode l) = pretty l
instance Null PartitionElemNode where
    isMissing (PartitionElemNode l) = isMissing l

data QuantificationExpressionNode
    = QuantificationExpressionNode
        SToken
        (Sequence AbstractPatternNode)
        QuantificationOverNode
        (Maybe QuanticationGuard)
        LToken -- dot
        ExpressionNode
    deriving (Show, Data) -- MAYBE?

instance Pretty QuantificationExpressionNode where
    pretty (QuantificationExpressionNode q pats over m_guard lDot body) =
        group $ hd <+> flatIndent 4 (pretty body)
      where
        hd = group $ pretty q <+> pretty pats <+> pretty over <+> pretty m_guard <+> pretty lDot
data QuantificationOverNode
    = QuantifiedSubsetOfNode SToken ExpressionNode
    | QuantifiedMemberOfNode SToken ExpressionNode
    | QuantifiedDomainNode OverDomainNode
    deriving (Show, Data)
instance Pretty QuantificationOverNode where
    pretty q = case q of
        QuantifiedSubsetOfNode lt en -> pretty lt <+> pretty en
        QuantifiedMemberOfNode lt en -> pretty lt <+> pretty en
        QuantifiedDomainNode odn -> pretty odn

data OverDomainNode = OverDomainNode LToken DomainNode
    deriving (Show, Data)
instance Pretty OverDomainNode where
    pretty (OverDomainNode a b) = pretty a <+> pretty b
data AbstractPatternNode
    = AbstractIdentifier NameNodeS
    | AbstractMetaVar SToken
    | AbstractPatternTuple (Maybe LToken) (ListNode AbstractPatternNode)
    | AbstractPatternMatrix (ListNode AbstractPatternNode)
    | AbstractPatternSet (ListNode AbstractPatternNode)
    deriving (Show, Data)
instance Pretty AbstractPatternNode where
    pretty a = case a of
        AbstractIdentifier nn -> pretty nn
        AbstractMetaVar lt -> pretty lt
        AbstractPatternTuple m_lt ln -> pretty m_lt <> pretty ln
        AbstractPatternMatrix ln -> pretty ln
        AbstractPatternSet ln -> pretty ln

instance Null AbstractPatternNode where
    isMissing (_) = False
data QuanticationGuard = QuanticationGuard SToken ExpressionNode
    deriving (Show, Data)
instance Pretty QuanticationGuard where
    pretty (QuanticationGuard a e) = pretty a <+> pretty e
data QuantificationPattern
    = QuantificationPattern ExpressionNode
    deriving (Show, Data)

data ComprehensionExpressionNode
    = ComprehensionExpressionNode
        LToken
        ExpressionNode
        LToken
        (Sequence ComprehensionBodyNode)
        LToken
    deriving (Show, Data)

data ComprehensionBodyNode
    = CompBodyCondition ExpressionNode
    | CompBodyDomain (Sequence AbstractPatternNode) SToken DomainNode
    | CompBodyGenExpr (Sequence AbstractPatternNode) SToken ExpressionNode
    | CompBodyLettingNode SToken AbstractPatternNode LToken ExpressionNode
    deriving (Show, Data)

instance Pretty ComprehensionBodyNode where
    pretty x = case x of
        CompBodyCondition en -> pretty en
        CompBodyDomain se lt dn -> pretty se <+> pretty lt <+> pretty dn
        CompBodyGenExpr se lt en -> pretty se <+> pretty lt <+> pretty en
        CompBodyLettingNode lt apn lt' en -> pretty lt <+> pretty apn <+> pretty lt' <+> pretty en

instance Null ComprehensionBodyNode where
    isMissing (CompBodyCondition a) = isMissing a
    isMissing (CompBodyDomain a b c) = isMissing a && isMissing b && isMissing c
    isMissing (CompBodyGenExpr s t e) = isMissing s && isMissing t && isMissing e
    isMissing (CompBodyLettingNode t p l e) = isMissing t && isMissing p && isMissing l && isMissing e
data OperatorExpressionNode
    = PostfixOpNode ExpressionNode PostfixOpNode
    | PrefixOpNode SToken ExpressionNode
    | BinaryOpNode ExpressionNode SToken ExpressionNode
    deriving (Show, Data)

instance Pretty OperatorExpressionNode where
    pretty x = case x of
        PostfixOpNode en pon -> pretty en <> pretty pon
        PrefixOpNode lt en -> pretty lt <> pretty en
        BinaryOpNode en lt en' -> group $ sep [pretty en, pretty lt, pretty en']

data PostfixOpNode
    = IndexedNode (ListNode RangeNode)
    | OpFactorial SToken
    | ExplicitDomain SToken SToken DomainNode LToken
    | ApplicationNode (ListNode ExpressionNode)
    deriving (Show, Data)

instance Pretty PostfixOpNode where
    pretty o = case o of
        IndexedNode ln -> pretty ln
        OpFactorial lt -> pretty lt
        ExplicitDomain lt lt' dn lt2 -> pretty lt <+> pretty lt' <> pretty dn <> pretty lt2
        ApplicationNode ln -> pretty ln

-- data FunctionApplicationNode
--     = FunctionApplicationNode LToken (ListNode ExpressionNode)

data IndexerNode
    = Indexer
    deriving (Show, Data)

data ListNode itemType = ListNode
    { lOpBracket :: LToken
    , items :: Sequence itemType
    , lClBracket :: LToken
    }
    deriving (Show, Data)

instance Pretty a => Pretty (ListNode a) where
    pretty (ListNode start es end) =
        group $
            align $
                cat $
                    [ pretty start
                    , flatAlt (indent 4 $ pretty es) (pretty es)
                    , pretty end
                    ]

instance (Null a) => Null (ListNode a) where
    isMissing (ListNode l1 s l2) = isMissing l1 && isMissing s && isMissing l2

newtype Sequence itemType = Seq
    { elems :: [SeqElem itemType]
    }
    deriving (Show, Data)

instance Pretty a => Pretty (Sequence a) where
    pretty (Seq xs) = align $ sep $ map pretty xs

prettyElems :: (Pretty a) => Sequence a -> [Doc ann]
prettyElems (Seq xs) = map pretty xs

instance (Null a) => Null (SeqElem a) where
    isMissing (SeqElem i Nothing) = isMissing i
    isMissing (SeqElem i x) = isMissing i && isMissing x
    isMissing (MissingSeqElem _ c) = isMissing c

instance (Null a) => Null (Sequence a) where
    isMissing (Seq []) = True
    isMissing (Seq [a]) = isMissing a
    isMissing (Seq _) = False

-- deriving (Show, Data)
-- instance (Show a) => Show (Sequence a) where
--     show (Seq e) = "Seq:\n" ++ intercalate "\n\t" (map show e) ++ "\n"

data SeqElem itemType
    = SeqElem
        { item :: itemType
        , separator :: Maybe LToken
        }
    | MissingSeqElem LToken LToken
    deriving (Show, Data)
instance Pretty a => Pretty (SeqElem a) where
    pretty (SeqElem i s) = pretty i <> pretty s
    pretty _ = emptyDoc

class Null a where
    isMissing :: a -> Bool

instance (Null a) => Null (Maybe a) where
    isMissing Nothing = True
    isMissing (Just s) = isMissing s

prettyTokenAndComments :: LToken -> (Doc ann, Doc ann)
prettyTokenAndComments (RealToken (StrictToken [] t)) = prettySplitComments t
prettyTokenAndComments (o) = (emptyDoc, pretty o)

topLevelPretty :: [LToken] -> Doc ann -> Doc ann
topLevelPretty (t : (map pretty -> xs)) exprs =
    let (cs, ps) = prettyTokenAndComments t
        dec = ps <+> hsep xs
     in cs <> group (fill 7 dec <+> flatIndent 4 exprs) <> line
topLevelPretty _ exprs = group (fill 7 emptyDoc <+> flatIndent 4 exprs) <> line

flatIndent :: Int -> Doc ann -> Doc ann
flatIndent amt d = flatAlt (line <> indent amt d) d

renderAST :: Int -> ProgramTree -> Text
renderAST n = renderStrict . layoutSmart (LayoutOptions $ AvailablePerLine n 0.8) . pretty


