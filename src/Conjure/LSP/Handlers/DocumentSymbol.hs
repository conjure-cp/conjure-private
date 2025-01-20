module Conjure.LSP.Handlers.DocumentSymbol where

import Conjure.LSP.Util (ProcessedFile (ProcessedFile), regionToRange, withProcessedDoc)
import Conjure.Language (Type (..))
import Conjure.Language.Pretty (prettyT)
import Conjure.Language.Type (IntTag (..))
import Conjure.Language.Validator (Class (..), Kind (..), RegionInfo (..), RegionType (..), StructuralType (..), ValidatorState (regionInfo))
import Conjure.Prelude
import Control.Lens
import Data.Text (intercalate)
import Language.LSP.Server (Handlers, LspM, requestHandler)
import Language.LSP.Types (DocumentSymbol (..), SMethod (STextDocumentDocumentSymbol), SymbolKind (..), type (|?) (..))
import Language.LSP.Types qualified as T
import Language.LSP.Types.Lens (HasParams (..), HasTextDocument (textDocument))

docSymbolHandler :: Handlers (LspM ())
docSymbolHandler = requestHandler STextDocumentDocumentSymbol $ \req res -> do
  let ps = req ^. params . textDocument
  withProcessedDoc ps $ \(ProcessedFile _ _ (regionInfo -> ri) _) -> do
    res $ Right $ InL . T.List $ mapMaybe translate ri

translate :: RegionInfo -> Maybe T.DocumentSymbol
translate reg@(RegionInfo r rSel ty cs _) =
  ( \x ->
      DocumentSymbol
        (getRegionName reg)
        (getRegionDetail reg)
        x
        Nothing
        Nothing
        (regionToRange r)
        (regionToRange (fromMaybe r rSel))
        (Just . T.List $ mapMaybe translate cs)
  )
    <$> sk
  where
    sk = symbolKindFromDeclaration ty

getRegionName :: RegionInfo -> Text
getRegionName (rRegionType -> rType) = case rType of
  Definition txt _ -> txt
  LiteralDecl _ -> "Literal"
  Ref txt _ _ -> txt
  Structural st -> case st of
    SSuchThat -> "Constraints"
    SGiven -> "Parameters"
    SFind -> "Decision Variables"
    SLetting -> "Definitions"
    SEnum txt -> txt
    SBranching -> "Branch"
    SQuantification n _ -> "Quantification: " `mappend` n
    SComprehension _ -> "Comprehension"
    SBody -> "Body"
    SGuard -> "Guard"
    SGen -> "Generator"
    SWhere -> "Parameter validation"
    SGoal dir -> dir
  Documentation _ _ -> ""

getRegionDetail :: RegionInfo -> Maybe Text
getRegionDetail (RegionInfo {rRegionType = rType, rChildren = childDefs}) =
  case rType of
    Definition _ ki -> Just $ prettyT ki
    LiteralDecl ki -> Just $ prettyT ki
    Ref _ ki _ -> Just $ prettyT ki
    Structural st -> case st of
      SSuchThat -> Nothing
      SGiven -> Just $ getDefs childDefs
      SFind -> Just $ getDefs childDefs
      SLetting -> Just $ getDefs childDefs
      SEnum _ -> Just "new type enum"
      SQuantification _ ki -> Just $ prettyT ki
      SComprehension ki -> Just $ prettyT ki
      _ -> Nothing
    Documentation {} -> Nothing
  where
    getDefs :: [RegionInfo] -> Text
    getDefs rs = Data.Text.intercalate ", " [nm | Definition nm _ <- rRegionType <$> rs]

symbolKindFromDeclaration :: RegionType -> Maybe T.SymbolKind
symbolKindFromDeclaration (Definition _ t) = Just $ case t of
  Kind ValueType {} (TypeInt TagEnum {}) -> SkEnumMember
  Kind ValueType {} (TypeRecordMember {}) -> SkField
  Kind ValueType {} (TypeVariantMember {}) -> SkField
  Kind ValueType {} _ -> SkVariable
  Kind DomainType _ -> SkTypeParameter
symbolKindFromDeclaration (LiteralDecl t) = Just $ case t of
  Kind _ ty -> case ty of
    TypeBool -> SkBoolean
    TypeInt it -> case it of
      TaggedInt {} -> SkNumber
      TagInt -> SkNumber
      TagEnum _ -> SkEnumMember
      TagUnnamed _ -> SkNumber
    TypeEnum _ -> SkEnum
    TypeUnnamed _ -> SkEnum
    _ -> SkConstant
symbolKindFromDeclaration (Structural st) =
  Just
    $ ( case st of
          SSuchThat -> SkInterface
          SGiven -> SkProperty
          SFind -> SkField
          SLetting -> SkField
          SBranching -> SkClass
          SEnum _ -> SkEnum
          SQuantification _ _ -> SkOperator
          SComprehension _ -> SkArray
          SGuard -> SkBoolean
          SGen -> SkEvent
          SBody -> SkNamespace
          SGoal _ -> SkVariable
          SWhere -> SkObject
      )
symbolKindFromDeclaration _ = Nothing