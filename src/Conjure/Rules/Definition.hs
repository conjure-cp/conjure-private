{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}

module Conjure.Rules.Definition
    ( Rule(..), RuleResult(..), namedRule, namedRuleZ
    , Question(..), QuestionType(..), Answer(..)
    , LogOrModel, LogOr
    , Driver, Strategy(..), viewAuto, parseStrategy
    , Config(..)
    , UnnamedSymmetryBreaking(..), USBQuickOrComplete(..), USBScope(..), USBIndependentlyOrAltogether(..)
    , ModelZipper, mkModelZipper, fromModelZipper
    , ModelWIP(..), modelWIPOut, updateModelWIPInfo
    , isAtomic
    , representationOf, hasRepresentation
    , sameRepresentation, sameRepresentationTree
    , matchFirst
    ) where

import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Type ( TypeCheckerMode )
import Conjure.Language.Expression.Op

import Conjure.Language.RepresentationOf
import Conjure.Process.Enumerate ( EnumerateDomain )

-- uniplate
import Data.Generics.Uniplate.Zipper ( Zipper, fromZipper, zipperBi )

import qualified Data.HashMap.Strict as M       -- unordered-containers


type LogOr a = Either (LogLevel, Doc) a
type LogOrModel = LogOr Model

data Question = Question
    { qType       :: QuestionType
    , qHole       :: Expression
    , qAscendants :: [Expression]
    , qAnswers    :: [Answer]
    }

data QuestionType
    = ChooseRepr
    | ChooseRepr_Find Name
    | ChooseRepr_Given Name
    | ChooseRepr_Auxiliary
    | ChooseRepr_Quantified
    | ChooseRepr_Cut Name
    | ExpressionRefinement
    deriving (Eq, Ord, Show, Generic)

instance Hashable QuestionType

data Answer = Answer
    { aText      :: Doc
    , aBefore    :: Expression
    , aAnswer    :: Expression
    , aFullModel :: ModelWIP
    , aRuleName  :: Doc
    }

type Driver = (forall m . (MonadIO m, MonadLog m) => [Question] -> m [ModelWIP])

type ModelZipper = Zipper (LanguageVersion, [Statement]) Expression

mkModelZipper :: Model -> Maybe ModelZipper
mkModelZipper (Model lang stmts _) = zipperBi (lang, stmts)

fromModelZipper :: ModelZipper -> ModelInfo -> Model
fromModelZipper z info =
    let (lang, stmts) = fromZipper z
    in  Model lang stmts info

data ModelWIP = StartOver Model | TryThisFirst ModelZipper ModelInfo

modelWIPOut :: ModelWIP -> Model
modelWIPOut (StartOver m) = m
modelWIPOut (TryThisFirst z minfo) =
    let (lang, stmts) = fromZipper z
    in Model lang stmts minfo

updateModelWIPInfo :: (ModelInfo -> ModelInfo) -> ModelWIP -> ModelWIP
updateModelWIPInfo upd (StartOver model) = StartOver model { mInfo = upd (mInfo model) }
updateModelWIPInfo upd (TryThisFirst z info) = TryThisFirst z (upd info)


data Config = Config
    { logLevel                   :: LogLevel
    , verboseTrail               :: Bool
    , rewritesTrail              :: Bool
    , logRuleFails               :: Bool
    , logRuleSuccesses           :: Bool
    , logRuleAttempts            :: Bool
    , logChoices                 :: Bool
    , followTrail                :: M.HashMap Int -- Question hash
                                              Int -- Answer hash
    , strategyQ                  :: Strategy
    , strategyA                  :: Strategy
    , representations            :: Strategy
    , representationsFinds       :: Strategy
    , representationsGivens      :: Strategy
    , representationsAuxiliaries :: Strategy
    , representationsQuantifieds :: Strategy
    , representationsCuts        :: Strategy
    , outputDirectory            :: FilePath
    , channelling                :: Bool
    , representationLevels       :: Bool
    , unnamedSymmetryBreaking    :: Maybe UnnamedSymmetryBreaking
    , limitModels                :: Maybe Int
    , numberingStart             :: Int
    , smartFilenames             :: Bool
    , lineWidth                  :: Int
    , responses                  :: Maybe [Int]
    , responsesRepresentation    :: Maybe [(Name, Int)]
    , generateStreamliners       :: Maybe [Int]
    , estimateNumberOfModels     :: Bool
    }
    deriving (Eq, Ord, Show, Data, Typeable)

instance Default Config where
    def = Config
        { logLevel                   = LogNone
        , verboseTrail               = False
        , rewritesTrail              = False
        , logRuleFails               = False
        , logRuleSuccesses           = False
        , logRuleAttempts            = False
        , logChoices                 = False
        , followTrail                = M.empty
        , strategyQ                  = Interactive
        , strategyA                  = Interactive
        , representations            = Interactive
        , representationsFinds       = Interactive
        , representationsGivens      = Interactive
        , representationsAuxiliaries = Interactive
        , representationsQuantifieds = Interactive
        , representationsCuts        = Interactive
        , outputDirectory            = "conjure-output"
        , channelling                = True
        , representationLevels       = True
        , unnamedSymmetryBreaking    = Nothing
        , limitModels                = Nothing
        , numberingStart             = 1
        , smartFilenames             = False
        , lineWidth                  = 120
        , responses                  = Nothing
        , responsesRepresentation    = Nothing
        , generateStreamliners       = Nothing
        , estimateNumberOfModels     = False
        }


-- 1. Quick/Complete. Quick is x .<= p(x)
--                    Complete is x .<= y /\ y = p(x)
-- 2. Scope.          Consecutive
--                    AllPairs
--                    AllPermutations
-- 3. Independently/Altogether
-- in addition, we have
--      none
--      fast: Quick-Consecutive-Independently
--      full: Complete-AllPermutations-Altogether
data UnnamedSymmetryBreaking =
        UnnamedSymmetryBreaking
            USBQuickOrComplete
            USBScope
            USBIndependentlyOrAltogether
    deriving (Eq, Ord, Show, Read, Data, Typeable)
data USBQuickOrComplete = USBQuick | USBComplete
    deriving (Eq, Ord, Show, Read, Data, Typeable)
data USBScope = USBConsecutive | USBAllPairs | USBAllPermutations
    deriving (Eq, Ord, Show, Read, Data, Typeable)
data USBIndependentlyOrAltogether = USBIndependently | USBAltogether
    deriving (Eq, Ord, Show, Read, Data, Typeable)


data RuleResult m = RuleResult
    { ruleResultDescr :: Doc                        -- describe this transformation
    , ruleResultType  :: QuestionType
    , ruleResult      :: m Expression               -- the result
    , ruleResultHook  :: Maybe (Model -> m Model)   -- post-application hook
    }

data Rule = Rule
    { rName  :: Doc
    , rApply
        :: forall n m a .
            ( MonadFailDoc n, MonadUserError n, MonadLog n
            , NameGen n, EnumerateDomain n, MonadReader (Zipper a Expression) n
                -- a fail in {n} means that the rule isn't applicable
            , MonadFailDoc m, MonadUserError m, MonadLog m
            , NameGen m, EnumerateDomain m
                -- a failDoc in {m} means a bug
            , ?typeCheckerMode :: TypeCheckerMode
            )
        => Zipper a Expression            -- to query context
        -> Expression
        -> n [RuleResult m]
    }

namedRule
    :: Doc
    -> (forall n m a .
            ( MonadFailDoc n, MonadUserError n, MonadLog n
            , NameGen n, EnumerateDomain n, MonadReader (Zipper a Expression) n
            ,  MonadFailDoc m, MonadUserError m, MonadLog m
            , NameGen m, EnumerateDomain m
            , ?typeCheckerMode :: TypeCheckerMode
            ) => Expression -> n (Doc, m Expression))
    -> Rule
namedRule nm f = Rule
    { rName = nm
    , rApply = \ z x -> do
        (rResultDescr, rResult) <- runReaderT (f x) z
        return [RuleResult rResultDescr ExpressionRefinement rResult Nothing]
    }

namedRuleZ
    :: Doc
    -> (forall n m a .
            ( MonadFailDoc  n, MonadUserError n, MonadLog n
            , NameGen n, EnumerateDomain n, MonadReader (Zipper a Expression) n
            , MonadFailDoc m, MonadUserError m, MonadLog m
            , NameGen m, EnumerateDomain m
            , ?typeCheckerMode :: TypeCheckerMode
            ) => Zipper a Expression -> Expression -> n (Doc, m Expression))
    -> Rule
namedRuleZ nm f = Rule
    { rName = nm
    , rApply = \ z x -> do
        (rResultDescr, rResult) <- runReaderT (f z x) z
        return [RuleResult rResultDescr ExpressionRefinement rResult Nothing]
    }

isAtomic :: Expression -> Bool
isAtomic Reference{} = True
isAtomic (Op (MkOpIndexing (OpIndexing a _))) = isAtomic a
isAtomic _ = False


matchFirst
    :: MonadFailDoc m
    => [a]                  -- list of things to try matching on
    -> (a -> Maybe b)       -- the matcher
    -> m ( [a]              -- befores
         , b                -- the matching one
         , [a]              -- afters
         )
matchFirst = helper []
    where
        helper _ [] _ = na "matchFirst"
        helper befores (x:xs) f = case f x of
            Nothing -> helper (x:befores) xs f
            Just y  -> return (reverse befores, y, xs)
