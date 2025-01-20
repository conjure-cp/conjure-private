{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs

module Conjure.UI ( UI(..), OutputFormat(..), ui, versionLine ) where

-- conjure
import Conjure.Prelude
import Conjure.RepositoryVersion ( repositoryVersion )
import Paths_conjure_cp ( version )

-- base
import Data.Version ( showVersion )

-- cmdargs
import System.Console.CmdArgs hiding ( Default(..) )


data UI
    = Modelling
        { essence                    :: FilePath            -- essence, mandatory
        -- flags related to output
        , outputDirectory            :: FilePath
        , numberingStart             :: Int
        , smartFilenames             :: Bool
        , responses                  :: String
        , responsesRepresentation    :: String
        , estimateNumberOfModels     :: Bool                -- if set Conjure will calculate
                                                            -- a lower bound on the number of models,
                                                            -- instead of running the usual modelling mode
        -- flags related to logging
        , logLevel                   :: LogLevel
        , verboseTrail               :: Bool
        , rewritesTrail              :: Bool
        , logRuleFails               :: Bool
        , logRuleSuccesses           :: Bool
        , logRuleAttempts            :: Bool
        , logChoices                 :: Bool
        -- flags related to modelling decisions
        , portfolio                  :: Maybe Int
        , strategyQ                  :: String
        , strategyA                  :: String
        , representations            :: Maybe String        -- (def: strategyA)
        , representationsFinds       :: Maybe String        -- (def: representations)
        , representationsGivens      :: Maybe String        -- (def: s)
        , representationsAuxiliaries :: Maybe String        -- (def: representations)
        , representationsQuantifieds :: Maybe String        -- (def: representations)
        , representationsCuts        :: Maybe String        -- (def: representations)
        , channelling                :: Bool
        , representationLevels       :: Bool                -- (def: True)
        , unnamedSymmetryBreaking    :: String
        , followModel                :: FilePath            -- this is a model to be followed
        , seed                       :: Maybe Int
        , limitModels                :: Maybe Int
        , limitTime                  :: Maybe Int
        , savedChoices               :: Maybe FilePath
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        -- streamlining
        , generateStreamliners       :: String
        }
    | TranslateParameter
        { eprime                     :: FilePath            -- eprime, mandatory
        , essenceParam               :: FilePath            -- essence-param, mandatory
        , eprimeParam                :: Maybe FilePath      -- eprime-param, optional
                                                            -- by default (essenceParam <-.> "eprime-param")
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | TranslateSolution
        { eprime                     :: FilePath            -- eprime, mandatory
        , essenceParamO              :: Maybe FilePath      -- essence-param, optional
        , eprimeSolution             :: FilePath            -- eprime-solution, mandatory
        , essenceSolutionO           :: Maybe FilePath      -- essence-solution, optional
                                                            -- by default (eprimeSolution <-.> "solution")
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | ValidateSolution
        { essence                    :: FilePath            -- essence, mandatory
        , essenceParamO              :: Maybe FilePath      -- essence-param, optional
        , essenceSolution            :: FilePath            -- essence-solution, mandatory
                                                            -- by default (eprimeSolution <-.> "solution")
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | Solve
        { essence                    :: FilePath            -- essence, mandatory
        , essenceParams              :: [FilePath]
        , validateSolutionsOpt       :: Bool
        -- flags related to output
        , outputDirectory            :: FilePath
        , numberingStart             :: Int
        , smartFilenames             :: Bool
        , responses                  :: String
        , responsesRepresentation    :: String
        , solutionsInOneFile         :: Bool
        , runsolverCPUTimeLimit      :: Maybe Int
        , runsolverWallTimeLimit     :: Maybe Int
        , runsolverMemoryLimit       :: Maybe Int
        -- flags related to logging
        , logLevel                   :: LogLevel
        , verboseTrail               :: Bool
        , rewritesTrail              :: Bool
        , logRuleFails               :: Bool
        , logRuleSuccesses           :: Bool
        , logRuleAttempts            :: Bool
        , logChoices                 :: Bool
        -- flags related to modelling decisions
        , portfolio                  :: Maybe Int
        , strategyQ                  :: String
        , strategyA                  :: String
        , representations            :: Maybe String
        , representationsFinds       :: Maybe String
        , representationsGivens      :: Maybe String
        , representationsAuxiliaries :: Maybe String
        , representationsQuantifieds :: Maybe String
        , representationsCuts        :: Maybe String
        , channelling                :: Bool
        , representationLevels       :: Bool                -- (def: True)
        , unnamedSymmetryBreaking    :: String
        , followModel                :: FilePath            -- this is a model to be followed
        , seed                       :: Maybe Int
        , limitModels                :: Maybe Int
        , limitTime                  :: Maybe Int
        , useExistingModels          :: [FilePath]          -- [] by default, which means generate models
        -- flags for SR and the selected solver
        , savilerowOptions           :: [String]
        , solverOptions              :: [String]
        , solver                     :: String
        , graphSolver                :: Bool
        , cgroups                    :: Bool
        , nbSolutions                :: String              -- a number, or "all". by default 1
        , copySolutions              :: Bool
        -- output
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        -- streamlining
        , generateStreamliners       :: String
        }
    | IDE
        { essence                    :: FilePath            -- Optional, will read from stdin if not provided
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , lineWidth                  :: Int                 -- 120 by default
        , dumpDeclarations           :: Bool
        , dumpRepresentations        :: Bool
        }
    | Pretty
        { essence                    :: FilePath
        , normaliseQuantified        :: Bool
        , removeUnused               :: Bool
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | Diff
        { file1                      :: FilePath
        , file2                      :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | TypeCheck
        { essence                    :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        }
    | Split
        { essence                    :: FilePath
        , outputDirectory            :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | SymmetryDetection
        { essence                    :: FilePath
        , json                       :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | ParameterGenerator
        { essence                    :: FilePath
        , minInt                     :: Integer
        , maxInt                     :: Integer
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | AutoIG
        { essence                    :: FilePath
        , outputFilepath             :: FilePath
        , generatorToIrace           :: Bool
        , removeAux                  :: Bool
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | Boost
        { essence                    :: FilePath
        , logLevel                   :: LogLevel
        , logRuleSuccesses           :: Bool
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | Streamlining
        { essence                    :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | LSP
        { logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize UI
instance Hashable  UI
instance ToJSON    UI where toJSON = genericToJSON jsonOptions
instance FromJSON  UI where parseJSON = genericParseJSON jsonOptions


data OutputFormat = Plain | Binary | ASTJSON | JSON | JSONStream | MiniZinc
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize OutputFormat
instance Hashable  OutputFormat
instance ToJSON    OutputFormat where toJSON = genericToJSON jsonOptions
instance FromJSON  OutputFormat where parseJSON = genericParseJSON jsonOptions

instance Default OutputFormat where def = Plain


ui :: UI
ui = modes
    [ Modelling
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , outputDirectory
            = "conjure-output"
            &= typDir
            &= name "output-directory"
            &= name "o"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Where to save generated models.\n\
                    \Default value: 'conjure-output'"
        , numberingStart
            = 1
            &= name "numbering-start"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Starting value for output files.\n\
                    \Default value: 1"
        , smartFilenames
            = False
            &= name "smart-filenames"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Use \"smart names\" for models.\n\
                    \Directs Conjure to use the answers when producing \
                    \a filename and to ignore the order of questions. \
                    \Only useful if 'f' is used for questions."
        , responses
            = ""
            &= name "responses"
            &= groupname "Model generation"
            &= explicit
            &= help "A comma separated list of integers.\n\
                    \If provided, these will be used as the answers during \
                    \interactive model generation instead of prompting the user."
        , responsesRepresentation
            = ""
            &= name "responses-representation"
            &= groupname "Model generation"
            &= explicit
            &= help "A comma separated list of variable name : integer pairs.\n\
                    \If provided, these will be used as the answers during \
                    \interactive model generation instead of prompting the user \
                    \for the variable representation questions.\n\
                    \See --dump-representations for a list of available representation options."
        , estimateNumberOfModels
            = False
            &= name "estimate-number-of-models"
            &= groupname "Model generation"
            &= explicit
            &= help "Calculate (a lower bound on) the number of models, \
                    \instead of running the usual modelling mode."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , verboseTrail
            = False
            &= name "verbose-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate verbose trails."
        , rewritesTrail
            = False
            &= name "rewrites-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate trails about the applied rewritings."
        , logRuleFails
            = False
            &= name "log-rule-fails"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule failures. (Caution: can be a lot!)"
        , logRuleSuccesses
            = False
            &= name "log-rule-successes"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule applications."
        , logRuleAttempts
            = False
            &= name "log-rule-attempts"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule attempts. (Caution: can be a lot!)"
        , logChoices
            = False
            &= name "log-choices"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Store the choices in a way that can be reused by -al"
        , portfolio
            = Nothing
            &= typ "PORTFOLIO"
            &= name "portfolio"
            &= groupname "Model generation"
            &= explicit
            &= help "Portfolio size. When it is set to N, Conjure will try to generate up to N models."
        , strategyQ
            = "f"
            &= typ "STRATEGY"
            &= name "strategy-q"
            &= name "q"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for selecting the next question to answer. \
                    \Options: f (for first), i (for interactive), r (for random), x (for all). \
                    \Prepend a (for auto) to automatically skip \
                    \when there is only one option at any point.\n\
                    \Default value: f"
        , strategyA
            = "ai"
            &= typ "STRATEGY"
            &= name "strategy-a"
            &= name "a"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for selecting an answer. Same options as strategy-q.\n\
                    \ c picks the most 'compact' option \
                    \at every decision point.\n\
                    \ s picks the 'sparsest' option \
                    \at every decision point: \
                    \useful for --representations-givens\n\
                    \Default value: ai"
        , representations
            = Nothing
            &= typ "STRATEGY"
            &= name "representations"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation.\n\
                    \Default value: same as --strategy-a"
        , representationsFinds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-finds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a decision variable.\n\
                    \Default value: same as --representations"
        , representationsGivens
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-givens"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a parameter.\n\
                    \Default value: s (for sparse)"
        , representationsAuxiliaries
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-auxiliaries"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for an auxiliary variable.\n\
                    \Default value: same as --representations"
        , representationsQuantifieds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-quantifieds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a quantified variable.\n\
                    \Default value: same as --representations"
        , representationsCuts
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-cuts"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for cuts in 'branching on'.\n\
                    \Default value: same as --representations"
        , channelling
            = True
            &= name "channelling"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to produce channelled models \
                    \(true by default).\n"
        , representationLevels
            = True
            &= name "representation-levels"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to use built-in precedence levels when choosing representations. \
                    \Used to cut down the number of generated models.\n\
                    \Default: true"
        , unnamedSymmetryBreaking
            = "none"
            &= name "unnamed-symmetry-breaking"
            &= groupname "Model generation"
            &= explicit
            &= help "Level to use for breaking symmetries arising from unnamed types. \
                    \Options: none / fast-consecutive / fast-allpairs / complete-independently / complete.\n\
                    \Default: none"
        , followModel
            = ""
            &= name "follow-model"
            &= groupname "Model generation"
            &= explicit
            &= help "Provide a Conjure-generated Essence Prime model to be used as a guide during model generation. \
                    \Conjure will try to imitate the modelling decisions from this file."
        , seed
            = Nothing
            &= name "seed"
            &= groupname "Model generation"
            &= explicit
            &= help "Random number generator seed."
        , limitModels
            = Nothing
            &= name "limit-models"
            &= groupname "Model generation"
            &= explicit
            &= help "Maximum number of models to generate."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , savedChoices
            = def
            &= typFile
            &= name "choices"
            &= groupname "Model generation"
            &= explicit
            &= help "Choices to use for -al, \
                     \either an eprime file (created by --log-choices), or a json file."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        , generateStreamliners
            = ""
            &= name "generate-streamliners"
            &= groupname "Streamlining"
            &= explicit
            &= help "A comma separated list of integers.\n\
                    \If provided, the streamlining constraints that correspond to the given integers will be generated.\n\
                    \Run \"conjure streamlining ESSENCE_FILE\" to generate a list of all applicable streamliners."
        }   &= name "modelling"
            &= explicit
            &= help "The main act. Given a problem specification in Essence, \
                    \produce constraint programming models in Essence'."
            &= auto
    , TranslateParameter
        { eprime
            = def
            &= typ "ESSENCE_FILE"
            &= name "eprime"
            &= explicit
            &= help "An Essence' model generated by Conjure."
        , essenceParam
            = def
            &= typFile
            &= name "essence-param"
            &= explicit
            &= help "An Essence parameter for the original problem specification."
        , eprimeParam
            = def
            &= typFile
            &= name "eprime-param"
            &= explicit
            &= help "An Essence' parameter matching the Essence' model.\n\
                    \Default is 'foo.eprime-param' \
                    \if the Essence parameter file is named 'foo.param'."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "translate-parameter"
            &= explicit
            &= help "Refinement of Essence parameter files for a \
                    \particular Essence' model.\n\
                    \The model needs to be generated by Conjure."
    , TranslateSolution
        { eprime
            = def
            &= typFile
            &= name "eprime"
            &= explicit
            &= help "An Essence' model generated by Conjure.\n\
                     \Mandatory."
        , essenceParamO
            = def
            &= typFile
            &= name "essence-param"
            &= explicit
            &= help "An Essence parameter for the original problem specification.\n\
                     \Mandatory."
        , eprimeSolution
            = def
            &= typFile
            &= name "eprime-solution"
            &= explicit
            &= help "An Essence' solution for the corresponding Essence' model."
        , essenceSolutionO
            = def
            &= typFile
            &= name "essence-solution"
            &= explicit
            &= help "An Essence solution for the original problem specification.\n\
                    \By default, its value is the value of --eprime-solution \
                    \with extensions replaced by '.solution'."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "translate-solution"
            &= explicit
            &= help "Translation of solutions back to Essence."
    , ValidateSolution
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= name "essence"
            &= explicit
            &= help "Problem specification in Essence."
        , essenceParamO
            = def
            &= typFile
            &= name "param"
            &= explicit
            &= help "Essence parameter file."
        , essenceSolution
            = def
            &= typFile
            &= name "solution"
            &= explicit
            &= help "Essence solution."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "validate-solution"
            &= explicit
            &= help "Validating a solution."
    , Solve
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , essenceParams
            = []
            &= typ "PARAMETER_FILE(s)"
            &= args
        , validateSolutionsOpt
            = False
            &= name "validate-solutions"
            &= groupname "General"
            &= explicit
            &= help "Enable solution validation."
        , outputDirectory
            = "conjure-output"
            &= typDir
            &= name "output-directory"
            &= name "o"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Where to save generated models.\n\
                    \Default value: 'conjure-output'"
        , numberingStart
            = 1
            &= name "numbering-start"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Starting value for output files.\n\
                    \Default value: 1"
        , smartFilenames
            = False
            &= name "smart-filenames"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Use \"smart names\" for models.\n\
                    \Directs Conjure to use the answers when producing \
                    \a filename and to ignore the order of questions. \
                    \Only useful if 'f' is used for questions."
        , responses
            = ""
            &= name "responses"
            &= groupname "Model generation"
            &= explicit
            &= help "A comma separated list of integers.\n\
                    \If provided, these will be used as the answers during \
                    \interactive model generation instead of prompting the user."
        , responsesRepresentation
            = ""
            &= name "responses-representation"
            &= groupname "Model generation"
            &= explicit
            &= help "A comma separated list of variable name : integer pairs.\n\
                    \If provided, these will be used as the answers during \
                    \interactive model generation instead of prompting the user \
                    \for the variable representation questions.\n\
                    \See --dump-representations for a list of available representation options."
        , solutionsInOneFile
            = False
            &= name "solutions-in-one-file"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Place all solutions in a single file instead of generating a separate file per solution.\n\
                    \Off by default."
        , runsolverCPUTimeLimit
            = def
            &= name "runsolver-cpu-time-limit"
            &= groupname "runsolver"
            &= explicit
            &= help "Use runsolver to limit total CPU time (in seconds)"
        , runsolverWallTimeLimit
            = def
            &= name "runsolver-wall-time-limit"
            &= groupname "runsolver"
            &= explicit
            &= help "Use runsolver to limit total elapsed time (in seconds)"
        , runsolverMemoryLimit
            = def
            &= name "runsolver-memory-limit"
            &= groupname "runsolver"
            &= explicit
            &= help "Use runsolver to limit total memory usage (Maximum RSS - in megabytes)."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , verboseTrail
            = False
            &= name "verbose-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate verbose trails."
        , rewritesTrail
            = False
            &= name "rewrites-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate trails about the applied rewritings."
        , logRuleFails
            = False
            &= name "log-rule-fails"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule failures. (Caution: can be a lot!)"
        , logRuleSuccesses
            = False
            &= name "log-rule-successes"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule applications."
        , logRuleAttempts
            = False
            &= name "log-rule-attempts"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule attempts. (Caution: can be a lot!)"
        , logChoices
            = False
            &= name "log-choices"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Store the choices in a way that can be reused by -al"
        , portfolio
            = Nothing
            &= typ "PORTFOLIO"
            &= name "portfolio"
            &= groupname "Model generation"
            &= explicit
            &= help "Portfolio size. When it is set to N, Conjure will try to generate up to N models."
        , strategyQ
            = "f"
            &= typ "STRATEGY"
            &= name "strategy-q"
            &= name "q"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for selecting the next question to answer. \
                    \Options: f (for first), i (for interactive), r (for random), x (for all). \
                    \Prepend a (for auto) to automatically skip \
                    \when there is only one option at any point.\n\
                    \Default value: f"
        , strategyA
            = "c"
            &= typ "STRATEGY"
            &= name "strategy-a"
            &= name "a"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for selecting an answer. Same options as strategy-q.\n\
                    \ c picks the most 'compact' option \
                    \at every decision point.\n\
                    \ s picks the 'sparsest' option \
                    \at every decision point: \
                    \useful for --representations-givens\n\
                    \Default value: c"
        , representations
            = Nothing
            &= typ "STRATEGY"
            &= name "representations"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation.\n\
                    \Default value: same as --strategy-a"
        , representationsFinds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-finds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a decision variable.\n\
                    \Default value: same as --representations"
        , representationsGivens
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-givens"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a parameter.\n\
                    \Default value: s (for sparse)"
        , representationsAuxiliaries
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-auxiliaries"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for an auxiliary variable.\n\
                    \Default value: same as --representations"
        , representationsQuantifieds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-quantifieds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a quantified variable.\n\
                    \Default value: same as --representations"
        , representationsCuts
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-cuts"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for cuts in 'branching on'.\n\
                    \Default value: same as --representations"
        , channelling
            = True
            &= name "channelling"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to produce channelled models \
                    \(true by default).\n"
        , representationLevels
            = True
            &= name "representation-levels"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to use built-in precedence levels when choosing representations. \
                    \Used to cut down the number of generated models.\n\
                    \Default: true"
        , unnamedSymmetryBreaking
            = "none"
            &= name "unnamed-symmetry-breaking"
            &= groupname "Model generation"
            &= explicit
            &= help "Level to use for breaking symmetries arising from unnamed types. \
                    \Options: none / fast-consecutive / fast-allpairs / complete-independently / complete.\n\
                    \Default: none"
        , followModel
            = ""
            &= name "follow-model"
            &= groupname "Model generation"
            &= explicit
            &= help "Provide a Conjure-generated Essence Prime model to be used as a guide during model generation. \
                    \Conjure will try to imitate the modelling decisions from this file."
        , seed
            = Nothing
            &= name "seed"
            &= groupname "Model generation"
            &= explicit
            &= help "Random number generator seed."
        , limitModels
            = Nothing
            &= name "limit-models"
            &= groupname "Model generation"
            &= explicit
            &= help "Maximum number of models to generate."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , useExistingModels
            = []
            &= name "use-existing-models"
            &= groupname "Model generation"
            &= explicit
            &= typFile
            &= help "File names of Essence' models generated beforehand.\n\
                    \If given, Conjure skips the modelling phase and uses the existing models for solving.\n\
                    \The models should be inside the output directory (See -o)."
        , savilerowOptions
            = def
            &= name "savilerow-options"
            &= groupname "Options for other tools"
            &= explicit
            &= help "Options passed to Savile Row."
        , solverOptions
            = def
            &= name "solver-options"
            &= groupname "Options for other tools"
            &= explicit
            &= help "Options passed to the backend solver."
        , solver
            = "minion"
            &= name "solver"
            &= groupname "Options for other tools"
            &= explicit
            &= help "Backend solver. \
                    \Possible values:\n\
                    \ - minion (CP solver)\n\
                    \ - gecode (CP solver)\n\
                    \ - chuffed (CP solver)\n\
                    \ - or-tools (CP solver)\n\
                    \ - glucose (SAT solver)\n\
                    \ - glucose-syrup (SAT solver)\n\
                    \ - lingeling/plingeling/treengeling (SAT solver)\n\
                    \ - cadical (SAT solver)\n\
                    \ - kissat (SAT solver)\n\
                    \ - minisat (SAT solver)\n\
                    \ - bc_minisat_all (AllSAT solver, only works with --number-of-solutions=all)\n\
                    \ - nbc_minisat_all (AllSAT solver, only works with --number-of-solutions=all)\n\
                    \ - open-wbo (MaxSAT solver, only works with optimisation problems)\n\
                    \ - coin-or (MIP solver, implemented via MiniZinc)\n\
                    \ - cplex (MIP solver, implemented via MiniZinc)\n\
                    \ - boolector (SMT solver, supported logics: bv)\n\
                    \ - yices (SMT solver, supported logics: bv, lia, idl)\n\
                    \ - z3 (SMT solver, supported logics: bv, lia, nia, idl)\n\
                    \Default: minion\n\n\
                    \Default logic for SMT solvers is bitvector (bv).\n\
                    \Append a dash and the name of a logic to the solver name to choose a different logic. For example yices-idl."
        , graphSolver
            = False
            &= name "graph-solver"
            &= groupname "General"
            &= explicit
            &= help "Create input files for the Glasgow graph solver."
        , cgroups
            = False
            &= name "cgroups"
            &= groupname "General"
            &= explicit
            &= help "Setup and use cgroups when solving with Savile Row."
        , nbSolutions
            = "1"
            &= name "number-of-solutions"
            &= groupname "General"
            &= explicit
            &= help "Number of solutions to find; \
                    \\"all\" enumerates all solutions.\n\
                    \Default: 1"
        , copySolutions
            = True
            &= name "copy-solutions"
            &= groupname "General"
            &= explicit
            &= help "Whether to place a copy of solution(s) next to the Essence file or not.\n\
                    \Default: on"
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        , generateStreamliners
            = ""
            &= name "generate-streamliners"
            &= groupname "Streamlining"
            &= explicit
            &= help "A comma separated list of integers.\n\
                    \If provided, the streamlining constraints that correspond to the given integers will be generated.\n\
                    \Run \"conjure streamlining ESSENCE_FILE\" to generate a list of all applicable streamliners."
        }   &= name "solve"
            &= explicit
            &= help "A combined mode for convenience.\n\
                    \Runs Conjure in modelling mode followed by \
                    \parameter translation if required, \
                    \then Savile Row + Minion to solve, and \
                    \then solution translation."
    , IDE
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , dumpDeclarations
            = False
            &= name "dump-declarations"
            &= groupname "IDE Features"
            &= explicit
            &= help "Print information about top level declarations."
        , dumpRepresentations
            = False
            &= name "dump-representations"
            &= groupname "IDE Features"
            &= explicit
            &= help "List the available representations for decision variables and parameters."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "ide"
            &= explicit
            &= help "IDE support features for Conjure.\n\
                    \Not intended for direct use."
    , Pretty
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , normaliseQuantified
            = False
            &= name "normalise-quantified"
            &= explicit
            &= help "Normalise the names of quantified variables."
        , removeUnused
            = False
            &= name "remove-unused"
            &= explicit
            &= help "Remove unused declarations."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "pretty"
            &= explicit
            &= help "Pretty print as Essence file to stdout.\n\
                    \This mode can be used to view a binary Essence file in textual form."
    , Diff
        { file1
            = def
            &= typFile
            &= argPos 0
        , file2
            = def
            &= typFile
            &= argPos 1
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "diff"
            &= explicit
            &= help "Diff on two Essence files. Works on models, parameters, and solutions."
    , TypeCheck
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        }   &= name "type-check"
            &= explicit
            &= help "Type-checking a single Essence file."
    , Split
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , outputDirectory
            = "conjure-output"
            &= typDir
            &= name "output-directory"
            &= name "o"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Where to save generated models.\n\
                    \Default value: 'conjure-output'"
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "split"
            &= explicit
            &= help "Split an Essence file to various smaller files. Useful for testing."
    , SymmetryDetection
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , json
            = def
            &= typ "JSON_FILE"
            &= name "json"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Output JSON file.\n\
                    \Default is 'foo.essence-json'\n\
                    \if the Essence file is named 'foo.essence'"
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "symmetry-detection"
            &= explicit
            &= help "Dump some JSON to be used as input to ferret for symmetry detection."
    , ParameterGenerator
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , minInt
            = 0
            &= typ "INT"
            &= name "MININT"
            &= groupname "Integer bounds"
            &= explicit
            &= help "The minimum integer value for the parameter values.\nDefault: 0"
        , maxInt
            = 100
            &= typ "INT"
            &= name "MAXINT"
            &= groupname "Integer bounds"
            &= explicit
            &= help "The maximum integer value for the parameter values.\nDefault: 100"
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "parameter-generator"
            &= explicit
            &= help "Generate an Essence model describing the instances of the problem class \
                    \defined in the input Essence model.\n\
                    \An error will be printed if the model has infinitely many instances."
    , AutoIG
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , outputFilepath
            = def
            &= typ "OUTPUT_FILE"
            &= argPos 1
        , generatorToIrace
            = False
            &= name "generator-to-irace"
            &= explicit
            &= help "Convert the givens in a hand written generator model to irace syntax."
        , removeAux
            = False
            &= name "remove-aux"
            &= explicit
            &= help "Remove lettings whose name start with Aux"
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "autoig"
            &= explicit
            &= help "Functionality to support the AutoIG workflow."
    , Boost
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , logRuleSuccesses
            = False
            &= name "log-rule-successes"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule applications."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output. All output formats can also be used for input.\n\
                    \    plain: default\n\
                    \    binary: a binary encoding\n\
                    \    astjson: a JSON dump of the internal data structures, quite verbose\n\
                    \    json: a simplified JSON format, only used for parameters and solutions\n\
                    \    jsonstream: same as JSON, except in one special case. when multiple solutions are saved in a single file as json, this mode prints one solution per line\n\
                    \    minizinc: minizinc format for data files, only used for solutions\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
        }   &= name "boost"
            &= explicit
            &= help "Strengthen an Essence model as described in \"Reformulating \
                    \Essence Specifications for Robustness\",\n\
                    \which aims to make search faster."
    , Streamlining
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
        }   &= name "streamlining"
            &= explicit
            &= help "Generate streamlined Essence models."
    , LSP {
        logLevel = def,
        limitTime = Nothing
    } &= name "lsp"
    ]      &= program "conjure"
           &= helpArg [explicit, name "help"]
           &= versionArg [explicit, name "version"]
           &= summary (unlines [ "Conjure: The Automated Constraint Modelling Tool"
                               , versionLine
                               ])
           &= help "The command line interface of Conjure takes a command name as the first argument \
                   \followed by more arguments depending on the command.\n\
                   \This help text gives a list of the available commands.\n\
                   \For details of a command, pass the --help flag after the command name.\n\
                   \For example: 'conjure translate-solution --help'"

versionLine :: String
versionLine = "Conjure v" ++ showVersion version ++ " (Repository version " ++ repositoryVersion ++ ")"
