module Conjure.UI.TranslateSolution ( translateSolution ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Constant ( normaliseConstant )
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Process.Enums ( removeEnumsFromParam, addEnumsAndUnnamedsBack )
import Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.UI.TranslateParameter ( translateParameter )
import Conjure.Representations ( up )

-- text
import Data.Text as T ( pack )


translateSolution
    :: ( MonadFail m
       , MonadUserError m
       , MonadLog m
       , NameGen m
       , EnumerateDomain m
       )
    => Model      -- eprime model
    -> Model      -- essence param
    -> Model      -- eprime solution
    -> m Model    -- essence solution
translateSolution eprimeModel essenceParam' eprimeSolution = do

    eprimeParam  <- translateParameter eprimeModel essenceParam'
    essenceParam <- removeEnumsFromParam eprimeModel essenceParam'

    let eprimeLettingsForEnums =
            [ (nm, fromInt (genericLength vals))
            | nm1                                          <- eprimeModel |> mInfo |> miEnumGivens
            , Declaration (LettingDomainDefnEnum nm2 vals) <- essenceParam' |> mStatements
            , nm1 == nm2
            , let nm = nm1 `mappend` "_EnumSize"
            ]

    let eprimeLettings = extractLettings essenceParam ++
                         extractLettings eprimeParam ++
                         extractLettings eprimeSolution ++
                         extractLettings eprimeModel ++
                         eprimeLettingsForEnums
    let essenceFindNames = eprimeModel |> mInfo |> miFinds
    let essenceFinds     = eprimeModel |> mInfo |> miRepresentations
                                       |> filter (\ (n,_) -> n `elem` essenceFindNames )

    eprimeLettings' <- forM eprimeLettings $ \ (name, val) -> do
        constant <- instantiateExpression eprimeLettings val
        return (name, constant)

    essenceFinds' <- forM essenceFinds $ \ (name, dom) -> do
        constant <- instantiateDomain eprimeLettings dom
        return (name, constant)

    essenceLettings <- forM essenceFinds' $ \ (name, domain) -> do
        (_, constant) <- up eprimeLettings' (name, domain)
        let origDomain = eprimeModel
                |> mInfo |> miOriginalDomains
                |> lookup name
                |> fromMaybe (bug ("Missing original domain for:" <+> pretty name))
        return (name, origDomain, constant)

    let
        intToEnumConstant :: [((Integer, Name), Constant)]
        intToEnumConstant = concat
            [ [ ((i,ename), ConstantEnum ename vals v)
              | (i,v) <- zip allNats vals
              ]
            | Declaration (LettingDomainDefnEnum ename vals)
                    <- mStatements essenceParam'
                    ++ eprimeModel |> mInfo |> miEnumLettings |> map Declaration
            ]

    let
        unnameds :: [(Name, Expression)]
        unnameds = eprimeModel |> mInfo |> miUnnameds

    unnamedsAsEnumDomains <- forM unnameds $ \ (n, s') -> do
        s <- instantiateExpression eprimeLettings s'
        case s of
            ConstantInt size -> return $
                let nms = [ mconcat [n, "_", Name (T.pack (show i))]
                          | i <- [1 .. size]
                          ]
                in  Declaration (LettingDomainDefnEnum n nms)
            _ -> fail $ vcat [ "Expecting an integer value for" <+> pretty n
                             , "But got:" <+> pretty s
                             ]

    let outStmts =
            unnamedsAsEnumDomains ++
            sortNub
                [ Declaration (Letting n (Constant (normaliseConstant y)))
                | (n, d, x) <- essenceLettings
                , let y = addEnumsAndUnnamedsBack
                                (map fst unnameds)
                                intToEnumConstant
                                d x
                ]

    let undefs = [ u | u@ConstantUndefined{} <- universeBi outStmts ]

    if null undefs
        then return def { mStatements = outStmts }
        else bug $ vcat
            [ "Undefined values in the output:" <++> vcat (map pretty undefs)
            , ""
            , "Complete output would have been the following."
            , ""
            , pretty $ def { mStatements = outStmts }
            ]
