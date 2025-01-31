module Conjure.Language.ModelStats
    ( givens, nbGivens, nbAbstractGivens
    , finds, nbFinds, nbAbstractFinds
    , declarations, nbDeclarations, nbAbstractDeclarations
    , lettings
    , domainNeedsRepresentation
    , modelInfo
    , modelDeclarationsJSON
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty

-- containers
import qualified Data.Map.Strict as M


givens :: Model -> [(Name, Domain () Expression)]
givens m = [ (nm,d) | Declaration (FindOrGiven Given nm d) <- mStatements m ]

nbGivens :: Model -> Int
nbGivens = length . givens

nbAbstractGivens :: Model -> Int
nbAbstractGivens = length . filter domainNeedsRepresentation . map snd . givens


finds :: Model -> [(Name, Domain () Expression)]
finds m = [ (nm,d) | Declaration (FindOrGiven Find nm d) <- mStatements m ]

nbFinds :: Model -> Int
nbFinds = length . finds

nbAbstractFinds :: Model -> Int
nbAbstractFinds = length . filter domainNeedsRepresentation . map snd . finds


declarations :: Model -> [(Name, Domain () Expression)]
declarations m = [ (nm,d) | Declaration (FindOrGiven _ nm d) <- mStatements m ]

nbDeclarations :: Model -> Int
nbDeclarations = length . declarations

nbAbstractDeclarations :: Model -> Int
nbAbstractDeclarations = length . filter domainNeedsRepresentation . map snd . declarations


lettings :: Model -> [(Name, Expression)]
lettings m = [ (nm,x) | Declaration (Letting nm x) <- mStatements m ]


domainNeedsRepresentation :: (Pretty r, Pretty x) => Domain r x -> Bool
domainNeedsRepresentation DomainBool{} = False
domainNeedsRepresentation DomainInt{} = False
domainNeedsRepresentation DomainEnum{} = False
domainNeedsRepresentation DomainUnnamed{} = False
domainNeedsRepresentation DomainTuple{} = False
domainNeedsRepresentation DomainRecord{} = False
domainNeedsRepresentation DomainVariant{} = False
domainNeedsRepresentation (DomainMatrix _ inner) = domainNeedsRepresentation inner
domainNeedsRepresentation DomainSet{}       = True
domainNeedsRepresentation DomainMSet{}      = True
domainNeedsRepresentation DomainFunction{}  = True
domainNeedsRepresentation DomainSequence{}  = True
domainNeedsRepresentation DomainRelation{}  = True
domainNeedsRepresentation DomainPartition{} = True
domainNeedsRepresentation DomainPermutation{} = True
domainNeedsRepresentation (DomainReference _ (Just _)) = True
domainNeedsRepresentation d = bug $ "domainNeedsRepresentation:" <+> pretty (show d)


modelInfo :: Model -> Doc
modelInfo m = vcat
    [ "Contains" <+> pretty   (nbGivens m) <+> "parameters        "
                 <+> prParens (pretty (nbAbstractGivens m) <+> "abstract")
    , "        " <+> pretty   (nbFinds  m) <+> "decision variables"
                 <+> prParens (pretty (nbAbstractFinds m ) <+> "abstract")
    ]


modelDeclarationsJSON :: Model -> JSONValue
modelDeclarationsJSON m = toJSON
    [ M.fromList $ case d of
        FindOrGiven forg name dom ->
            [ "kind"   ~~ show forg
            , "name"   ~~ r name
            , "domain" ~~ r dom
            ]
        Letting name x ->
            [ "kind"   ~~ "letting"
            , "name"   ~~ r name
            , "value"  ~~ r x
            ]
        GivenDomainDefnEnum name ->
            [ "kind"   ~~ "enumerated type"
            , "name"   ~~ r name
            ]
        LettingDomainDefnEnum name vals ->
            [ "kind"   ~~ "enumerated type"
            , "name"   ~~ r name
            , "values" ~~ r (prettyList id "," vals)
            ]
        LettingDomainDefnUnnamed name size ->
            [ "kind"   ~~ "unnamed type"
            , "name"   ~~ r name
            , "size"   ~~ r size
            ]
    | Declaration d <- mStatements m
    ]
    where
        (~~) :: String -> String -> (String, String)
        x ~~ y = (x, y)
        r s = render 100000 (pretty s)
