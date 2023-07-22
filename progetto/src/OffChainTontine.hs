module OffChainTontine where 


import System.Process

-- en modifiant le nom du module du onchain code, ce n'est plus le code original, donc je dois 
-- a avoir le code du script deja deployé

{-il faut faire des fonctions pour chaque action : 
    ouvrir la tontine , fermer la tontine , tontiner , beneficier -}

{-la tx d'ouverture de la tontine est la premier tx. a la sortie de cette tx je met un datum avec
    l'inforamtion ouverte-}