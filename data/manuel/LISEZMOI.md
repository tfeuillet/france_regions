# Données facultatives fournies à la main

## ressources_epci.csv  (active l'onglet « Ressources »)

Fichier CSV au format français (séparateur `;`, décimale `,`) :

    epci;foret;agricole;artificialise;puissance_enr
    200054781;2,1;0,4;96,3;0,02
    ...

- 1re colonne `epci` : code SIREN de l'EPCI (9 chiffres) ;
- colonnes suivantes : autant de variables numériques que souhaité
  (elles sont centrées-réduites, puis régionalisées par SKATER).

Dès que ce fichier existe, `Rscript run.R` ajoute l'onglet tout seul.
