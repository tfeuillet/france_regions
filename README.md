# Redessiner les régions françaises

À quoi ressembleraient les régions françaises si l'on faisait varier les critères de régionalisation ? Cet outil interactif permet de tester différentes configurations avec les critères suivants : la compétitivité économique au niveau européen, la solidarité entre territoires riches et pauvres, les trajets domicile-travail, le respect des provinces d'Ancien Régime ou les bassins-versants des grands fleuves. Des réglages avancés ajoutent les anciennes régions (avant 2016), l'équilibre démographique entre régions, deux contraintes de forme, ainsi qu'une contrainte qui garde l'Île-de-France dans ses huit départements.

On part des 1 232 intercommunalités de métropole. On choisit le nombre de régions, entre 6 et 22, puis le poids de chaque critère, et la carte se redessine. L'intérêt de ces simulations est de montrer ce que chaque critère implique sur la géographie des régions.

[Thierry Feuillet](https://tfeuillet.github.io/), Université de Caen Normandie, [UMR CNRS IDEES](https://umr-idees.fr/)\
[thierry.feuillet\@unicaen.fr](mailto:thierry.feuillet@unicaen.fr)

## Refaire la carte chez soi

``` r
install.packages(c("sf", "dplyr", "spdep", "jsonlite", "data.table", "melodi", "rmapshaper", "servr"))
source("run.R")          # télécharge les données et prépare docs/data/
servr::httd("docs")      # ouvre la carte
```

Les données sont téléchargées automatiquement auprès de l'Insee, d'Etalab, du Sandre et d'Eurostat. Une seule adresse doit être mise à jour une fois par an. C'est celle des trajets domicile-travail, notée `url_flux` au début de `R/01_donnees.R`.

Pour publier la carte, il suffit de pousser le dépôt sur GitHub puis d'activer GitHub Pages sur la branche `main` et le dossier `/docs`.

## Ce que contient le dépôt

Le dossier `R/` prépare les données. Le script `01_donnees.R` les télécharge et `02_export.R` écrit les fichiers lus par la carte.

Le dossier `docs/` contient la page (`index.html`), le calcul des régions qui s'exécute dans le navigateur (`moteur.js`) et une page de tests de robustesse (`robustesse.html`).

Le fichier `.github/workflows/maj.yml` met les données à jour le premier jour de chaque mois.

## La méthode en quelques mots

Chaque région grandit autour d'une grande ville, appelée pôle. Les intercommunalités la rejoignent une à une, en commençant par celles qui lui correspondent le mieux d'après les critères choisis et leur poids. Les frontières sont ensuite retouchées sans jamais couper une région en deux. Le détail de la méthode, les paramètres fixés et leur justification, les sources et les limites figurent dans l'onglet « Méthode et sources » de la carte.

## À savoir avant de s'en servir

Les provinces historiques sont approchées par la province dominante de chaque département, et les bassins des grands fleuves par un regroupement des régions hydrographiques du Sandre. Ces deux tables sont écrites à la main dans `R/00_fonctions.R`.

Le PIB des régions est une estimation, obtenue en répartissant le PIB de chaque département selon l'emploi salarié.

Un petit changement de poids peut déplacer une frontière disputée entre deux villes. La page de tests mesure l'ampleur de ces variations.

## Contribuer

Les remarques, corrections et idées sont les bienvenues, par courriel ou en ouvrant une proposition (*issue*) sur GitHub. Le bouton « Proposer une amélioration » de la carte prépare le message avec le lien de la carte affichée.
