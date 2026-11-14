# Redessiner les régions : une carte, plusieurs critères

Carte interactive comparant plusieurs régionalisations de la France métropolitaine
(régions construites à partir des EPCI), chacune optimisant un critère différent :
eau (bassins-versants), économie (régions homogènes), solidarité (régions hétérogènes),
flux humains, villes. Un curseur fixe le nombre de régions (6 à 22 ; 13 aujourd'hui en métropole)
et la carte se met à jour aussitôt. Un onglet « Explications » présente critères, méthodes,
indicateurs de comparaison et métadonnées.

## Contenu

| Fichier | Rôle |
|---|---|
| `run.R` | lance toute la chaîne |
| `R/01_donnees.R` | télécharge et prépare les données (toutes les adresses sont en tête de fichier) |
| `R/02_regionalisations.R` | calcule les découpages, les indicateurs, et écrit `docs/data/` |
| `R/00_fonctions.R` | fonctions communes (SKATER, fusions, indicateurs) |
| `docs/index.html` | la page web (Leaflet), qui lit `docs/data/` |
| `.github/workflows/maj.yml` | mise à jour automatique mensuelle (GitHub Actions) |

## Utilisation

```r
install.packages(c("sf", "dplyr", "spdep", "jsonlite", "data.table", "melodi", "rmapshaper"))
```

Puis, depuis la racine du dépôt :

```
Rscript run.R
```

Compter quelques minutes (téléchargements, puis 2 à 5 minutes de calcul : tous les découpages,
de 6 à 22 régions, sont calculés d'avance).
Pour voir la carte en local : `python3 -m http.server --directory docs` puis http://localhost:8000
(ouvrir `index.html` par double-clic ne suffit pas : le navigateur bloque la lecture des données).

## Publication (GitHub Pages)

1. Pousser le dépôt sur GitHub, dossier `docs/data/` compris (après un premier `Rscript run.R`).
2. *Settings → Pages → Build and deployment* : source **Deploy from a branch**, branche `main`, dossier **/docs**.
3. La carte est en ligne à `https://<compte>.github.io/<depot>/`.
4. Mise à jour automatique : le fichier `maj.yml` relance les calculs le 1er de chaque mois et
   enregistre le résultat. On peut aussi le lancer à la main (*Actions → Mise à jour des données → Run workflow*).

## Données

Tout est téléchargé par le script ; **aucun téléchargement manuel n'est obligatoire**.

| Donnée | Source | Accès | Actualisation |
|---|---|---|---|
| Contours et population des EPCI, table commune → EPCI → région | API Découpage administratif (`geo.api.gouv.fr`) | automatique | automatique (géographie en vigueur) |
| Niveau de vie médian, taux de pauvreté | Insee, Filosofi, jeu `DS_FILOSOFI_CC` (catalogue Melodi) | automatique, package `melodi` | automatique (dernier millésime) |
| Emploi salarié en 5 secteurs | Insee, Flores, jeu `DS_FLORES_A5` (Melodi) | automatique | automatique (dernier millésime) |
| Régions hydrographiques | Sandre, BD Carthage 2017, secteurs hydrographiques | automatique | référentiel figé (les bassins ne changent pas) |
| Navettes domicile-travail entre communes | Insee, recensement, base « flux de mobilité » | automatique | **adresse à changer une fois par an** (`url_flux`, voir ci-dessous) |
| Ressources (facultatif) | à fournir : `data/manuel/ressources_epci.csv` | manuel | manuel |

**Navettes.** L'Insee publie cette base sur une page différente chaque année, sans adresse permanente.
Quand un nouveau millésime paraît (en général l'été), chercher « Mobilités professionnelles :
déplacements domicile - lieu de travail, base flux de mobilité » sur insee.fr, copier le lien du
fichier CSV et le coller dans `url_flux` (en tête de `R/01_donnees.R`). Adresse actuelle : millésime 2022.

**Ressources (onglet facultatif).** Voir `data/manuel/LISEZMOI.md`. Pistes de variables par EPCI :
parts de forêt, de terres agricoles et de sols artificialisés (CORINE Land Cover, téléchargeable par EPCI
sur l'Observatoire des territoires), orientation agricole (Agreste, recensement agricole),
puissance électrique renouvelable installée (ODRÉ, registre national des installations).

## Méthodes

- **Eau** : chaque EPCI est rattaché à sa région hydrographique dominante ; fusions au sein d'un même grand bassin en priorité. Le nombre de régions est plafonné par le nombre de régions hydrographiques (environ 23).
- **Économie** (homogène) : SKATER (`spdep`) sur revenu, pauvreté et structure de l'emploi, population minimale 1 M.
- **Solidarité** (hétérogène) : fusions qui ramènent le niveau de vie de chaque région vers la moyenne nationale.
- **Flux humains** : fusions selon les échanges de navetteurs.
- **Villes** : des pôles (grands et éloignés les uns des autres), chaque EPCI au pôle le plus proche.
- La Corse forme toujours une région à elle seule. Outre-mer non traité.
- Pour un critère donné, les découpages sont emboîtés : passer de 13 à 14 régions coupe une région en deux.

Paramètres en tête de `R/02_regionalisations.R` : `KS` (nombres de régions proposés par le curseur),
`K_DEFAUT` (valeur affichée à l'ouverture) et `POP_MIN`.

## Ajouter un critère

Dans `R/02_regionalisations.R`, ajouter un bloc sur le modèle des autres :
`G$mon_critere <- ...` (une matrice : une ligne par EPCI du continent, une colonne par nombre de régions ;
`regions_skater()` et `regions_fusion()` la renvoient directement) et `decrire("mon_critere", ...)`.
L'onglet, les indicateurs et la ligne du tableau d'explications se créent tout seuls.

## État des tests

Les calculs (`02`) et la page web ont été testés sur des données fictives. Les téléchargements (`01`)
n'ont pas pu être exécutés lors de l'écriture du code. Points à vérifier au premier lancement :

- codes des mesures Filosofi (`MED_SL`, `PR_MD60`) : s'ils diffèrent, le script s'arrête en listant les codes disponibles ;
- nom de la colonne du code de secteur dans le fichier Sandre (cherchée automatiquement, message explicite sinon) ;
- réponse de `geo.api.gouv.fr` pour les contours des EPCI (une adresse de secours est prévue) ;
- le workflow GitHub Actions.
