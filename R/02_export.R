# ------------------------------------------------------------------
# 2. Export des données pour la carte web
#    (les régionalisations sont calculées dans le navigateur : docs/moteur.js)
#    Entrées : data/epci.gpkg, data/nuts1.gpkg, data/flux.rds, data/sources.json
#    Sorties : docs/data/ (donnees.json, epci.geojson, aretes.geojson, nuts1.geojson)
# ------------------------------------------------------------------
library(sf)
library(dplyr)
library(spdep)
library(jsonlite)
source("R/00_fonctions.R")

KS <- 6:22         # nombres de régions proposés par le curseur (Corse comprise)
K_DEFAUT <- 13     # nombre actuel de régions métropolitaines
FLUX_MIN <- 5      # on néglige les échanges de moins de 5 actifs entre deux EPCI

epci <- st_read("data/epci.gpkg", quiet = TRUE)
nuts1 <- st_read("data/nuts1.gpkg", quiet = TRUE)
flux <- readRDS("data/flux.rds")
sources <- fromJSON("data/sources.json")
d <- st_drop_geometry(epci)

# valeurs manquantes -> médiane nationale
for (v in names(d)[sapply(d, is.numeric)]) d[[v]][is.na(d[[v]])] <- median(d[[v]], na.rm = TRUE)

# ---- Voisinage (la Corse, île, reste à part) ---------------------------
terre <- d$reg_actuelle != "94"
ici <- which(terre)
nb <- voisinage(epci[terre, ])
a <- rep(seq_along(nb), lengths(nb))
b <- unlist(nb)
paires <- cbind(ici[a], ici[b])[b > a, ]

# ---- Géométrie allégée et limites communes entre EPCI voisins -----------
carte <- epci["code"]
if (requireNamespace("rmapshaper", quietly = TRUE))
  carte <- rmapshaper::ms_simplify(carte, keep = 0.3, keep_shapes = TRUE)
geom <- st_geometry(carte)
bord <- st_boundary(geom)
arete <- function(i, j) {
  l <- st_intersection(bord[i], bord[j])
  if (length(l) == 0) st_geometrycollection() else l[[1]]
}
aretes <- st_sf(i = paires[, 1] - 1, j = paires[, 2] - 1,      # numéros à partir de 0 (JavaScript)
                geometry = st_sfc(mapply(arete, paires[, 1], paires[, 2], SIMPLIFY = FALSE), crs = st_crs(geom)))
# longueur de la limite commune à chaque paire de voisins (km), pour mesurer la complexité des frontières
longueur <- as.numeric(st_length(aretes)) / 1000
nulles <- which(longueur == 0)          # contours qui ne se raccordent pas exactement : mesure à 50 m près
for (p in nulles)
  longueur[p] <- sum(as.numeric(st_length(st_intersection(bord[paires[p, 1]], st_buffer(geom[paires[p, 2]], 50))))) / 1000
cat(length(nulles), "limites communes mesurées avec tolérance, dont", sum(longueur == 0), "restent nulles (îles)\n")
perimetre <- as.numeric(st_length(bord)) / 1000
aretes <- aretes[!st_is_empty(aretes), ]
aretes <- suppressWarnings(st_collection_extract(aretes, "LINESTRING"))

# ---- Variables par EPCI --------------------------------------------------
xy <- st_coordinates(st_centroid(st_geometry(epci))) / 1000            # km
pt <- st_coordinates(st_transform(st_point_on_surface(st_geometry(epci)), 4326))
variables <- unique(c("revenu", "pauvrete", "emploi_hab", grep("^emploi_", names(d), value = TRUE)))
Z <- scale(d[, variables])                                              # profil économique centré-réduit
Z[is.na(Z)] <- 0
f <- which(flux >= FLUX_MIN, arr.ind = TRUE)

# ---- Export ----------------------------------------------------------------
dir.create("docs/data", recursive = TRUE, showWarnings = FALSE)
ecrire <- function(x, fichier) st_write(st_transform(x, 4326), fichier, delete_dsn = TRUE, quiet = TRUE,
                                        layer_options = "COORDINATE_PRECISION=4")
ecrire(carte, "docs/data/epci.geojson")
ecrire(aretes, "docs/data/aretes.geojson")
europe <- nuts1[nuts1$pays != "FR" & !is.na(nuts1$pop) & !is.na(nuts1$pib), ]
ecrire(europe, "docs/data/nuts1.geojson")
write_json(list(
  date = sources$date, ks = KS, k_defaut = K_DEFAUT, sources = sources, variables = variables,
  code = d$code, nom = d$nom, ville = d$ville, terre = as.integer(terre),
  pop = d$pop, pop_ville = d$pop_ville, aire = round(d$aire, 1), x = round(xy[, 1], 1), y = round(xy[, 2], 1),
  lon = round(pt[, 1], 3), lat = round(pt[, 2], 3),
  revenu = round(d$revenu), pib = round(d$pib, 1),
  bassin = d$bassin, fleuve = d$fleuve, agence = d$agence, dep = d$dep, anc_region = d$anc_region, province = d$province,
  reg_actuelle = d$reg_actuelle,
  z = round(Z, 3),
  voisins = unname(paires - 1),
  longueur = round(longueur, 2), perimetre = round(perimetre, 1),
  flux = list(i = f[, 1] - 1, j = f[, 2] - 1, v = round(flux[f])),
  nuts1 = st_drop_geometry(europe)
), "docs/data/donnees.json", auto_unbox = TRUE, digits = NA)
cat("Carte prête : docs/index.html (", nrow(epci), "EPCI,", nrow(paires), "voisinages,", nrow(f), "flux )\n")
