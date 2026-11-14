# ------------------------------------------------------------------
# 2. Calcul des régionalisations et export pour la carte web
#    Entrées : data/epci.gpkg, data/flux.rds, data/sources.json
#    Sorties : docs/data/ (epci.geojson, aretes.geojson, decoupages.json, meta.json)
# ------------------------------------------------------------------
library(sf)
library(dplyr)
library(spdep)
library(jsonlite)
source("R/00_fonctions.R")

KS <- 6:22         # nombres de régions proposés par le curseur (Corse comprise)
K_DEFAUT <- 13     # nombre actuel de régions métropolitaines
POP_MIN <- 1e6     # population minimale d'une région (SKATER)

epci <- st_read("data/epci.gpkg", quiet = TRUE)
flux <- readRDS("data/flux.rds")
sources <- fromJSON("data/sources.json")

# valeurs manquantes -> médiane nationale
for (v in names(epci)[sapply(st_drop_geometry(epci), is.numeric)])
  epci[[v]][is.na(epci[[v]])] <- median(epci[[v]], na.rm = TRUE)

# La Corse (île) forme toujours une région : on découpe le continent en K - 1
terre <- epci$reg_actuelle != "94"
e <- epci[terre, ]
d <- st_drop_geometry(e)
fl <- flux[terre, terre]
pop <- d$pop
ks <- KS - 1
nb <- voisinage(e)

G <- list()      # par scénario : matrice EPCI du continent x nombre de régions
S <- list()      # la description de chaque scénario (onglet « Explications »)
decrire <- function(id, titre, logique, methode, donnees)
  S[[id]] <<- data.frame(id, titre, logique, methode, donnees)

# ---- Régions actuelles (référence, identique quel que soit K) --------
G$actuelles <- matrix(as.integer(factor(d$reg_actuelle)), nrow(d), length(ks))
decrire("actuelles", "Régions actuelles", "Référence",
        "Chaque EPCI est rattaché à la région où vit la majorité de sa population. Le curseur n'a pas d'effet sur cet onglet.",
        sources$epci)

# ---- Eau : bassins-versants ------------------------------------------
# Départ : régions hydrographiques (1re lettre du code Carthage), puis fusion
# de la moins peuplée avec une voisine du même grand bassin (agence de l'eau)
agence <- c(A = "Rhin-Meuse", B = "Rhin-Meuse", D = "Artois-Picardie", E = "Artois-Picardie",
            F = "Seine-Normandie", G = "Seine-Normandie", H = "Seine-Normandie", I = "Seine-Normandie",
            J = "Loire-Bretagne", K = "Loire-Bretagne", L = "Loire-Bretagne", M = "Loire-Bretagne",
            N = "Loire-Bretagne", O = "Adour-Garonne", P = "Adour-Garonne", Q = "Adour-Garonne",
            R = "Adour-Garonne", S = "Adour-Garonne", U = "Rhône-Méditerranée", V = "Rhône-Méditerranée",
            W = "Rhône-Méditerranée", X = "Rhône-Méditerranée", Y = "Rhône-Méditerranée")
ag <- agence[d$bassin]
G$eau <- regions_fusion(nb, pop, ks, depart = d$bassin,
  affinite = function(a, b) isTRUE(ag[a[1]] == ag[b[1]]) - sum(pop[b]) / 1e9)
decrire("eau", "Eau", "Appartenance à un bassin-versant",
        "Chaque EPCI est rattaché à sa région hydrographique dominante (en surface). Les régions hydrographiques sont ensuite fusionnées, la moins peuplée d'abord, avec une voisine du même grand bassin (agence de l'eau). Le nombre de régions est plafonné par le nombre de régions hydrographiques.",
        sources$eau)

# ---- Économie : régions homogènes -------------------------------------
X <- d[, c("revenu", "pauvrete", "emploi_hab", grep("^emploi_", names(d), value = TRUE))]
X <- X[, !duplicated(names(X))]
G$economie <- regions_skater(nb, X, ks, pop, POP_MIN)
decrire("economie", "Économie", "Homogénéité (régions qui se ressemblent en interne)",
        "SKATER : arbre couvrant minimal des EPCI voisins, élagué pour former des régions homogènes. Variables centrées-réduites : niveau de vie médian, taux de pauvreté, emplois salariés par habitant, parts de l'emploi salarié en 5 secteurs. Population minimale par région.",
        paste(sources$revenus, sources$emploi, sep = " ; "))

# ---- Solidarité : régions hétérogènes ---------------------------------
moyenne <- weighted.mean(d$revenu, pop)
G$solidarite <- regions_fusion(nb, pop, ks,
  affinite = function(a, b) -abs(weighted.mean(d$revenu[c(a, b)], pop[c(a, b)]) - moyenne))
decrire("solidarite", "Solidarité", "Hétérogénéité (chaque région mêle territoires riches et pauvres)",
        "Fusions successives : la région la moins peuplée fusionne avec la voisine qui rapproche le plus leur niveau de vie moyen de la moyenne nationale. Logique inverse de l'onglet Économie.",
        sources$revenus)

# ---- Flux humains : navettes domicile-travail -------------------------
G$flux <- regions_fusion(nb, pop, ks,
  affinite = function(a, b) sum(fl[a, b]) + sum(fl[b, a]))
decrire("flux", "Flux humains", "Interaction (régions vécues)",
        "Fusions successives : la région la moins peuplée fusionne avec la voisine avec laquelle elle échange le plus d'actifs (navettes domicile-travail, dans les deux sens).",
        sources$flux)

# ---- Villes : aires d'influence des métropoles ------------------------
xy <- st_coordinates(st_centroid(st_geometry(e)))
D <- as.matrix(dist(xy))
poles <- which.max(pop)
while (length(poles) < max(ks)) {
  dmin <- apply(D[, poles, drop = FALSE], 1, min)
  poles <- c(poles, which.max(pop * dmin))        # grande ET loin des pôles déjà choisis
}
G$villes <- sapply(ks, function(k) apply(D[, poles[1:k]], 1, which.min))
decrire("villes", "Villes", "Polarisation (une métropole et son arrière-pays)",
        "Choix de pôles : l'EPCI le plus peuplé, puis, un à un, celui qui maximise population × distance au pôle le plus proche. Chaque EPCI est rattaché au pôle le plus proche (distance à vol d'oiseau).",
        sources$epci)

# ---- Ressources (facultatif : data/manuel/ressources_epci.csv) --------
if (any(grepl("^res_", names(d)))) {
  G$ressources <- regions_skater(nb, d[, grep("^res_", names(d))], ks, pop, POP_MIN)
  decrire("ressources", "Ressources", "Homogénéité",
          paste("SKATER sur les variables fournies :", paste(sub("^res_", "", grep("^res_", names(d), value = TRUE)), collapse = ", ")),
          "Fichier data/manuel/ressources_epci.csv")
}

# ---- Assemblage : continent + Corse ------------------------------------
groupes <- lapply(G, function(m) {
  x <- matrix(0L, nrow(epci), length(KS), dimnames = list(NULL, KS))
  x[terre, ] <- m
  x[!terre, ] <- rep(apply(m, 2, max) + 1L, each = sum(!terre))
  x
})

# ---- Indicateurs de comparaison (pour chaque scénario et chaque K) -----
carte <- epci
if (requireNamespace("rmapshaper", quietly = TRUE))
  carte <- rmapshaper::ms_simplify(carte, keep = 0.3, keep_shapes = TRUE)
geom <- st_geometry(carte)

indicateurs <- function(g) {
  contour <- st_geometry(summarise(group_by(st_sf(g = g, geometry = geom), g)))
  perim <- as.numeric(st_length(st_cast(contour, "MULTILINESTRING")))
  data.frame(
    n_regions = length(unique(g)),
    pop_min = min(tapply(epci$pop, g, sum)),
    pop_max = max(tapply(epci$pop, g, sum)),
    navettes_internes = sum(flux[outer(g, g, "==")]) / sum(flux),
    variance_inter_revenu = variance_inter(epci$revenu, g, epci$pop),
    integrite_bassins = integrite(epci$bassin, g, epci$pop),
    compacite = mean(4 * pi * as.numeric(st_area(contour)) / perim^2))
}
indic <- bind_rows(lapply(names(groupes), function(s)
  bind_rows(lapply(seq_along(KS), function(i)
    cbind(id = s, k = KS[i], indicateurs(groupes[[s]][, i]))))))

# ---- Ressemblance entre découpages (indice de Rand ajusté, pour chaque K)
ari <- lapply(seq_along(KS), function(i)
  round(sapply(groupes, function(a) sapply(groupes, function(b) rand_ajuste(a[, i], b[, i]))), 2))
names(ari) <- KS

# ---- Limites communes entre EPCI voisins (pour tracer les frontières) ---
ici <- which(terre)
a <- rep(seq_along(nb), lengths(nb))
b <- unlist(nb)
paires <- cbind(ici[a], ici[b])[b > a, ]
bord <- st_boundary(geom)
arete <- function(i, j) {
  l <- st_intersection(bord[i], bord[j])
  if (length(l) == 0) st_geometrycollection() else l[[1]]
}
aretes <- st_sf(i = paires[, 1] - 1, j = paires[, 2] - 1,      # numéros à partir de 0 (JavaScript)
                geometry = st_sfc(mapply(arete, paires[, 1], paires[, 2], SIMPLIFY = FALSE), crs = st_crs(geom)))
aretes <- aretes[!st_is_empty(aretes), ]
aretes <- suppressWarnings(st_collection_extract(aretes, "LINESTRING"))

# ---- Export --------------------------------------------------------------
dir.create("docs/data", recursive = TRUE, showWarnings = FALSE)
ecrire <- function(x, f) st_write(st_transform(x, 4326), f, delete_dsn = TRUE, quiet = TRUE,
                                  layer_options = "COORDINATE_PRECISION=4")
pt <- st_coordinates(st_transform(st_point_on_surface(geom), 4326))
carte$lon <- round(pt[, 1], 3)
carte$lat <- round(pt[, 2], 3)
carte$revenu <- round(carte$revenu)
ecrire(carte[, c("code", "nom", "pop", "revenu", "ville", "lon", "lat")], "docs/data/epci.geojson")
ecrire(aretes, "docs/data/aretes.geojson")
write_json(lapply(groupes, function(m) as.list(as.data.frame(m, optional = TRUE))),
           "docs/data/decoupages.json")
write_json(list(
  date = sources$date, ks = KS, k_defaut = K_DEFAUT, pop_min = POP_MIN,
  scenarios = bind_rows(S),
  indicateurs = indic,
  ari = list(id = names(groupes), valeurs = ari),
  voisins = unname(paires - 1),
  sources = sources
), "docs/data/meta.json", auto_unbox = TRUE, digits = 4)
tout <- do.call(cbind, lapply(names(groupes), function(s)
  setNames(as.data.frame(groupes[[s]]), paste0(s, "_", KS))))
write.csv2(cbind(code = epci$code, nom = epci$nom, tout), "docs/data/epci_regions.csv", row.names = FALSE)
cat("Carte prête : docs/index.html\n")
print(subset(indic, k == K_DEFAUT), digits = 3, row.names = FALSE)
