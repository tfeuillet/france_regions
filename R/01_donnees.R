# ------------------------------------------------------------------
# 1. Téléchargement et préparation des données (maille : EPCI)
#    Sorties : data/epci.gpkg, data/nuts1.gpkg, data/flux.rds, data/sources.json
# ------------------------------------------------------------------
library(sf)
library(dplyr)
library(jsonlite)
source("R/00_fonctions.R")
options(timeout = 900)
dir.create("data/brut", recursive = TRUE, showWarnings = FALSE)

# ---- Adresses des serveurs (tout est ici) -------------------------
url_communes <- "https://geo.api.gouv.fr/communes?fields=code,nom,population,codeEpci,codeRegion&format=json"
url_epci     <- "https://etalab-datasets.geo.data.gouv.fr/contours-administratifs/latest/geojson/epci-100m.geojson"
url_secteurs <- "https://services.sandre.eaufrance.fr/telechargement/geo/ETH/BDCarthage/FXX/2017/France_metropole_entiere/SHP/SecteurHydro_FXX-shp.zip"
# Seule adresse à changer une fois par an (nouveau millésime du recensement) :
url_flux     <- "https://www.insee.fr/fr/statistiques/fichier/8582949/base-flux-mobilite-domicile-lieu-travail-2022_csv.zip"
url_eurostat <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/"
url_gisco    <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/"
# Jeux de données Insee (catalogue Melodi) : toujours le dernier millésime
jeu_revenus  <- "DS_FILOSOFI_CC"
jeu_emploi   <- "DS_FLORES_A5"
mesure_revenu   <- "MED_SL"     # niveau de vie médian
mesure_pauvrete <- "PR_MD60"    # taux de pauvreté (seuil 60 %)
mesure_emploi   <- "EMPL3112"   # effectifs salariés fin décembre

# ---- 1. Communes et EPCI (API Découpage administratif) ------------
metropole <- c("11", "24", "27", "28", "32", "44", "52", "53", "75", "76", "84", "93", "94")
com <- fromJSON(telecharger(url_communes, "data/brut/communes.json")) |>
  filter(codeRegion %in% metropole, !is.na(codeEpci)) |>
  rename(epci = codeEpci)
com$dep <- substr(com$code, 1, 2)
com$population[is.na(com$population)] <- 0

fichier <- telecharger(url_epci, "data/brut/epci.geojson")
resume <- com |>
  group_by(epci) |>
  summarise(pop = sum(population),
            ville = nom[which.max(population)],
            pop_ville = max(population),
            dep = names(which.max(tapply(population, dep, sum))),
            reg_actuelle = names(which.max(tapply(population, codeRegion, sum))))
epci <- st_read(fichier, quiet = TRUE) |>
  select(code, nom) |>
  inner_join(resume, by = c("code" = "epci")) |>
  st_transform(2154) |>
  st_make_valid() |>
  arrange(code)
n <- nrow(epci)
cat(n, "EPCI en métropole\n")
epci$aire <- as.numeric(st_area(epci)) / 1e6      # km²

# Histoire : département (1790), ancienne région (avant 2016), province (1789)
epci$anc_region <- unname(table_anciennes_regions()[epci$dep])
epci$province <- unname(table_provinces()[epci$dep])

# ---- 2. Eau : région hydrographique dominante de chaque EPCI ------
telecharger(url_secteurs, "data/brut/secteurs.zip", jours = 3650)
unzip("data/brut/secteurs.zip", exdir = "data/brut/secteurs")
sh <- st_read(list.files("data/brut/secteurs", "\\.shp$", recursive = TRUE, full.names = TRUE)[1],
              quiet = TRUE)
st_geometry(sh) <- fermer_anneaux(st_geometry(sh))     # le fichier contient des anneaux non fermés
sh <- sh |> st_transform(2154) |> st_make_valid()
col <- grep("^cdsect", names(sh), ignore.case = TRUE, value = TRUE)[1]
if (is.na(col)) stop("Colonne du code de secteur introuvable. Colonnes : ", paste(names(sh), collapse = ", "))
sh$lettre <- substr(sh[[col]], 1, 1)       # 1er caractère = région hydrographique
rh <- sh |> group_by(lettre) |> summarise()
inter <- st_intersection(epci["code"], rh)
inter$aire <- as.numeric(st_area(inter))
bassin <- inter |>
  st_drop_geometry() |>
  group_by(code) |>
  mutate(part = aire / sum(aire)) |>
  slice_max(aire, n = 1, with_ties = FALSE)
epci$bassin <- bassin$lettre[match(epci$code, bassin$code)]
epci$part_bassin <- bassin$part[match(epci$code, bassin$code)]   # part de l'EPCI dans son bassin dominant
epci$bassin[is.na(epci$bassin)] <- "?"
# îles et EPCI sans région hydrographique reconnue : on reprend celle de l'EPCI connu le plus proche
connu <- epci$bassin %in% names(table_fleuves())
if (any(!connu)) {
  centres <- st_centroid(st_geometry(epci))
  epci$bassin[!connu] <- epci$bassin[connu][st_nearest_feature(centres[!connu], centres[connu])]
}
# grand bassin (agence de l'eau) de chaque région hydrographique
agences <- c(A = "Rhin-Meuse", B = "Rhin-Meuse", D = "Artois-Picardie", E = "Artois-Picardie",
             F = "Seine-Normandie", G = "Seine-Normandie", H = "Seine-Normandie", I = "Seine-Normandie",
             J = "Loire-Bretagne", K = "Loire-Bretagne", L = "Loire-Bretagne", M = "Loire-Bretagne",
             N = "Loire-Bretagne", O = "Adour-Garonne", P = "Adour-Garonne", Q = "Adour-Garonne",
             R = "Adour-Garonne", S = "Adour-Garonne", U = "Rhône-Méditerranée", V = "Rhône-Méditerranée",
             W = "Rhône-Méditerranée", X = "Rhône-Méditerranée", Y = "Rhône-Méditerranée")
epci$agence <- unname(agences[epci$bassin])
epci$agence[is.na(epci$agence)] <- "?"
# Grand fleuve de chaque EPCI. On l'attribue secteur par secteur, et non par région hydrographique entière,
# car certaines régions hydrographiques mêlent plusieurs bassins (voir corrections_fleuves dans 00_fonctions.R).
sh$fleuve <- unname(table_fleuves()[sh$lettre])
nom_sect <- grep("^lb|^nom|libel", names(sh), ignore.case = TRUE, value = TRUE)[1]
if (!is.na(nom_sect)) {
  for (f in names(corrections_fleuves())) {
    motif <- paste0("\\b(", paste(corrections_fleuves()[[f]], collapse = "|"), ")\\b")
    sh$fleuve[sh$lettre %in% c("E", "S") & grepl(motif, sh[[nom_sect]], ignore.case = TRUE)] <- f
  }
  cat("Secteurs des régions hydrographiques E et S, avec le fleuve attribué (à vérifier) :\n")
  print(st_drop_geometry(sh)[sh$lettre %in% c("E", "S"), c(col, nom_sect, "fleuve")], row.names = FALSE)
} else cat("Pas de colonne de nom de secteur : corrections des fleuves non appliquées.\n")
fl <- sh |> filter(!is.na(fleuve)) |> group_by(fleuve) |> summarise()
inter_fl <- st_intersection(epci["code"], fl)
inter_fl$aire <- as.numeric(st_area(inter_fl))
dominant <- inter_fl |> st_drop_geometry() |> group_by(code) |> slice_max(aire, n = 1, with_ties = FALSE)
epci$fleuve <- dominant$fleuve[match(epci$code, dominant$code)]
manque_fl <- is.na(epci$fleuve)       # îles : fleuve tiré de leur région hydrographique
epci$fleuve[manque_fl] <- unname(table_fleuves()[epci$bassin[manque_fl]])
epci$fleuve[is.na(epci$fleuve)] <- "?"
# Les côtiers méditerranéens forment deux ensembles séparés par le delta du Rhône : on les distingue
languedoc <- epci$fleuve == "Côtiers méditerranéens" & epci$dep %in% c("30", "34", "11", "66", "48")
epci$fleuve[languedoc] <- "Côtiers du Languedoc-Roussillon"
epci$fleuve[epci$fleuve == "Côtiers méditerranéens"] <- "Côtiers de Provence et de Corse"
inconnues <- setdiff(unique(epci$bassin), names(table_fleuves()))
if (length(inconnues)) cat("Régions hydrographiques sans fleuve attribué (à compléter dans table_fleuves) :", inconnues, "\n")

# ---- 3. Revenus (Insee, Filosofi) ----------------------------------
filo <- melodi::get_all_data(jeu_revenus)
filo$GEO <- as.character(filo$GEO)
filo$OBS_VALUE <- as.numeric(filo$OBS_VALUE)
filo <- filo[filo$TIME_PERIOD == max(filo$TIME_PERIOD), ]
indicateur <- function(mesure) {
  d <- filo[filo$FILOSOFI_MEASURE == mesure, ]
  if (nrow(d) == 0) stop("Mesure ", mesure, " absente. Mesures disponibles : ",
                         paste(unique(filo$FILOSOFI_MEASURE), collapse = ", "))
  e <- d[d$GEO_OBJECT == "EPCI", ]
  v <- e$OBS_VALUE[match(epci$code, e$GEO)]
  # repli si l'EPCI manque : moyenne des communes pondérée par la population
  cm <- d[d$GEO_OBJECT == "COM", ] |>
    inner_join(com, by = c("GEO" = "code")) |>
    group_by(epci) |>
    summarise(v = weighted.mean(OBS_VALUE, population, na.rm = TRUE))
  ifelse(is.na(v), cm$v[match(epci$code, cm$epci)], v)
}
epci$revenu <- indicateur(mesure_revenu)
epci$pauvrete <- indicateur(mesure_pauvrete)

# ---- 4. Emploi salarié par secteur (Insee, Flores) -----------------
flo <- melodi::get_all_data(jeu_emploi)
flo$GEO <- as.character(flo$GEO)
flo <- flo[flo$GEO_OBJECT == "COM" & flo$FLORES_MEASURE == mesure_emploi & flo$ACTIVITY != "_T", ]
flo <- flo[flo$TIME_PERIOD == max(flo$TIME_PERIOD), ]
# autres dimensions (taille, forme juridique…) : on garde le total "_T"
for (v in setdiff(names(flo), c("GEO", "GEO_OBJECT", "FLORES_MEASURE", "TIME_PERIOD", "ACTIVITY", "OBS_VALUE")))
  if ("_T" %in% flo[[v]]) flo <- flo[flo[[v]] == "_T", ]
flo$epci <- factor(com$epci[match(flo$GEO, com$code)], levels = epci$code)
emploi <- xtabs(as.numeric(OBS_VALUE) ~ epci + ACTIVITY, data = flo)
parts <- as.data.frame.matrix(prop.table(emploi, 1))
names(parts) <- paste0("emploi_", names(parts))
epci <- cbind(epci, parts)
epci$emploi_hab <- rowSums(emploi) / epci$pop

# ---- 5. Navettes domicile-travail (Insee, recensement) -------------
telecharger(url_flux, "data/brut/flux.zip")
unzip("data/brut/flux.zip", exdir = "data/brut/flux")
f <- data.table::fread(list.files("data/brut/flux", "\\.csv$", full.names = TRUE)[1],
                       colClasses = list(character = c("CODGEO", "DCLT")), data.table = FALSE)
col_flux <- grep("^NBFLUX", names(f), value = TRUE)[1]
# arrondissements de Paris, Lyon, Marseille -> commune
commune <- function(x) {
  x[grepl("^751", x)] <- "75056"
  x[grepl("^6938", x)] <- "69123"
  x[grepl("^132", x)] <- "13055"
  x
}
o <- match(com$epci[match(commune(f$CODGEO), com$code)], epci$code)
d <- match(com$epci[match(commune(f$DCLT), com$code)], epci$code)
ok <- !is.na(o) & !is.na(d)
flux <- xtabs(f[[col_flux]][ok] ~ factor(o[ok], 1:n) + factor(d[ok], 1:n))
flux <- matrix(as.numeric(flux), n, n)      # flux[i, j] : actifs résidant en i, travaillant en j

# ---- 6. Europe : régions NUTS 1 (contours, population, PIB) et PIB des EPCI ----
lire_eurostat <- function(jeu) read.csv(telecharger(
  paste0(url_eurostat, jeu, "?format=SDMX-CSV&startPeriod=2018"), paste0("data/brut/", jeu, ".csv")))
lire_gisco <- function(niveau, echelle) {
  f <- paste0("NUTS_RG_", echelle, "_2024_4326_LEVL_", niveau, ".geojson")
  st_read(telecharger(paste0(url_gisco, f), paste0("data/brut/", f), jours = 365), quiet = TRUE)
}
pib <- lire_eurostat("nama_10r_3gdp")            # PIB, tous niveaux NUTS (NUTS 3 = départements)
pib <- pib[pib$unit == "MIO_EUR" & !is.na(pib$OBS_VALUE), ]
annee <- max(pib$TIME_PERIOD[grepl("^FR...$", pib$geo)])
pib <- pib[pib$TIME_PERIOD == annee, ]
hab <- lire_eurostat("nama_10r_3popgdp")         # population moyenne, en milliers
hab <- hab[!is.na(hab$OBS_VALUE) & hab$TIME_PERIOD <= annee, ]
hab <- hab[order(-hab$TIME_PERIOD), ]
hab <- hab[!duplicated(hab$geo), ]

# PIB d'un EPCI : PIB de son département, réparti au prorata de l'emploi salarié
nuts3 <- lire_gisco(3, "10M")
nuts3 <- nuts3[nuts3$CNTR_CODE == "FR", ]
points <- st_transform(st_point_on_surface(st_geometry(epci)), 4326)
epci$nuts3 <- nuts3$NUTS_ID[st_nearest_feature(points, nuts3)]
emploi_total <- epci$emploi_hab * epci$pop
emploi_total[is.na(emploi_total)] <- 0
epci$pib <- pib$OBS_VALUE[match(epci$nuts3, pib$geo)] * emploi_total / ave(emploi_total, epci$nuts3, FUN = sum)

nuts1 <- lire_gisco(1, "20M") |>
  transmute(id = NUTS_ID, nom = NAME_LATN, pays = CNTR_CODE,
            pop = 1000 * hab$OBS_VALUE[match(NUTS_ID, hab$geo)],
            pib = pib$OBS_VALUE[match(NUTS_ID, pib$geo)])
st_write(nuts1, "data/nuts1.gpkg", delete_dsn = TRUE, quiet = TRUE)

# ---- Contrôle des jointures : valeurs manquantes par variable ------------
manque <- sapply(st_drop_geometry(epci), function(v) sum(is.na(v) | v == "?"))
manque <- manque[manque > 0]
cat("\nEPCI sans valeur, par variable (sur", n, ") :\n")
print(manque)
part_flux <- sum(f[[col_flux]][ok]) / sum(f[[col_flux]])
cat("Navettes rattachées à un EPCI de métropole :", round(100 * part_flux, 1), "%\n")
if (any(manque > 0.05 * n)) warning("Plus de 5 % des EPCI sans valeur pour : ",
                                    paste(names(manque)[manque > 0.05 * n], collapse = ", "),
                                    ". Vérifier les millésimes de géographie communale des sources.")
controle <- paste0(if (length(manque)) paste0(names(manque), " : ", manque, " EPCI", collapse = " ; ") else "aucune",
                   " (remplacées par la médiane nationale) ; ", round(100 * part_flux, 1), " % des navettes rattachées à un EPCI")

# ---- Enregistrement -------------------------------------------------
st_write(epci, "data/epci.gpkg", delete_dsn = TRUE, quiet = TRUE)
saveRDS(flux, "data/flux.rds")
write_json(list(
  date = format(Sys.Date()),
  controle = controle,
  epci = paste0(n, " EPCI, géographie en vigueur. Contours Etalab d'après l'IGN, populations de l'API Découpage administratif"),
  eau = "Sandre, BD Carthage 2017, secteurs hydrographiques",
  histoire = "Départements et régions d'avant 2016 (code officiel géographique)",
  provinces = "Provinces de 1789 : province dominante de chaque département (table dans R/00_fonctions.R)",
  europe = paste0("Eurostat, PIB (nama_10r_3gdp) et population (nama_10r_3popgdp), année ", annee,
                  " ; contours NUTS 2024 © EuroGeographics (GISCO)"),
  revenus = paste0("Insee, Filosofi (", jeu_revenus, "), millésime ", max(filo$TIME_PERIOD)),
  emploi = paste0("Insee, Flores (", jeu_emploi, "), millésime ", max(flo$TIME_PERIOD)),
  flux = paste0("Insee, recensement, base flux de mobilité domicile-travail (", col_flux, ")")
), "data/sources.json", auto_unbox = TRUE, pretty = TRUE)
cat("Données prêtes.\n")
