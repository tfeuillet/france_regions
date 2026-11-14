# ------------------------------------------------------------------
# 1. Téléchargement et préparation des données (maille : EPCI)
#    Sorties : data/epci.gpkg, data/flux.rds, data/sources.json
# ------------------------------------------------------------------
library(sf)
library(dplyr)
library(jsonlite)
source("R/00_fonctions.R")
options(timeout = 900)
dir.create("data/brut", recursive = TRUE, showWarnings = FALSE)

# ---- Adresses des serveurs (tout est ici) -------------------------
url_communes <- "https://geo.api.gouv.fr/communes?fields=code,nom,population,codeEpci,codeRegion&format=json"
url_epci     <- "https://geo.api.gouv.fr/epcis?fields=code,nom&format=geojson&geometry=contour"
url_epci_bis <- "https://etalab-datasets.geo.data.gouv.fr/contours-administratifs/latest/geojson/epci-100m.geojson"
url_secteurs <- "https://services.sandre.eaufrance.fr/telechargement/geo/ETH/BDCarthage/FXX/2017/France_metropole_entiere/SHP/SecteurHydro_FXX-shp.zip"
# Seule adresse à changer une fois par an (nouveau millésime du recensement) :
url_flux     <- "https://www.insee.fr/fr/statistiques/fichier/8582949/base-flux-mobilite-domicile-lieu-travail-2022_csv.zip"
# Jeux de données Insee (catalogue Melodi) : toujours le dernier millésime
jeu_revenus  <- "DS_FILOSOFI_CC"
jeu_emploi   <- "DS_FLORES_A5"
mesure_revenu   <- "MED_SL"     # niveau de vie médian
mesure_pauvrete <- "PR_MD60"    # taux de pauvreté (seuil 60 %)
mesure_emploi   <- "EMPL3112"   # effectifs salariés fin décembre

# ---- 1. Communes et EPCI (API Découpage administratif) ------------
metropole <- c("11", "24", "27", "28", "32", "44", "52", "53", "75", "76", "84", "93", "94")
com <- fromJSON(url_communes) |>
  filter(codeRegion %in% metropole, !is.na(codeEpci)) |>
  rename(epci = codeEpci)
com$population[is.na(com$population)] <- 0

fichier <- tryCatch(telecharger(url_epci, "data/brut/epci.geojson"),
                    error = function(e) telecharger(url_epci_bis, "data/brut/epci.geojson"))
resume <- com |>
  group_by(epci) |>
  summarise(pop = sum(population),
            ville = nom[which.max(population)],
            reg_actuelle = names(which.max(tapply(population, codeRegion, sum))))
epci <- st_read(fichier, quiet = TRUE) |>
  select(code, nom) |>
  inner_join(resume, by = c("code" = "epci")) |>
  st_transform(2154) |>
  st_make_valid() |>
  arrange(code)
n <- nrow(epci)
cat(n, "EPCI en métropole\n")

# ---- 2. Eau : région hydrographique dominante de chaque EPCI ------
telecharger(url_secteurs, "data/brut/secteurs.zip", jours = 3650)
unzip("data/brut/secteurs.zip", exdir = "data/brut/secteurs")
sh <- st_read(list.files("data/brut/secteurs", "\\.shp$", recursive = TRUE, full.names = TRUE)[1],
              quiet = TRUE) |>
  st_transform(2154) |>
  st_make_valid()
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

# ---- 6. Données facultatives fournies à la main ---------------------
# data/manuel/ressources_epci.csv : colonne "epci" (code SIREN) + variables numériques
if (file.exists("data/manuel/ressources_epci.csv")) {
  res <- read.csv2("data/manuel/ressources_epci.csv", colClasses = c(epci = "character"))
  names(res)[-1] <- paste0("res_", names(res)[-1])
  epci <- left_join(epci, res, by = c("code" = "epci"))
}

# ---- Enregistrement -------------------------------------------------
st_write(epci, "data/epci.gpkg", delete_dsn = TRUE, quiet = TRUE)
saveRDS(flux, "data/flux.rds")
write_json(list(
  date = format(Sys.Date()),
  epci = paste0("API Découpage administratif (geo.api.gouv.fr), ", n, " EPCI, géographie en vigueur"),
  eau = "Sandre, BD Carthage 2017, secteurs hydrographiques",
  revenus = paste0("Insee, Filosofi (", jeu_revenus, "), millésime ", max(filo$TIME_PERIOD)),
  emploi = paste0("Insee, Flores (", jeu_emploi, "), millésime ", max(flo$TIME_PERIOD)),
  flux = paste0("Insee, recensement, base flux de mobilité domicile-travail (", col_flux, ")")
), "data/sources.json", auto_unbox = TRUE, pretty = TRUE)
cat("Données prêtes.\n")
