# ------------------------------------------------------------------
# Fonctions communes
# ------------------------------------------------------------------

# Télécharger un fichier, sans le retélécharger s'il a moins de `jours` jours.
# En cas d'échec, on réessaie trois fois ; si le serveur ne répond toujours pas,
# on garde la copie déjà téléchargée, s'il y en a une.
telecharger <- function(url, fichier, jours = 25, essais = 3) {
  if (file.exists(fichier) && difftime(Sys.time(), file.mtime(fichier), units = "days") < jours) return(fichier)
  temporaire <- paste0(fichier, ".tmp")
  for (k in seq_len(essais)) {
    ok <- tryCatch({ download.file(url, temporaire, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (ok && file.size(temporaire) > 0) { file.rename(temporaire, fichier); return(fichier) }
    message("Téléchargement impossible (essai ", k, " sur ", essais, ") : ", url)
    Sys.sleep(10 * k)
  }
  unlink(temporaire)
  if (file.exists(fichier)) {
    warning("Serveur injoignable, on garde la copie du ", format(file.mtime(fichier), "%d/%m/%Y"), " : ", fichier)
    return(fichier)
  }
  stop("Serveur injoignable et aucune copie locale : ", url, "\nRéessayer plus tard.")
}

# Réparer des polygones dont certains anneaux ne sont pas fermés
# (le dernier point doit répéter le premier) ou ont moins de 4 points
fermer_anneaux <- function(g) {
  st_sfc(lapply(st_zm(g), function(x) {
    x <- if (inherits(x, "POLYGON")) list(unclass(x)) else unclass(x)
    x <- lapply(x, function(p) {
      p <- lapply(p, function(r) if (any(r[1, ] != r[nrow(r), ])) rbind(r, r[1, ]) else r)
      p[sapply(p, nrow) >= 4]
    })
    st_multipolygon(x[lengths(x) > 0])
  }), crs = st_crs(g))
}

# Graphe de contiguïté d'un seul tenant : les morceaux isolés (îles)
# sont reliés à l'EPCI le plus proche
voisinage <- function(x) {
  nb <- poly2nb(x, snap = 100)
  g <- st_geometry(x)
  repeat {
    comp <- n.comp.nb(nb)
    if (comp$nc == 1) break
    petit <- which(comp$comp.id == which.min(table(comp$comp.id)))
    reste <- setdiff(seq_along(nb), petit)
    d <- st_distance(g[petit], g[reste])
    ij <- which(d == min(d), arr.ind = TRUE)[1, ]
    a <- petit[ij[1]]
    b <- reste[ij[2]]
    nb[[a]] <- sort(setdiff(union(nb[[a]], b), 0L))
    nb[[b]] <- sort(setdiff(union(nb[[b]], a), 0L))
  }
  nb
}

# Tables d'appartenance par département -----------------------------------------------------

# Régions d'avant 2016
table_anciennes_regions <- function() {
  l <- list(
    "Alsace" = c("67", "68"), "Aquitaine" = c("24", "33", "40", "47", "64"),
    "Auvergne" = c("03", "15", "43", "63"), "Basse-Normandie" = c("14", "50", "61"),
    "Bourgogne" = c("21", "58", "71", "89"), "Bretagne" = c("22", "29", "35", "56"),
    "Centre" = c("18", "28", "36", "37", "41", "45"), "Champagne-Ardenne" = c("08", "10", "51", "52"),
    "Corse" = c("2A", "2B"), "Franche-Comté" = c("25", "39", "70", "90"),
    "Haute-Normandie" = c("27", "76"), "Île-de-France" = c("75", "77", "78", "91", "92", "93", "94", "95"),
    "Languedoc-Roussillon" = c("11", "30", "34", "48", "66"), "Limousin" = c("19", "23", "87"),
    "Lorraine" = c("54", "55", "57", "88"), "Midi-Pyrénées" = c("09", "12", "31", "32", "46", "65", "81", "82"),
    "Nord-Pas-de-Calais" = c("59", "62"), "Pays de la Loire" = c("44", "49", "53", "72", "85"),
    "Picardie" = c("02", "60", "80"), "Poitou-Charentes" = c("16", "17", "79", "86"),
    "Provence-Alpes-Côte d'Azur" = c("04", "05", "06", "13", "83", "84"),
    "Rhône-Alpes" = c("01", "07", "26", "38", "42", "69", "73", "74"))
  setNames(rep(names(l), lengths(l)), unlist(l))
}

# Provinces à la veille de la Révolution : province dominante de chaque département.
# Approximation : les départements de 1790 ne suivent pas exactement les limites des provinces.
table_provinces <- function() {
  l <- list(
    "Bretagne" = c("22", "29", "35", "56", "44"), "Normandie" = c("14", "27", "50", "61", "76"),
    "Île-de-France" = c("75", "77", "78", "91", "92", "93", "94", "95", "60", "02"),
    "Picardie" = "80", "Artois" = "62", "Flandre" = "59",
    "Champagne" = c("08", "10", "51", "52"), "Lorraine" = c("54", "55", "57", "88"), "Alsace" = c("67", "68"),
    "Franche-Comté" = c("25", "39", "70", "90"), "Bourgogne" = c("21", "71", "89", "01"),
    "Nivernais" = "58", "Bourbonnais" = "03", "Berry" = c("18", "36"), "Orléanais" = c("45", "41", "28"),
    "Touraine" = "37", "Anjou" = "49", "Maine" = c("72", "53"), "Poitou" = c("86", "79", "85"),
    "Aunis et Saintonge" = "17", "Angoumois" = "16", "Limousin" = c("19", "87"), "Marche" = "23",
    "Auvergne" = c("63", "15", "43"), "Lyonnais" = c("69", "42"), "Dauphiné" = c("38", "26", "05"),
    "Savoie" = c("73", "74"), "Provence" = c("13", "83", "04", "84"), "Comté de Nice" = "06",
    "Languedoc" = c("31", "81", "34", "30", "11", "48", "07"), "Roussillon" = "66", "Comté de Foix" = "09",
    "Guyenne et Gascogne" = c("33", "24", "47", "46", "12", "82", "40", "32", "65"),
    "Béarn et Pays basque" = "64", "Corse" = c("2A", "2B"))
  setNames(rep(names(l), lengths(l)), unlist(l))
}

# Grand fleuve (ou ensemble de fleuves côtiers) de chaque région hydrographique de la BD Carthage,
# désignée par la première lettre de son code
table_fleuves <- function() c(
  A = "Rhin", B = "Meuse", D = "Meuse", E = "Somme et côtiers du Nord",   # D : la Sambre, affluent de la Meuse
  F = "Seine", H = "Seine", G = "Côtiers normands", I = "Côtiers normands", J = "Côtiers bretons",
  K = "Loire", L = "Loire", M = "Loire", N = "Côtiers vendéens",
  O = "Garonne-Dordogne", P = "Garonne-Dordogne", Q = "Adour", R = "Charente", S = "Côtiers aquitains",
  U = "Rhône", V = "Rhône", W = "Rhône", X = "Rhône", Y = "Côtiers méditerranéens")

# Secteurs hydrographiques à rattacher à un autre fleuve que celui de leur région hydrographique,
# repérés par un mot de leur nom. La région S mêle l'estuaire de la Gironde aux côtiers des Landes,
# et la région E mêle l'Escaut (Lille, Valenciennes) à la Somme et aux côtiers du Nord.
corrections_fleuves <- function() list(
  "Garonne-Dordogne" = c("gironde"),
  "Escaut" = c("escaut", "scarpe", "lys", "deûle", "deule", "marque", "sensée", "sensee",
               "rhonelle", "écaillon", "ecaillon", "hogneau"))   # pas « selle » : une Selle se jette aussi dans la Somme
