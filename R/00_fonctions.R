# ------------------------------------------------------------------
# Fonctions communes
# ------------------------------------------------------------------

# Télécharger un fichier (sans le retélécharger s'il a moins de `jours` jours)
telecharger <- function(url, fichier, jours = 25) {
  recent <- file.exists(fichier) &&
    difftime(Sys.time(), file.mtime(fichier), units = "days") < jours
  if (!recent) download.file(url, fichier, mode = "wb", quiet = TRUE)
  fichier
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

# Les deux fonctions de régionalisation renvoient une matrice :
# une ligne par EPCI, une colonne par nombre de régions demandé (ks).

# Régionalisation par HOMOGÉNÉITÉ : SKATER (arbre couvrant minimal élagué,
# une coupe à la fois), avec une population minimale par région
regions_skater <- function(nb, X, ks, pop, pop_min) {
  X <- scale(X)
  res <- matrix(1L, nrow(X), length(ks), dimnames = list(NULL, ks))
  sk <- mstree(nb2listw(nb, nbcosts(nb, X), style = "B"))[, 1:2]
  for (k in 2:max(ks)) {
    sk <- skater(sk, X, ncuts = 1, crit = pop_min, vec.crit = pop)
    if (k %in% ks) res[, as.character(k)] <- sk$groups
  }
  res
}

# Régionalisation par FUSIONS : à chaque étape, la région la moins peuplée
# fusionne avec sa voisine de plus forte affinité.
# affinite(a, b) reçoit les numéros des EPCI des deux régions.
regions_fusion <- function(nb, pop, ks, affinite, depart = seq_along(pop)) {
  reg <- as.integer(factor(depart))
  res <- matrix(NA_integer_, length(pop), length(ks), dimnames = list(NULL, ks))
  repeat {
    n_reg <- length(unique(reg))
    res[, ks >= n_reg & is.na(res[1, ])] <- as.integer(factor(reg))
    if (n_reg <= min(ks)) break
    p <- tapply(pop, reg, sum)
    r <- as.integer(names(p)[which.min(p)])
    membres <- which(reg == r)
    cand <- setdiff(unique(reg[unlist(nb[membres])]), r)
    a <- sapply(cand, function(v) affinite(membres, which(reg == v)))
    reg[membres] <- cand[which.max(a)]
  }
  res
}

# Part de la variance d'une variable située ENTRE les régions (0 à 1)
variance_inter <- function(x, g, w) {
  m <- weighted.mean(x, w)
  mg <- tapply(x * w, g, sum) / tapply(w, g, sum)
  sum(tapply(w, g, sum) * (mg - m)^2) / sum(w * (x - m)^2)
}

# Part d'un poids (population) qui reste dans la région principale de sa
# catégorie (ex. : un bassin non découpé = 1)
integrite <- function(categorie, g, w) {
  t <- xtabs(w ~ categorie + g)
  sum(apply(t, 1, max)) / sum(t)
}

# Indice de Rand ajusté : ressemblance entre deux découpages (1 = identiques)
rand_ajuste <- function(a, b) {
  t <- table(a, b)
  c2 <- function(x) sum(x * (x - 1) / 2)
  attendu <- c2(rowSums(t)) * c2(colSums(t)) / c2(sum(t))
  (c2(t) - attendu) / ((c2(rowSums(t)) + c2(colSums(t))) / 2 - attendu)
}
