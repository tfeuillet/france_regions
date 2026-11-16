// ------------------------------------------------------------------
// Moteur de régionalisation (s'exécute dans le navigateur)
//
// 1. On choisit des PÔLES, un par région : de grandes villes éloignées les unes
//    des autres.
// 2. Chaque région grandit autour de son pôle : à chaque étape, on rattache le
//    couple (EPCI libre, région voisine) qui obtient la meilleure note, moyenne
//    des notes des critères pondérée par les poids.
// Les régions sont donc toujours d'un seul tenant.
// ------------------------------------------------------------------

// groupe "base" : critères proposés à tous ; groupe "avance" : réglages avancés.
// type "contrainte" : règle de dessin plutôt que raison de réunir des territoires.
const CRITERES = [
  { id: "competitivite", groupe: "base", nom: "Compétitivité économique",
    aide: "Chaque région doit atteindre une taille économique suffisante pour compter face aux grandes régions européennes. Un territoire rejoint donc de préférence la région qui en a le plus besoin, c'est-à-dire celle dont le PIB est encore le plus faible." },
  { id: "solidarite", groupe: "base", nom: "Solidarité riches-pauvres",
    aide: "Un territoire rejoint de préférence la région dont il rapproche le niveau de vie de la moyenne nationale. Chaque région mêle ainsi des territoires aisés et des territoires modestes." },
  { id: "flux", groupe: "base", nom: "Trajets domicile-travail",
    aide: "Un territoire rejoint de préférence la région où travaillent ses habitants, ou d'où viennent ceux qui travaillent chez lui." },
  { id: "provinces", groupe: "base", nom: "Provinces historiques",
    aide: "Un territoire rejoint de préférence la région où domine sa province d'Ancien Régime, comme la Bretagne, le Languedoc ou le Dauphiné." },
  { id: "eau", groupe: "base", nom: "Bassins des grands fleuves",
    aide: "Un territoire rejoint de préférence la région où domine le bassin de son fleuve, comme la Seine, la Loire ou le Rhône." },
  { id: "histoire", groupe: "avance", nom: "Anciennes régions",
    aide: "Un territoire rejoint de préférence la région où domine son ancienne région, celle d'avant 2016." },
  { id: "equilibre", groupe: "avance", nom: "Équilibre démographique",
    aide: "Une région déjà très peuplée attire moins, ce qui rapproche les populations des régions." },
  { id: "compacite", groupe: "avance", type: "contrainte", nom: "Simplicité des frontières",
    aide: "Un territoire déjà largement entouré par une région la rejoint en priorité. Les frontières sont plus courtes et les régions moins étirées." }
];

const REGLAGES = {
  eloignement: 0.08,   // proximité du pôle : note réduite de 0,08 par « rayon moyen » de distance au pôle
  partMin: 0.15,       // chaque région atteint d'abord 15 % de la population moyenne
  retouches: 10,       // nombre maximal de passes de retouche des frontières
  marge: 0.02,         // gain de note minimal pour qu'un EPCI change de région lors des retouches
  villes: 150,         // pôles possibles : les 150 plus grandes villes
  expoCaptee: 0.5,     // choix des pôles : exposant de la population captée
  expoDistance: 1 / 3  // choix des pôles : exposant de la distance au pôle le plus proche
};
// Proximité du pôle, de 0 (contrainte presque nulle) à 4 (régions serrées autour de leur pôle).
// Le niveau 2 est le réglage par défaut.
const REGULARITE = [0.02, 0.04, 0.08, 0.12, 0.16];
function regulariser(niveau) { REGLAGES.eloignement = REGULARITE[niveau]; }
const APPARTENANCES = ["provinces", "histoire", "eau"];
// Contrainte facultative (poids.idf = 1) : l'Île-de-France garde ses huit départements, Paris en est le pôle
const IDF = ["75", "77", "78", "91", "92", "93", "94", "95"];

// Préparation, faite une seule fois : voisinages, échanges d'actifs, moyennes nationales
function preparer(D) {
  if (D.prep) return D.prep;
  const n = D.pop.length, terre = [];
  for (let i = 0; i < n; i++) if (D.terre[i]) terre.push(i);
  const dist = (i, j) => Math.hypot(D.x[i] - D.x[j], D.y[i] - D.y[j]);        // km
  // Taille d'une ville : moyenne géométrique de la population de la commune-centre
  // et de celle de son intercommunalité (ni l'une ni l'autre ne suffit seule)
  const taille = D.pop.map((p, i) => Math.sqrt(p * D.pop_ville[i]));
  const voisins = D.pop.map(() => []);      // pour chaque EPCI : [voisin, longueur de la limite commune en km]
  D.voisins.forEach(([i, j], e) => { voisins[i].push([j, D.longueur[e]]); voisins[j].push([i, D.longueur[e]]); });
  const echanges = D.pop.map(() => new Map()), total = new Float64Array(n);
  for (let t = 0; t < D.flux.i.length; t++) {
    const i = D.flux.i[t], j = D.flux.j[t], v = D.flux.v[t];
    if (i === j || !D.terre[i] || !D.terre[j]) continue;
    echanges[i].set(j, (echanges[i].get(j) || 0) + v); echanges[j].set(i, (echanges[j].get(i) || 0) + v);
    total[i] += v; total[j] += v;
  }
  let pop = 0, rev = 0, variance = 0;
  for (const i of terre) { pop += D.pop[i]; rev += D.pop[i] * D.revenu[i]; }
  const revMoyen = rev / pop;
  for (const i of terre) variance += D.pop[i] * (D.revenu[i] - revMoyen) ** 2;
  const villes = [...terre].sort((a, b) => taille[b] - taille[a]);       // villes, de la plus grande à la plus petite
  let aire = 0; for (const i of terre) aire += D.aire[i];
  // repère européen : PIB médian des grandes régions (NUTS 1) des autres pays, en millions d'euros
  const pibs = D.nuts1.map(r => r.pib).sort((a, b) => a - b), m = pibs.length >> 1;
  const pibEurope = pibs.length % 2 ? pibs[m] : (pibs[m - 1] + pibs[m]) / 2;
  return D.prep = { terre, dist, taille, villes, aire, voisins, echanges, total, pop, revMoyen, revEcart: Math.sqrt(variance / pop), pibEurope };
}

// Choix des k pôles. Le premier est la plus grande ville. Les suivants sont choisis un à un : la ville
// qui maximise  taille × racine(population qu'elle capterait) × racine cubique(distance au pôle le plus proche),
// la population captée étant celle des EPCI plus proches d'elle que de tout pôle déjà choisi.
// Deux pôles forment toujours deux régions distinctes. Une ville située dans la même province, la même
// ancienne région ou le même bassin fluvial qu'un pôle déjà choisi ne devient donc pas pôle à son tour
// (d'autant moins que ces critères pèsent) : elle rejoindra la région de ce pôle, ce qui garde la province
// ou le bassin entier.
function choisirPoles(D, poids, k) {
  const P = preparer(D), total = CRITERES.reduce((s, c) => s + (poids[c.id] || 0), 0) || 1;
  const wp = (poids.provinces || 0) / total, wh = (poids.histoire || 0) / total, we = (poids.eau || 0) / total;
  const parente = (i, p) => wp * (D.province[i] === D.province[p]) + wh * (D.anc_region[i] === D.anc_region[p]) +
    we * (0.6 * (D.fleuve[i] === D.fleuve[p]) + 0.4 * (D.agence[i] === D.agence[p]));
  const idf = poids.idf ? i => IDF.includes(D.dep[i]) : () => false;
  // avec la contrainte Île-de-France, aucune autre ville francilienne ne peut être pôle
  const villes = P.villes.slice(0, REGLAGES.villes).filter((c, r) => r === 0 || !idf(c)), poles = [villes[0]];
  const proche = P.terre.map(i => P.dist(i, poles[0]));       // distance de chaque EPCI au pôle le plus proche
  while (poles.length < Math.min(k, villes.length)) {
    let meilleur = -1, note = -1;
    for (const c of villes) {
      if (poles.includes(c)) continue;
      let captee = 0, distance = Infinity, nouveaute = 1;
      P.terre.forEach((i, t) => { if (P.dist(i, c) < proche[t]) captee += D.pop[i]; });
      for (const p of poles) { distance = Math.min(distance, P.dist(c, p)); nouveaute = Math.min(nouveaute, 1 - 0.999 * parente(c, p)); }
      const v = P.taille[c] * captee ** REGLAGES.expoCaptee * distance ** REGLAGES.expoDistance * nouveaute;
      if (v > note) { note = v; meilleur = c; }
    }
    poles.push(meilleur);
    P.terre.forEach((i, t) => { proche[t] = Math.min(proche[t], P.dist(i, meilleur)); });
  }
  return poles;
}

// Renvoie, pour K régions (Corse comprise), le numéro de région de chaque EPCI
// (rang du pôle de sa région, ou -1 pour la Corse)
function regionaliser(D, poids, K) {
  const P = preparer(D), n = D.pop.length;
  const k = K - (P.terre.length < n ? 1 : 0), g = new Array(n).fill(-1), R = [];
  const poles = choisirPoles(D, poids, k);
  // ajoute (sens = 1) ou retire (sens = -1) l'EPCI i de la région r
  const joindre = (r, i, sens = 1) => {
    const a = sens * D.aire[i], p = sens * D.pop[i];
    g[i] = sens > 0 ? r.id : -1; r.pop += p; r.aire += a; r.srev += p * D.revenu[i]; r.pib += sens * D.pib[i];
    r.sx += a * D.x[i]; r.sy += a * D.y[i]; r.sq += a * (D.x[i] ** 2 + D.y[i] ** 2); r.propre += sens * D.aire[i] ** 2 / (2 * Math.PI);
    for (const [cat, v] of [["bassin", a], ["fleuve", a], ["agence", a], ["dep", p], ["anc_region", p], ["province", p]])
      r[cat].set(D[cat][i], (r[cat].get(D[cat][i]) || 0) + v);
    for (const [j, v] of P.echanges[i]) r.echanges.set(j, (r.echanges.get(j) || 0) + sens * v);
  };
  poles.forEach((p, s) => {
    R.push({ id: s, pole: p, pop: 0, pib: 0, aire: 0, srev: 0, sx: 0, sy: 0, sq: 0, propre: 0,
      bassin: new Map(), fleuve: new Map(), agence: new Map(), dep: new Map(), anc_region: new Map(), province: new Map(), echanges: new Map() });
    joindre(R[s], p);
  });
  // contrainte Île-de-France : les huit départements forment d'emblée la région du premier pôle (Paris)
  const fige = new Uint8Array(n);
  if (poids.idf) for (const i of P.terre) if (IDF.includes(D.dep[i])) { fige[i] = 1; if (g[i] < 0) joindre(R[0], i); }
  const cible = P.pop / k, part = (m, cle, tot) => (m.get(cle) || 0) / tot;
  const total = CRITERES.reduce((s, c) => s + (poids[c.id] || 0), 0) || 1;
  const libre = 1 - APPARTENANCES.reduce((s, c) => s + (poids[c] || 0), 0) / total;      // part des critères sans géographie propre
  const rayon = Math.sqrt(P.aire / (k * Math.PI));                                        // rayon d'une région moyenne (km)
  // Note de chaque critère (entre 0 et 1) pour le rattachement de l'EPCI i à la région r
  const notes = {
    // plus la région pèse peu face à la médiane européenne, plus elle attire
    competitivite: (i, r) => 1 / (1 + r.pib / P.pibEurope),
    // le rattachement rapproche-t-il le niveau de vie moyen de la région de la moyenne nationale ?
    solidarite: (i, r) => {
      const avant = r.srev / r.pop, apres = (r.srev + D.pop[i] * D.revenu[i]) / (r.pop + D.pop[i]);
      return 0.5 + 0.5 * Math.tanh(20 * (Math.abs(avant - P.revMoyen) - Math.abs(apres - P.revMoyen)) / P.revEcart);
    },
    flux: (i, r) => (r.echanges.get(i) || 0) / (P.total[i] || 1),
    provinces: (i, r) => 0.75 * part(r.province, D.province[i], r.pop) + 0.25 * part(r.dep, D.dep[i], r.pop),
    histoire: (i, r) => 0.75 * part(r.anc_region, D.anc_region[i], r.pop) + 0.25 * part(r.dep, D.dep[i], r.pop),
    eau: (i, r) => 0.5 * part(r.fleuve, D.fleuve[i], r.aire) + 0.3 * part(r.agence, D.agence[i], r.aire) + 0.2 * part(r.bassin, D.bassin[i], r.aire),
    equilibre: (i, r) => 1 - Math.min(1, r.pop / (2 * cible)),
    // moyenne de deux mesures : la part du pourtour de l'EPCI qui touche déjà la région (frontières courtes),
    // et l'étalement de la région une fois l'EPCI rattaché (1 pour un disque, proche de 0 pour une bande)
    compacite: (i, r) => {
      let commun = 0; for (const [j, l] of P.voisins[i]) if (g[j] === r.id) commun += l;
      const a = D.aire[i], aire = r.aire + a, sx = r.sx + a * D.x[i], sy = r.sy + a * D.y[i];
      const inertie = r.sq + a * (D.x[i] ** 2 + D.y[i] ** 2) - (sx * sx + sy * sy) / aire + r.propre + a * a / (2 * Math.PI);
      return (Math.min(1, commun / D.perimetre[i]) + aire * aire / (2 * Math.PI * inertie)) / 2;
    }
  };
  const actifs = CRITERES.map(c => c.id).filter(c => poids[c] > 0), somme = actifs.reduce((s, c) => s + poids[c], 0) || 1;
  const eloignement = (i, r) => libre * REGLAGES.eloignement * P.dist(i, r.pole) / rayon;
  const note = (i, r) => {
    let s = 0; for (const c of actifs) s += poids[c] * notes[c](i, r);
    return s / somme - eloignement(i, r);
  };

  const candidats = new Map();          // EPCI libre -> Map(région voisine -> note)
  const ouvrir = (i, r) => { if (poids.idf && r.id === 0) return; let m = candidats.get(i); if (!m) candidats.set(i, m = new Map()); m.set(r.id, note(i, r)); };
  for (const i of P.terre) if (g[i] >= 0) for (const [j] of P.voisins[i]) if (g[j] < 0 && D.terre[j]) ouvrir(j, R[g[i]]);
  while (candidats.size) {
    // les régions encore trop petites passent d'abord ; sinon, le meilleur couple toutes régions confondues
    let bi = -1, br = -1, meilleure = -Infinity, petite = false;
    for (const [i, m] of candidats) for (const [r, s] of m) {
      const p = R[r].pop < REGLAGES.partMin * cible;
      if ((p && !petite) || (p === petite && s > meilleure)) { petite = p; meilleure = s; bi = i; br = r; }
    }
    const r = R[br];
    joindre(r, bi); candidats.delete(bi);
    for (const [i, m] of candidats) if (m.has(br)) m.set(br, note(i, r));      // la région a changé : ses notes aussi
    for (const [j] of P.voisins[bi]) if (g[j] < 0 && D.terre[j]) ouvrir(j, r);
  }

  // Retouches : un EPCI de frontière change de région si une région voisine lui convient mieux,
  // à condition de ne pas couper en deux la région qu'il quitte. Cela résorbe les bras et les
  // quasi-enclaves laissés par la croissance, et rend le résultat moins dépendant de l'ordre des rattachements.
  const tientSans = (r, i) => {         // la région r reste-t-elle d'un seul tenant sans l'EPCI i ?
    const vus = new Set([i]), pile = [r.pole];
    vus.add(r.pole);
    let n = 1;
    while (pile.length) for (const [j] of P.voisins[pile.pop()]) if (g[j] === r.id && !vus.has(j)) { vus.add(j); pile.push(j); n++; }
    return n === r.n - 1;
  };
  for (const r of R) r.n = 0;
  for (const i of P.terre) R[g[i]].n++;
  for (let passe = 0, bouge = true; passe < REGLAGES.retouches && bouge; passe++) {
    bouge = false;
    for (const i of P.terre) {
      const depart = R[g[i]];
      if (i === depart.pole || fige[i]) continue;
      const voisines = new Set(); for (const [j] of P.voisins[i]) if (g[j] !== depart.id && g[j] >= 0 && !(poids.idf && g[j] === 0)) voisines.add(g[j]);
      if (!voisines.size || !tientSans(depart, i)) continue;
      joindre(depart, i, -1);
      let mieux = depart, meilleure = note(i, depart) + REGLAGES.marge;
      for (const v of voisines) { const s = note(i, R[v]); if (s > meilleure) { meilleure = s; mieux = R[v]; } }
      joindre(mieux, i);
      if (mieux !== depart) { depart.n--; mieux.n++; bouge = true; }
    }
  }

  // Détail du calcul pour un EPCI (sert à l'exemple de l'onglet « Méthode ») : notes de chaque critère
  // pour sa région et ses régions voisines, l'EPCI étant retiré un instant de sa région
  g.detail = i => {
    const depart = R[g[i]], cand = new Set([depart.id]);
    for (const [j] of P.voisins[i]) if (g[j] >= 0 && !(poids.idf && g[j] === 0)) cand.add(g[j]);
    joindre(depart, i, -1);
    const contact = id => { let c = 0; for (const [j, l] of P.voisins[i]) if (g[j] === id) c += l; return Math.min(1, c / D.perimetre[i]); };
    const res = [...cand].map(id => { const r = R[id]; return { pole: r.pole, pop: r.pop, pib: r.pib, revenu: r.srev / r.pop,
      distance: P.dist(i, r.pole), contact: contact(id),
      parts: { province: part(r.province, D.province[i], r.pop), anc_region: part(r.anc_region, D.anc_region[i], r.pop),
        fleuve: part(r.fleuve, D.fleuve[i], r.aire) }, notes: Object.fromEntries(actifs.map(c => [c, notes[c](i, R[id])])),
      eloignement: eloignement(i, r), total: note(i, r) }; });
    joindre(depart, i);
    return { actifs, somme, poids, regions: res, actuelle: depart.pole, revMoyen: P.revMoyen, popMoyenne: cible };
  };
  return g;
}

// Description d'un découpage g (un identifiant de région par EPCI) : régions et indicateurs
// poles (facultatif) : pôle de chaque région, qui lui donne alors son nom
function decrire(D, g, poles) {
  const P = preparer(D), n = g.length, regions = new Map();
  let popTot = 0, revTot = 0;
  for (let i = 0; i < n; i++) {
    let r = regions.get(g[i]);
    if (!r) regions.set(g[i], r = { id: g[i], pop: 0, pib: 0, aire: 0, n: 0, srev: 0, chef: i, perimetre: 0, voisines: new Set(), corse: true });
    if (D.terre[i]) r.corse = false;          // une région dont aucune intercommunalité n'est sur le continent : la Corse
    const p = D.pop[i];
    r.pop += p; r.pib += D.pib[i]; r.aire += D.aire[i]; r.n++; r.srev += p * D.revenu[i]; r.perimetre += D.perimetre[i];
    if (P.taille[i] > P.taille[r.chef]) r.chef = i;          // à défaut de pôle, la plus grande ville donne son nom
    popTot += p; revTot += p * D.revenu[i];
  }
  D.voisins.forEach(([i, j], e) => {
    if (g[i] === g[j]) regions.get(g[i]).perimetre -= 2 * D.longueur[e];       // limite intérieure : pas une frontière
    else { regions.get(g[i]).voisines.add(g[j]); regions.get(g[j]).voisines.add(g[i]); }
  });
  if (poles) poles.forEach((p, rang) => { if (regions.has(rang)) regions.get(rang).chef = p; });
  const liste = [...regions.values()].sort((a, b) => b.pop - a.pop);
  const m = revTot / popTot;
  let inter = 0, totale = 0;
  liste.forEach(r => {
    r.revenu = r.srev / r.pop; r.ville = D.ville[r.chef]; r.pibHab = 1e6 * r.pib / r.pop;
    r.compacite = 4 * Math.PI * r.aire / r.perimetre ** 2;
    inter += r.pop * (r.revenu - m) ** 2;
  });
  for (let i = 0; i < n; i++) totale += D.pop[i] * (D.revenu[i] - m) ** 2;
  let dedans = 0, tout = 0;
  for (let t = 0; t < D.flux.i.length; t++) { tout += D.flux.v[t]; if (g[D.flux.i[t]] === g[D.flux.j[t]]) dedans += D.flux.v[t]; }
  // part de la population vivant dans la région principale de sa catégorie (bassin, ancienne région)
  const integrite = cat => {
    const t = new Map();
    for (let i = 0; i < n; i++) { let m2 = t.get(cat[i]); if (!m2) t.set(cat[i], m2 = new Map()); m2.set(g[i], (m2.get(g[i]) || 0) + D.pop[i]); }
    let s = 0; for (const m2 of t.values()) s += Math.max(...m2.values());
    return s / popTot;
  };
  // Les indicateurs qui comparent les régions entre elles laissent la Corse de côté : elle a sa propre
  // collectivité, ne bouge dans aucun scénario, et sa petite taille fausserait minimum, maximum et moyennes.
  const cont = liste.filter(r => !r.corse), pops = cont.map(r => r.pop), revs = cont.map(r => r.revenu), pibEurope = P.pibEurope;
  // indice de Gini des niveaux de vie moyens des régions, chaque région pesant sa population
  let gini = 0, pc = 0, rc = 0;
  for (const a of cont) { pc += a.pop; rc += a.pop * a.revenu; for (const b of cont) gini += a.pop * b.pop * Math.abs(a.revenu - b.revenu); }
  gini /= 2 * pc * rc;
  return { regions: liste, parId: regions, ind: {
    n: liste.length, pop_min: Math.min(...pops), pop_max: Math.max(...pops),
    navettes: dedans / tout, variance_revenu: inter / totale,
    rapport_revenu: Math.max(...revs) / Math.min(...revs), gini,
    sous_europe: cont.filter(r => r.pib < pibEurope).length, n_continent: cont.length,
    bassins: integrite(D.fleuve), histoire: integrite(D.anc_region), provinces: integrite(D.province),
    compacite: cont.reduce((s, r) => s + r.compacite, 0) / cont.length } };
}

// Indice de Rand ajusté : ressemblance entre deux découpages (1 = identiques)
function randAjuste(a, b) {
  const t = new Map(), la = new Map(), lb = new Map(), c2 = x => x * (x - 1) / 2;
  for (let i = 0; i < a.length; i++) {
    const k = a[i] + "|" + b[i];
    t.set(k, (t.get(k) || 0) + 1); la.set(a[i], (la.get(a[i]) || 0) + 1); lb.set(b[i], (lb.get(b[i]) || 0) + 1);
  }
  const somme = m => [...m.values()].reduce((s, x) => s + c2(x), 0);
  const attendu = somme(la) * somme(lb) / c2(a.length);
  return (somme(t) - attendu) / ((somme(la) + somme(lb)) / 2 - attendu);
}

if (typeof module !== "undefined") module.exports = { IDF, CRITERES, REGLAGES, REGULARITE, regulariser, preparer, choisirPoles, regionaliser, decrire, randAjuste };
