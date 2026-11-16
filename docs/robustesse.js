// ------------------------------------------------------------------
// Tests de robustesse (utilisés par robustesse.html, et exécutables avec Node)
// ------------------------------------------------------------------

// Anomalies d'un découpage calculé : ce qui ne devrait jamais arriver, ou alerter
function controler(D, poids, K) {
  const P = preparer(D), g = regionaliser(D, poids, K), poles = choisirPoles(D, poids, K - 1), d = decrire(D, g, poles);
  const alertes = [];
  if (d.ind.n !== K) alertes.push(d.ind.n + " régions au lieu de " + K);
  // chaque région doit être d'un seul tenant
  const vus = new Set(); let morceaux = 0;
  for (const i of P.terre) { if (vus.has(i)) continue; morceaux++; const pile = [i]; vus.add(i);
    while (pile.length) for (const [j] of P.voisins[pile.pop()]) if (!vus.has(j) && g[j] === g[i]) { vus.add(j); pile.push(j); } }
  if (morceaux !== K - 1) alertes.push((morceaux - K + 1) + " région(s) en plusieurs morceaux");
  // région minuscule, ou très étirée (un EPCI à plus de 3,5 rayons moyens de son pôle)
  const rayon = Math.sqrt(P.aire / ((K - 1) * Math.PI)); let etirement = 0, loin = -1;
  for (const i of P.terre) { const e = P.dist(i, poles[g[i]]) / rayon; if (e > etirement) { etirement = e; loin = i; } }
  const petite = d.regions.filter(r => r.id >= 0).reduce((a, r) => r.pop < a.pop ? r : a);
  if (petite.pop < 0.01 * P.pop) alertes.push("région de " + petite.ville + " : " + Math.round(petite.pop / 1000) + " 000 habitants seulement");
  if (etirement > 3.5) alertes.push(D.ville[loin] + " rattachée à " + D.ville[poles[g[loin]]] + ", à " + Math.round(P.dist(loin, poles[g[loin]])) + " km");
  return { g, alertes, etirement, popMin: petite.pop, popMax: d.ind.pop_max, compacite: d.ind.compacite };
}

// Ressemblance moyenne et minimale (indice de Rand ajusté) quand on déplace un seul poids d'un cran
function sensibilitePoids(D, poids, K) {
  const ref = regionaliser(D, poids, K), v = [];
  for (const c of CRITERES) for (const pas of [-1, 1]) {
    const p = { ...poids, [c.id]: (poids[c.id] || 0) + pas };
    if (p[c.id] >= 0 && p[c.id] <= 10) v.push({ critere: c.nom, pas, ari: randAjuste(ref, regionaliser(D, p, K)) });
  }
  return v;
}

// Ressemblance avec le découpage de référence quand on change un réglage interne
function sensibiliteReglage(D, poids, K, cle, valeur) {
  const ref = regionaliser(D, poids, K), avant = REGLAGES[cle];
  REGLAGES[cle] = valeur;
  const ari = randAjuste(ref, regionaliser(D, poids, K));
  REGLAGES[cle] = avant;
  return ari;
}
const VARIANTES = {        // réglage : [valeur basse, valeur haute]
  eloignement: [0.04, 0.16], partMin: [0.05, 0.3], retouches: [0, 30], marge: [0.005, 0.05],
  villes: [75, 300], expoCaptee: [0.25, 0.75], expoDistance: [0.17, 0.5]
};

if (typeof module !== "undefined") module.exports = { controler, sensibilitePoids, sensibiliteReglage, VARIANTES };
