// ── Evidence synthesis: built-in conversion factors, studies, pipeline ──
// In production these factors are pulled from the World Bank API at build
// time (PPP conversion factors + GDP deflator series; 2027 inflation is
// projected). Values here are ILLUSTRATIVE for the mock.
// Exports to window: FACTORS, STUDIES, standardize, poolStrategy, formatCur, TARGET_YEAR

const TARGET_YEAR = 2027;

// pppToKES = KES per 1 local-currency unit, PPP-adjusted for target year.
// infl = annual GDP-deflator inflation (projected through 2027).
const FACTORS = {
  KES: {country:'Kenya',         infl:0.065, pppToKES:1,      src:'World Bank ICP 2021 + KNBS deflator'},
  USD: {country:'United States', infl:0.026, pppToKES:47.0,   src:'World Bank ICP 2021 + BEA deflator'},
  GBP: {country:'United Kingdom',infl:0.028, pppToKES:64.0,   src:'World Bank ICP 2021 + ONS deflator'},
  INR: {country:'India',         infl:0.045, pppToKES:2.10,   src:'World Bank ICP 2021 + MOSPI deflator'},
  ZAR: {country:'South Africa',  infl:0.051, pppToKES:9.30,   src:'World Bank ICP 2021 + StatsSA deflator'},
  TZS: {country:'Tanzania',      infl:0.040, pppToKES:0.061,  src:'World Bank ICP 2021 + NBS deflator'},
};

// Each study reports cost in its own currency + price year, and an effect
// already harmonized to the common outcome (assumption: 1 QALY = 1 DALY,
// applied internally — not user-selectable).
const STUDIES = [
  {id:'s1', strategy:'Mass Vaccination',            author:'Ochieng et al.',      journal:'PLOS Med', year:2019, currency:'USD', cost:142000,    effect:1180, n:520},
  {id:'s2', strategy:'Mass Vaccination',            author:'Sharma et al.',       journal:'Value Health', year:2021, currency:'INR', cost:9800000,  effect:1320, n:880},
  {id:'s3', strategy:'Mass Vaccination',            author:'van der Merwe et al.',journal:'Cost Eff Resour Alloc', year:2018, currency:'ZAR', cost:1850000, effect:1090, n:410},
  {id:'s4', strategy:'Community Health Education',  author:'Mbeki et al.',        journal:'Trop Med Int Health', year:2020, currency:'TZS', cost:95000000, effect:760,  n:300},
  {id:'s5', strategy:'Community Health Education',  author:'Wanjiru et al.',      journal:'East Afr Med J', year:2022, currency:'KES', cost:5400000,  effect:880,  n:640},
  {id:'s6', strategy:'Status Quo',                  author:'MoH Kenya (baseline)',journal:'National HTA report', year:2023, currency:'KES', cost:2300000,  effect:450,  n:1000},
];

function standardize(study, toYear=TARGET_YEAR) {
  const f = FACTORS[study.currency];
  const years = toYear - study.year;
  const inflated = study.cost * Math.pow(1 + f.infl, years);  // step 1: inflate in original currency
  const kes = inflated * f.pppToKES;                          // step 2: PPP-convert to KES
  return {inflated, kes, factor:f, years};
}

function poolStrategy(studies, method='weighted') {
  const std = studies.map(s => ({...s, ...standardize(s)}));
  const costs = std.map(s => s.kes);
  const sumN = std.reduce((a,s)=>a+s.n, 0);
  let cost;
  if (method === 'mean') {
    cost = costs.reduce((a,b)=>a+b,0) / costs.length;
  } else if (method === 'weighted') {
    cost = std.reduce((a,s)=>a + s.kes*s.n, 0) / sumN;          // sample-size weighted
  } else { // ivw — inverse-variance proxy (precision ∝ n), down-weights small studies more
    const w = std.map(s => s.n*s.n);
    const sw = w.reduce((a,b)=>a+b,0);
    cost = std.reduce((a,s,i)=>a + s.kes*w[i], 0) / sw;
  }
  const effect = std.reduce((a,s)=>a + s.effect*s.n, 0) / sumN; // effect always sample-weighted
  const low = Math.min(...costs), high = Math.max(...costs);
  return {cost, effect, low, high, n:studies.length, std, method, sumN};
}

function formatCur(n, code) {
  if (n == null) return '—';
  if (code === 'KES') return 'KES ' + Math.round(n).toLocaleString();
  return code + ' ' + Math.round(n).toLocaleString();
}

Object.assign(window, {FACTORS, STUDIES, standardize, poolStrategy, formatCur, TARGET_YEAR});
