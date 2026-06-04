// ── CEA charts + analysis helpers (clinical academic palette) ──────
// Exports to window: calcICER, runPSA, fmt, fmtN, OUTCOME_LABELS, SHA_LEVELS,
// CEPlanePlot, TornadoPlot, CEACPlot, PSAScatterPlot, PriceThresholdPlot
const { useEffect, useRef } = React;

const fmt = n => n == null ? '—' : 'KES ' + Math.round(n).toLocaleString();
const fmtN = n => n == null ? '—' : n.toLocaleString(undefined, {maximumFractionDigits:1});

// Clinical academic palette — restrained, ink-forward
const PALETTE = ['#0f766e','#9a6a3a','#1e3a5f','#7c5295','#b45309','#0a0a0a'];
const THRESH_COLOR = '#dc2626';
const GRID = '#ededed';
const INK = '#0a0a0a';
const MUTED = '#9a9a9a';
const AXIS_FONT = {family:'Archivo', size:11, weight:'500'};

const SHA_LEVELS = {2240:'Level 3 (2,240)', 3360:'Level 4 (3,360)', 3920:'Level 5 (3,920)', 4480:'Level 6 (4,480)'};
const OUTCOME_LABELS = {qaly:'QALY',daly:'DALY',lyg:'life year gained',lives:'life saved',hosp_days:'day of hospitalisation averted'};

function calcICER(strategies) {
  const sorted = [...strategies].sort((a,b) => a.cost - b.cost);
  const ref = sorted[0];
  return sorted.map((s,i) => {
    if (i === 0) return {...s, inc_cost: null, inc_effect: null, icer: null, status:'REF'};
    const incCost = s.cost - ref.cost;
    const incEffect = s.effect - ref.effect;
    if (incEffect <= 0) return {...s, inc_cost: incCost, inc_effect: incEffect, icer: null, status:'DOM'};
    return {...s, inc_cost: incCost, inc_effect: incEffect, icer: incCost/incEffect, status:'ND'};
  });
}

function runPSA(strategies, iterations, cv) {
  const results = [];
  for (let i = 0; i < iterations; i++) {
    const sampled = strategies.map(s => ({
      ...s,
      cost: Math.max(0, s.cost * (1 + (Math.random()*2-1)*cv/100)),
      effect: Math.max(0, s.effect * (1 + (Math.random()*2-1)*cv/100)),
    }));
    results.push(calcICER(sampled));
  }
  return results;
}

const baseChartOpts = (xTitle, yTitle, xTickFmt, yTickFmt) => ({
  responsive: true, maintainAspectRatio: false,
  plugins: { legend: {labels: {font: AXIS_FONT, color: INK, boxWidth: 12, boxHeight: 12}} },
  scales: {
    x: {title:{display:true, text:xTitle, font:AXIS_FONT, color:MUTED}, grid:{color:GRID}, ticks:{font:AXIS_FONT, color:MUTED, callback: xTickFmt}, border:{color:'#d4d4d4'}},
    y: {title:{display:true, text:yTitle, font:AXIS_FONT, color:MUTED}, grid:{color:GRID}, ticks:{font:AXIS_FONT, color:MUTED, callback: yTickFmt}, border:{color:'#d4d4d4'}},
  }
});
const kFmt = v => 'KES ' + (v/1000).toFixed(0) + 'k';

function CEPlanePlot({icer_rows, threshold, outcomeLabel}) {
  const ref = useRef(null), chart = useRef(null);
  useEffect(() => {
    if (!ref.current) return;
    chart.current?.destroy();
    const nonRef = icer_rows.filter(r => r.status !== 'REF');
    const datasets = nonRef.map((r,i) => ({
      label: r.strategy, data: [{x:r.inc_effect||0, y:r.inc_cost||0}],
      backgroundColor: PALETTE[i%PALETTE.length], pointRadius: 9, pointHoverRadius: 11, pointStyle:'rectRot',
    }));
    const maxEffect = Math.max(...nonRef.map(r=>Math.abs(r.inc_effect||0)))*1.5 || 10;
    datasets.push({label:`CE Threshold`, data:[{x:0,y:0},{x:maxEffect,y:threshold*maxEffect}], type:'line',
      borderColor:THRESH_COLOR, borderWidth:1.5, borderDash:[5,4], pointRadius:0, fill:false});
    const opts = baseChartOpts(`Incremental effect (${outcomeLabel}s)`, 'Incremental cost (KES)', null, kFmt);
    opts.plugins.tooltip = {callbacks:{label: ctx => ctx.dataset.label.startsWith('CE Threshold') ? null
      : `${ctx.dataset.label}: Δ${fmtN(ctx.parsed.x)} / ${fmt(ctx.parsed.y)}`}};
    chart.current = new Chart(ref.current, {type:'scatter', data:{datasets}, options:opts});
    return () => chart.current?.destroy();
  }, [icer_rows, threshold]);
  return <div className="chart-canvas-wrap"><canvas ref={ref}/></div>;
}

function TornadoPlot({strategies, threshold}) {
  const ref = useRef(null), chart = useRef(null);
  useEffect(() => {
    if (!ref.current || strategies.length < 2) return;
    chart.current?.destroy();
    const refS = [...strategies].sort((a,b)=>a.cost-b.cost)[0];
    const target = strategies.filter(s=>s.strategy!==refS.strategy)[0];
    if (!target) return;
    const base = (target.cost-refS.cost)/(target.effect-refS.effect);
    const sets = [
      {label:`${target.strategy} cost ±20%`, low:(target.cost*0.8-refS.cost)/(target.effect-refS.effect), high:(target.cost*1.2-refS.cost)/(target.effect-refS.effect)},
      {label:`${target.strategy} effect ±20%`, low:(target.cost-refS.cost)/(target.effect*1.2-refS.effect), high:(target.cost-refS.cost)/(target.effect*0.8-refS.effect)},
      {label:`${refS.strategy} cost ±20%`, low:(target.cost-refS.cost*1.2)/(target.effect-refS.effect), high:(target.cost-refS.cost*0.8)/(target.effect-refS.effect)},
      {label:`${refS.strategy} effect ±20%`, low:(target.cost-refS.cost)/(target.effect-refS.effect*0.8), high:(target.cost-refS.cost)/(target.effect-refS.effect*1.2)},
    ].sort((a,b)=>Math.abs(b.high-b.low)-Math.abs(a.high-a.low));
    const opts = baseChartOpts('ΔICER from base (KES)', '', kFmt, null);
    opts.indexAxis='y'; opts.scales.y.grid={display:false};
    opts.plugins.tooltip = {callbacks:{label: ctx => `ΔICER: KES ${Math.round(ctx.parsed.x).toLocaleString()}`}};
    chart.current = new Chart(ref.current, {type:'bar', data:{
      labels: sets.map(p=>p.label),
      datasets:[
        {label:'−20%', data:sets.map(p=>Math.abs(Math.min(p.low,p.high)-base)), backgroundColor:PALETTE[0], barThickness:18},
        {label:'+20%', data:sets.map(p=>Math.abs(Math.max(p.low,p.high)-base)), backgroundColor:PALETTE[1], barThickness:18},
      ]
    }, options:opts});
    return () => chart.current?.destroy();
  }, [strategies, threshold]);
  return <div className="chart-canvas-wrap"><canvas ref={ref}/></div>;
}

function CEACPlot({strategies, psaData}) {
  const ref = useRef(null), chart = useRef(null);
  useEffect(() => {
    if (!ref.current || !psaData?.length) return;
    chart.current?.destroy();
    const wtp = Array.from({length:40},(_,i)=>i*500);
    const datasets = strategies.map((s,si) => {
      if (si===0) return null;
      const data = wtp.map(w => {
        const c = psaData.filter(it => {
          const row = it.find(r=>r.strategy===s.strategy);
          return row && row.status!=='REF' && row.status!=='DOM' && row.icer!=null && row.icer<=w;
        }).length;
        return {x:w, y:c/psaData.length};
      });
      return {label:s.strategy, data, borderColor:PALETTE[(si-1)%PALETTE.length], borderWidth:2, pointRadius:0, tension:0.3, backgroundColor:'transparent'};
    }).filter(Boolean);
    const opts = baseChartOpts('Willingness-to-pay threshold (KES)', 'Probability cost-effective', kFmt, v=>(v*100)+'%');
    opts.scales.x.type='linear'; opts.scales.y.min=0; opts.scales.y.max=1;
    chart.current = new Chart(ref.current, {type:'line', data:{datasets}, options:opts});
    return () => chart.current?.destroy();
  }, [strategies, psaData]);
  return <div className="chart-canvas-wrap"><canvas ref={ref}/></div>;
}

function PSAScatterPlot({strategies, psaData, threshold}) {
  const ref = useRef(null), chart = useRef(null);
  useEffect(() => {
    if (!ref.current || !psaData?.length) return;
    chart.current?.destroy();
    const sorted=[...strategies].sort((a,b)=>a.cost-b.cost);
    const nonRef = strategies.filter(s=>s.strategy!==sorted[0].strategy);
    const datasets = nonRef.map((s,si) => ({
      label:s.strategy,
      data: psaData.map(it=>{const row=it.find(r=>r.strategy===s.strategy); return row?{x:row.inc_effect||0,y:row.inc_cost||0}:null;}).filter(Boolean).slice(0,200),
      backgroundColor: PALETTE[si%PALETTE.length]+'44', borderColor: PALETTE[si%PALETTE.length], borderWidth:0.5, pointRadius:2.5,
    }));
    const maxEff = Math.max(...psaData.flatMap(it=>nonRef.map(s=>{const row=it.find(r=>r.strategy===s.strategy); return Math.abs(row?.inc_effect||0);})))*1.3||10;
    datasets.push({label:'CE Threshold', data:[{x:0,y:0},{x:maxEff,y:threshold*maxEff}], type:'line', borderColor:THRESH_COLOR, borderWidth:1.5, borderDash:[5,4], pointRadius:0, fill:false, backgroundColor:'transparent'});
    const opts = baseChartOpts('Incremental effect', 'Incremental cost (KES)', null, kFmt);
    chart.current = new Chart(ref.current, {type:'scatter', data:{datasets}, options:opts});
    return () => chart.current?.destroy();
  }, [strategies, psaData, threshold]);
  return <div className="chart-canvas-wrap"><canvas ref={ref}/></div>;
}

function PriceThresholdPlot({strategies, threshold, outcomeLabel}) {
  const ref = useRef(null), chart = useRef(null);
  useEffect(() => {
    if (!ref.current || strategies.length<2) return;
    chart.current?.destroy();
    const sorted=[...strategies].sort((a,b)=>a.cost-b.cost);
    const refS=sorted[0], target=sorted[1];
    const ie = target.effect-refS.effect;
    const prices = Array.from({length:60},(_,i)=>target.cost*0.2 + target.cost*2.8*i/59);
    const datasets = [
      {label:`ICER for ${target.strategy}`, data:prices.map(p=>({x:p, y: ie<=0?null:(p-refS.cost)/ie})), borderColor:PALETTE[0], borderWidth:2, pointRadius:0, tension:0.2, fill:false, backgroundColor:'transparent'},
      {label:`CE Threshold`, data:prices.map(p=>({x:p,y:threshold})), borderColor:THRESH_COLOR, borderWidth:1.5, borderDash:[5,4], pointRadius:0, fill:false, backgroundColor:'transparent'},
    ];
    const opts = baseChartOpts(`Price of ${target.strategy} (KES)`, `ICER (KES per ${outcomeLabel})`, kFmt, kFmt);
    opts.scales.x.type='linear';
    chart.current = new Chart(ref.current, {type:'line', data:{datasets}, options:opts});
    return () => chart.current?.destroy();
  }, [strategies, threshold]);
  return <div className="chart-canvas-wrap"><canvas ref={ref}/></div>;
}

Object.assign(window, {
  calcICER, runPSA, fmt, fmtN, OUTCOME_LABELS, SHA_LEVELS,
  CEPlanePlot, TornadoPlot, CEACPlot, PSAScatterPlot, PriceThresholdPlot,
});
