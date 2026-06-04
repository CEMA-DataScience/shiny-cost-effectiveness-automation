# Handoff: Cost-Effectiveness Analysis Tool — Sensitivity Analysis, Drawer Layout & Evidence Synthesis

## Overview
This package documents a redesign and feature expansion of an existing **R Shiny** cost-effectiveness analysis (CEA) tool used in a Kenyan health-policy context (costs in KES). It covers three things:

1. A new **visual direction** ("Clinical Academic") replacing the default Bootstrap/Flatly theme.
2. A **Drawer-based results layout** plus a full **sensitivity-analysis suite**: cost-effectiveness plane (scatter), tornado diagram, PSA scatter, cost-effectiveness acceptability curve (CEAC), and price-threshold analysis.
3. An **Evidence Synthesis** module that standardises costs from published studies to a common currency/year (KES, 2027) using **PPP conversion + inflation adjustment**, pools them per strategy, and carries **provenance** through to the analysis.

## About the Design Files
The files in this bundle are **design references created in HTML/React** — interactive prototypes showing intended look and behaviour. **They are not production code to copy directly.**

**The target environment is the existing R Shiny application** (see `app.R`, `R/cea_functions.R`, `R/mod_input.R` in the current repo, which already use `shiny`, `bslib`, `DT`, `dplyr`, and `dampack`). The task is to **recreate these HTML designs as Shiny UI + server logic**, reusing the established module pattern (`mod_*_ui` / `mod_*_server`) and computing real numbers with `dampack`. Charts should be implemented with **`ggplot2`** (optionally `plotly` for interactivity) rendered via `renderPlot`/`renderPlotly` — the prototype uses Chart.js purely to demonstrate intent.

The JavaScript analysis math in the prototypes (`cea-charts.jsx`, `synthesis-data.jsx`) is a faithful spec of the intended calculations; port it to R, do not embed JS.

## Fidelity
**High-fidelity.** Final colours, typography, spacing, and interactions are specified below and should be matched closely. Where the prototype hand-rolls something Shiny gives for free (tabs, tables, modals), prefer the idiomatic Shiny/`bslib` equivalent styled to match — fidelity is about the *visual result*, not the DOM structure.

---

## Screens / Views

### 1. Global chrome (navbar + hero)
- **Navbar**: 52px tall, white background, **2px solid black bottom border** (`#0a0a0a`). Left: brand "Cost-Effectiveness Analysis" (Archivo 700, 15px). Then nav links: **Evidence Synthesis**, **Analysis**, **Help**. Active link is black text, 600 weight, with a **2px teal (`#0f766e`) bottom border**; inactive links are `#737373`. The Analysis link shows a teal pill badge with the strategy count when strategies exist.
- **Hero**: left-padded block with a **3px teal left border**, 22px/24px padding. Title in Archivo 700, 24px, letter-spacing −0.02em. Subtitle `#737373`, 13px. Title/subtitle change per tab.

### 2. Evidence Synthesis tab
- **Purpose**: Standardise published study costs to KES 2027 and pool into per-strategy estimates.
- **Layout**: Centered shell (max-width 1480px), 24px horizontal padding.
  - **Target bar**: full-width black (`#0a0a0a`) bar, white text, radius 4px, 11px/16px padding. Shows three labelled values separated by thin dividers: "Conversion target: KES · 2027 · PPP-adjusted" (mono) · "Inflation: GDP deflator → projected to 2027" · "Currency: World Bank PPP factors". Right-aligned clay tag "factors pulled at build time · illustrative".
  - **Study table card**: header row "Study cost database — standardisation & pooling" with a right-aligned **pooling-method segmented control** (Simple mean / Sample-weighted / Inverse-variance).
  - **Table**, grouped by strategy. Columns: Study (author + journal subtext) · Country · Yr · Reported cost (right-aligned, original currency) · **Inflated → 2027** (right, original currency, on `#fcfcfc` "step" background) · **KES (PPP, 2027)** (right, bold, step background) · Effect (right) · n (right).
    - Each strategy is preceded by a **group-header row** (`#fafafa`, uppercase 700 11px).
    - Each strategy ends with a **Pooled row**: top border 1.5px teal, background `#f7faf9`. Shows "Pooled · N studies", the across-study range (low–), the pooled KES (bold), pooled effect, and Σn.
  - **Footer**: a methodology note (left, max 560px, `#a3a3a3` 11px) and a teal primary button **"Send N strategies to Analysis →"**.

### 3. Analysis tab
- **Purpose**: Run ICER + sensitivity analysis on synthesised or manual strategies.
- **Empty state** (no strategies): centered card with book icon, "No strategies yet", and two buttons — "Go to Evidence Synthesis →" (teal) and "Add manual strategies".
- **Populated layout**: two-column grid `1.5fr / 1fr`, 18px gap, items aligned to top.
  - **Left — Strategies card**: table with columns Strategy · Cost (KES) · Effect · **Source** · Ref · ICER. Strategy/Cost/Effect cells are **double-click editable** (inline input with teal border). Source column: literature-derived rows show a clay **"📖 N studies"** button (opens provenance drawer); manual rows show a grey "Manual" tag. Ref column shows a grey "Ref" tag on row 0. ICER column shows formatted ICER, "—" for reference, or amber "Dom" tag for dominated. Card footer has "+ Add manual" button.
  - **Right — Analysis Parameters card**: Health Outcome radio group (Days of Hospitalisation Averted / QALYs / DALYs / Life Years Gained / Lives Saved); Threshold radio group (0.5× GDP per capita 154,000 / SHA Hospital Rates) with a conditional SHA-level `<select>` (Levels 3–6: 2,240 / 3,360 / 3,920 / 4,480); a PSA checkbox with a note that PSA uses the across-study range as the uncertainty band; and a full-width black **"Run Analysis"** button.

### 4. Results drawer (opens on Run)
- **Trigger**: clicking Run Analysis. Slides in from the right over a 40%-black backdrop.
- **Drawer**: fixed right, width `min(960px, 80vw)`, white, shadow `-8px 0 30px rgba(0,0,0,0.15)`, slide-in animation 0.25s ease. Header: 2px black bottom border, title "Analysis Results" + subtitle (strategy count · outcome · threshold), close (×) icon button. Body scrolls.
- **Result tabs** (1.5px black bottom border, active tab teal): **ICER · CE Plane · Tornado · PSA Scatter · CEAC · Price Threshold**. PSA Scatter & CEAC only appear when PSA is enabled.
  - **ICER**: incremental results table (Strategy, Cost, Effect, Inc. Cost, Inc. Effect, ICER, Status) + an interpretation box (green left border if a cost-effective option exists, amber otherwise) listing per-strategy verdicts and a bold recommendation line.
  - **CE Plane**: scatter of incremental effect (x) vs incremental cost (y) per non-reference strategy, with a dashed red threshold line from origin (slope = threshold).
  - **Tornado**: horizontal bar chart of ΔICER from base for ±20% variation of each of the four parameters (target cost/effect, reference cost/effect), sorted by influence.
  - **PSA Scatter**: cloud of simulated incremental cost/effect pairs across iterations + a dashed red threshold line; chips show % cost-effective vs not.
  - **CEAC**: probability cost-effective (y, 0–100%) vs willingness-to-pay threshold (x, KES) line per strategy.
  - **Price Threshold**: ICER (y) vs price of the target intervention (x), with a dashed red threshold line, plus four stat tiles — Current Price, Break-even Price (green if above current, red if below), Current ICER, and Headroom/Reduction.

### 5. Provenance drawer (opens from Source column)
- **Trigger**: clicking a "N studies" button in the Analysis strategies table. Narrower drawer (`min(720px, 72vw)`).
- **Contents**: header with strategy name + "Evidence provenance · N studies · pooled by <method>". Three summary stat tiles (Pooled cost KES 2027, Across-study range → PSA, Pooled effect). A "Conversion detail" table: Study · Country · Reported · Infl. rate (%/yr) · PPP factor · KES 2027. Footnote describing the pipeline (inflate in original currency → PPP-convert) and that factors are World-Bank-API-sourced at build time with 2027 inflation projected and 1 QALY = 1 DALY harmonisation applied internally.

---

## Interactions & Behavior
- **Pooling-method switch**: recomputes every pooled row live (and, after sending, the strategy estimates).
- **Send to Analysis**: maps each strategy's pooled cost/effect into the strategies table, records provenance, switches to the Analysis tab, shows a toast.
- **Inline cell edit**: double-click a Strategy/Cost/Effect cell → input; Enter or blur commits; numeric fields parse to number.
- **Run Analysis**: 650ms simulated compute (replace with real `dampack` call), then opens results drawer. PSA (when enabled) runs N iterations sampling cost/effect ±20% (replace with `dampack::gen_psa_samp`/`run_psa`).
- **Drawer dismissal**: click backdrop or × icon.
- **Toast**: bottom-center black pill, auto-dismiss ~3s.

## State Management
- `tab`: 'synthesis' | 'analysis' | 'help'.
- `method`: pooling method 'mean' | 'weighted' | 'ivw'.
- `strategies`: array of `{strategy, cost, effect}`.
- `prov`: map of strategy name → pooled object `{cost, effect, low, high, n, std, method, sumN}`.
- `outcomeType`, `thresholdType`, `shaLevel`, `enablePSA`: analysis parameters.
- `res`, `psaData`: last analysis result + PSA iterations.
- `resultsOpen`, `provDrawer`: drawer visibility/contents.
- In Shiny: use `reactiveValues` for strategies/prov/results; the two drawers map naturally to `modalDialog` (or a `shinyjs`/`bslib` offcanvas styled to match).

## Calculations to port to R (from the prototypes)
**ICER** (`calcICER` in `cea-charts.jsx`): sort strategies by cost; first = reference; for each other, inc_cost/inc_effect vs reference; if inc_effect ≤ 0 → Dominated; else ICER = inc_cost/inc_effect. *Use `dampack::calculate_icers` in production — it also handles extended dominance, which the prototype simplifies.*

**PSA** (`runPSA`): N iterations, each sampling cost & effect ±20% (uniform in prototype; use normal/appropriate distributions via `dampack::gen_psa_samp`). Recompute ICERs per iteration.

**Standardisation** (`synthesis-data.jsx → standardize`): `inflated = cost × (1 + infl)^(targetYear − studyYear)` in original currency, then `kes = inflated × pppToKES`. Order matters: **inflate first (original currency), then PPP-convert**. Source `infl` from GDP-deflator series and `pppToKES` from World Bank PPP factors (`wbstats`/`httr2`); project 2027 inflation.

**Pooling** (`poolStrategy`): `mean` = simple mean of KES costs; `weighted` = Σ(kes×n)/Σn; `ivw` = inverse-variance proxy (weight ∝ n²). Effect always sample-weighted. Range = min/max KES across studies → feeds PSA bounds. **Note:** pool **costs and effects separately, then compute the ICER** — do not average ICERs directly (ratio statistic; negatives/dominated points distort the mean). Consider adding a **forest plot** of study-level ICERs for transparency.

## Design Tokens
**Colours**
- Ink `#0a0a0a`; ink-soft `#404040`; muted `#737373`; faint `#a3a3a3`
- Background/surface `#ffffff`; panel `#fafafa`; step-bg `#fcfcfc`/`#f7faf9`
- Border `#e5e5e5`
- Teal (primary accent) `#0f766e`; teal-soft `#ecfdf5`
- Clay (literature/provenance) `#9a6a3a`; clay-soft `#fbf6f0`
- Green `#047857` / soft `#ecfdf5`; Red `#b91c1c` / soft `#fef2f2`; Amber `#b45309` / soft `#fffbeb`
- Threshold line on charts: `#dc2626`
- Chart series palette: `#0f766e`, `#9a6a3a`, `#1e3a5f`, `#7c5295`, `#b45309`, `#0a0a0a`

**Typography**
- UI/headings: **Archivo** (400/500/600/700/800)
- Numbers, tables, code: **IBM Plex Mono** (400/500/600)
- Base 14px / line-height 1.5. Table headers 10px uppercase, letter-spacing 0.07em, `#737373`, 1.5px black bottom border. Section labels 11px uppercase 700.

**Spacing / shape**
- Border radius: 4px (cards, buttons, tags 2px)
- Card padding 16px; card header 11px/15px
- Drawer widths: results `min(960px,80vw)`, provenance `min(720px,72vw)`
- Shadows: drawer `-8px 0 30px rgba(0,0,0,0.15)`; toast `0 6px 20px rgba(0,0,0,0.2)`

## Alternate layouts considered
The team evaluated four results layouts (Split-pane, Tab-switch, Collapse-bar, Drawer) in `CEA Tool (Clinical).html` and **selected Drawer**. The other three are preserved in that file behind a layout switch for reference only — implement **Drawer**.

## Assets
No external image assets. All icons are inline SVG (calculator, table, scatter, tornado, CEAC curve, price curve, book, check, play, ×). Fonts load from Google Fonts (Archivo, IBM Plex Mono). Replace inline SVGs with the codebase's icon set (e.g. `fontawesome`/`bsicons` already available via `shiny::icon`) where practical.

## Files
- `CEA Synthesis.html` — **primary reference**: Evidence Synthesis tab + Analysis tab + both drawers (the consolidated target).
- `CEA Tool (Clinical).html` — clinical theme + the four layout options (Drawer was chosen).
- `cea-charts.jsx` — analysis math (`calcICER`, `runPSA`) + all five Chart.js plot components; the calculation spec to port to R/`dampack`/`ggplot2`.
- `synthesis-data.jsx` — conversion factors, study schema, `standardize`/`poolStrategy`/`formatCur`; the synthesis spec to port to R.
- Existing repo (target): `app.R`, `R/cea_functions.R`, `R/mod_input.R`.
