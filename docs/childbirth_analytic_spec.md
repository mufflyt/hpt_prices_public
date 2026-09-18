# Childbirth analytic specification (design lock)

Status: design approved 2026-09-14. The amendments made while implementing
it are listed at the end. This file makes no empirical claims, and there are
no CDC-derived numbers in the repository. Code: `R/ntsv_county.R` and
`analysis/18_ntsv_midwife_supply.R`.

**Causal chain under study:** midwifery supply → NTSV cesarean utilization
→ implied facility price consequences. Prices enter only in the last step,
as an accounting translation.

## 1. Primary exposure

**Midwife supply per 1,000 births.** The numerator is active AMCB-certified
midwives (CNM and CM) from the national AMCB-NPI linkage freeze
(`amcb_npi_linkage_FROZEN.csv`), placed at their NPPES practice
ZIP. The count covers midwives within 30 miles of the county's 2020 Census
center of population. The denominator is NVSS resident births of the counties
whose centers lie in the same catchment. The model uses
log2(midwives per 1,000 births + 0.5), as in `analysis/17`. Sensitivity radii are 15 and 60 miles. Secondary exposures:
a CABC-accredited birth center within 30 miles, and distance to the nearest
one.

**Roster coverage.** The linkage freeze covers all 50 states and DC (12,170
midwives, against 11,093 over 40 states in the roster it replaced), so no
catchment is set to missing any more. The 40-state roster left out AK, DC, DE,
HI, ND, NJ, RI, SD, VT, WV and WY, and a catchment reaching any ZIP (ZCTA) in
them had its exposure dropped rather than count those midwives as zero. That
check (`roster_uncovered_zctas()`) still runs and must now come back empty;
`national_roster_coverage(strict = TRUE)` stops the run if a future input
covers less ground. The freeze is verified against the sha256 in its tracked
manifest, so a stale copy under the same name cannot be read by mistake.

**Excluded as an exposure: the CNM-attended share of births.** Birth
certificates name the delivering attendant, and cesareans are attended by
physicians. A higher CNM share therefore lowers the cesarean rate by
construction. That share is a descriptor or mediator, not an exposure.

## 2. Outcome

**NTSV cesarean rate (the NCHS "low-risk cesarean" rate).**
- Numerator: cesarean deliveries.
- Denominator: live births that are first births (Live Birth Order 1),
  singleton, at 37 weeks or more by obstetric estimate, and cephalic.
- Births with an unknown delivery method are excluded from the denominator.

The total cesarean rate is reported only as a secondary outcome, because it
mixes in repeat cesareans and case mix.

## 3. Geography: county of mother's residence

**Hospital level is not possible.** No public national source gives
hospital-level NTSV. CMS PC-02 is "Not Available" for every hospital.

**Occurrence geography is not possible.** Neither public data source reports
where births occurred:
- The NCHS public-use microdata "does not include geographic detail (e.g.,
  state or county of birth)" (2023 User Guide).
- CDC WONDER (Natality 2016-2024 expanded) offers only the mother's legal
  residence. It covers the nation, regions, divisions, states, and counties
  of 100,000+ population; smaller counties are pooled by state and cells of
  1-9 are suppressed.
- Occurrence and hospital identifiers exist only in restricted NCHS files,
  which require an application.

**HRRs cannot be built.** WONDER has no ZIP field.

**Why residence is the right geography, not just the available one.**
- The question is population-level: do people who live near more midwives
  have fewer low-risk first-birth cesareans, wherever they deliver? A
  residence-based rate answers exactly that.
- Occurrence rates at tertiary centers are distorted by referral inflow,
  though NTSV restriction limits this.

**Coverage and sensitivity.**
- Primary unit: WONDER-identified counties.
- Complete-coverage sensitivity: state of residence (51 units).
- Pooled "Unidentified Counties" rows are kept for state totals but are
  never modeled as counties.
- Connecticut 2022-2024 falls entirely into pooled rows (the planning-region
  break) and is analyzed at state level only.

**Known limitation.** Excluding small counties removes most rural counties.
Rural findings rest on the state analysis.

## 4. Unit of analysis

**Utilization model.** County of residence, with NTSV births pooled over
the study years:
- Primary: a linear model of the NTSV cesarean rate in percentage points,
  weighted by NTSV births, with state fixed effects.
- Inference: the wild cluster restricted bootstrap by state, using the
  existing `wild_cluster_bootstrap()`.
- Sensitivity: a quasi-binomial logit with CR1 errors clustered by state.

**Price step.** Hospital, only to describe the local facility price
differential: the negotiated price for DRG 788 (cesarean) minus DRG 807
(vaginal), by payer. For each county it is the median over hospitals within
30 miles of the county's center of population that list both prices, or the
state median when none do. There is no hospital-level causal model.

## 5. Year alignment

| Input | Period |
|---|---|
| Prices (Trilliant snapshot 2026-07-21) | Rates posted 2025-2026 |
| Midwife roster | Current AMCB and NPPES |
| Natality | Pooled 2022-2024 |

- Pooling 2022-2024 stabilizes county rates and avoids 2020-2021.
- The WONDER release date is recorded at extraction.
- Stated assumption: midwife supply and prices are slow-moving, so a 1-3
  year lag is a cross-sectional approximation.
- A 2016-2019 outcome serves as a placebo or pre-period check. Current
  supply should not "predict" the past more strongly than the present.

## 6. Guarding against ecological overinterpretation

Findings may not be stated at the level of a person or a hospital:
- Not as "midwife care lowers a woman's cesarean risk".
- Not as "hospitals near midwives price lower".

**Supply may be endogenous.** Midwives cluster where hospitals employ them,
where midwife-led units exist, and in urban areas. Hospital-employed CNMs are
counted in local supply.

**Robustness checks:**
- MAUP: radii of 15, 30, and 60 miles, and county versus state units.
- The placebo outcome from section 5.
- A negative-control outcome: the multiple-birth (twin or more) share of
  all births. It tracks maternal age, fertility treatment, and income, but
  midwife supply cannot plausibly change it. An association would point to
  confounding.

**The price step is arithmetic, not an estimate.** Implied facility price
consequence = estimated change in NTSV cesarean rate x NTSV births x local
facility price differential. It is labeled the "implied facility price
differential". It is never called savings or value: it ignores professional
fees, downstream care, repeat cesareans, and outcomes.

## 7. Confounders

| Confounder | Source | Status |
|---|---|---|
| Parity | NTSV definition (first births only) | Handled by design |
| Maternal age | WONDER Age of Mother 9: shares 35+ and under 20 | Export |
| Race and Hispanic origin | WONDER Mother's Hispanic Origin x Single Race 6: Hispanic, NH Black, NH Asian shares | Export |
| Payer mix | WONDER Source of Payment for Delivery: Medicaid share | Export |
| Clinical risk | WONDER Pre-pregnancy BMI (obesity share); gestational hypertension and diabetes (extended model) | Export |
| Income, uninsurance | ACS median household income (log), % uninsured | Loaded |
| Rurality | RUCC 2023 | Loaded |
| State | Fixed effects | Available |
| Birth volume | NVSS resident births | Loaded |
| Regional obstetric supply | L&D hospitals within the radius per 1,000 births (CMS SM-7) | Built |
| Obstetrician supply | AHRF county OB/GYN counts within the radius per 1,000 births (log2) | Built |
| MFM supply | NPPES taxonomy 207VM0101X | Not built |
| System ownership, PE | AHRQ CHSP, `pe_hospital_systems` | Loaded (hospital level) |
| Hospital type | CMS roster | Loaded (hospital level) |
| Teaching status, bed size | HCRIS (IME resident FTEs, beds) | Not built |

The hospital attributes (system ownership, hospital type, teaching status,
bed size) enter only through a county's hospitals as descriptors.

**Missing entirely:**
- hospital-level NTSV and case mix;
- occurrence-based rates;
- small-county rates;
- level of maternal care.

## Natality source decision and the exact WONDER query

**No reproducible download exists.**
- The public-use microdata lacks geography.
- The WONDER API rejects sub-national natality ("Only national data are
  available for this dataset when using the WONDER web service").
- There is no existing repository extract. The midwifery repo holds only
  CNM-attended births by county.

So manual WONDER web exports are required. The analysis records each
export's sha256, dataset, and query date, and it checks that the Notes block
shows the NTSV filters. Every export uses these settings:

- **Dataset:** Natality, 2016-2024 expanded.
- **NTSV filters:**
  - Live Birth Order = "1st child born alive to mother";
  - Plurality = "Single";
  - OE Gestational Age Recode 11 = "37-38 weeks", "39 weeks", "40 weeks",
    "41 weeks", "42 or more weeks";
  - Fetal Presentation = "Cephalic".
- **Options:** show totals, show zero values, show suppressed values;
  export as tab-delimited.

Save each file under `reference/cdc_wonder/` with the name below
(`wonder_ntsv_exports()` holds the same list):

| File | Group by | Years | NTSV filters | Role |
|---|---|---|---|---|
| `ntsv_county_2022_2024.txt` | County of Residence; Delivery Method | 2022-2024 | yes | required |
| `ntsv_state_2022_2024.txt` | State of Residence; Delivery Method | 2022-2024 | yes | required |
| `ntsv_county_2016_2019.txt` | County of Residence; Delivery Method | 2016-2019 | yes | required (placebo) |
| `ntsv_county_age_2022_2024.txt` | County of Residence; Age of Mother 9 | 2022-2024 | yes | required |
| `ntsv_county_payment_2022_2024.txt` | County of Residence; Source of Payment for Delivery | 2022-2024 | yes | required |
| `ntsv_county_race_2022_2024.txt` | County of Residence; Mother's Hispanic Origin; Mother's Single Race 6 | 2022-2024 | yes | required |
| `ntsv_county_bmi_2022_2024.txt` | County of Residence; Mother's Pre-pregnancy BMI | 2022-2024 | yes | required |
| `ntsv_county_hypertension_2022_2024.txt` | County of Residence; Gestational Hypertension | 2022-2024 | yes | extended |
| `ntsv_county_diabetes_2022_2024.txt` | County of Residence; Gestational Diabetes | 2022-2024 | yes | extended |
| `births_county_plurality_2022_2024.txt` | County of Residence; Plurality | 2022-2024 | no | negative control |

**Suppression.** Cells of 1-9 births are never read as zero. Each rate or
share uses 5 for a suppressed cell and is set to missing when moving that
cell anywhere from 1 to 9 would shift it by more than 1 percentage point.

## Amendments made during implementation (2026-09-14)

1. **Negative control.** The multiple-birth share replaces the preterm
   share. Trials of midwife-led continuity of care report fewer preterm
   births, so preterm birth is not a clean negative control.
2. **Primary model.** A births-weighted linear model in percentage points
   replaces the binomial model. The wild cluster bootstrap is defined for
   linear models, and the percentage-point slope feeds the price
   arithmetic directly. The quasi-binomial logit stays as a sensitivity
   analysis.
3. **Covariates as composition.** Maternal age, race, payer, and BMI enter
   as shares of each county's NTSV births, not through direct
   standardization. Stratum-specific rates would run into suppression.
4. **Obstetrician supply.** This comes from AHRF county counts, which are
   already loaded, not from NPPES.
5. **Race and Hispanic origin.** WONDER has no combined race and Hispanic
   variable, so the export groups by both.
6. **Roster coverage, then withdrawn.** Catchments reaching the 11
   jurisdictions missing from the 40-state roster got a missing exposure, and
   the state model used only the covered states. Superseded on 2026-09-18: the
   national linkage freeze was on the machine all along, verified against its
   tracked manifest, so nothing is masked now. It is not a pure addition. The
   freeze also holds 294 more midwives in states the roster already covered,
   which moves the exposure for 730 hospitals that were never excluded.
7. **Payer split of the price arithmetic.** The commercial differential
   applies to privately insured NTSV births and the Medicaid differential to
   Medicaid births. The arithmetic covers only counties with a measured
   exposure.
