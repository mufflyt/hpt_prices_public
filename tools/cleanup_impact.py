#!/usr/bin/env python3
"""Before/after impact of a change to the cleaning rules on the headline hpt_prices outputs.

Build the "before" outputs from the older code into a separate data folder that symlinks the same
inputs (prices/, crosswalk/, reference/) and copies output/files_rates_above_gross.csv, so both
builds exclude the same files and every difference comes from the rules. Then run analysis/09,
11, 14, and 12 there (see docs/cleanup_impact.md), and:

  tools/cleanup_impact.py <after HPT_DATA_DIR> <before HPT_DATA_DIR>  > table.md

Needs the DuckDB CLI (for the cross-database counts). Writes a markdown table to stdout.
"""
import csv
import io
import subprocess
import sys

if len(sys.argv) != 3:
    sys.exit(__doc__)
A, B = sys.argv[1], sys.argv[2]
OPPS_45378 = 950.10  # CMS OPPS Addendum B, July 2026, APC 5311


def q(sql):
    out = subprocess.run(["duckdb", "-csv", "-c", "SET memory_limit='5GB'; " + sql], capture_output=True, text=True, check=True).stdout
    return list(csv.DictReader(io.StringIO(out)))


def one(sql):
    rows = q(sql)
    return rows[0] if rows else {}


def med(root, code, payer, fee="facility"):
    r = one(f"SELECT median_price, n_hospitals FROM '{root}/output/state_insurance_medians.parquet' "
            f"WHERE state='US' AND code='{code}' AND fee_type='{fee}' AND insurance_type='{payer}'")
    return float(r["median_price"]), int(r["n_hospitals"])


def spread(root, code, payer):
    r = one(f"SELECT quantile_cont(median_price, 0.9) / quantile_cont(median_price, 0.1) AS s, count(*) AS n "
            f"FROM '{root}/output/state_insurance_medians.parquet' WHERE state NOT IN ('US') AND code='{code}' "
            f"AND fee_type='facility' AND insurance_type='{payer}' AND n_hospitals >= 5 "
            f"AND state IN ('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY','DC')")
    return float(r["s"]), int(r["n"])


def ratio(root, code, payer):
    for r in csv.DictReader(open(f"{root}/output/payer_to_medicare_ratios.csv")):
        if r["code"] == code and r["fee_type"] == "professional" and r["payer_type"] == payer:
            return float(r["median_ratio"]), int(r["n_hospitals"])


def addon(root, variant, payer):
    for r in csv.DictReader(open(f"{root}/output/addon_value_by_state.csv")):
        if r["state"] == "US" and r["variant"] == variant and r["insurance_type"] == payer:
            return r


def addon_all(root, variant):
    return [r for r in csv.DictReader(open(f"{root}/output/addon_value_by_state.csv")) if r["state"] == "US" and r["variant"] == variant]


rows = []


def add(output, old, new, fmt, reason, rel=True):
    change = ""
    if isinstance(old, (int, float)) and isinstance(new, (int, float)):
        d = new - old
        change = fmt(d, signed=True)
        if rel and old:
            change += f" ({100 * d / old:+.1f}%)"
    rows.append((output, fmt(old) if not isinstance(old, str) else old, fmt(new) if not isinstance(new, str) else new, change or "", reason))


def dollars(x, signed=False):
    sign = ("+" if x >= 0 else "-") if signed else ("-" if x < 0 else "")
    return f"{sign}${abs(x):,.0f}"


def num(digits):
    def f(x, signed=False):
        return f"{x:+.{digits}f}" if signed else f"{x:.{digits}f}"
    return f


# prices
for code, payer, label, reason in [
    ("45378", "commercial", "45378 commercial facility median (national)", "blank-class professional fees removed from facility rows"),
    ("45378", "medicaid", "45378 Medicaid facility median", "same"),
    ("45378", "medicare_advantage", "45378 Medicare Advantage facility median", "same; MA is paid at Medicare rates"),
    ("58300", "commercial", "58300 IUD insertion commercial facility median", "OR case lines and office-procedure case rates removed"),
]:
    (o, on), (n, nn) = med(B, code, payer), med(A, code, payer)
    add(label, o, n, dollars, f"{reason} (hospitals {on:,} to {nn:,})")
(o, _), (n, _) = med(B, "45378", "medicare"), med(A, "45378", "medicare")
add("45378 Medicare facility median / OPPS rate ($950.10)", o / OPPS_45378, n / OPPS_45378, num(3), "professional fees had pulled the facility median below OPPS", rel=False)

# multiplier
(o, on), (n, nn) = ratio(B, "58120", "commercial"), ratio(A, "58120", "commercial")
add("Commercial D&C (58120) payer-to-Medicare multiplier", o, n, num(3), f"OR case lines removed (hospitals {on} to {nn})")

# add-on model
ob, nb = addon(B, "diagnostic_45378", "commercial"), addon(A, "diagnostic_45378", "commercial")
add("EMB at colonoscopy, commercial net value per add-on", float(ob["net_value"]), float(nb["net_value"]), dollars, "higher colonoscopy price raises the displaced-case cost; lower EMB rate lowers add-on revenue")
add("EMB at colonoscopy, commercial break-even added minutes", float(ob["dT_star"]), float(nb["dT_star"]), num(1), "same", rel=False)
oa = addon_all(B, "drg621_mirena"); na = addon_all(A, "drg621_mirena")
opos = sum(float(r["net_value"]) > 0 for r in oa); npos = sum(float(r["net_value"]) > 0 for r in na)
oc = addon(B, "drg621_mirena", "commercial"); nc = addon(A, "drg621_mirena", "commercial")
add("IUD at bariatric surgery (MS-DRG 621), commercial net value", float(oc["net_value"]), float(nc["net_value"]), dollars, "IUD insertion rate cleaned; conclusion unchanged")
rows.append(("IUD at bariatric surgery: payers with positive net value", f"{opos} of {len(oa)}", f"{npos} of {len(na)}", "", "negative for every payer before and after"))

# geography
for payer, label in [("commercial", "Commercial"), ("medicaid", "Medicaid"), ("medicare_advantage", "Medicare Advantage")]:
    (o, on), (n, nn) = spread(B, "45378", payer), spread(A, "45378", payer)
    add(f"{label} 45378 state variation (p90/p10 of state medians, states with 5+ hospitals)", o, n, num(2), f"states {on} to {nn}", rel=False)

# counts from the current database (and the before database for payer types)
c = one(f"""ATTACH '{A}/hpt.duckdb' AS a (READ_ONLY);
SELECT
  count(*) FILTER (WHERE r.case_line) AS case_rows,
  count(DISTINCT r.file_id) FILTER (WHERE r.case_line) AS case_files,
  count(*) FILTER (WHERE r.fee_type_inferred AND r.fee_type = 'professional') AS inferred_rows,
  count(DISTINCT r.file_id) FILTER (WHERE r.fee_type_inferred AND r.fee_type = 'professional') AS inferred_files,
  count(*) FILTER (WHERE CAST(c.concept AS VARCHAR) IN ('emb', 'iud_insertion') AND r.methodology IN ('case rate', 'per diem')) AS pkg_rows,
  count(DISTINCT r.file_id) FILTER (WHERE CAST(c.concept AS VARCHAR) IN ('emb', 'iud_insertion') AND r.methodology IN ('case rate', 'per diem')) AS pkg_files,
  count(*) AS total_rows, count(DISTINCT r.file_id) AS total_files
FROM a.fact_rate r JOIN a.dim_code c USING (code_id)""")
p = one(f"""ATTACH '{A}/hpt.duckdb' AS a (READ_ONLY); ATTACH '{B}/hpt.duckdb' AS b (READ_ONLY);
WITH chg AS (SELECT pa.payer_id, CAST(pb.payer_type AS VARCHAR) AS old_t, CAST(pa.payer_type AS VARCHAR) AS new_t
             FROM a.dim_payer pa JOIN b.dim_payer pb USING (payer_key)
             WHERE CAST(pa.payer_type AS VARCHAR) IS DISTINCT FROM CAST(pb.payer_type AS VARCHAR))
SELECT (SELECT count(*) FROM chg) AS plans,
       (SELECT count(*) FROM a.fact_rate r JOIN chg USING (payer_id)) AS rate_rows,
       (SELECT string_agg(DISTINCT old_t || ' to ' || new_t, '; ') FROM chg) AS moves""")
s = one(f"""ATTACH '{A}/hpt.duckdb' AS a (READ_ONLY); ATTACH '{B}/hpt.duckdb' AS b (READ_ONLY);
SELECT count(*) AS files FROM a.dim_file fa JOIN b.dim_file fb USING (mrf_file_id)
WHERE fa.file_state IS DISTINCT FROM fb.file_state""")
ex = sum(1 for _ in open(f"{A}/output/median_excluded_file_ids.csv")) - 1
tr, tf = int(c["total_rows"]), int(c["total_files"])
rows.append(("Rates flagged as operating-room case lines", "0", f"{int(c['case_rows']):,} rows in {int(c['case_files']):,} files", "", f"of {tr:,} rates in {tf:,} files; outpatient procedures only"))
rows.append(("Rates reclassified professional (blank billing class, professional-level gross)", "0", f"{int(c['inferred_rows']):,} rows in {int(c['inferred_files']):,} files", "", "left out of both facility and professional fees"))
rows.append(("EMB/IUD case-rate and per-diem rates excluded", "0", f"{int(c['pkg_rows']):,} rows in {int(c['pkg_files']):,} files", "", "package prices for a surgical case"))
rows.append(("Payer plans with a changed payer type", "", f"{int(p['plans']):,} plans ({int(p['rate_rows']):,} rates)", "", p.get("moves") or ""))
rows.append(("Files with a corrected state", "", f"{int(s['files']):,}", "", "street tokens (PO, NW, SE) replaced by the USPS code from the address, or dropped"))
rows.append(("Files excluded from medians (rates above gross, cross-source duplicates)", f"{ex}", f"{ex}", "", "same list in both builds, so it does not drive the differences"))

print("| Output | Before | After | Change | Reason |")
print("|---|---|---|---|---|")
for r in rows:
    print("| " + " | ".join(str(x) for x in r) + " |")
