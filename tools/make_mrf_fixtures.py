#!/usr/bin/env python3
"""Build the MRF parser fixtures (tests/testthat/fixtures/mrf/*) from the CMS v3.0.0 templates.

The header rows (hospital metadata, attestation, and the charge-column header) are read verbatim
from CMS's own v3.0.0 tall and wide CSV templates, so the fixtures cannot drift from the real
column names. Everything else is synthetic: hospitals, payers, codes, and dollar amounts are
invented to exercise the parsers (code-type gating, multi-code lines, wide unpivot, v2.2 headers,
BOM/CRLF/spaced pipes/stray quotes, invalid UTF-8, UTF-16LE, and JSON v2/v3 layouts).

Usage (from the repository root):
  tools/make_mrf_fixtures.py [--templates DIR] [--out DIR]

--templates  directory holding V3.0.0_Tall_CSV_Format_Template.csv and
             V3.0.0_Wide_CSV_Format_Template.csv. Default: download them from
             github.com/CMSgov/hospital-price-transparency at the pinned commit below.
--out        output directory. Default: a new temporary directory (printed at the end).
             Point it at tests/testthat/fixtures/mrf only to regenerate on purpose; diff first.

Output from the pinned templates is byte-identical to the committed fixtures
(checked 2026-09-13).
"""
import argparse, csv, io, json, os, subprocess, tempfile

CMS_COMMIT = "33833d4c8970c649362449c04e51d620b0e2d593"  # last change to the v3.0.0 CSV templates (2025-12-02)
CMS_RAW = "https://raw.githubusercontent.com/CMSgov/hospital-price-transparency/" + CMS_COMMIT + "/documentation/CSV/templates/"
TEMPLATES = {"tall": "V3.0.0_Tall_CSV_Format_Template.csv", "wide": "V3.0.0_Wide_CSV_Format_Template.csv"}

parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("--templates", default=None)
parser.add_argument("--out", default=None)
args = parser.parse_args()

if args.templates is None:
    args.templates = tempfile.mkdtemp(prefix="cms_templates_")
    for name in TEMPLATES.values():
        # curl, not urllib: it uses the system certificate store, which some Python builds lack
        subprocess.run(["curl", "-fsSL", "-o", os.path.join(args.templates, name), CMS_RAW + name], check=True)
OUT = os.path.join(args.out or tempfile.mkdtemp(prefix="mrf_fixtures_"), "")
os.makedirs(OUT, exist_ok=True)

def template_rows(name):
    with open(os.path.join(args.templates, name), newline="") as f:
        return list(csv.reader(f))

tall = template_rows(TEMPLATES["tall"])
wide = template_rows(TEMPLATES["wide"])

def expand_codes(header, n=2):
    out = []
    for h in header:
        if h == "code|[i]":
            out += [f"code|{i}" for i in range(1, n + 1) for _ in [0]] and [x for i in range(1, n + 1) for x in (f"code|{i}", f"code|{i}|type")]
        elif h == "code|[i]|type":
            continue
        else:
            out.append(h)
    return out

def payer_block(header, payers):
    out = []
    block = [h for h in header if "[payer_name]" in h]
    for h in header:
        if "[payer_name]" in h:
            if h == block[0]:
                for payer, plan in payers:
                    out += [b.replace("[payer_name]", payer).replace("[plan_name]", plan) for b in block]
        else:
            out.append(h)
    return out

def write_csv(path, rows, bom=False, crlf=False, spaced=False, blank_after_meta=False):
    buf = io.StringIO()
    w = csv.writer(buf, lineterminator="\r\n" if crlf else "\n")
    for i, r in enumerate(rows):
        w.writerow(r)
        if blank_after_meta and i == 1:
            buf.write(",,,\r\n" if crlf else ",,,\n")
    text = buf.getvalue()
    if spaced:
        text = text.replace("|", " | ")
    data = text.encode("utf-8")
    if bom:
        data = b"\xef\xbb\xbf" + data
    with open(OUT + path, "wb") as f:
        f.write(data)

# ---- v3 tall ---------------------------------------------------------------
row1 = [h.replace("[state]", "CO") for h in tall[0]]
row2 = ["Test General Hospital", "2026-01-15", "3.0.0",
        "Test General Hospital|Test General North",
        "1 Main St, Denver, CO 80204|2 North Rd, Denver, CO 80221",
        "12345", "1234567890|1098765432", "true", "Jane Doe"]
row2 += [""] * (len(row1) - len(row2))
hdr = expand_codes(tall[2])
assert len(hdr) == 24, len(hdr)
def tall_row(desc, c1, t1, c2, t2, setting, gross, cash, payer, plan, dollar, pct, alg, med, p10, p90, cnt, meth, mn, mx, notes=""):
    return [desc, c1, t1, c2, t2, "", setting, "", "", gross, cash, payer, plan, dollar, pct, alg, med, p10, p90, cnt, meth, mn, mx, notes]
rows = [row1, row2, hdr,
    tall_row("Endometrial biopsy", "58100", "CDM", "58100", "CPT", "outpatient", "1200", "600", "Aetna", "Aetna PPO", "450.25", "", "", "440", "300", "520", "25", "fee schedule", "380", "610"),
    tall_row("Endometrial biopsy", "58100", "CDM", "58100", "CPT", "outpatient", "1200", "600", "Blue Cross Blue Shield", "BCBS HMO", "380", "", "", "375", "350", "400", "1 through 10", "fee schedule", "380", "610"),
    tall_row("Colonoscopy diagnostic", "45378", "CPT", "G0121", "HCPCS", "outpatient", "3000", "1500", "Cigna", "Open Access Plus", "900", "", "", "880", "700", "1100", "40", "fee schedule", "900", "900"),
    tall_row("IUD insertion", "58300", "CPT", "", "", "outpatient", "500", "300", "", "", "", "", "", "", "", "", "", "", "", ""),
    tall_row("Uterine procedures (APR grouper)", "742", "APR-DRG", "", "", "inpatient", "25000", "12000", "Aetna", "Aetna PPO", "15000", "", "", "", "", "", "", "case rate", "15000", "15000"),
    tall_row("Uterine and adnexa procedures w CC/MCC", "0742", "MS-DRG", "", "", "inpatient", "30000", "15000", "Aetna", "Aetna PPO", "", "55", "", "16500", "12000", "21000", "1 through 10", "percent of total billed charges", "", "", "Percent of billed charges, capped"),
    tall_row("Chargemaster biopsy tray", "58100", "CDM", "4001", "RC", "outpatient", "80", "40", "Aetna", "Aetna PPO", "60", "", "", "", "", "", "", "fee schedule", "60", "60"),
    tall_row("Office visit", "99213", "CPT", "", "", "outpatient", "200", "100", "Aetna", "Aetna PPO", "95", "", "", "", "", "", "", "fee schedule", "95", "95"),
]
write_csv("v3_tall.csv", rows)

# ---- v3 wide ---------------------------------------------------------------
row1w = [h.replace("[state]", "TX") for h in wide[0]]
row2w = ["Wide Valley Hospital", "02/01/2026", "3.0.0", "Wide Valley Hospital", "7 River Rd, Houston, TX 77002",
         "TX-7788", "1111111111", "true", "Sam Lee"]
row2w += [""] * (len(row1w) - len(row2w))
payers = [("Blue Cross Blue Shield", "PPO Select"), ("United Healthcare", "Choice Plus")]
hdrw = payer_block(expand_codes(wide[2]), payers)
assert len(hdrw) == 11 + 18 + 3, len(hdrw)
def wide_row(desc, c1, t1, c2, t2, gross, cash, b1, b2, mn, mx, notes=""):
    return [desc, c1, t1, c2, t2, "", "outpatient", "", "", gross, cash] + b1 + b2 + [mn, mx, notes]
E = [""] * 9
rows = [row1w, row2w, hdrw,
    wide_row("Endometrial biopsy", "58100", "CPT", "", "", "1100", "550",
             ["410", "", "", "405", "380", "430", "25", "fee schedule", ""],
             ["395", "", "", "390", "370", "420", "30", "fee schedule", ""], "395", "410"),
    wide_row("Colonoscopy with biopsy", "45378", "CPT", "45380", "CPT", "3200", "1600",
             ["1000", "", "", "990", "900", "1100", "50", "fee schedule", ""],
             ["", "60", "", "950", "800", "1200", "1 through 10", "percent of total billed charges", "Paid at 60 percent of billed charges"], "1000", "1000"),
    wide_row("IUD insertion", "58300", "CPT", "", "", "500", "300", E, E, "", ""),
    wide_row("Office visit", "99213", "CPT", "", "", "200", "100",
             ["90", "", "", "", "", "", "", "fee schedule", ""], ["95", "", "", "", "", "", "", "fee schedule", ""], "90", "95"),
]
write_csv("v3_wide.csv", rows)

# ---- v2.2 tall -------------------------------------------------------------
row1v2 = ["hospital_name", "last_updated_on", "version", "hospital_location", "hospital_address", "license_number|CA",
          "To the best of its knowledge and belief, the hospital has included all applicable standard charge information in accordance with the requirements of 45 CFR 180.50, and the information encoded is true, accurate, and complete as of the date indicated."]
row2v2 = ["Legacy Community Hospital", "07/01/2025", "2.2.0", "Legacy Community Hospital", "9 Old Rd, Fresno, CA 93701", "CA-998877", "true"]
hdrv2 = ["description", "code|1", "code|1|type", "code|2", "code|2|type", "billing_class", "setting",
         "drug_unit_of_measurement", "drug_type_of_measurement", "modifiers", "standard_charge|gross",
         "standard_charge|discounted_cash", "payer_name", "plan_name", "standard_charge|negotiated_dollar",
         "standard_charge|negotiated_percentage", "standard_charge|negotiated_algorithm", "estimated_amount",
         "standard_charge|min", "standard_charge|max", "standard_charge|methodology", "additional_generic_notes"]
def v2_row(desc, c1, t1, bc, setting, gross, cash, payer, plan, dollar, pct, est, mn, mx, meth):
    return [desc, c1, t1, "", "", bc, setting, "", "", "", gross, cash, payer, plan, dollar, pct, "", est, mn, mx, meth, ""]
rows = [row1v2, row2v2, hdrv2,
    v2_row("Colonoscopy with snare", "45385", "CPT", "facility", "outpatient", "2800", "1400", "Humana", "Humana Gold", "1250", "", "1250", "1100", "1300", "fee schedule"),
    v2_row("Colonoscopy with snare", "45385", "CPT", "professional", "outpatient", "600", "300", "Humana", "Humana Gold", "275", "", "275", "250", "300", "fee schedule"),
    v2_row("Uterine procedures without CC/MCC", "743", "MS-DRG", "facility", "inpatient", "22000", "11000", "Kaiser", "Kaiser HMO", "", "70", "15400", "", "", "percent of total billed charges"),
    v2_row("Screening colonoscopy", "G0121", "HCPCS", "facility", "outpatient", "1800", "900", "Humana", "Humana Gold", "700", "", "700", "700", "700", "fee schedule"),
]
write_csv("v22_tall.csv", rows)

# ---- messy tall: BOM, CRLF, spaced pipes, blank row, $ amounts, stray quote --
row1m = [h.replace("[state]", "TX") for h in tall[0][:9]]
row2m = ["Messy Regional Medical Center", "3/2/2026", "3.0.0", "Messy Regional Medical Center",
         "5 Elm St, Austin, TX 78701", "TX-4455", "1212121212 | 3434343434", "true", "Pat Smith"]
buf = io.StringIO()
w = csv.writer(buf, lineterminator="\r\n")
w.writerow(row1m); w.writerow(row2m)
buf.write(",,,,\r\n")
w.writerow(hdr)
text = buf.getvalue().replace("|", " | ")
# a raw (not csv-module) line with an unescaped quote mid-field, as real files have
text += 'SUPPLY 12" TRAY,A4314,HCPCS,,,,outpatient,,,40,20,Aetna,Aetna PPO,30,,,,,,,fee schedule,30,30,\r\n'
buf = io.StringIO()
w = csv.writer(buf, lineterminator="\r\n")
w.writerow(tall_row("Biopsy, endometrium", " 58100 ", "cpt", "", "", "outpatient", "$1,250.00", "$625.00", "UnitedHealthcare", "Choice Plus", "$512.40", "", "", "", "", "", "", "fee schedule", "$400.00", "$600.00"))
w.writerow(tall_row("Colonoscopy", "45378", "CPT", "", "", "outpatient", "3100", "1550", "Aetna", "Aetna PPO", "925", "", "", "", "", "", "", "fee schedule", "925", "925", "Line one\r\nline two"))
w.writerow(tall_row("Uterine procedures w CC/MCC", "742.0", "MS-DRG", "", "", "inpatient", "31000", "15500", "Aetna", "Aetna PPO", "17250", "", "", "", "", "", "", "case rate", "17250", "17250"))
text += buf.getvalue().replace("|", " | ")
with open(OUT + "messy_tall.csv", "wb") as f:
    f.write(b"\xef\xbb\xbf" + text.encode("utf-8"))

# ---- v3 JSON ---------------------------------------------------------------
attest = tall[0][7]
v3 = {
  "hospital_name": "Test General Hospital",
  "last_updated_on": "2026-01-15",
  "version": "3.0.0",
  "location_name": ["Test General Hospital", "Test General North"],
  "hospital_address": ["1 Main St, Denver, CO 80204", "2 North Rd, Denver, CO 80221"],
  "license_information": {"license_number": "12345", "state": "CO"},
  "attestation": {"attestation": attest, "confirm_attestation": True, "attester_name": "Jane Doe"},
  "type_2_npi": ["1234567890", "1098765432"],
  "standard_charge_information": [
    {"description": "Endometrial biopsy",
     "code_information": [{"code": "58100", "type": "CDM"}, {"code": "58100", "type": "CPT"}],
     "standard_charges": [
       {"setting": "outpatient", "gross_charge": 1200, "discounted_cash": 600, "minimum": 380, "maximum": 610,
        "modifier_code": ["50", "59"],
        "payers_information": [
          {"payer_name": "Aetna", "plan_name": "Aetna PPO", "standard_charge_dollar": 450.25, "median_amount": 440,
           "10th_percentile": 300, "90th_percentile": 520, "count": "25", "methodology": "fee schedule"},
          {"payer_name": "Blue Cross Blue Shield", "plan_name": "BCBS HMO", "standard_charge_percentage": 60,
           "median_amount": 400, "10th_percentile": 350, "90th_percentile": 450, "count": "1 through 10",
           "methodology": "percent of total billed charges"}]},
       {"setting": "inpatient", "gross_charge": 1500, "discounted_cash": 700}]},
    {"description": "Colonoscopy diagnostic",
     "code_information": [{"code": "45378", "type": "CPT"}, {"code": "G0121", "type": "HCPCS"}],
     "standard_charges": [
       {"setting": "outpatient", "gross_charge": 3000, "discounted_cash": 1500, "minimum": 900, "maximum": 900,
        "payers_information": [{"payer_name": "Cigna", "plan_name": "Open Access Plus", "standard_charge_dollar": 900,
                                "methodology": "fee schedule"}]}]},
    {"description": "Uterine procedures (APR grouper)",
     "code_information": [{"code": "742", "type": "APR-DRG"}],
     "standard_charges": [{"setting": "inpatient", "gross_charge": 25000,
        "payers_information": [{"payer_name": "Aetna", "plan_name": "Aetna PPO", "standard_charge_dollar": 15000, "methodology": "case rate"}]}]},
    {"description": "Uterine and adnexa procedures w CC/MCC",
     "code_information": [{"code": "0742", "type": "MS-DRG"}],
     "standard_charges": [{"setting": "inpatient", "gross_charge": 30000, "discounted_cash": 15000, "minimum": 18000, "maximum": 18000,
        "payers_information": [{"payer_name": "Aetna", "plan_name": "Aetna PPO", "standard_charge_dollar": 18000, "methodology": "case rate"}]}]},
    {"description": "Office visit",
     "code_information": [{"code": "99213", "type": "CPT"}],
     "standard_charges": [{"setting": "outpatient", "gross_charge": 200,
        "payers_information": [{"payer_name": "Aetna", "plan_name": "Aetna PPO", "standard_charge_dollar": 95, "methodology": "fee schedule"}]}]}
  ],
  "modifier_information": [
    {"description": "Bilateral procedure", "code": "50",
     "modifier_payer_information": [{"payer_name": "Aetna", "plan_name": "Aetna PPO", "description": "150 percent of the base rate"}]}]
}
with open(OUT + "v3.json", "w") as f:
    json.dump(v3, f)

# ---- v2 JSON (metadata after the charge array, code as a bare number) ------
v2 = {
  "hospital_name": "Legacy Community Hospital",
  "standard_charge_information": [
    {"description": "Colonoscopy with biopsy",
     "code_information": [{"code": 45380, "type": "CPT"}],
     "standard_charges": [{"setting": "outpatient", "billing_class": "facility", "gross_charge": 2500, "discounted_cash": 1250,
        "minimum": 1100, "maximum": 1100,
        "payers_information": [{"payer_name": "Humana", "plan_name": "Humana Gold", "standard_charge_dollar": 1100,
                                "estimated_amount": 1100, "methodology": "fee schedule"}]}]},
    {"description": "Endometrial biopsy",
     "code_information": [{"code": "58100", "type": "CPT"}],
     "standard_charges": [{"setting": "outpatient", "billing_class": "facility", "gross_charge": 900, "discounted_cash": 450}]}
  ],
  "last_updated_on": "2025-07-01",
  "version": "2.2.0",
  "hospital_location": ["Legacy Community Hospital"],
  "hospital_address": ["9 Old Rd, Fresno, CA 93701"],
  "license_information": {"license_number": "CA-998877", "state": "CA"},
  "affirmation": {"affirmation": "To the best of its knowledge and belief, the hospital has included all applicable standard charge information in accordance with the requirements of 45 CFR 180.50, and the information encoded is true, accurate, and complete as of the date indicated.", "confirm_affirmation": True}
}
with open(OUT + "v2.json", "w") as f:
    json.dump(v2, f)

# ---- encoding fixtures: one stray Windows-1252 byte; UTF-16LE with BOM -----
small = [row1, row2, hdr,
    tall_row("Endometrial biopsy, physician\x00s office", "58100", "CPT", "", "", "outpatient", "1200", "600", "Aetna", "Aetna PPO", "450.25", "", "", "", "", "", "", "fee schedule", "450.25", "450.25"),
    tall_row("Colonoscopy diagnostic", "45378", "CPT", "", "", "outpatient", "3000", "1500", "Cigna", "Open Access Plus", "900", "", "", "", "", "", "", "fee schedule", "900", "900"),
]
buf = io.StringIO()
csv.writer(buf, lineterminator="\n").writerows(small)
data = buf.getvalue().encode("utf-8").replace(b"\x00", b"\x92")  # a lone cp1252 right quote
with open(OUT + "invalid_utf8_tall.csv", "wb") as f:
    f.write(data)
with open(OUT + "utf16le_tall.csv", "wb") as f:
    f.write(b"\xff\xfe" + buf.getvalue().replace("\x00", "'").encode("utf-16-le"))

print(f"MRF fixtures written to {OUT}")
