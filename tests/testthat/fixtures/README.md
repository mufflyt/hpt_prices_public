# Test fixtures

Small files the offline test suite reads. None holds hospital price data: dollar amounts are
invented, except the public CMS OPPS payment rates in `validation/`. Names, addresses, CCNs, and
NPIs that appear are public CMS roster facts. Header rows are copied verbatim from the real formats,
so a change to a real column name shows up as a fixture mismatch, not as a silent parser miss.

Regenerate a directory only on purpose. Write to a temporary directory first, `diff -r` it against
the committed files, and copy over only the changes you mean to make.

## `mrf/`: machine-readable file parsers

Built by `tools/make_mrf_fixtures.py`. The metadata, attestation, and charge-column header rows come
verbatim from CMS's v3.0.0 tall and wide CSV templates (github.com/CMSgov/hospital-price-transparency,
pinned to commit `33833d4c`, the last change to those templates, 2025-12-02). Every row below the
headers is synthetic.

| File | Exercises |
|---|---|
| `v3_tall.csv` | v3.0 tall layout; code-type gating (a CDM "58100" and an APR-DRG 742 never match; "0742" as MS-DRG 742); one line carrying two target codes; percent-of-charges rows; a non-target 99213 line |
| `v3_wide.csv` | v3.0 wide layout; one payer/plan column block per payer, unpivoted |
| `v22_tall.csv` | v2.2 headers and professional vs facility billing classes |
| `messy_tall.csv` | UTF-8 BOM, CRLF, spaced pipes in headers, a blank row, dollar-formatted amounts, an unescaped quote mid-field, an embedded newline, "742.0" |
| `invalid_utf8_tall.csv` | a lone Windows-1252 byte in otherwise UTF-8 text |
| `utf16le_tall.csv` | UTF-16LE with a byte-order mark |
| `v3.json`, `v2.json` | v3 JSON and v2 JSON (metadata after the charge array, a code stored as a bare number) |

`tools/make_mrf_fixtures.py --out <dir>` downloads the pinned templates and reproduces all eight files
byte for byte (checked 2026-09-13).

## `crosswalk/`: CCN matching

Built by `tools/make_crosswalk_fixtures.R`. Each header row is read verbatim from the real public file
under `HPT_DATA_DIR/reference`:

| File | Header source |
|---|---|
| `hospital_general_information.csv` | CMS Hospital General Information |
| `hospital_enrollments.csv`, `hospital_additional_npis.csv` | CMS Hospital Enrollments and Additional NPIs (2026-07-31 release) |
| `manifest.csv`, `compliance.csv`, `gaps.csv` | cms-hpt-tracker (github.com/anthonyisnotadev/cms-hpt-tracker, commit `27c08db`), used as data only |
| `chsp_hospital_linkage.csv` | AHRQ Compendium of US Health Systems, 2023 hospital linkage |

The rows are hand-written cases:
- a CCN that lost its leading zero (`60011`);
- a psychiatric-unit CCN (`06S011`);
- one system MRF shared by two hospitals;
- three "Mercy" hospitals in different states that share one MRF URL;
- a federal hospital that is not applicable;
- a linkage row without a CCN.

The generator reproduces all seven files byte for byte against the 2026-07-31 CMS releases and tracker
commit `27c08db` (checked 2026-09-13).

## `discovery/`: `cms-hpt.txt` and footer crawl

Hand-written:
- `cms_example_two_locations.txt` follows the two-location example in CMS's `cms-hpt.txt`
  specification (Example Hospital East/West).
- The other TXT files vary it: CRLF with a BOM and uppercase keys, missing blank lines between
  entries, and a pointer to a vendor-hosted MRF.
- The HTML files are a homepage with a "Price Transparency" footer link, the price page it leads to,
  and an HTML soft 404 returned for `/cms-hpt.txt`.

## `ownership/`

`hospital_all_owners.csv` has the verbatim header of CMS Hospital All Owners (2026-07 release) and nine
synthetic rows: invented enrollment IDs, organizations, and owners, with role codes and PE flags
chosen to exercise the classification.

## `validation/`

`opps_addendum_b_sample.csv` is a short excerpt in the layout of CMS OPPS Addendum B (July 2026). It
keeps the Windows-1252 title and copyright lines, the "HCPCS Code" header, and the dollar-formatted
payment rates the parser has to handle. The payment rates are CMS's public national rates for the
listed codes. The validation suite builds its small database at test time from synthetic rows (see
`test-validation.R`), and the known-answer check uses test-only values, not `config/known_answers.csv`.
