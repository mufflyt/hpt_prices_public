# Downloading and preparing the Trilliant data

How to get the Trilliant Health "Full Data Download" onto a machine and ready for
`analysis/01_trilliant_extract.R`. Written from the 2026-07-21 snapshot. Check the sizes and
ETag for the snapshot you download.

## Terms first

The data come from Trilliant Health's Oria site under its terms of service:

- **Allowed:** the consolidated download, which is Trilliant's sanctioned bulk route.
- **Not allowed:**
  - scripted scraping of the per-hospital pages (2.3(xii));
  - redistributing rate data derived from the files (2.3(i), (iii)).
- **Required:** attribution to Trilliant Health (2.2(b)).

Keep the zip, the extracted lake, and every derived table off GitHub; `.gitignore` already
excludes them. Confirm with Trilliant before publishing anything built on the data.

## What you need

| Item | Amount |
|---|---|
| Disk | About 160 GB free on the data drive: 79.6 GB for the zip, about the same again extracted. Delete the zip after verifying the extraction. |
| Tools | `curl`, Python 3, the DuckDB CLI 1.5 or newer (DuckLake 1.0 needs it), and R with the packages in `R/00_source_all.R` |
| Memory | The pipeline caps DuckDB at 5 GB and 3 threads, which works on a 16 GB machine |

## Steps

### 1. Get a download link

Log in at oria.trillianthealth.com and choose **Full Data Download**. The link is a signed URL
that expires, usually within a day or two. It is personal, so keep it out of files and git.
Note the size shown on the page (79,646,417,056 bytes for 2026-07-21).

```sh
export TRILLIANT_URL='<the signed link>'
curl -sI "$TRILLIANT_URL" | grep -i -E 'content-length|etag'
```

The ETag is what you verify against in step 3 (2026-07-21:
`da6d4a4ae4896ce5b33e8f86da87ab49-1520`).

### 2. Download

```sh
DATA=/path/to/data/hpt_prices            # becomes HPT_DATA_DIR
tools/trilliant_download.sh "$DATA/trilliant/20260721/mrf_lake_20260721.zip" 79646417056
```

The download resumes after dropped connections, so you can rerun the same command at any time.
If a single stream is slow, add a third argument to fetch the remaining bytes as parallel 1 GiB
ranges:

```sh
tools/trilliant_download.sh "$DATA/trilliant/20260721/mrf_lake_20260721.zip" 79646417056 16
```

That is how the 2026-07-21 zip was finished after a single-stream download stalled at 39.9 GB.
Each range is fetched and checked separately, then appended in order. If the signed link expires
mid-download, get a new one, export it, and rerun: finished bytes and ranges are kept.

Save straight to the data drive. A browser download goes to `~/Downloads` on the internal disk
and can fill it.

### 3. Verify

A size match is not enough, because a corrupt range would still have the right length. Check the
S3 multipart ETag:

```sh
tools/etag_verify.py "$DATA/trilliant/20260721/mrf_lake_20260721.zip" \
  --etag da6d4a4ae4896ce5b33e8f86da87ab49-1520 --part-mib 50
```

It prints `MATCH` or `MISMATCH`. Per-part hashes are cached next to the zip, so a rerun only
hashes what is left. On a mismatch, delete the zip and download again.

### 4. Extract

```sh
tools/fast_unzip.py "$DATA/trilliant/20260721/mrf_lake_20260721.zip" "$DATA/trilliant/20260721"
```

The zip holds a `lake/` folder, so extract into the snapshot directory. The files are stored
uncompressed. macOS `unzip` managed about 7 MB/s; this tool copies at disk speed, checks each
file's CRC, and resumes if interrupted. The result is `lake/` containing:

- `catalog.duckdb` and `metadata.ducklake`, the DuckLake catalog;
- `data/`, the Parquet data files;
- Trilliant's `README.md` and `open-lake.sh`/`open-lake.sql` for opening it in the DuckDB CLI.

If `analysis/01_trilliant_extract.R` finds the zip but no `lake/catalog.duckdb`, it falls back
to `unzip`, which works but is slow.

### 5. Point the pipeline at it and extract

```sh
export HPT_DATA_DIR="$DATA"
Rscript analysis/01_trilliant_extract.R   # codebook rows from the lake to prices/source=trilliant/
Rscript analysis/02_ccn_crosswalk.R       # CMS CCN matching (downloads public CMS files)
Rscript analysis/09_build_database.R      # hpt.duckdb with the cleaning rules
Rscript analysis/10_validate.R
Rscript analysis/11_state_medians.R
```

Then run `12` to `15` as needed. `03` to `07` are the own-crawl gap fill and are only needed
to cover hospitals Trilliant is missing.

`config/paths.R` defaults to `/Volumes/MufflySamsung 1/hpt_prices` and checks that the path is a
real mount point. On another machine, set `HPT_DATA_DIR` instead. For a snapshot other than
2026-07-21, set `HPT_TRILLIANT_SNAPSHOT` (for example `20261021`) and use that date in the paths
above.

The extract streams the lake in two stages, so peak memory stays under the 5 GB cap.
`HPT_REUSE_STAGE=true` reuses the stage-1 file when you rerun only stage 2 (the codebook codes
must be unchanged).

## What is in the lake

These are the parts `R/trilliant.R` reads:

| Relation | What it holds |
|---|---|
| `current_charge_details` | Payer-level rates for each hospital's current run: codes, setting, billing class, gross and cash, payer and plan, negotiated dollar, percentage, algorithm, methodology |
| `current_hospitals` | One row per facility for its current run, keyed by `internal_id` and `run_date` (`hospital_id` is 1 everywhere, so do not key on it) |
| `lake.hospital_versions` | `mrf_url` and `mrf_content_hash`. Several facilities often share one file, so the extract reads each hash once |
| `lake.hospital_identity` | Facility NPI and license state |
| `directory_provider`, `directory_organization` | Provider directory; not used by the price pipeline |

See `docs/appendix.md` for how the extract, crosswalk, and cleaning rules use these.
