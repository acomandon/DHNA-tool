"""Extract KY Renthub listings from the raw Dewey parquet tree to a CSV.

Prep sidecar for scripts/build_rent_buffer.R. R's `arrow` package is not
reliably installed on this machine, so the columnar read + KY filter runs
here in the `dewey` conda env (pyarrow), matching the Python-sidecar
convention already used for the Dewey downloader. The much smaller filtered
CSV is then read back into R for the spatial join + quarterly aggregation.

Filters applied (cheap, row-reducing; the rest happens in R):
  - STATE == 'KY'
  - RENT_PRICE in (0, 6000)   (drop nulls + the >=6000 outlier tail)
  - LATITUDE / LONGITUDE not null
  (the DATE_POSTED >= 2014-02 floor is applied in R, which owns the month
   rounding + dedup anyway)

Columns kept: DATE_POSTED, ADDRESS, ZIP, BEDS, BATHS, SQFT, RENT_PRICE,
LATITUDE, LONGITUDE.

Run:
  conda activate dewey
  python scripts/extract_renthub_ky.py

Output: data/processed/renthub_ky_listings.csv
"""
import os

import pyarrow.dataset as ds
import pyarrow.compute as pc
import pyarrow.csv as pacsv

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(ROOT, "data", "dewey", "renthub")
OUT_DIR = os.path.join(ROOT, "data", "processed")
OUT = os.path.join(OUT_DIR, "renthub_ky_listings.csv")

os.makedirs(OUT_DIR, exist_ok=True)

COLS = ["DATE_POSTED", "ADDRESS", "ZIP", "BEDS", "BATHS", "SQFT",
        "RENT_PRICE", "LATITUDE", "LONGITUDE"]

dataset = ds.dataset(SRC, format="parquet")

flt = (
    (pc.field("STATE") == "KY")
    & (pc.field("RENT_PRICE") > 0)
    & (pc.field("RENT_PRICE") < 6000)
    & pc.field("LATITUDE").is_valid()
    & pc.field("LONGITUDE").is_valid()
)

print(f"Scanning {SRC} ...")
table = dataset.scanner(columns=COLS, filter=flt).to_table()
print(f"  {table.num_rows:,} KY listing rows after filter")

pacsv.write_csv(table, OUT)
print(f"Wrote {OUT}")
