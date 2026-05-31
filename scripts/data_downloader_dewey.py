"""
Downloads Dewey-hosted residential rental datasets used by the DHNA pipeline:

- Renthub          (slug: rental-data-united-states)  listing-level rents, 2014-present
- Dwellsy TotalIQ  (slug: dwellsy-totaliq)            listing-level rents, 2020-09-present

Wraps the official `deweypy` CLI (no R SDK available). Output lands in
<repo>/data/dewey/<folder>/ as parquet shards, readable from R via
`arrow::open_dataset()`.

Requires:
- conda env `dewey` (see environment.yml). Activate before running:
      conda activate dewey
- DEWEY_API_KEY user env var. Set once with:
      setx DEWEY_API_KEY "<your-key>"
  then reopen the shell.

Usage:
  python scripts/data_downloader_dewey.py                  # full pull, both datasets
  python scripts/data_downloader_dewey.py --dataset renthub
  python scripts/data_downloader_dewey.py --after 2024-01-01 --before 2024-01-02   # smoke test
"""
from __future__ import annotations

import argparse
import os
import subprocess
import sys
import time
from pathlib import Path

# Per-subscription Dewey folder IDs (prj_..__fldr_..). NOT the public catalog
# slugs — those 404 on the download endpoint. Look up your folder ID in the
# Dewey web console (or `https://api.deweydata.io/api/v1/external/data/<id>`).
DATASETS: dict[str, str] = {
    "dwellsy": "prj_q3hheoq6__fldr_nv7rkaicsvpg3tkw4",
    "renthub": "prj_q3hheoq6__fldr_cggezfmh4zsrfevk8",  # full US, ~180 GB
}

REPO_ROOT = Path(__file__).resolve().parent.parent
DOWNLOAD_ROOT = REPO_ROOT / "data" / "dewey"


def run_one(folder_name: str, slug: str, *, after: str | None, before: str | None) -> None:
    DOWNLOAD_ROOT.mkdir(parents=True, exist_ok=True)

    cmd = [
        sys.executable, "-m", "deweypy",
        "--download-directory", str(DOWNLOAD_ROOT),
        "--auto-create-download-directory",
        "speedy-download", slug,
        "--folder-name", folder_name,
    ]
    if after:
        cmd += ["--partition-key-after", after]
    if before:
        cmd += ["--partition-key-before", before]

    print(f"\n--- {folder_name}  ({slug}) ---", flush=True)
    print(f"  destination: {DOWNLOAD_ROOT / folder_name}", flush=True)
    if after or before:
        print(f"  window:      after={after or '-inf'}  before={before or '+inf'}", flush=True)
    t0 = time.monotonic()
    subprocess.run(cmd, check=True)
    print(f"  {folder_name} done in {time.monotonic() - t0:,.1f}s", flush=True)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[1])
    parser.add_argument(
        "--dataset",
        choices=[*DATASETS.keys(), "all"],
        default="all",
        help="Which dataset to pull (default: all).",
    )
    parser.add_argument("--after", help="partition_key_after (YYYY-MM-DD). Optional.")
    parser.add_argument("--before", help="partition_key_before (YYYY-MM-DD). Optional.")
    args = parser.parse_args()

    if not os.environ.get("DEWEY_API_KEY"):
        print(
            'ERROR: DEWEY_API_KEY env var not set.\n'
            '  Set once with: setx DEWEY_API_KEY "<your-key>"  (then open a new shell)',
            file=sys.stderr,
        )
        return 2

    if args.dataset == "all":
        targets = list(DATASETS.items())
    else:
        targets = [(args.dataset, DATASETS[args.dataset])]

    for folder_name, slug in targets:
        run_one(folder_name, slug, after=args.after, before=args.before)

    return 0


if __name__ == "__main__":
    sys.exit(main())
