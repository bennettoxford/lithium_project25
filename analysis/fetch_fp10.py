import json
import sys
import time
from dataclasses import dataclass
from io import BytesIO
from pathlib import Path
from typing import Callable
from urllib.parse import quote
from urllib.request import Request, urlopen

import pandas as pd


PROJECT_ROOT = Path(__file__).resolve().parents[1]
BASE_URL = "https://opendata.nhsbsa.net/api/3/action"
DATASET_ID = "hospital-prescribing-dispensed-in-the-community"
RESOURCE_PREFIX = "HOSPITAL_DISP_COMMUNITY"
START_YEAR = 2017
START_MONTH = 1
END_YEAR = 2025
END_MONTH = 12
OUTPUT_DIR = PROJECT_ROOT / "data" / "secondary_care_fp10"

LITHIUM_PREFIXES = ("0402030K0", "0402030P0")
MONTH_ABBR = {
    "JAN": 1,
    "FEB": 2,
    "MAR": 3,
    "APR": 4,
    "MAY": 5,
    "JUN": 6,
    "JUL": 7,
    "AUG": 8,
    "SEP": 9,
    "OCT": 10,
    "NOV": 11,
    "DEC": 12,
}


@dataclass(frozen=True)
class Resource:
    name: str
    yyyymm: int
    url: str


def request_json(url: str) -> dict[str, object]:
    request = Request(url, headers={"User-Agent": "lithium-project25/0.1"})
    with urlopen(request) as response:
        return json.loads(response.read().decode("utf-8"))


def parse_ckan_response(response: dict[str, object]) -> dict[str, object]:
    if response.get("success") is True:
        return response

    error = response.get("error")
    if isinstance(error, dict):
        message = error.get("message") or error.get("__type") or "unknown"
    else:
        message = "unknown"
    raise RuntimeError(f"API error: {message}")


def parse_monthly_hospital_name(name: str) -> int | None:
    prefix = "MONTHLY HOSPITAL DATA "
    if not name.startswith(prefix) or len(name) != len(prefix) + 5:
        return None

    month = MONTH_ABBR.get(name[-5:-2])
    if month is None or not name[-2:].isdigit():
        return None

    year_suffix = int(name[-2:])
    year = 2000 + year_suffix if year_suffix < 50 else 1900 + year_suffix
    return year * 100 + month


def parse_hospital_resource_name(name: str) -> int | None:
    prefix = f"{RESOURCE_PREFIX}_"
    if not name.startswith(prefix):
        return None

    suffix = name[len(prefix) :]
    if suffix.endswith("FINAL"):
        suffix = suffix[: -len("FINAL")]
    if len(suffix) != 6 or not suffix.isdigit():
        return None
    return int(suffix)


def dedupe_resources_by_month(resources: list[Resource]) -> list[Resource]:
    chosen: dict[int, Resource] = {}
    for resource in resources:
        existing = chosen.get(resource.yyyymm)
        if existing is None:
            chosen[resource.yyyymm] = resource
        elif existing.name.startswith("MONTHLY HOSPITAL DATA") and resource.name.startswith(RESOURCE_PREFIX):
            chosen[resource.yyyymm] = resource
    return [chosen[yyyymm] for yyyymm in sorted(chosen)]


def resources_from_package(package: dict[str, object]) -> list[Resource]:
    if package.get("success") is not True:
        raise RuntimeError("Invalid package response")

    result = package.get("result")
    if not isinstance(result, dict) or not result.get("resources"):
        raise RuntimeError("Invalid package response")

    min_yyyymm = START_YEAR * 100 + START_MONTH
    max_yyyymm = END_YEAR * 100 + END_MONTH
    output: list[Resource] = []
    for raw_resource in result["resources"]:
        name = raw_resource.get("name") or ""
        url = raw_resource.get("url") or ""
        if not url:
            continue

        hospital_yyyymm = parse_hospital_resource_name(name)
        if hospital_yyyymm is not None:
            if min_yyyymm <= hospital_yyyymm <= max_yyyymm:
                output.append(Resource(name=name, yyyymm=hospital_yyyymm, url=url))
            continue

        legacy_yyyymm = parse_monthly_hospital_name(name)
        if legacy_yyyymm is not None and min_yyyymm <= legacy_yyyymm <= max_yyyymm:
            output.append(Resource(name=name, yyyymm=legacy_yyyymm, url=url))

    if not output:
        raise RuntimeError("No matching resources found in date range")
    return dedupe_resources_by_month(output)


def get_resources() -> list[Resource]:
    package_url = f"{BASE_URL}/package_show?id={quote(DATASET_ID, safe='')}"
    return resources_from_package(parse_ckan_response(request_json(package_url)))


def download_bytes(url: str) -> bytes:
    request = Request(url, headers={"User-Agent": "lithium-project25/0.1"})
    with urlopen(request) as response:
        return response.read()


def filter_lithium_bnf(frame: pd.DataFrame) -> pd.DataFrame:
    if "BNF_CODE" not in frame.columns:
        raise RuntimeError("No BNF_CODE column in CSV")

    prefixes = frame["BNF_CODE"].astype("string").str.slice(0, 9)
    return frame.loc[prefixes.isin(LITHIUM_PREFIXES).fillna(False)].copy()


def fetch_month_csv(download_url: str) -> pd.DataFrame | None:
    frame = pd.read_csv(BytesIO(download_bytes(download_url)))
    frame = frame.rename(columns=lambda column: str(column).replace(" ", "_"))
    filtered = filter_lithium_bnf(frame)
    if filtered.empty:
        return None
    return filtered


def fetch_month(
    resource: Resource,
    *,
    fetch_csv: Callable[[str], pd.DataFrame | None] = fetch_month_csv,
) -> pd.DataFrame | None:
    return fetch_csv(resource.url)


def run_pipeline(
    *,
    resources: list[Resource] | None = None,
    output_dir: Path = OUTPUT_DIR,
    skip_existing: bool = True,
    fetch_month_func: Callable[[Resource], pd.DataFrame | None] = fetch_month,
    sleep_seconds: float = 0.2,
) -> None:
    resources = get_resources() if resources is None else resources
    output_dir.mkdir(parents=True, exist_ok=True)
    total = len(resources)
    print(
        f"Fetching lithium-only data for {total} months (Jan {START_YEAR} - Dec {END_YEAR})",
        file=sys.stderr,
    )

    for index, resource in enumerate(resources, start=1):
        yyyymm = f"{resource.yyyymm:06d}"
        output_path = output_dir / f"fp10_{yyyymm}.csv"
        if skip_existing and output_path.exists():
            print(f"[{index}/{total}] Skipping {yyyymm} (already exists)", file=sys.stderr)
            continue

        print(f"[{index}/{total}] Fetching {yyyymm} ({resource.name}) ...", file=sys.stderr)
        try:
            frame = fetch_month_func(resource)
            if frame is None or frame.empty:
                print(f"  No lithium records for {yyyymm}", file=sys.stderr)
                continue

            frame.to_csv(output_path, index=False)
            print(f"  Saved {len(frame)} rows to {output_path.name}", file=sys.stderr)
        except Exception as exc:
            print(f"  ERROR: {exc}", file=sys.stderr)
        time.sleep(sleep_seconds)

    print(f"Done. Data saved to {output_dir}", file=sys.stderr)


if __name__ == "__main__":
    run_pipeline()
