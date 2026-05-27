import json
import re
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Callable
from urllib.parse import quote
from urllib.request import Request, urlopen

import pandas as pd


PROJECT_ROOT = Path(__file__).resolve().parents[1]
BASE_URL = "https://opendata.nhsbsa.net/api/3/action"
# Legacy EPD (BNF only): data until June 2025
DATASET_ID_LEGACY = "english-prescribing-data-epd"
# EPD with SNOMED: from July 2025 onwards
DATASET_ID_SNOMED = "english-prescribing-dataset-epd-with-snomed-code"
RESOURCE_PREFIX = "EPD"

CUTOFF_YYYYMM = 202507
LEGACY_MAX_YYYYMM = CUTOFF_YYYYMM - 1
SQL_LIMIT = 32000
START_YEAR = 2015
START_MONTH = 1
END_YEAR = 2025
END_MONTH = 12
OUTPUT_DIR = PROJECT_ROOT / "data" / "primary_care"
PRACTICE_CODES_FILE = PROJECT_ROOT / "output" / "data" / "ord_ro76_practices.csv"

LITHIUM_PREFIXES = ("0402030K0", "0402030P0")
RESOURCE_NAME_PATTERN = re.compile(rf"^{RESOURCE_PREFIX}(_SNOMED)?_(\d{{6}})$")

LEGACY_COLUMNS = [
    "YEAR_MONTH",
    "REGIONAL_OFFICE_NAME",
    "REGIONAL_OFFICE_CODE",
    "ICB_NAME",
    "ICB_CODE",
    "PCO_NAME",
    "PCO_CODE",
    "PRACTICE_NAME",
    "PRACTICE_CODE",
    "ADDRESS_1",
    "ADDRESS_2",
    "ADDRESS_3",
    "ADDRESS_4",
    "POSTCODE",
    "BNF_CHEMICAL_SUBSTANCE",
    "CHEMICAL_SUBSTANCE_BNF_DESCR",
    "BNF_CODE",
    "BNF_DESCRIPTION",
    "BNF_CHAPTER_PLUS_CODE",
    "QUANTITY",
    "TOTAL_QUANTITY",
    "ADQUSAGE",
    "NIC",
    "UNIDENTIFIED",
    "SNOMED_CODE",
]


@dataclass(frozen=True)
class Resource:
    resource_id: str
    yyyymm: int
    is_snomed: bool


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


def paginate_records(
    fetch_page: Callable[[int], dict[str, object]],
    extract_records: Callable[[dict[str, object]], list[dict[str, object]] | None],
) -> list[dict[str, object]]:
    all_records: list[dict[str, object]] = []
    offset = 0
    while True:
        page = fetch_page(offset)
        records = extract_records(page)
        if not records:
            break
        all_records.extend(records)
        offset += len(records)
        if len(records) < SQL_LIMIT:
            break
        time.sleep(0.1)
    return all_records


def load_practice_codes() -> set[str]:
    if not PRACTICE_CODES_FILE.exists():
        raise FileNotFoundError(
            f"No ord_ro76_practices.csv found at {PRACTICE_CODES_FILE} - aborting"
        )
    codes = pd.read_csv(PRACTICE_CODES_FILE)
    return set(codes["practice_code"].dropna().astype(str).unique())


def resources_from_package_response(
    package: dict[str, object],
    min_yyyymm: int,
    max_yyyymm: int,
    *,
    is_snomed: bool,
) -> list[Resource]:
    result = package.get("result")
    if not isinstance(result, dict):
        return []

    raw_resources = result.get("resources")
    if not raw_resources:
        return []

    matched: list[Resource] = []
    for raw_resource in raw_resources:
        name = raw_resource.get("name") or ""
        match = RESOURCE_NAME_PATTERN.match(name)
        if not match:
            continue
        yyyymm = int(match.group(2))
        if min_yyyymm <= yyyymm <= max_yyyymm:
            matched.append(Resource(resource_id=name, yyyymm=yyyymm, is_snomed=is_snomed))

    return sorted(matched, key=lambda resource: resource.yyyymm)


def resources_from_package(
    dataset_id: str,
    min_yyyymm: int,
    max_yyyymm: int,
    *,
    is_snomed: bool,
) -> list[Resource]:
    package_url = f"{BASE_URL}/package_show?id={quote(dataset_id, safe='')}"
    package = parse_ckan_response(request_json(package_url))
    return resources_from_package_response(
        package,
        min_yyyymm,
        max_yyyymm,
        is_snomed=is_snomed,
    )


def get_resources() -> list[Resource]:
    min_yyyymm = START_YEAR * 100 + START_MONTH
    max_yyyymm = END_YEAR * 100 + END_MONTH

    legacy_max = min(LEGACY_MAX_YYYYMM, max_yyyymm)
    legacy = (
        resources_from_package(
            DATASET_ID_LEGACY,
            min_yyyymm,
            legacy_max,
            is_snomed=False,
        )
        if min_yyyymm <= legacy_max
        else []
    )

    snomed_min = max(CUTOFF_YYYYMM, min_yyyymm)
    snomed = (
        resources_from_package(
            DATASET_ID_SNOMED,
            snomed_min,
            max_yyyymm,
            is_snomed=True,
        )
        if snomed_min <= max_yyyymm
        else []
    )

    resources = legacy + snomed
    if not resources:
        raise RuntimeError("No matching resources found in date range")
    return resources


def build_sql(resource_id: str, offset: int = 0) -> str:
    in_list = ", ".join(f"'{prefix}'" for prefix in LITHIUM_PREFIXES)
    return (
        f"SELECT * FROM `{resource_id}` "
        f"WHERE BNF_CHEMICAL_SUBSTANCE IN ({in_list}) "
        f"LIMIT {SQL_LIMIT} OFFSET {offset}"
    )


def extract_legacy_records(page: dict[str, object]) -> list[dict[str, object]] | None:
    result = page.get("result")
    if not isinstance(result, dict):
        return None
    nested = result.get("result")
    if isinstance(nested, dict) and nested.get("records"):
        records = nested["records"]
    else:
        records = result.get("records")
    if not records:
        return None
    if not isinstance(records, list):
        return None
    return records


def fetch_page_legacy(resource_id: str, offset: int = 0) -> dict[str, object]:
    sql = build_sql(resource_id, offset)
    url = (
        f"{BASE_URL}/datastore_search_sql?"
        f"resource_id={quote(resource_id, safe='')}"
        f"&sql={quote(sql, safe='')}"
    )
    return parse_ckan_response(request_json(url))


def fetch_month_legacy(resource_id: str) -> pd.DataFrame | None:
    records = paginate_records(
        lambda offset: fetch_page_legacy(resource_id, offset),
        extract_legacy_records,
    )
    if not records:
        return None
    return pd.DataFrame(records)


def fetch_page_snomed(
    resource_id: str,
    chemical_code: str,
    offset: int = 0,
    *,
    query: str = "filters",
) -> dict[str, object]:
    payload = json.dumps({"BNF_CHEMICAL_SUBSTANCE_CODE": chemical_code})
    param = "filters" if query == "filters" else "q"
    url = (
        f"{BASE_URL}/datastore_search?"
        f"resource_id={quote(resource_id, safe='')}"
        f"&limit={SQL_LIMIT}"
        f"&offset={offset}"
        f"&{param}={quote(payload, safe='')}"
    )
    return parse_ckan_response(request_json(url))


def extract_snomed_records(page: dict[str, object]) -> list[dict[str, object]] | None:
    result = page.get("result")
    if not isinstance(result, dict):
        return None
    records = result.get("records")
    if not records or not isinstance(records, list):
        return None
    return records


def fetch_snomed_chemicals(resource_id: str, query: str) -> list[dict[str, object]]:
    all_records: list[dict[str, object]] = []
    for chemical in LITHIUM_PREFIXES:
        records = paginate_records(
            lambda offset, chemical=chemical: fetch_page_snomed(
                resource_id, chemical, offset, query=query
            ),
            extract_snomed_records,
        )
        all_records.extend(records)
    return all_records


def fetch_month_snomed(resource_id: str) -> pd.DataFrame | None:
    records = fetch_snomed_chemicals(resource_id, "filters")
    if not records:
        records = fetch_snomed_chemicals(resource_id, "q")
    if not records:
        return None
    return pd.DataFrame(records)


def select_epd_output_columns(frame: pd.DataFrame) -> pd.DataFrame:
    return frame.reindex(columns=[col for col in LEGACY_COLUMNS if col in frame.columns])


def filter_lithium(frame: pd.DataFrame) -> pd.DataFrame:
    if "BNF_CHEMICAL_SUBSTANCE_CODE" in frame.columns:
        return frame.loc[frame["BNF_CHEMICAL_SUBSTANCE_CODE"].isin(LITHIUM_PREFIXES)].copy()
    if "BNF_CODE" in frame.columns:
        prefixes = frame["BNF_CODE"].astype("string").str.slice(0, 9)
        return frame.loc[prefixes.isin(LITHIUM_PREFIXES).fillna(False)].copy()
    if "BNF_PRESENTATION_CODE" in frame.columns:
        prefixes = frame["BNF_PRESENTATION_CODE"].astype("string").str.slice(0, 9)
        return frame.loc[prefixes.isin(LITHIUM_PREFIXES).fillna(False)].copy()
    return frame.copy()


def normalize_snomed_to_legacy(frame: pd.DataFrame, yyyymm: int) -> pd.DataFrame:
    frame = filter_lithium(frame)
    if frame.empty:
        return frame

    yyyymm_str = f"{yyyymm:06d}"
    normalized = frame.copy()
    if "YEAR_MONTH" in normalized.columns:
        normalized["YEAR_MONTH"] = (
            normalized["YEAR_MONTH"].astype("string").str.replace("-", "", regex=False)
        )
    else:
        normalized["YEAR_MONTH"] = yyyymm_str

    normalized["BNF_CODE"] = normalized["BNF_PRESENTATION_CODE"]
    normalized["BNF_DESCRIPTION"] = normalized["BNF_PRESENTATION_NAME"]
    normalized["CHEMICAL_SUBSTANCE_BNF_DESCR"] = normalized["BNF_CHEMICAL_SUBSTANCE"]
    normalized["BNF_CHEMICAL_SUBSTANCE"] = normalized["BNF_CHEMICAL_SUBSTANCE_CODE"]
    if "ADQ_USAGE" in normalized.columns:
        normalized["ADQUSAGE"] = normalized["ADQ_USAGE"]
    else:
        normalized["ADQUSAGE"] = pd.NA

    drop_columns = [
        "BNF_PRESENTATION_CODE",
        "BNF_PRESENTATION_NAME",
        "BNF_CHEMICAL_SUBSTANCE_CODE",
        "ADQ_USAGE",
    ]
    normalized = normalized.drop(columns=[col for col in drop_columns if col in normalized.columns])
    normalized = normalized.reindex(columns=[col for col in LEGACY_COLUMNS if col in normalized.columns])
    return normalized.drop_duplicates()


def fetch_month(resource: Resource) -> pd.DataFrame | None:
    if resource.is_snomed:
        frame = fetch_month_snomed(resource.resource_id)
        if frame is None or frame.empty:
            return None
        return normalize_snomed_to_legacy(frame, resource.yyyymm)
    frame = fetch_month_legacy(resource.resource_id)
    if frame is None or frame.empty:
        return None
    return select_epd_output_columns(frame)


def run_pipeline(
    *,
    resources: list[Resource] | None = None,
    output_dir: Path = OUTPUT_DIR,
    practice_codes: set[str] | None = None,
    skip_existing: bool = True,
    fetch_month_func: Callable[[Resource], pd.DataFrame | None] = fetch_month,
    sleep_seconds: float = 0.2,
) -> None:
    practice_codes = load_practice_codes() if practice_codes is None else practice_codes
    print(f"Filtering to {len(practice_codes)} ORD RO76 practices", file=sys.stderr)

    resources = get_resources() if resources is None else resources
    output_dir.mkdir(parents=True, exist_ok=True)
    total = len(resources)
    print(
        f"Fetching lithium-only EPD data for {total} months "
        f"(Jan {START_YEAR} - Dec {END_YEAR})",
        file=sys.stderr,
    )
    print("  Legacy EPD (BNF) up to Jun 2025; EPD with SNOMED from Jul 2025", file=sys.stderr)

    for index, resource in enumerate(resources, start=1):
        yyyymm = f"{resource.yyyymm:06d}"
        output_path = output_dir / f"epd_lithium_{yyyymm}.csv"
        if skip_existing and output_path.exists():
            print(f"[{index}/{total}] Skipping {yyyymm} (already exists)", file=sys.stderr)
            continue

        source = "SNOMED" if resource.is_snomed else "legacy"
        print(f"[{index}/{total}] Querying {yyyymm} ({source}) ...", file=sys.stderr)
        try:
            frame = fetch_month_func(resource)
            if frame is None or frame.empty:
                print(f"  No lithium records for {yyyymm}", file=sys.stderr)
                continue

            n_before = len(frame)
            frame = frame.loc[frame["PRACTICE_CODE"].astype(str).isin(practice_codes)].copy()
            print(f"  Filtered {n_before} -> {len(frame)} rows (ORD practices only)", file=sys.stderr)
            if frame.empty:
                print(f"  No ORD practice records for {yyyymm}", file=sys.stderr)
                continue

            frame.to_csv(output_path, index=False)
            print(f"  Saved {len(frame)} rows to {output_path.name}", file=sys.stderr)
        except Exception as exc:
            print(f"  ERROR: {exc}", file=sys.stderr)
        time.sleep(sleep_seconds)

    print(f"Done. Data saved to {output_dir}", file=sys.stderr)


if __name__ == "__main__":
    run_pipeline()
