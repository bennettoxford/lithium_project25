import json
import re
import sys
import time
from pathlib import Path
from typing import Callable
from urllib.error import HTTPError
from urllib.parse import urlencode
from urllib.request import Request, urlopen

import pandas as pd


# ORD API: https://digital.nhs.uk/developer/api-catalogue/organisation-data-service-ord
# RO197 = NHS Trust, RO24 = NHS Foundation Trust
# RE5 for ICB, RE2 for region.
PROJECT_ROOT = Path(__file__).resolve().parents[1]
BASE_URL = "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations"
ROLES = ("RO197", "RO24")
RE5 = "RE5"
RE2 = "RE2"
LIMIT = 1000
OUTPUT_DIR = PROJECT_ROOT / "data"
OUTPUT_FILE = OUTPUT_DIR / "ord_trusts.csv"
REQUEST_SLEEP_SECONDS = 0.2

OUTPUT_COLUMNS = [
    "ods_code",
    "ods_name",
    "successors",
    "predecessors",
    "ultimate_successors",
    "legal_closed_date",
    "operational_closed_date",
    "legal_open_date",
    "operational_open_date",
    "postcode",
    "region_code",
    "region",
    "icb_code",
    "icb",
]


def request_json(url: str) -> dict[str, object]:
    request = Request(url, headers={"User-Agent": "lithium-project25/0.1"})
    try:
        with urlopen(request) as response:
            return json.loads(response.read().decode("utf-8"))
    except HTTPError as exc:
        body = exc.read().decode("utf-8", errors="replace")
        raise RuntimeError(f"ORD API error: {exc.code} - {body}") from exc


def get_nested(data: object, *keys: str, default: object = None) -> object:
    current = data
    for key in keys:
        if not isinstance(current, dict):
            return default
        current = current.get(key)
        if current is None:
            return default
    return current


def as_list(value: object) -> list[object]:
    if value is None:
        return []
    if isinstance(value, list):
        return value
    return [value]


def target_org_id(rel: dict[str, object]) -> str | None:
    extension = get_nested(rel, "Target", "OrgId", "extension")
    return str(extension) if extension else None


def format_org_name(name: str | None) -> str:
    if not name:
        return ""

    value = re.sub(r"COMMISSIONING REGION", "", name, flags=re.IGNORECASE)
    value = re.sub(r"INTEGRATED CARE BOARD", "", value, flags=re.IGNORECASE)
    words = value.split()
    formatted: list[str] = []
    for word in words:
        if word.upper() == "NHS":
            formatted.append(word)
        elif word:
            formatted.append(word[:1].upper() + word[1:].lower())
    out = " ".join(formatted)
    out = out.replace("'S ", "'s ")
    out = out.replace("'S,", "'s,")
    return out.strip()


def fetch_all_trusts_from_ord() -> list[str]:
    all_orgs: list[str] = []
    for role in ROLES:
        query = urlencode({"Roles": role, "Limit": LIMIT})
        data = request_json(f"{BASE_URL}?{query}")
        orgs = data.get("Organisations")
        if not orgs:
            continue
        for org in as_list(orgs):
            org_id = org.get("OrgId")
            if org_id:
                all_orgs.append(str(org_id))
    return sorted(set(all_orgs))


def fetch_org_details_single(org_id: str) -> dict[str, object]:
    return request_json(f"{BASE_URL}/{org_id}")


def fetch_org_details(
    orgs: list[str],
    *,
    fetch_single: Callable[[str], dict[str, object]] = fetch_org_details_single,
    sleep_seconds: float = REQUEST_SLEEP_SECONDS,
) -> dict[str, dict[str, object]]:
    total = len(orgs)
    print(f"Fetching details for {total} organisations", file=sys.stderr)
    result: dict[str, dict[str, object]] = {}
    for index, org_id in enumerate(orgs, start=1):
        result[org_id] = fetch_single(org_id)
        if index % 50 == 0 or index == total:
            pct = round(100 * index / total, 1)
            print(f"Progress: {index}/{total} ({pct}%)", file=sys.stderr)
        time.sleep(sleep_seconds)
    return result


def process_org_details(
    all_orgs_details: dict[str, dict[str, object]],
) -> dict[str, object]:
    filtered: dict[str, dict[str, object]] = {}
    predecessors: dict[str, list[str]] = {}
    successors: dict[str, list[str]] = {}
    icbs: dict[str, str] = {}

    for org_code, data in all_orgs_details.items():
        country = get_nested(data, "Organisation", "GeoLoc", "Location", "Country") or ""
        if country != "ENGLAND":
            continue

        filtered[org_code] = data
        org = data["Organisation"]

        for succession in as_list(get_nested(org, "Succs", "Succ")):
            typ = succession.get("Type") or ""
            target = target_org_id(succession)
            if not target:
                continue
            if typ == "Predecessor":
                predecessors.setdefault(org_code, []).append(target)
            elif typ == "Successor":
                successors.setdefault(org_code, []).append(target)

        for rel in as_list(get_nested(org, "Rels", "Rel")):
            if rel.get("id") != RE5 or rel.get("Status") != "Active":
                continue
            icb = target_org_id(rel)
            if icb:
                icbs[org_code] = icb
            break

    # RYK (Dudley) dissolved Oct 2024; staff/services transferred to TAJ (Black Country Healthcare)
    # Ref: https://www.england.nhs.uk/publication/dudley-and-walsall-mental-health-partnership-nhs-trust/
    successors["RYK"] = sorted(set([*(successors.get("RYK") or []), "TAJ"]))
    predecessors["TAJ"] = sorted(set([*(predecessors.get("TAJ") or []), "RYK"]))

    print(f"Found {len(icbs)} trusts with ICB mapping", file=sys.stderr)
    return {
        "icbs": icbs,
        "successors": successors,
        "predecessors": predecessors,
        "filtered_org_details": filtered,
    }


def get_icb_regions(
    icb_codes: list[str],
    *,
    fetch_details: Callable[[list[str]], dict[str, dict[str, object]]] = fetch_org_details,
) -> dict[str, str]:
    codes = sorted({code for code in icb_codes if code})
    if not codes:
        return {}

    print(f"Fetching regions for {len(codes)} ICBs", file=sys.stderr)
    icb_details = fetch_details(codes)
    result: dict[str, str] = {}
    for icb, data in icb_details.items():
        org = data["Organisation"]
        for rel in as_list(get_nested(org, "Rels", "Rel")):
            if rel.get("id") != RE2 or rel.get("Status") != "Active":
                continue
            region = target_org_id(rel)
            if region:
                result[icb] = region
            break
    return result


def get_org_names(
    org_codes: list[str],
    *,
    fetch_details: Callable[[list[str]], dict[str, dict[str, object]]] = fetch_org_details,
) -> dict[str, str]:
    codes = sorted({code for code in org_codes if code})
    if not codes:
        return {}

    print(f"Fetching names for {len(codes)} organisations", file=sys.stderr)
    details = fetch_details(codes)
    result: dict[str, str] = {}
    for code, data in details.items():
        name = data["Organisation"].get("Name")
        if name:
            result[code] = format_org_name(str(name))
    return result


def resolve_ultimate_successors(successors_dict: dict[str, list[str]]) -> dict[str, list[str]]:
    ultimate: dict[str, list[str]] = {}
    for org_code, current_successors in successors_dict.items():
        final_successors: list[str] = []
        visited: set[str] = set()

        for successor in current_successors:
            if successor not in successors_dict or not successors_dict.get(successor):
                final_successors.append(successor)
                continue

            current = successor
            chain = [current]
            while current in successors_dict and successors_dict.get(current):
                if current in visited:
                    break
                visited.add(current)
                next_succ = successors_dict[current][0]
                if next_succ in chain:
                    break
                chain.append(next_succ)
                current = next_succ

            if chain:
                final_successors.append(chain[-1])

        ultimate[org_code] = list(dict.fromkeys(final_successors))
    return ultimate


def create_org_mapping_df(
    successors: dict[str, list[str]],
    predecessors: dict[str, list[str]],
    orgs_details: dict[str, dict[str, object]],
    icbs: dict[str, str],
    icb_regions: dict[str, str],
    region_names: dict[str, str],
    icb_names: dict[str, str],
) -> pd.DataFrame:
    ultimate_successors = resolve_ultimate_successors(successors)
    rows: list[dict[str, object]] = []

    for org_code, org_details in orgs_details.items():
        org = org_details["Organisation"]

        icb = icbs.get(org_code)
        icb_name = icb_names.get(icb) if icb else None
        region = icb_regions.get(icb) if icb else None
        region_name = region_names.get(region) if region else None

        legal: dict[str, str | None] = {}
        operational: dict[str, str | None] = {}
        for date in as_list(org.get("Date")):
            typ = date.get("Type")
            if not typ:
                continue
            entry = {
                "Start": str(date["Start"]) if date.get("Start") else None,
                "End": str(date["End"]) if date.get("End") else None,
            }
            if typ == "Legal":
                legal = entry
            elif typ == "Operational":
                operational = entry

        succ_vec = successors.get(org_code, [])
        pred_vec = predecessors.get(org_code, [])
        ult_vec = ultimate_successors.get(org_code, [])

        rows.append(
            {
                "ods_code": org_code,
                "ods_name": format_org_name(str(org.get("Name") or "")),
                "successors": ",".join(succ_vec) if succ_vec else None,
                "predecessors": ",".join(pred_vec) if pred_vec else None,
                "ultimate_successors": ",".join(ult_vec) if ult_vec else None,
                "legal_closed_date": legal.get("End"),
                "operational_closed_date": operational.get("End"),
                "legal_open_date": legal.get("Start"),
                "operational_open_date": operational.get("Start"),
                "postcode": get_nested(org, "GeoLoc", "Location", "PostCode"),
                "region_code": region,
                "region": region_name,
                "icb_code": icb,
                "icb": icb_name,
            }
        )

    return pd.DataFrame(rows, columns=OUTPUT_COLUMNS)


def run_pipeline(
    *,
    output_file: Path = OUTPUT_FILE,
    fetch_trusts: Callable[[], list[str]] = fetch_all_trusts_from_ord,
    fetch_details: Callable[[list[str]], dict[str, dict[str, object]]] = fetch_org_details,
) -> pd.DataFrame:
    print("Fetching NHS Trusts (RO197, RO24) from ORD API ...", file=sys.stderr)
    all_orgs = fetch_trusts()
    if not all_orgs:
        print("No organisations found.", file=sys.stderr)
        return pd.DataFrame(columns=OUTPUT_COLUMNS)

    print(f"Found {len(all_orgs)} trust organisations", file=sys.stderr)
    all_orgs_details = fetch_details(all_orgs)
    processed = process_org_details(all_orgs_details)

    icb_list = sorted(set(processed["icbs"].values()))
    icb_regions = get_icb_regions(icb_list, fetch_details=fetch_details)
    region_codes = sorted(set(icb_regions.values()))
    region_names = get_org_names(region_codes, fetch_details=fetch_details)
    icb_names = get_org_names(icb_list, fetch_details=fetch_details)

    org_df = create_org_mapping_df(
        processed["successors"],
        processed["predecessors"],
        processed["filtered_org_details"],
        processed["icbs"],
        icb_regions,
        region_names,
        icb_names,
    )

    # Oxleas (RPG): override region so hospital FP10 totals match SCMD.
    org_df.loc[org_df["ods_code"] == "RPG", ["region_code", "region"]] = ["Y59", "South East"]

    output_file.parent.mkdir(parents=True, exist_ok=True)
    org_df.to_csv(output_file, index=False, na_rep="NA")
    print(f"Saved {len(org_df)} organisation records to {output_file}", file=sys.stderr)
    return org_df


if __name__ == "__main__":
    run_pipeline()
