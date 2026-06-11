import argparse
import sys
import time
from collections import defaultdict
from pathlib import Path
from typing import Iterable
from xml.etree import ElementTree as ET

import pandas as pd


PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = PROJECT_ROOT / "data" / "trud"
OUT_DIR = PROJECT_ROOT / "output" / "data"

PRACTICES_CSV = OUT_DIR / "ord_ro76_practices.csv"
PERIODS_CSV = OUT_DIR / "ord_ro76_practice_periods.csv"

GP_PRACTICE_ROLE = "RO76"
INCLUDED_COUNTRY = "ENGLAND"
COMMISSIONER_ROLE = "RO98"
REGION_ROLE = "RO261"
REL_LOCATED_IN_GEOGRAPHY = "RE5"
REL_OPERATED_BY = "RE6"

PRACTICE_FIELDS = [
    "practice_code",
    "ord_name",
    "ord_status",
    "country",
    "ref_only",
    "sub_icb_code",
    "region_code",
    "region",
    "data_source",
    "ro76_period_count",
]

PERIOD_FIELDS = [
    "practice_code",
    "period_id",
    "unique_role_id",
    "role_status",
    "operational_start",
    "operational_end",
    "period_source",
]


def resolve_trud_xml(kind: str, path: Path | None = None) -> Path:
    """Resolve a TRUD HSCOrgRefData XML path, optionally from an explicit path."""
    if path is not None:
        resolved = path.expanduser().resolve()
        if not resolved.is_file():
            raise FileNotFoundError(f"TRUD {kind} XML not found: {resolved}")
        return resolved

    candidates = sorted(DATA_DIR.glob(f"HSCOrgRefData_{kind}_*.xml"))
    if not candidates:
        raise FileNotFoundError(
            f"No HSCOrgRefData_{kind}_*.xml found in {DATA_DIR}. "
            f"Download from TRUD and place under data/trud/, or pass --{kind.lower()}."
        )
    return candidates[-1]


def child_text(element: ET.Element, name: str) -> str:
    """Return the stripped text for a direct child element, if present."""
    child = element.find(name)
    return (child.text or "").strip() if child is not None else ""


def find_text(element: ET.Element, path: str) -> str:
    """Return the stripped text for the first element matching an XML path."""
    descendant = element.find(path)
    return (descendant.text or "").strip() if descendant is not None else ""


def child_attr(element: ET.Element, name: str, attr: str) -> str:
    """Return an attribute from a direct child element, if present."""
    child = element.find(name)
    return child.get(attr, "") if child is not None else ""


def normalise_country(country: str) -> str:
    """Normalise country text before filtering."""
    return country.strip().upper()


def is_included_country(country: str) -> bool:
    """Return whether an organisation is in the selected country."""
    return normalise_country(country) == INCLUDED_COUNTRY


def normalise_ord_region_name(name: str) -> str:
    """Map ORD region organisation names onto the analysis region labels."""
    value = name.strip().upper()
    if not value:
        return ""

    region_patterns = [
        ("MIDLANDS AND EAST OF ENGLAND", "Midlands and East of England"),
        ("NORTH EAST AND YORKSHIRE", "North East and Yorkshire"),
        ("NORTH WEST", "North West"),
        ("EAST OF ENGLAND", "East of England"),
        ("MIDLANDS", "Midlands"),
        ("LONDON", "London"),
        ("SOUTH EAST", "South East"),
        ("SOUTH WEST", "South West"),
        ("NORTH OF ENGLAND", "North of England"),
        ("SOUTH OF ENGLAND", "South of England"),
    ]
    for pattern, label in region_patterns:
        if pattern in value:
            return label
    return ""


def should_replace_relationship(candidate: dict[str, str], current: dict[str, str] | None) -> bool:
    """Choose the active relationship, or otherwise the most recent one."""
    if current is None:
        return True

    if (candidate["status"] == "Active") != (current["status"] == "Active"):
        return candidate["status"] == "Active"
    return candidate["operational_start"] >= current["operational_start"]


def should_replace_org_record(candidate_ref_only: bool, candidate_source: str, current_ref_only: bool, current_source: str) -> bool:
    """Prefer complete records, then the full XML file over the archive."""
    if candidate_ref_only != current_ref_only:
        return not candidate_ref_only
    return candidate_source == "full" and current_source != "full"


def operational_period(element: ET.Element) -> tuple[str, str]:
    """Return the operational start and end dates for a role or relationship."""
    for date in element.findall("Date"):
        date_type = child_attr(date, "Type", "value")
        if date_type == "Operational":
            return child_attr(date, "Start", "value"), child_attr(date, "End", "value")
    return "", ""


def parse_ro76_periods(organisation: ET.Element, practice_code: str) -> list[dict[str, object]]:
    """Extract all GP practice role periods from an organisation block."""
    roles = organisation.find("Roles")
    if roles is None:
        return []

    periods: list[dict[str, object]] = []
    for role in roles.findall("Role"):
        if role.get("id") != GP_PRACTICE_ROLE:
            continue

        start, end = operational_period(role)
        periods.append(
            {
                "practice_code": practice_code,
                "unique_role_id": role.get("uniqueRoleId", ""),
                "role_status": child_attr(role, "Status", "value"),
                "operational_start": start,
                "operational_end": end,
            }
        )
    return periods


def parse_relationships(organisation: ET.Element) -> dict[str, dict[str, str]]:
    """Extract the best RE5 and RE6 target relationships for an organisation."""
    rels = organisation.find("Rels")
    if rels is None:
        return {}

    best: dict[str, dict[str, str]] = {}
    for rel in rels.findall("Rel"):
        rel_id = rel.get("id", "")
        if rel_id not in {REL_LOCATED_IN_GEOGRAPHY, REL_OPERATED_BY}:
            continue

        target_org = rel.find(".//OrgId")
        if target_org is None or not target_org.get("extension"):
            continue

        target_role = rel.find(".//PrimaryRoleId")
        start, _ = operational_period(rel)
        candidate = {
            "target_org": target_org.get("extension", ""),
            "target_role": target_role.get("id", "") if target_role is not None else "",
            "status": child_attr(rel, "Status", "value"),
            "operational_start": start,
        }

        if should_replace_relationship(candidate, best.get(rel_id)):
            best[rel_id] = candidate

    return best


def parse_organisation(organisation: ET.Element, source: str) -> tuple[dict[str, object] | None, dict[str, object]]:
    """Parse one organisation block into practice and lookup rows."""
    practice_code = child_attr(organisation, "OrgId", "extension")
    country = normalise_country(find_text(organisation, ".//GeoLoc//Country"))
    relationships = parse_relationships(organisation)

    re5 = relationships.get(REL_LOCATED_IN_GEOGRAPHY, {})
    re6 = relationships.get(REL_OPERATED_BY, {})
    org_row = {
        "org_code": practice_code,
        "org_name": child_text(organisation, "Name"),
        "country": country,
        "ref_only": organisation.get("refOnly") == "true",
        "data_source": source,
        "re5_target_org": re5.get("target_org", ""),
        "re5_target_role": re5.get("target_role", ""),
        "re6_target_org": re6.get("target_org", ""),
        "re6_target_role": re6.get("target_role", ""),
    }

    periods = parse_ro76_periods(organisation, practice_code)
    org_row["_has_ro76"] = bool(periods)
    if not is_included_country(country) or not practice_code:
        return None, org_row

    sub_icb_code = re6.get("target_org", "") if re6.get("target_role") == COMMISSIONER_ROLE else ""
    region_code = re5.get("target_org", "") if re5.get("target_role") == REGION_ROLE else ""
    practice = {
        "practice_code": practice_code,
        "ord_name": org_row["org_name"],
        "ord_status": child_attr(organisation, "Status", "value"),
        "country": country,
        "ref_only": org_row["ref_only"],
        "sub_icb_code": sub_icb_code,
        "region_code": region_code,
        "region": "",
    }
    return {"practice": practice, "periods": periods}, org_row


def stream_organisations(path: Path, label: str, progress_every: int = 50_000) -> dict[str, list[dict[str, object]]]:
    """Stream a HSCOrgRefData XML file and collect English organisations."""
    if not path.exists():
        raise FileNotFoundError(f"XML file not found: {path}")

    source = label.lower()
    print(f"Streaming {label}: {path}", file=sys.stderr)
    started = time.monotonic()
    practices: list[dict[str, object]] = []
    org_rows: list[dict[str, object]] = []
    org_count = 0
    non_england_count = 0

    for _event, element in ET.iterparse(path, events=("end",)):
        if element.tag != "Organisation":
            continue

        parsed, org_row = parse_organisation(element, source)
        org_count += 1
        org_rows.append(org_row)
        if parsed is not None:
            practices.append(parsed)
        elif not is_included_country(str(org_row["country"])):
            non_england_count += 1

        if org_count % progress_every == 0:
            elapsed_minutes = (time.monotonic() - started) / 60
            print(
                f"  {label}: {org_count:,} organisations read "
                f"({len(practices):,} ENGLAND orgs, {elapsed_minutes:.1f}m)",
                file=sys.stderr,
            )

        element.clear()

    print(
        f"  {label} done: {len(practices):,} ENGLAND orgs "
        f"(excluded {non_england_count:,} non-England) from {org_count:,} organisation blocks",
        file=sys.stderr,
    )
    return {"practices": practices, "org_rows": org_rows}


def best_org_rows(org_rows: Iterable[dict[str, object]]) -> dict[str, dict[str, object]]:
    """Keep the preferred organisation row for each organisation code."""
    best: dict[str, dict[str, object]] = {}
    for row in org_rows:
        code = str(row["org_code"])
        if not code:
            continue

        existing = best.get(code)
        if existing is None or should_replace_org_record(
            bool(row["ref_only"]),
            str(row["data_source"]),
            bool(existing["ref_only"]),
            str(existing["data_source"]),
        ):
            best[code] = row
    return best


def dedupe_periods(periods_by_code: dict[str, list[dict[str, object]]]) -> list[dict[str, object]]:
    """Remove duplicate practice periods, preferring rows from the full XML."""
    output: list[dict[str, object]] = []
    for code in sorted(periods_by_code):
        chosen: dict[tuple[str, str, str, str], dict[str, object]] = {}
        for row in periods_by_code[code]:
            key = (
                str(row.get("unique_role_id") or ""),
                str(row.get("operational_start") or ""),
                str(row.get("operational_end") or ""),
                str(row.get("role_status") or ""),
            )
            existing = chosen.get(key)
            if existing is None or row["period_source"] == "full":
                chosen[key] = {**row, "practice_code": code}

        rows = sorted(
            chosen.values(),
            key=lambda row: (
                str(row.get("unique_role_id") or ""),
                str(row.get("operational_start") or ""),
                str(row.get("operational_end") or ""),
            ),
        )
        for period_id, row in enumerate(rows, start=1):
            output.append({**row, "period_id": period_id})

    return output


def merge_xml_sources(
    full_results: dict[str, list[dict[str, object]]],
    archive_results: dict[str, list[dict[str, object]]],
) -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    """Merge full and archive XML extracts into final practice and period rows."""
    practice_map: dict[str, dict[str, object]] = {}
    practice_sources: dict[str, set[str]] = defaultdict(set)
    periods_by_code: dict[str, list[dict[str, object]]] = defaultdict(list)

    for source, batch in (
        ("full", full_results["practices"]),
        ("archive", archive_results["practices"]),
    ):
        for item in batch:
            practice = item["practice"]
            item_periods = item["periods"]
            code = str(practice["practice_code"])
            practice_sources[code].add(source)

            existing = practice_map.get(code)
            if existing is None or should_replace_org_record(
                bool(practice["ref_only"]),
                source,
                bool(existing["ref_only"]),
                str(existing["_source"]),
            ):
                practice_map[code] = {**practice, "_source": source}

            periods_by_code[code].extend({**period, "period_source": source} for period in item_periods)

    periods = dedupe_periods(periods_by_code)
    period_counts = defaultdict(int)
    for period in periods:
        period_counts[str(period["practice_code"])] += 1

    org_lookup = best_org_rows([*full_results["org_rows"], *archive_results["org_rows"]])
    practices: list[dict[str, object]] = []
    for code in sorted(practice_map):
        practice = {key: value for key, value in practice_map[code].items() if key != "_source"}
        sub_icb = str(practice.get("sub_icb_code") or "")
        region_code = str(practice.get("region_code") or "")
        if not region_code and sub_icb:
            sub_icb_row = org_lookup.get(sub_icb)
            if sub_icb_row and sub_icb_row.get("re5_target_role") == REGION_ROLE:
                region_code = str(sub_icb_row.get("re5_target_org") or "")

        region_name = ""
        if region_code:
            region_row = org_lookup.get(region_code)
            if region_row:
                region_name = normalise_ord_region_name(str(region_row.get("org_name") or ""))

        sources = practice_sources[code]
        data_source = "both" if len(sources) > 1 else next(iter(sources))
        practices.append(
            {
                **practice,
                "region_code": region_code,
                "region": region_name,
                "data_source": data_source,
                "ro76_period_count": period_counts[code],
            }
        )

    return practices, periods


def write_csv(path: Path, rows: list[dict[str, object]], fieldnames: list[str]) -> None:
    """Write rows to CSV with the requested column order."""
    path.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows, columns=fieldnames).to_csv(path, index=False)


def run_pipeline(
    *,
    full_xml: Path | None = None,
    archive_xml: Path | None = None,
) -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    """Run the ORD practice extraction and write both output CSVs."""
    full_path = resolve_trud_xml("Full", full_xml)
    archive_path = resolve_trud_xml("Archive", archive_xml)
    print(f"Using Full XML: {full_path}", file=sys.stderr)
    print(f"Using Archive XML: {archive_path}", file=sys.stderr)

    full_results = stream_organisations(full_path, "Full")
    archive_results = stream_organisations(archive_path, "Archive")
    practices, periods = merge_xml_sources(full_results, archive_results)

    write_csv(PRACTICES_CSV, practices, PRACTICE_FIELDS)
    write_csv(PERIODS_CSV, periods, PERIOD_FIELDS)
    return practices, periods


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Extract English organisations and RO76 role periods from TRUD ODS XML."
    )
    parser.add_argument(
        "--full",
        type=Path,
        default=None,
        help="Path to HSCOrgRefData_Full_*.xml (default: latest in data/trud/)",
    )
    parser.add_argument(
        "--archive",
        type=Path,
        default=None,
        help="Path to HSCOrgRefData_Archive_*.xml (default: latest in data/trud/)",
    )
    return parser.parse_args(argv)


if __name__ == "__main__":
    args = parse_args()
    run_pipeline(full_xml=args.full, archive_xml=args.archive)
