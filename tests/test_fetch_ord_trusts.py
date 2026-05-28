import pandas as pd

from analysis import fetch_ord_trusts


def test_format_org_name_strips_icb_suffix_and_title_cases() -> None:
    name = "NHS SOUTH EAST LONDON INTEGRATED CARE BOARD"
    assert fetch_ord_trusts.format_org_name(name) == "NHS South East London"


def test_resolve_ultimate_successors_follows_chains() -> None:
    successors = {
        "A": ["B"],
        "B": ["C"],
        "C": [],
    }

    ultimate = fetch_ord_trusts.resolve_ultimate_successors(successors)

    assert ultimate == {"A": ["C"], "B": ["C"], "C": []}


def test_process_org_details_keeps_england_and_manual_ryk_override() -> None:
    details = {
        "EN1": {
            "Organisation": {
                "GeoLoc": {"Location": {"Country": "ENGLAND"}},
                "Succs": {"Succ": {"Type": "Successor", "Target": {"OrgId": {"extension": "EN2"}}}},
                "Rels": {
                    "Rel": {
                        "id": fetch_ord_trusts.RE5,
                        "Status": "Active",
                        "Target": {"OrgId": {"extension": "ICB1"}},
                    }
                },
            }
        },
        "WL1": {
            "Organisation": {
                "GeoLoc": {"Location": {"Country": "WALES"}},
            }
        },
        "RYK": {
            "Organisation": {
                "GeoLoc": {"Location": {"Country": "ENGLAND"}},
            }
        },
        "TAJ": {
            "Organisation": {
                "GeoLoc": {"Location": {"Country": "ENGLAND"}},
            }
        },
    }

    processed = fetch_ord_trusts.process_org_details(details)

    assert set(processed["filtered_org_details"]) == {"EN1", "RYK", "TAJ"}
    assert processed["successors"]["RYK"] == ["TAJ"]
    assert processed["predecessors"]["TAJ"] == ["RYK"]
    assert processed["icbs"]["EN1"] == "ICB1"


def test_create_org_mapping_df_builds_expected_columns() -> None:
    orgs_details = {
        "R0A": {
            "Organisation": {
                "Name": "MANCHESTER UNIVERSITY NHS FOUNDATION TRUST",
                "Date": [
                    {"Type": "Legal", "Start": "2017-10-01", "End": None},
                    {"Type": "Operational", "Start": "2017-08-02", "End": None},
                ],
                "GeoLoc": {"Location": {"PostCode": "M13 9WL"}},
            }
        }
    }

    frame = fetch_ord_trusts.create_org_mapping_df(
        successors={},
        predecessors={"R0A": ["RW6", "RM2"]},
        orgs_details=orgs_details,
        icbs={"R0A": "QOP"},
        icb_regions={"QOP": "Y62"},
        region_names={"Y62": "North West"},
        icb_names={"QOP": "NHS Greater Manchester"},
    )

    row = frame.iloc[0]
    assert row["ods_code"] == "R0A"
    assert row["predecessors"] == "RW6,RM2"
    assert row["region_code"] == "Y62"
    assert row["icb"] == "NHS Greater Manchester"
    assert row["legal_open_date"] == "2017-10-01"
