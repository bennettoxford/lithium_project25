from pathlib import Path

from analysis import fetch_ord_practices


def write_xml(path: Path, organisations: str) -> None:
    path.write_text(
        f"""<?xml version="1.0" encoding="UTF-8"?>
<HSCOrgRefData>
  {organisations}
</HSCOrgRefData>
""",
        encoding="utf-8",
    )


def organisation(
    code: str,
    name: str,
    *,
    country: str = "ENGLAND",
    status: str = "Active",
    role_status: str = "Active",
    role_start: str = "2021-04-01",
    role_end: str = "",
    ref_only: bool = False,
    re6_target: str = "SUB1",
    re6_role: str = "RO98",
    re6_status: str = "Active",
    re6_start: str = "2021-04-01",
    re5_target: str = "",
    re5_role: str = "",
    re5_status: str = "Active",
    re5_start: str = "2021-04-01",
    include_ro76: bool = True,
) -> str:
    ref_only_attr = ' refOnly="true"' if ref_only else ""
    role_end_tag = f'<End value="{role_end}" />' if role_end else ""
    ro76_role = (
        f"""
      <Role id="RO76" uniqueRoleId="{code}-role">
        <Date>
          <Type value="Operational" />
          <Start value="{role_start}" />
          {role_end_tag}
        </Date>
        <Status value="{role_status}" />
      </Role>"""
        if include_ro76
        else ""
    )
    re5 = (
        f"""
      <Rel id="RE5" uniqueRelId="{code}-re5">
        <Date><Type value="Operational" /><Start value="{re5_start}" /></Date>
        <Status value="{re5_status}" />
        <Target>
          <OrgId extension="{re5_target}" />
          <PrimaryRoleId id="{re5_role}" />
        </Target>
      </Rel>"""
        if re5_target
        else ""
    )
    return f"""
  <Organisation orgRecordClass="RC1"{ref_only_attr}>
    <Name>{name}</Name>
    <OrgId extension="{code}" />
    <Status value="{status}" />
    <GeoLoc><Location><Country>{country}</Country></Location></GeoLoc>
    <Roles>{ro76_role}</Roles>
    <Rels>
      <Rel id="RE6" uniqueRelId="{code}-re6">
        <Date><Type value="Operational" /><Start value="{re6_start}" /></Date>
        <Status value="{re6_status}" />
        <Target>
          <OrgId extension="{re6_target}" />
          <PrimaryRoleId id="{re6_role}" />
        </Target>
      </Rel>
      {re5}
    </Rels>
  </Organisation>
"""


def test_stream_extracts_english_ro76_practice(tmp_path: Path) -> None:
    xml_path = tmp_path / "full.xml"
    write_xml(
        xml_path,
        organisation("A12345", "AMPERSAND &amp; PARTNERS", re5_target="REG1", re5_role="RO261"),
    )

    result = fetch_ord_practices.stream_organisations(xml_path, "Full", progress_every=10)

    assert len(result["practices"]) == 1
    parsed = result["practices"][0]
    assert parsed["practice"]["practice_code"] == "A12345"
    assert parsed["practice"]["ord_name"] == "AMPERSAND & PARTNERS"
    assert parsed["practice"]["sub_icb_code"] == "SUB1"
    assert parsed["practice"]["region_code"] == "REG1"
    assert parsed["periods"] == [
        {
            "practice_code": "A12345",
            "unique_role_id": "A12345-role",
            "role_status": "Active",
            "operational_start": "2021-04-01",
            "operational_end": "",
        }
    ]


def test_stream_filters_non_english_ro76_practices(tmp_path: Path) -> None:
    xml_path = tmp_path / "full.xml"
    write_xml(xml_path, organisation("W12345", "WELSH PRACTICE", country="WALES"))

    result = fetch_ord_practices.stream_organisations(xml_path, "Full", progress_every=10)

    assert result["practices"] == []
    assert result["org_rows"][0]["org_code"] == "W12345"


def test_stream_includes_english_org_without_ro76_role(tmp_path: Path) -> None:
    xml_path = tmp_path / "full.xml"
    write_xml(
        xml_path,
        organisation("A99999", "NO RO76 PRACTICE", include_ro76=False, re5_target="REG1", re5_role="RO261"),
    )

    result = fetch_ord_practices.stream_organisations(xml_path, "Full", progress_every=10)

    assert len(result["practices"]) == 1
    parsed = result["practices"][0]
    assert parsed["practice"]["practice_code"] == "A99999"
    assert parsed["periods"] == []


def test_merge_prefers_full_practice_and_marks_codes_seen_in_both() -> None:
    full_item = {
        "practice": {
            "practice_code": "A12345",
            "ord_name": "FULL NAME",
            "ord_status": "Active",
            "country": "ENGLAND",
            "ref_only": False,
            "sub_icb_code": "SUB1",
            "region_code": "",
            "region": "",
        },
        "periods": [
            {
                "practice_code": "A12345",
                "unique_role_id": "role-1",
                "role_status": "Active",
                "operational_start": "2021-04-01",
                "operational_end": "",
            }
        ],
    }
    archive_item = {
        "practice": {**full_item["practice"], "ord_name": "ARCHIVE NAME"},
        "periods": [{**full_item["periods"][0]}],
    }
    full = {
        "practices": [full_item],
        "org_rows": [
            {
                "org_code": "SUB1",
                "org_name": "SUB ICB",
                "country": "ENGLAND",
                "ref_only": False,
                "data_source": "full",
                "re5_target_org": "REG1",
                "re5_target_role": "RO261",
                "re6_target_org": "",
                "re6_target_role": "",
            },
            {
                "org_code": "REG1",
                "org_name": "NHS ENGLAND - SOUTH EAST",
                "country": "ENGLAND",
                "ref_only": False,
                "data_source": "full",
                "re5_target_org": "",
                "re5_target_role": "",
                "re6_target_org": "",
                "re6_target_role": "",
            },
        ],
    }
    archive = {"practices": [archive_item], "org_rows": []}

    practices, periods = fetch_ord_practices.merge_xml_sources(full, archive)

    assert practices[0]["ord_name"] == "FULL NAME"
    assert practices[0]["data_source"] == "both"
    assert practices[0]["region_code"] == "REG1"
    assert practices[0]["region"] == "South East"
    assert practices[0]["ro76_period_count"] == 1
    assert periods[0]["period_source"] == "full"


def test_normalise_region_checks_specific_legacy_names_first() -> None:
    assert (
        fetch_ord_practices.normalise_ord_region_name("NHS Midlands and East of England")
        == "Midlands and East of England"
    )


def test_resolve_trud_xml_uses_explicit_path(tmp_path: Path) -> None:
    xml_path = tmp_path / "custom_full.xml"
    xml_path.write_text("<HSCOrgRefData></HSCOrgRefData>", encoding="utf-8")

    resolved = fetch_ord_practices.resolve_trud_xml("Full", xml_path)

    assert resolved == xml_path.resolve()


def test_resolve_trud_xml_picks_latest_in_data_dir(tmp_path: Path, monkeypatch) -> None:
    monkeypatch.setattr(fetch_ord_practices, "DATA_DIR", tmp_path)
    (tmp_path / "HSCOrgRefData_Full_20260101.xml").write_text("a", encoding="utf-8")
    latest = tmp_path / "HSCOrgRefData_Full_20260421.xml"
    latest.write_text("b", encoding="utf-8")

    resolved = fetch_ord_practices.resolve_trud_xml("Full", None)

    assert resolved == latest.resolve()


def test_resolve_trud_xml_missing_raises(tmp_path: Path, monkeypatch) -> None:
    monkeypatch.setattr(fetch_ord_practices, "DATA_DIR", tmp_path)

    try:
        fetch_ord_practices.resolve_trud_xml("Archive", None)
    except FileNotFoundError as exc:
        assert "HSCOrgRefData_Archive" in str(exc)
    else:
        raise AssertionError("Expected FileNotFoundError")
