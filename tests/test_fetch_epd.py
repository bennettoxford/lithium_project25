from pathlib import Path

import pandas as pd
import pytest

from analysis import fetch_epd


def test_resources_from_package_filters_by_pattern_and_date() -> None:
    package = {
        "success": True,
        "result": {
            "resources": [
                {"name": "EPD_201501"},
                {"name": "EPD_201412"},
                {"name": "EPD_SNOMED_202507"},
                {"name": "OTHER_202501"},
            ]
        },
    }

    resources = fetch_epd.resources_from_package_response(
        package,
        201501,
        202506,
        is_snomed=False,
    )

    assert resources == [
        fetch_epd.Resource(resource_id="EPD_201501", yyyymm=201501, is_snomed=False),
    ]


def test_get_resources_combines_legacy_and_snomed_ranges(monkeypatch: pytest.MonkeyPatch) -> None:
    def fake_resources_from_package(
        dataset_id: str,
        min_yyyymm: int,
        max_yyyymm: int,
        *,
        is_snomed: bool,
    ) -> list[fetch_epd.Resource]:
        if dataset_id == fetch_epd.DATASET_ID_LEGACY:
            return [
                fetch_epd.Resource(resource_id="EPD_202506", yyyymm=202506, is_snomed=False),
            ]
        return [
            fetch_epd.Resource(
                resource_id="EPD_SNOMED_202507",
                yyyymm=202507,
                is_snomed=True,
            ),
        ]

    monkeypatch.setattr(fetch_epd, "resources_from_package", fake_resources_from_package)

    resources = fetch_epd.get_resources()

    assert [resource.yyyymm for resource in resources] == [202506, 202507]
    assert resources[0].is_snomed is False
    assert resources[1].is_snomed is True


def test_filter_lithium_uses_chemical_code_column() -> None:
    frame = pd.DataFrame(
        {
            "BNF_CHEMICAL_SUBSTANCE_CODE": ["0402030K0", "0402030Q0"],
            "TOTAL_QUANTITY": [10, 20],
        }
    )

    filtered = fetch_epd.filter_lithium(frame)

    assert filtered["BNF_CHEMICAL_SUBSTANCE_CODE"].tolist() == ["0402030K0"]
    assert filtered["TOTAL_QUANTITY"].tolist() == [10]


def test_select_epd_output_columns_keeps_legacy_schema_only() -> None:
    frame = pd.DataFrame(
        {
            "PRACTICE_CODE": ["A12345"],
            "TOTAL_QUANTITY": [5],
            "ITEMS": [1],
            "ACTUAL_COST": [2.5],
        }
    )

    selected = fetch_epd.select_epd_output_columns(frame)

    assert list(selected.columns) == ["PRACTICE_CODE", "TOTAL_QUANTITY"]


def test_build_sql_orders_by_business_key() -> None:
    sql = fetch_epd.build_sql("EPD_201512", offset=32000)

    assert "SELECT * FROM `EPD_201512`" in sql
    assert (
        "ORDER BY YEAR_MONTH, PRACTICE_CODE, BNF_CODE, QUANTITY, ITEMS, TOTAL_QUANTITY, "
        "NIC, ACTUAL_COST, BNF_DESCRIPTION" in sql
    )
    assert "LIMIT 32000 OFFSET 32000" in sql


def test_build_snomed_sql_orders_by_business_key() -> None:
    sql = fetch_epd.build_snomed_sql("EPD_SNOMED_202507", offset=32000)

    assert "SELECT * FROM `EPD_SNOMED_202507`" in sql
    assert "WHERE BNF_CHEMICAL_SUBSTANCE_CODE IN ('0402030K0', '0402030P0')" in sql
    assert (
        "ORDER BY YEAR_MONTH, PRACTICE_CODE, BNF_PRESENTATION_CODE, QUANTITY, ITEMS, "
        "TOTAL_QUANTITY, NIC, ACTUAL_COST, BNF_PRESENTATION_NAME"
        in sql
    )
    assert "LIMIT 32000 OFFSET 32000" in sql


def test_fetch_month_legacy_fails_when_ordered_query_fails(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    calls: list[int] = []

    def fake_fetch_page_legacy(
        _resource_id: str,
        _offset: int = 0,
    ) -> dict[str, object]:
        calls.append(_offset)
        raise fetch_epd.HTTPError(
            url="https://example.com",
            code=500,
            msg="boom",
            hdrs=None,
            fp=None,
        )

    monkeypatch.setattr(fetch_epd, "fetch_page_legacy", fake_fetch_page_legacy)

    with pytest.raises(RuntimeError, match="Legacy EPD query failed"):
        fetch_epd.fetch_month_legacy("EPD_201501")
    assert calls == [0]


def test_fetch_month_legacy_raises_on_duplicate_rows(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    def fake_fetch_page_legacy(
        _resource_id: str,
        _offset: int = 0,
    ) -> dict[str, object]:
        if _offset == 0:
            return {
                "result": {
                    "records": [
                        {"PRACTICE_CODE": "A12345", "TOTAL_QUANTITY": 1},
                        {"PRACTICE_CODE": "A12345", "TOTAL_QUANTITY": 1},
                    ]
                }
            }
        return {"result": {"records": []}}

    monkeypatch.setattr(fetch_epd, "fetch_page_legacy", fake_fetch_page_legacy)

    with pytest.raises(RuntimeError, match="returned duplicate rows"):
        fetch_epd.fetch_month_legacy("EPD_201501")


def test_fetch_month_snomed_fails_when_ordered_sql_query_fails(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    calls: list[int] = []

    def fake_fetch_page_snomed_sql(
        _resource_id: str,
        _offset: int = 0,
    ) -> dict[str, object]:
        calls.append(_offset)
        raise fetch_epd.HTTPError(
            url="https://example.com",
            code=500,
            msg="boom",
            hdrs=None,
            fp=None,
        )

    monkeypatch.setattr(fetch_epd, "fetch_page_snomed_sql", fake_fetch_page_snomed_sql)

    with pytest.raises(RuntimeError, match="SNOMED EPD query failed"):
        fetch_epd.fetch_month_snomed("EPD_SNOMED_202507")
    assert calls == [0]


def test_normalize_snomed_to_legacy_maps_columns() -> None:
    frame = pd.DataFrame(
        {
            "YEAR_MONTH": ["2025-10"],
            "BNF_PRESENTATION_CODE": ["0402030K0123"],
            "BNF_PRESENTATION_NAME": ["Lithium carbonate 250mg tablets"],
            "BNF_CHEMICAL_SUBSTANCE": ["Lithium carbonate"],
            "BNF_CHEMICAL_SUBSTANCE_CODE": ["0402030K0"],
            "ADQ_USAGE": [1.5],
            "PRACTICE_CODE": ["A12345"],
            "TOTAL_QUANTITY": [3],
        }
    )

    normalized = fetch_epd.normalize_snomed_to_legacy(frame, 202510)

    assert normalized["YEAR_MONTH"].tolist() == ["202510"]
    assert normalized["BNF_CODE"].tolist() == ["0402030K0123"]
    assert normalized["BNF_CHEMICAL_SUBSTANCE"].tolist() == ["0402030K0"]
    assert normalized["CHEMICAL_SUBSTANCE_BNF_DESCR"].tolist() == ["Lithium carbonate"]
    assert normalized["ADQUSAGE"].tolist() == [1.5]


def test_run_pipeline_keeps_all_practices_and_writes_csv(tmp_path: Path) -> None:
    resources = [
        fetch_epd.Resource(resource_id="EPD_202401", yyyymm=202401, is_snomed=False),
    ]

    def fetch_month(_resource: fetch_epd.Resource) -> pd.DataFrame:
        return pd.DataFrame(
            {
                "YEAR_MONTH": ["202401", "202401"],
                "PRACTICE_CODE": ["A12345", "Z99999"],
                "BNF_CODE": ["0402030K0123", "0402030K0123"],
                "TOTAL_QUANTITY": [1, 2],
            }
        )

    fetch_epd.run_pipeline(
        resources=resources,
        output_dir=tmp_path,
        fetch_month_func=fetch_month,
        sleep_seconds=0,
    )

    output = pd.read_csv(tmp_path / "epd_lithium_202401.csv")
    assert output["PRACTICE_CODE"].tolist() == ["A12345", "Z99999"]
    assert output["TOTAL_QUANTITY"].tolist() == [1, 2]


def test_run_pipeline_raises_on_month_failure(tmp_path: Path) -> None:
    resources = [
        fetch_epd.Resource(resource_id="EPD_202401", yyyymm=202401, is_snomed=False),
    ]

    def failing_fetch_month(_resource: fetch_epd.Resource) -> pd.DataFrame:
        raise ValueError("upstream timeout")

    with pytest.raises(RuntimeError, match="Failed to fetch 202401"):
        fetch_epd.run_pipeline(
            resources=resources,
            output_dir=tmp_path,
            fetch_month_func=failing_fetch_month,
            sleep_seconds=0,
        )
