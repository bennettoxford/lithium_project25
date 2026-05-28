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
            "ITEMS": [1, 2],
        }
    )

    filtered = fetch_epd.filter_lithium(frame)

    assert filtered["BNF_CHEMICAL_SUBSTANCE_CODE"].tolist() == ["0402030K0"]
    assert filtered["ITEMS"].tolist() == [1]


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
            "ITEMS": [3],
        }
    )

    normalized = fetch_epd.normalize_snomed_to_legacy(frame, 202510)

    assert normalized["YEAR_MONTH"].tolist() == ["202510"]
    assert normalized["BNF_CODE"].tolist() == ["0402030K0123"]
    assert normalized["BNF_CHEMICAL_SUBSTANCE"].tolist() == ["0402030K0"]
    assert normalized["CHEMICAL_SUBSTANCE_BNF_DESCR"].tolist() == ["Lithium carbonate"]
    assert normalized["ADQUSAGE"].tolist() == [1.5]


def test_run_pipeline_filters_practices_and_writes_csv(tmp_path: Path) -> None:
    resources = [
        fetch_epd.Resource(resource_id="EPD_202401", yyyymm=202401, is_snomed=False),
    ]

    def fetch_month(_resource: fetch_epd.Resource) -> pd.DataFrame:
        return pd.DataFrame(
            {
                "PRACTICE_CODE": ["A12345", "Z99999"],
                "ITEMS": [1, 2],
            }
        )

    fetch_epd.run_pipeline(
        resources=resources,
        output_dir=tmp_path,
        practice_codes={"A12345"},
        fetch_month_func=fetch_month,
        sleep_seconds=0,
    )

    output = pd.read_csv(tmp_path / "epd_lithium_202401.csv")
    assert output["PRACTICE_CODE"].tolist() == ["A12345"]
    assert output["ITEMS"].tolist() == [1]
