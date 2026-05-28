from pathlib import Path

import pandas as pd

from analysis import fetch_fp10


def test_get_resources_parses_and_dedupes_monthly_resources() -> None:
    package = {
        "success": True,
        "result": {
            "resources": [
                {
                    "name": "MONTHLY HOSPITAL DATA JAN17",
                    "url": "https://example.test/jan17.csv",
                },
                {
                    "name": "HOSPITAL_DISP_COMMUNITY_201701",
                    "url": "https://example.test/jan17-new.csv",
                },
                {
                    "name": "HOSPITAL_DISP_COMMUNITY_202505FINAL",
                    "url": "https://example.test/may25.csv",
                },
                {
                    "name": "HOSPITAL_DISP_COMMUNITY_201612",
                    "url": "https://example.test/too-old.csv",
                },
            ]
        },
    }

    resources = fetch_fp10.resources_from_package(package)

    assert resources == [
        fetch_fp10.Resource(
            name="HOSPITAL_DISP_COMMUNITY_201701",
            yyyymm=201701,
            url="https://example.test/jan17-new.csv",
        ),
        fetch_fp10.Resource(
            name="HOSPITAL_DISP_COMMUNITY_202505FINAL",
            yyyymm=202505,
            url="https://example.test/may25.csv",
        ),
    ]


def test_filter_lithium_bnf_keeps_only_lithium_rows() -> None:
    frame = pd.DataFrame(
        {
            "BNF_CODE": ["0402030K0123", "0101010A0123", "0402030P0456"],
            "TOTAL_QUANTITY": [1, 2, 3],
        }
    )

    filtered = fetch_fp10.filter_lithium_bnf(frame)

    assert filtered["BNF_CODE"].tolist() == ["0402030K0123", "0402030P0456"]
    assert filtered["TOTAL_QUANTITY"].tolist() == [1, 3]


def test_select_fp10_output_columns_keeps_analysis_columns_only() -> None:
    frame = pd.DataFrame(
        {
            "PERIOD": [202401],
            "BNF_CODE": ["0402030K0123"],
            "BNF_NAME": ["Lithium carbonate 250mg tablets"],
            "HOSPITAL_TRUST_CODE": ["R1H"],
            "HOSPITAL_TRUST": ["Example Trust"],
            "TOTAL_QUANTITY": [5],
            "TOTAL_ITEMS": [1],
            "TOTAL_ACTUAL_COST": [2.5],
            "TOTAL_NIC": [3.0],
        }
    )

    selected = fetch_fp10.select_fp10_output_columns(frame)

    assert list(selected.columns) == fetch_fp10.FP10_OUTPUT_COLUMNS


def test_fetch_month_downloads_and_filters_csv() -> None:
    resource = fetch_fp10.Resource(
        name="HOSPITAL_DISP_COMMUNITY_202401",
        yyyymm=202401,
        url="https://example.test/202401.csv",
    )

    def fetch_csv(_url: str) -> pd.DataFrame:
        return pd.DataFrame({"BNF_CODE": ["0402030K0123"], "TOTAL_QUANTITY": [1]})

    result = fetch_fp10.fetch_month(resource, fetch_csv=fetch_csv)

    assert result["TOTAL_QUANTITY"].tolist() == [1]


def test_run_pipeline_writes_one_csv_per_month(tmp_path: Path) -> None:
    resources = [
        fetch_fp10.Resource(name="HOSPITAL_DISP_COMMUNITY_202401", yyyymm=202401, url="url-1"),
        fetch_fp10.Resource(name="HOSPITAL_DISP_COMMUNITY_202402", yyyymm=202402, url="url-2"),
    ]

    def fetch_month(resource: fetch_fp10.Resource) -> pd.DataFrame:
        return pd.DataFrame(
            {
                "PERIOD": [resource.yyyymm],
                "BNF_CODE": ["0402030K0123"],
                "BNF_NAME": ["Lithium carbonate 250mg tablets"],
                "HOSPITAL_TRUST_CODE": ["R1H"],
                "HOSPITAL_TRUST": ["Example Trust"],
                "TOTAL_QUANTITY": [1],
            }
        )

    fetch_fp10.run_pipeline(
        resources=resources,
        output_dir=tmp_path,
        fetch_month_func=fetch_month,
        sleep_seconds=0,
    )

    assert sorted(path.name for path in tmp_path.iterdir()) == ["fp10_202401.csv", "fp10_202402.csv"]
    output = pd.read_csv(tmp_path / "fp10_202402.csv")
    assert output["PERIOD"].tolist() == [202402]
    assert list(output.columns) == fetch_fp10.FP10_OUTPUT_COLUMNS
