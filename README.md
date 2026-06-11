# Lithium prescribing in England

Analysis of lithium prescribing across three data sources in NHS England:

- **Primary care** — English Prescribing Dataset (EPD), GP practices only
- **Secondary care** — Secondary Care Medicines Data (SCMD)
- **Hospital FP10** — hospital prescribing dispensed in the community


## Requirements

| Tool | Version | Purpose |
|------|---------|---------|
| [R](https://cran.r-project.org/) | 4.5+ (see `renv.lock`) | Analysis and plotting |
| [renv](https://rstudio.github.io/renv/) | — | Reproducible R dependencies |
| [Python](https://www.python.org/) | ≥ 3.11 | Data fetching |
| [uv](https://docs.astral.sh/uv/) | — | Python package management |

## Project layout

```
analysis/          R analysis scripts and Python fetchers
data/              Input datasets
output/            Generated tables and plots
tests/             pytest tests for Python fetch logic
```

## Setup

### R

Open `lithium_project25.Rproj` in RStudio, then:

```r
renv::restore()
```

### Python


```
uv sync
```

## Fetching data

### 1. GP practice list

Get ODS XML Organisation Data from [TRUD](https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/5/items/341/releases). Add **HSCOrgRefData** full and archive XML files under `data/trud/`.

```
uv run python analysis/fetch_ord_practices.py
```

Writes:

- `output/data/ord_practices.csv`
- `output/data/ord_ro76_practice_periods.csv`

### 2. Primary care EPD

Queries [NHSBSA open data](https://opendata.nhsbsa.net/) for BNF lithium carbonate (`0402030K0`) and citrate (`0402030P0`) and saves all lithium rows from EPD.

```
uv run python analysis/fetch_epd.py
```

Writes monthly files to `data/primary_care/epd_lithium_YYYYMM.csv` (Jan 2015 – Dec 2025). Legacy EPD (BNF) is used through June 2025; SNOMED EPD from July 2025.

### 3. Hospital FP10

```
uv run python analysis/fetch_fp10.py
```

Writes `data/secondary_care_fp10/fp10_YYYYMM.csv` (Jan 2017 – Dec 2025).

### 4. NHS trust regions (hospital FP10 analysis)

```
uv run python analysis/fetch_ord_trusts.py
```

Writes `data/ord_trusts.csv` (trust ODS prefix → NHS England region).


## Running the analysis

Run the [fetching steps](#fetching-data) first so EPD, FP10, ORD practice, and trust lookup files exist.

```r
source("analysis/run_all.R")
```

Pipeline order:

1. `00_region_populations.R` — ONS population by NHS region → `output/data/ons_nhs_england_region_population_estimates.csv`  
2. `01_setup.R` — shared libraries, region helpers, population joins  
3. `02_unique_products.R` — product catalogue across sources  
4. `03_primary_analysis.R` — primary care (EPD) DDD, 2015–2024 
5. `04_secondary_analysis.R` — secondary care (SCMD) DDD, 2019–2024  
6. `05_fp10_analysis.R` — hospital FP10 DDD, 2017–2024  
7. `06_combined_analysis.R` — combined trends and maps (reads CSVs from steps 4–6)  


## Tests

```
uv run pytest
```

## DDD methodology

Defined daily dose (DDD) is derived from dispensed quantity and strength mapping in `data/primary_care_fp10_products_strength.csv`:

- Lithium carbonate: mmol = mg / 37.04  
- Lithium citrate: mmol = mg / 94.26  
- DDD = mmol / 24  

Regional rates use ONS mid-year population estimates per NHS England region.

## Data sources

- [NHSBSA Open Data](https://opendata.nhsbsa.net/) — EPD and hospital FP10  
- [NHS Digital TRUD](https://isd.digital.nhs.uk/trud/) — ODS organisation reference (HSCOrgRefData)  
- [ONS population estimates](https://www.ons.gov.uk/) — regional denominators  
- [OpenPrescribing Hospitals](https://hospitals.openprescribing.net) — processed secondary care SCMD  
- [NHS Organisation Data Service (ORD)](https://digital.nhs.uk/services/organisation-data-service) — GP practices and trusts
