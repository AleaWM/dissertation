## Project: Flood Risk and Property Values


### Data Preparation Scripts

| File | Purpose | Inputs | Outputs | Status |
|------|---------|--------|---------|--------|
| `pull_parcel_sfs.R` | Download and convert Cook County parcel shapefiles for 2018 and 2023 | External shapefiles (downloaded via script) | `data/raw/parcels_2018.gpkg`, `data/raw/parcels_2023.gpkg` | Active |
| `pull_NFIP_redactedclaims.R` | Pull NFIP policy and claims data for Cook County from FEMA API | FEMA OpenFEMA API | `data/raw/nfippolicies_CookCounty_all.csv`, `data/raw/nfipclaims_CookCounty.csv` | Active |
| `pull_individual_assis` `tance_applicants.R`| Pull FEMA Individual Assistance (IA) application data for Cook County (accepted and denied) | FEMA IA API | `data/raw/indiv_assistance_CookCounty.csv` | Active |
| `pull_distinct_resPINs.R` | Extract all parcels that were residential from 2006–2023 and pull assessed values | PTAXSIM database | `data/raw/residential_pins_ever.csv`, `data/raw/pin_muni_key.csv`, `data/raw/muniname_taxcode_key.csv` |
| `pull_floodfactor` `_pin_list.R` | Join Flood Factor risk scores (2019 snapshot) to Cook County parcel universe | Parcel universe with PINs, Flood Factor data (external) | `data/raw/floodfactor_scores.csv` | Active |

---

### Exploratory & Descriptive Analysis

| File | Purpose | Inputs | Outputs | Status |
|------|---------|--------|---------|--------|
| `Flood_factor_pins.qmd` | Filter for PINs with Flood Factor ≥ 5; analyze post-2023 PTAX-245 flood appeals; calculates AV changes in affected land blocks | Parcel universe API, PTAX-245 appeal forms | Summary tables and figures | Active |
| `Flood_Appeal_PINs.qmd` | Identifies and summarizes AV appeal activity for PINs affected by 2023 flood; compares AV changes for appeal vs. neighbor parcels | PTAX-245 appeal forms, assessed values | Graphs of average AV changes, summary stats | Active |
| `CC_Individual_FEMA` `_Assistance.qmd` | Analyzes FEMA IA aid at ZIP level during recent flood events; descriptive stats on applicants and aid granted | `data/raw/indiv_assistance_CookCounty.csv` | ZIP-level aid summaries, descriptive statistics | Active |
| `Cook_Flood_Damage.qmd` | Summarizes NFIP claims and policies across geographies (ZIP, block, tract, muni) | `data/raw/nfipclaims_CookCounty.csv`, `data/raw/nfippolicies_CookCounty_all.csv` | Aggregated claim/policy stats | Active |
| `AssessedValue_Explor` `atoryGraphs.rmd` | Early exploration of AV trends in SFHA and flood-prone parcels using CCAO data | Parcel universe, AV data | Exploratory graphs and maps | Needs Review |
| `CookCounty_ParcelMaps.qmd` | Early mapping of parcels marked SFHA in CCAO data; not currently used | CCAO parcel universe | Static SFHA parcel maps | Deprecated |
| `Cook_Sales_data.qmd` | Links sales data with flood risk indicators (SFHA, LOMR, Flood Factor); identifies multi-sale parcels and treatment status | `data/raw/assessor_parcel_sales.csv`, `parcels_sfha_2018.gpkg`, `parcels_sfha_2024.gpkg`, `parcels_lomrs.gpkg`, `data/raw/pins_floodfactor_2019.csv` | Counts of PINs by treatment status, sales trends | Active |

---

### Data Cleaning Notes

Do filtering steps last. Keep all property classes until creating the panel dataset for analysis. 

## File Overview

---

### 1. `pull_parcel_sfs.R`  
**Purpose:**  
Downloads parcel shapefiles for Cook County for two time points: 2018 and 2023. Converts shapefiles into spatial databases.

**Output:**  
- `data/raw/parcels_2018.gpkg`  
- `data/raw/parcels_2023.gpkg`

**Status:** Active

---

### 2. `pull_NFIP_redactedclaims.R`  
**Purpose:**  
Pulls National Flood Insurance Program (NFIP) claims and policy data from FEMA’s open API, filtered to Cook County, IL.

**Output:**  
- `data/raw/nfippolicies_CookCounty_all.csv`  
- `data/raw/nfipclaims_CookCounty.csv`

**Status:** Active

---

### 3. `pull_individual_assistance_applicants.R`  
**Purpose:**  
Downloads FEMA Individual Assistance (IA) data from the OpenFEMA API for Cook County. Includes both approved and denied applications.

**Output:**  
- `data/raw/indiv_assistance_CookCounty.csv`

**Status:** Active

---

### 4. `pull_distinct_resPINs.R`  
**Purpose:**  
Extracts PINs that were residential at any time (2006–2023) using PTAXSIM and pulls corresponding assessed values. Also generates PIN–municipality key.

**Output:**  
- `data/raw/residential_pins_ever.csv`  
- `data/raw/pin_muni_key.csv`
- `data/raw/muniname_taxcode_key.csv`


**Status:** Active

---

### 5. `pull_floodfactor_pin_list.R`  
**Purpose:**  
Pulls 2019 Flood Factor scores and SFHA indicator used by Assessor from parcel universe. 

**Output:**  
- `data/raw/floodfactor_scores.csv` with variables 
`env_flood_factor_score`, `env_flood_factor_risk_direction`, `env_flood_fema_sfha`, 
and `nbhd_code` for the neighborhoods used by the Assessor during property valutation.

**Status:** Active

---

### 6. `Flood_factor_pins.qmd`  
**Purpose:**  
Filters for PINs with Flood Factor ≥ 5; integrates PTAX-245 appeal data after the 2023 flood; calculates differences in AV before and after appeals in affected land blocks.

**Output:**  
- Summary tables and figures on AV changes from appeals

**Status:** Active

---

### 7. `Flood_Appeal_PINs.qmd`  
**Purpose:**  
Identifies PINs that submitted PTAX-245 forms after the 2023 flood and compares changes in AV due to appeals across affected and nearby properties.

**Output:**  
- Graphs and summary tables of AV differences

**Status:** Active

---

### 8. `CC_Individual_FEMA_Assistance.qmd`  
**Purpose:**  
Summarizes FEMA IA data by ZIP code. Focuses on descriptive statistics and trends for recent flood events and aid received.

**Input:**  
- `data/raw/indiv_assistance_CookCounty.csv`

**Output:**  
- ZIP-level descriptive statistics  
- Summary tables and visualizations by event

**Status:** Active

---

### 9. `Cook_Flood_Damage.qmd`  
**Purpose:**  
Uses NFIP policy and claim data to summarize flood damage at various geographies (ZIP code, census block, census tract, municipality).

**Input:**  
- `data/raw/nfipclaims_CookCounty.csv`  
- `data/raw/nfippolicies_CookCounty_all.csv`

**Output:**  
- Aggregated stats by spatial unit

**Status:** Active

---

### 10. `Cook_Sales_data.qmd`  
**Purpose:**  
Integrates sales data with SFHA, LOMR, and Flood Factor overlays. Identifies PINs sold multiple times and tracks treatment exposure.

**Input:**  
- `data/raw/assessor_parcel_sales.csv`  
- `parcels_sfha_2018.gpkg`  
- `parcels_sfha_2024.gpkg`  
- `parcels_lomrs.gpkg`  
- `data/raw/pins_floodfactor_2019.csv`

**Output:**  
- Summary of sales by treatment group  
- Stats on repeat sales and exposure

**Status:** Active

---

### 11. `AssessedValue_ExploratoryGraphs.rmd`  
**Purpose:**  
Early exploration of AV trends for parcels inside and outside SFHAs using CCAO data. Includes preliminary figures and mapping.

**Input:**  
- Parcel universe  
- AV data

**Output:**  
- Exploratory maps and graphs of AV trends by risk status

**Status:** Needs Review

---

### 12. `CookCounty_ParcelMaps.qmd`  
**Purpose:**  
Initial mapping of parcels marked SFHA using CCAO parcel universe. Made for exploratory purposes and currently unused.

**Input:**  
- CCAO parcel universe

**Output:**  
- Parcel maps with SFHA indicators

**Status:** Deprecated

---


project/
│
├── data/
│   ├── raw/
│   ├── clean/
│   ├── joined/
│   ├── analysis/
│       └── parcel_panel.csv      <- final dataset
│
├── scripts/
│   ├── 01_clean_parcels.R
│   ├── 02_clean_sales.R
│   ├── 03_join_sfha.R
│   ├── 04_identify_treatment.R   <- defines in_sfha pre/post, LOMR, FF, treatment
│   ├── 05_create_panel.R         <- one row per PIN-year
│
├── analysis/
│   ├── diff_in_diff.qmd
│
├── README.md


Create a long-format dataset with:

PIN | year | in_sfha | flood_factor_score | in_lomr | av | sale_price | sold | treatment | post


Key Variables:
year: Matching the time of assessment or sale

in_sfha: 0/1 for floodplain presence that year

flood_factor_score: Ordinal flood risk score (e.g., 1–10)

in_lomr: 0/1 if parcel is in a LOMR revision area that year

av: Assessed value that year

sale_price: Sale price (if sold that year)

sold: 0/1 if there was a sale

treatment: 1 if the PIN was ever added to (or removed from) the SFHA (i.e., eligible for treatment)

post: 1 if observation is after the map change



added_to_sfha = (in_sfha_2024 == 1 & in_sfha_2018 == 0)
removed_from_sfha = (in_sfha_2024 == 0 & in_sfha_2018 == 1)


treatment = 1 if added_to_sfha | removed_from_sfha



If you're using multiple years of sales and assessed values, define:

- post = 1 for all years after the map change  

- pre = 1 for years before  

Optionally use event_time = year - treatment_year to do event study style DiD
