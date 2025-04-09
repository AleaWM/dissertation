## Project: Flood Risk and Property Values

DISCLAIMER: This is made from information I dropped into ChatGPT and it summarized it for me and I pasted it in here. 
I have not had time to go through it and check the information in the table. 


### New List

#### Data Preparation Scripts

| File | Purpose | Inputs | Outputs | Status |
|------|---------|--------|---------|--------|
| `pull_parcel_sfs.R` | Download and convert Cook County parcel shapefiles for 2018 and 2023 | External shapefiles (downloaded via script) | `data/raw/parcels_2018.gpkg`, `data/raw/parcels_2023.gpkg` | Active |
| `pull_NFIP_redactedclaims.R` | Pull NFIP policy and claims data for Cook County from FEMA API | FEMA OpenFEMA API | `data/raw/nfippolicies_CookCounty_all.csv`, `data/raw/nfipclaims_CookCounty.csv` | Active |
| `pull_individual_assistance_applicants.R` | Pull FEMA Individual Assistance (IA) application data for Cook County (accepted and denied) | FEMA IA API | `data/raw/indiv_assistance_CookCounty.csv` | Active |
| `pull_distinct_resPINs.R` | Extract all parcels that were residential from 2006–2023 and pull assessed values | PTAXSIM database | `data/raw/residential_pins_ever.csv`, `data/raw/pin_muni_key.csv` | Active |
| `pull_floodfactor_pin_list.R` | Join Flood Factor risk scores (2019 snapshot) to Cook County parcel universe | Parcel universe with PINs, Flood Factor data (external) | `data/raw/pins_floodfactor_2019.csv` | Active |

---

#### Exploratory & Descriptive Analysis

| File | Purpose | Inputs | Outputs | Status |
|------|---------|--------|---------|--------|
| `Flood_factor_pins.qmd` | Filter for PINs with Flood Factor ≥ 5; analyze post-2023 PTAX-245 flood appeals; calculates AV changes in affected land blocks | Parcel universe API, PTAX-245 appeal forms | Summary tables and figures | Active |
| `Flood_Appeal_PINs.qmd` | Identifies and summarizes AV appeal activity for PINs affected by 2023 flood; compares AV changes for appeal vs. neighbor parcels | PTAX-245 appeal forms, assessed values | Graphs of average AV changes, summary stats | Active |
| `CC_Individual_FEMA_Assistance.qmd` | Analyzes FEMA IA aid at ZIP level during recent flood events; descriptive stats on applicants and aid granted | `data/raw/indiv_assistance_CookCounty.csv` | ZIP-level aid summaries, descriptive statistics | Active |
| `Cook_Flood_Damage.qmd` | Summarizes NFIP claims and policies across geographies (ZIP, block, tract, muni) | `data/raw/nfipclaims_CookCounty.csv`, `data/raw/nfippolicies_CookCounty_all.csv` | Aggregated claim/policy stats | Active |
| `AssessedValue_ExploratoryGraphs.rmd` | Early exploration of AV trends in SFHA and flood-prone parcels using CCAO data | Parcel universe, AV data | Exploratory graphs and maps | Needs Review |
| `CookCounty_ParcelMaps.qmd` | Early mapping of parcels marked SFHA in CCAO data; not currently used | CCAO parcel universe | Static SFHA parcel maps | Deprecated |

---

#### Market and Policy Impact

| File | Purpose | Inputs | Outputs | Status |
|------|---------|--------|---------|--------|
| `Cook_Sales_data.qmd` | Links sales data with flood risk indicators (SFHA, LOMR, Flood Factor); identifies multi-sale parcels and treatment status | `data/raw/assessor_parcel_sales.csv`, `parcels_sfha_2018.gpkg`, `parcels_sfha_2024.gpkg`, `parcels_lomrs.gpkg`, `data/raw/pins_floodfactor_2019.csv` | Counts of PINs by treatment status, sales trends | Active |


### Key Data Preparation Scripts

| File | Purpose | Inputs | Outputs |
|------|---------|--------|---------|
| `pull_parcel_sfs.R` | Download and convert Cook County parcel shapefiles for 2018 and 2023 | External shapefiles (downloaded via script) | `data/raw/parcels_2018.gpkg`, `data/raw/parcels_2023.gpkg` |
| `pull_NFIP_redactedclaims.R` | Pull NFIP policy and claims data for Cook County from FEMA API | FEMA OpenFEMA API | `data/raw/nfippolicies_CookCounty_all.csv`, `data/raw/nfipclaims_CookCounty.csv` |
| `pull_individual_assistance_applicants.R` | Pull FEMA Individual Assistance (IA) application data for Cook County (accepted and denied) | FEMA IA API | `data/raw/indiv_assistance_CookCounty.csv` |
| `pull_distinct_resPINs.R` | Extract all parcels that were residential from 2006–2023 and pull assessed values | PTAXSIM database | `data/raw/residential_pins_ever.csv`, `data/raw/pin_muni_key.csv`, `data/raw/muniname_taxcode_key.csv` |
| `pull_floodfactor_pin_list.R` | Join Flood Factor risk scores (2019 snapshot) to Cook County parcel universe | Parcel universe with PINs, Flood Factor data (external) | `data/raw/pins_floodfactor_2019.csv` |


### Analysis & Mapping Scripts

| File | Purpose | Inputs | Outputs |
|------|---------|--------|---------|
| `Flood_factor_pins.qmd` | Filter for PINs with Flood Factor ≥ 5 from the parcel universe; merges with PTAX-245 appeals post-2023 flood; calculates differences in assessed values before/after appeals | Parcel universe API, PTAX-245 appeal forms | Summary tables and figures of AV differences for affected PINs |
| `CookCounty_ParcelMaps.qmd` | Early mapping of parcels marked as SFHA in CCAO parcel universe; not currently in use | CCAO parcel universe | Static parcel-SFHA maps (deprecated) |
| `Flood_Appeal_PINs.qmd` | Identifies and summarizes appeal activity for PINs within land blocks affected by flooding in 2023 using PTAX-245 data; compares AV changes from appeals | PTAX-245 appeal forms, assessed values | Graphs of average AV differences, summary stats for appeal PINs vs neighbors |
| `AssessedValue_ExploratoryGraphs.rmd` | Early exploratory graphs of AV trends in/outside SFHA and for high flood factor PINs using CCAO data | Parcel universe, AV data | Maps and exploratory figures of SFHA AV trends by year |


## 🔹 Script Overview

### 1. `pull_parcel_sfs.R`
**Purpose:**  
Downloads parcel shapefiles for Cook County for two time points: 2018 and 2023. Converts shapefiles into spatial databases.

**Output:**   
- `data/raw/parcels_2018.gpkg`  
- `data/raw/parcels_2023.gpkg`  

---

### 2. `pull_NFIP_redactedclaims.R`
**Purpose:**  
Pulls National Flood Insurance Program (NFIP) claims and policy data from FEMA’s open API, filtered to Cook County, IL.

**Output:**  
- `data/raw/nfippolicies_CookCounty_all.csv`  
- `data/raw/nfipclaims_CookCounty.csv`  

---

### 3. `pull_individual_assistance_applicants.R`
**Purpose:**  
Collects individual-level disaster assistance application data from FEMA’s API, including both approved and denied applications.

**Features:**
- Two access methods are demonstrated in the script for redundancy and robustness.

**Output:**
- `data/raw/indiv_assistance_CookCounty.csv`

---

### 4. `pull_distinct_resPINs.R`
**Purpose:**  
Extracts all parcels that were residential at any point from 2006 to 2023 using the PTAXSIM database. Also retrieves assessed values by year.

**Output:**   
- `data/raw/residential_pins_ever.csv`  
- `data/raw/pin_muni_key.csv`  
- `data/raw/muniname_taxcode_key.csv`  

---

### 5. `pull_floodfactor_pin_list.R`
**Purpose:**  
Merges Cook County parcel universe with Flood Factor risk scores (2019 snapshot), generating a list of PINs with their associated risk scores.

**Output:**  
- `data/raw/pins_floodfactor_2019.csv` *(or similar)*  

---

## 💡 Notes

- Scripts are modular and can be rerun independently.
- Most scripts assume Cook County boundaries are predefined or use a county code filter.
- All outputs from these scripts serve as the "raw" layer of the data pipeline and are not yet analysis-ready.


### Data Cleaning Notes

Do filtering steps last. Keep all property classes until creating the panel dataset for analysis. 