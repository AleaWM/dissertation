# _targets.R
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("sf", "dplyr", "readr", "readxl", "stringr", "tidyr", "lubridate")
)

# helper functions for the spatial joins + rollups
source("R/sfha_targets_functions.R")
source("R/sales_data_functions.R")

targets_out_dir <- "data/processed/targets"
sfha_out_dir    <- file.path(targets_out_dir, "sfha")
sales_out_dir   <- file.path(targets_out_dir, "sales")

# helper functions for the spatial joins + rollups
source("R/sfha_targets_functions.R")

targets_out_dir <- "data/processed/targets"
sfha_out_dir    <- file.path(targets_out_dir, "sfha")

list(
  # ---- External inputs (tracked) ----

  tar_file(ptaxsim_db_file, "../Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db"),

  # county border for clipping
  tar_target(
    cook_border_file,
    "inputs/Mapping_FIRMs/Cook_County_Border/Cook_County_Border.shp",
    format = "file"
  ),

  # county border used for clipping other shapefiles
  tar_target(
    border,
    read_border(cook_border_file, crs_out = 6454)
  ),

  tar_target(common_crs, sf::st_crs(border)),


  # ---- Centroids from PTAXSIM ----
  tar_target(
    pin_centroids_raw,
    {
      con <- DBI::dbConnect(RSQLite::SQLite(), ptaxsim_db_file)
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT DISTINCT pin10, start_year, end_year, longitude, latitude
           FROM pin_geometry_raw",
          .con = con
        )
      )


    }
  ),

  tar_target(
    pin_centroids,
    pin_centroids_raw |>
      dplyr::group_by(pin10) |>
      dplyr::summarize(
        longitude  = dplyr::first(longitude),
        latitude   = dplyr::first(latitude),
        start_year = min(start_year, na.rm = TRUE),
        end_year   = max(end_year,   na.rm = TRUE)
      ) |>
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  ),

  tar_target(
    firm_panels_2026,

    read_firm_panels_shp(
      shp_path = firm_panels_2026_shp,
      border   = border,
      crs_out  = sf::st_crs(border))
  ),

  # ---- Spatial join: which panel each centroid is within ----
  tar_target(
    pin_centroids_joined,
    # crs project matters for centroids to match polygon
    sf::st_join(pin_centroids, (firm_panels_2026 |> st_transform("EPSG:4326")), join = sf::st_within)
  ),

  tar_target(
    pin_centroids_clean,
    {
      x <- pin_centroids_joined |>
        sf::st_drop_geometry() |>
        dplyr::select(-dplyr::any_of(c("PCOMM", "PANEL", "SUFFIX", "ST_FIPS",
          "SCALE", "PANEL_TYP", "PNP_REASON", "BASE_TYP",
          "geometry")))
      x
    }
  ),

  # --- output dirs ---
  tar_target(
    targets_dirs,
    {
      dir.create(sfha_out_dir, recursive = TRUE, showWarnings = FALSE)
      TRUE
    }
  ),

  # ============================================================
  # STAGE 1: Build sfha_indicator_pins (the "hits-only" file)
  # ============================================================

  # --- spatial inputs (adjust paths to your repo) ---

  # parcel polygons as of tax year 2018
  tar_target(
    parcels_2018_gdb,
    "inputs/Historical_Parcels_-_2018.gdb/parcels_2018.gdb",
    format = "file"
  ),

  # parcel polygons as of tax year 2022
  tar_target(
    parcels_2022_gdb,
    "inputs/Mapping_Firms/Historical_Parcels_-_2022.gdb/ccao_2022parcels.gdb",
    format = "file"
  ),

  # historic nfhl from 2018 (before FIRM updates)
  tar_target(
    nfhl_2018_gdb,
    "inputs/Mapping_FIRMs/NFHL_17_20180129.gdb/NFHL_17_20180129.gdb",
    format = "file"
  ),

  # NFHL as of June 2024 (two batches of updated FIRMs)
  tar_target(
    nfhl_state_2024_gdb,
    "inputs/Mapping_Firms/NFHL_17_20240628/Statewide_NFHL_17_20240628.gdb",
    format = "file"
  ),

  tar_target(
    prelim_sfha_shp,
    "inputs/Cook_2026_download/S_Fld_Haz_Ar.shp",
    format = "file"
  ),

  # updated FIRM panels with their data. includes pending FIRMs for 2026
  tar_target(
    firm_panels_2026_shp,
    "inputs/Cook_2026_download/S_FIRM_Pan.shp",
    format = "file"
  ),

  # --- read in parcel polygons that are used to identify parcels that overlap with SFHA ---
  tar_target(
    parcels_2018,
    read_parcels_2018(parcels_2018_gdb, crs_out = sf::st_crs(border), border = border)
  ),

  tar_target(
    parcels_2022, # Used for 2024 NFHL SFHA areas and 2026 Pending FIRM areas
    read_parcels_2022(parcels_2022_gdb, crs_out = sf::st_crs(border), border = border)
  ),



  # --- hazard polygons (clipped) ---
  tar_target(
    sfha_2018_poly,
    read_nfhl_sfha(nfhl_2018_gdb, layer = "S_Fld_Haz_Ar", border = border, sfha_tf_field = "SFHA_TF")
  ),

  tar_target(
    sfha_2024_poly,
    read_nfhl_sfha(nfhl_state_2024_gdb, layer = "S_FLD_HAZ_AR", border = border, sfha_tf_field = "SFHA_TF")
  ),

  # pending FIRM changes were not a geodatabase during initial data availability, only shapefile

  tar_target(
    sfha_2026_poly,
    read_prelim_sfha(prelim_sfha_shp,  border = border, crs_out = sf::st_crs(border))
  ),

  tar_target(
    lomr_2018_poly,
    read_nfhl_lomr(nfhl_2018_gdb, layer = "S_LOMR", border = border)
  ),

  tar_target(
    lomr_2024_poly,
    read_nfhl_lomr(nfhl_state_2024_gdb, layer = "S_LOMR", border = border)
  ),


  # --- spatial joins: The big, time consuming ones.  ---
  tar_target(
    parcels_sfha_2018,
    join_parcels_to_zone(parcels_2018, sfha_2018_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    parcels_sfha_2024,
    join_parcels_to_zone(parcels_2022, sfha_2024_poly, id_field = "DFIRM_ID")
  ),

  # only identifies parcels in SFHA for the pending FIRMs.
  # Must back fill other parcels with 2024 NFHL data later
  tar_target(
    parcels_prelim_sfha,
    join_parcels_to_zone(parcels_2022, sfha_2026_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    parcels_lomr_2018,
    join_parcels_to_zone(parcels_2018, lomr_2018_poly, id_field = "LOMR_ID")
  ),

  tar_target(
    parcels_lomr_2024,
    join_parcels_to_zone(parcels_2022, lomr_2024_poly, id_field = "LOMR_ID")
  ),

  # --- rollup to sfha_indicator_pins (this is the “hits-only” file you mean) ---
  tar_target(
    sfha_indicator_pins,
    make_sfha_indicator_parcels(
      parcels_sfha_2018,
      parcels_sfha_2024,
      parcels_prelim_sfha,
      parcels_lomr_2018,
      parcels_lomr_2024
    )
  ),

  tar_target(
    parcels_with_firms,
    {
      drop_parcels <- c( # searched these manually in CookViewer to confirm they should be dropped. had missing FIRM information in pin10_firms
        "0508400001", "0508400002", "0508400003", "0508400004", # pins in lake
        "1405211017", "1405403020", "1416999001", "1710403001", # not residential parcels, some in water
        "1715113004", "2130108012", "2130108018", "2130108019", # land and partially in water parcels
        "2130108028", "2130108030", "2130108031", "2130108032", # land polygons along the lake, no residential buildings in them
        "2130108033", "2130114012", "2130114013", "2130114014", "2130114015", "2130114016",  # land polygons along the lake, no buildings within them
        "2130124001", "2130124002", "2130124003", "2130124004",  # almost completely in the lake
        "2130999001", "2132213002", # actual water canal in calumet area
        "2608202004", "2608400034", "3017211033" # also water pins.
      )

      firm_info <- readxl::read_xlsx("inputs/Cook_2026_download/S_FIRM_PAN.xlsx") |>
        select(FIRM_PAN, VERSION_ID, FIRM_ID, PRE_DATE, EFF_DATE)

      pin_centroids_clean |>
        dplyr::select(-c(DFIRM_ID, EFF_DATE)) |>
        filter(!pin10 %in% drop_parcels) |>
        mutate(FIRM_PAN = ifelse(pin10 == "2130111028", "17031C0539K", FIRM_PAN)) |>
        left_join(firm_info, by = "FIRM_PAN") |>
        mutate(in_prelim_panels = ifelse(VERSION_ID == "2.6.3.6", TRUE, FALSE))


    }
  ),


  # tar_target(
  #   firm_pan_xlsx_file,
  #   read_xlsx("inputs/Cook_2026_download/S_FIRM_PAN.xlsx")
  # ),

  tar_target(
    sfha_indicator_final,
    {
      parcels_with_firms |>
        dplyr::left_join(sfha_indicator_pins, by = "pin10", relationship = "many-to-one") |>

        mutate(
          sfha2018 = ifelse(!is.na(FLD_ZONE2018), 1, 0),
          sfha2024 = ifelse(!is.na(FLD_ZONE2024), 1, 0),
          lomr2018 =  ifelse(!is.na(lomr_yearlomr2018), 1, 0),
          lomr2024 = ifelse(!is.na(lomr_yearlomr2024), 1, 0),
          lomr_date = as.Date(lomr_date)
        ) |>
        # fills in sfha2026 areas that were not updated with NFHL sfha indicators from 2024 database
        # if in prelim panel, documents if it was a truly not in the
        mutate(sfha2026 =
          ifelse(in_prelim_panels == TRUE & !is.na(FLD_ZONE), 1,
            ifelse(in_prelim_panels == TRUE & is.na(FLD_ZONE), 0,
              ifelse(in_prelim_panels == FALSE, sfha2024, "CHECKME")))
        ) |>
        select(pin10, longitude, latitude, start_year, end_year, FIRM_PAN,
          VERSION_ID, PRE_DATE, EFF_DATE, in_prelim_panels,
          sfha2018, sfha2024, sfha2026, lomr2018, lomr2024, lomr_date)
    }
  ),



  # ---- Stage 2: Sales + dissertation-ready variables ----
  # Raw sales input (tracked). Update the path to match your project.
  tar_target(
    sales_csv_file,
    "data/raw/Assessor_-_Parcel_Sales_20250709.csv",
    format = "file"
  ),

  tar_target(
    sales_raw,
    read_sales_assessor(sales_csv_file, min_year = 2005)
  ),

  tar_target(
    sales_joined,
    merge_sales_sfha(sales_raw, sfha_indicator_final)
  ),

  tar_target(
    sales_prepped,
    make_sfha_timing_vars(sales_joined, min_analysis_year = 2009, min_price = 5000)
  ),

  tar_target(
    res_sales,
    build_res_sales(sales_prepped)
  ),

  tar_target(
    repeat_res_sales,
    build_repeat_res_sales(res_sales)
  ),

  # Write stable RDS outputs for Quarto + non-targets usage
  tar_target(
    sales_prepped_rds,
    {
      dir.create(sales_out_dir, recursive = TRUE, showWarnings = FALSE)
      out_file <- file.path(sales_out_dir, "sales_prepped.rds")
      saveRDS(sales_prepped, out_file)
      out_file
    },
    format = "file"
  ),

  tar_target(
    res_sales_rds,
    {
      dir.create(sales_out_dir, recursive = TRUE, showWarnings = FALSE)
      out_file <- file.path(sales_out_dir, "res_sales.rds")
      saveRDS(res_sales, out_file)
      out_file
    },
    format = "file"
  ), #

  # tar_target(
  #   repeat_res_sales_rds,
  #   {
  #     dir.create(sales_out_dir, recursive = TRUE, showWarnings = FALSE)
  #     out_file <- file.path(sales_out_dir, "repeat_res_sales.rds")
  #     saveRDS(repeat_res_sales, out_file)
  #     out_file
  #   },
  #   format = "file"
  # )


  # # ---- Stage 3: Render Quarto outputs (optional) ----
  tarchetypes::tar_render(
    create_datasets_report,
    "2_create_datasets.qmd"
  )
)
