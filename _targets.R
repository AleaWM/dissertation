# _targets.R
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("sf", "dplyr", "readr", "readxl", "stringr", "tidyr", "lubridate")
)


sfha_methods <- tibble::tibble(
  method   = c("land", "bldg", "ptaxsim"),
  out_stub = c("land", "bldg", "ptaxsim"))


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

  tar_target(
    pin10_geoms_raw,
    {
      con <- DBI::dbConnect(RSQLite::SQLite(), ptaxsim_db_file)
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbGetQuery(
        con,
        glue::glue_sql(
          "SELECT DISTINCT pin10, start_year, end_year, geometry
           FROM pin_geometry_raw",
          .con = con
        )
      )

    }
  ),

  tar_target(
    ptaxsim_parcels_sf,
    {
      pin10_geoms_raw |>
        sf::st_as_sf(wkt = "geometry", crs = 4326) |>
        sf::st_transform(sf::st_crs(border)) |>
        sf::st_make_valid() |>
        # dplyr::mutate(
        #   geometry = sf::st_collection_extract(geometry, "POLYGON")) |>
        dplyr::filter(!sf::st_is_empty(geometry))
    }
  ),



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
    sfha_indicator_land_parcels,
    make_sfha_indicator_parcels(
      parcels_sfha_2018,
      parcels_sfha_2024,
      parcels_prelim_sfha,
      parcels_lomr_2018,
      parcels_lomr_2024
    )
  ),


  tar_target(
    ptaxsim_sfha_indicator_parcel_polygons,
    make_sfha_indicator_parcels(
      ptaxsim_sfha_2018,
      ptaxsim_sfha_2024,
      ptaxsim_prelim_sfha,
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

  tar_target(
    sfha_indicator_final_land,
    {
      parcels_with_firms |>
        dplyr::left_join(sfha_indicator_land_parcels, by = "pin10", relationship = "many-to-one") |>

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
              ifelse(in_prelim_panels == FALSE, sfha2024, NA)))
        ) |>
        select(pin10, longitude, latitude, start_year, end_year, FIRM_PAN,
          VERSION_ID, PRE_DATE, EFF_DATE, in_prelim_panels,
          sfha2018, sfha2024, sfha2026, lomr2018, lomr2024, lomr_date)
    }
  ),


  tar_target(
    ptaxsim_sfha_indicator_final,
    {
      parcels_with_firms |>
        dplyr::left_join(ptaxsim_sfha_indicator_parcel_polygons, by = "pin10", relationship = "many-to-one") |>

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
          ifelse(in_prelim_panels == TRUE  & !is.na(FLD_ZONE), 1,
            ifelse(in_prelim_panels == TRUE & is.na(FLD_ZONE), 0,
              ifelse(in_prelim_panels == FALSE, sfha2024, NA)))
        ) |>
        select(pin10, longitude, latitude, start_year, end_year, FIRM_PAN,
          VERSION_ID, PRE_DATE, EFF_DATE, in_prelim_panels,
          sfha2018, sfha2024, sfha2026, lomr2018, lomr2024, lomr_date)
    }
  ),


  # ----  Stage 1B: Using Building Polygons for Identifying SFHA ----

  tar_target(
    buildings_files,
    c(
      "data/raw/cook_MS.shp",
      "data/raw/cook_MS.dbf",
      "data/raw/cook_MS.shx",
      "data/raw/cook_MS.prj"
    ),
    format = "file"
  ),

  tar_target(
    buildings_raw,
    read_buildings(buildings_files, border = border, crs_out = sf::st_crs(border))
  ),

  tar_target(
    buildings_with_pin,
    assign_buildings_to_parcels(buildings_raw, parcels_2022, parcel_pin_field = "pin")
  ),

  tar_target(
    buildings_sfha_2018,
    join_buildings_to_zone(buildings_with_pin, sfha_2018_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    buildings_sfha_2024,
    join_buildings_to_zone(buildings_with_pin, sfha_2024_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    buildings_prelim_sfha,
    join_buildings_to_zone(buildings_with_pin, sfha_2026_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    buildings_lomr_2018,
    join_buildings_to_zone(buildings_with_pin, lomr_2018_poly, id_field = "LOMR_ID")
  ),

  tar_target(
    buildings_lomr_2024,
    join_buildings_to_zone(buildings_with_pin, lomr_2024_poly, id_field = "LOMR_ID")
  ),


  tar_target(
    sfha_indicator_buildings,
    make_sfha_indicator_buildings(
      buildings_sfha_2018,
      buildings_sfha_2024,
      buildings_prelim_sfha,
      buildings_lomr_2018,
      buildings_lomr_2024
    )
  ),

  tar_target(
    sfha_indicator_final_buildings,
    parcels_with_firms |>
      dplyr::left_join(sfha_indicator_buildings, by = "pin10", relationship = "many-to-one") |>
      dplyr::mutate(
        bldg_sfha2018 = dplyr::if_else(!is.na(bldg_FLD_ZONE_2018), 1L, 0L),
        bldg_sfha2024 = dplyr::if_else(!is.na(bldg_FLD_ZONE_2024), 1L, 0L),
        bldg_sfha2026 =
          ifelse(in_prelim_panels == TRUE  & !is.na(bldg_FLD_ZONE_2026), 1,
            ifelse(in_prelim_panels == TRUE & is.na(bldg_FLD_ZONE_2026), 0,
              ifelse(in_prelim_panels == FALSE, bldg_FLD_ZONE_2024, NA))),

        bldg_lomr2018 = dplyr::if_else(!is.na(.data$lomr_yearlomr2018), 1L, 0L),
        bldg_lomr2024 = dplyr::if_else(!is.na(.data$lomr_yearlomr2024), 1L, 0L)
      ) |>
      dplyr::select(
        pin10, longitude, latitude, start_year, end_year,
        FIRM_PAN, VERSION_ID, PRE_DATE, EFF_DATE, in_prelim_panels,
        bldg_sfha2018, bldg_sfha2024, bldg_sfha2026, bldg_lomr2018, bldg_lomr2024
      )
  ),



  tar_target(
    ptaxsim_sfha_2018,
    join_parcels_to_zone(ptaxsim_parcels_sf, sfha_2018_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    ptaxsim_sfha_2024,
    join_parcels_to_zone(ptaxsim_parcels_sf, sfha_2024_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    ptaxsim_prelim_sfha,
    join_parcels_to_zone(ptaxsim_parcels_sf, sfha_2026_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    sfha_compare_pin10_threeway,
    sfha_indicator_final_land |>
      dplyr::select(pin10, sfha2018, sfha2024, sfha2026, lomr2018, lomr2024, lomr_date, PRE_DATE, EFF_DATE, FIRM_PAN, VERSION_ID) |>
      dplyr::left_join(
        sfha_indicator_final_buildings |>
          dplyr::select(pin10, bldg_sfha2018, bldg_sfha2024, bldg_sfha2026, bldg_lomr2018, bldg_lomr2024),
        by = "pin10"
      ) |>
      dplyr::left_join(
        ptaxsim_sfha_indicator_final |>
          dplyr::select(pin10, ptax_sfha2018 = sfha2018, ptax_sfha2024 = sfha2024, ptax_sfha2026 = sfha2026),
        by = "pin10"
      ) |>
      dplyr::mutate(
        diff_land_bldg_2024 = sfha2024 != bldg_sfha2024,
        diff_land_ptax_2024 = sfha2024 != ptax_sfha2024,
        diff_bldg_ptax_2024 = bldg_sfha2024 != ptax_sfha2024,
        diff_land_bldg_2026 = sfha2026 != bldg_sfha2026,
        diff_land_ptax_2026 = sfha2026 != ptax_sfha2026,
        diff_bldg_ptax_2026 = bldg_sfha2026 != ptax_sfha2026
      )
  ),

  # ---- Stage 2: Sales + dissertation-ready variables ----
  # Raw sales input (tracked). Update the path to match your project.
  tar_target(
    sales_csv_file,
    # "data/raw/Assessor_-_Parcel_Sales_20251229.csv",
    "data/raw/Assessor_-_Parcel_Sales_20250709.csv",
    format = "file"
  ),

  tar_target(
    sales_raw,
    read_sales_assessor(sales_csv_file, min_year = 2009)
  ),

  tar_target(
    res_sales,
    build_res_sales(sales_raw)
  ),

  # tar_target(
  #   sfha_indicator_final_both,
  #   sfha_indicator_final |>
  #     dplyr::left_join(
  #       sfha_indicator_final_buildings |>
  #         dplyr::select(
  #           pin10,
  #           bldg_sfha2018, bldg_sfha2024, bldg_sfha2026,
  #           bldg_lomr2018, bldg_lomr2024
  #         ),
  #       by = "pin10",
  #       relationship = "one-to-one"
  #     ) |>
  #     dplyr::mutate(
  #       diff_sfha2018 = sfha2018 != bldg_sfha2018,
  #       diff_sfha2024 = sfha2024 != bldg_sfha2024,
  #       diff_sfha2026 = sfha2026 != bldg_sfha2026,
  #       diff_lomr2018 = lomr2018 != bldg_lomr2018,
  #       diff_lomr2024 = lomr2024 != bldg_lomr2024
  #     )
  # ),


  # joins residential sales and the indicators for both parcel-based and building-based SFHA indicator variables
  tar_target(
    sales_joined, # has residential only sales now
    merge_sales_sfha(res_sales, sfha_compare_pin10_threeway)
  ),



  tarchetypes::tar_map(
    values = sfha_methods,
    names  = out_stub,   # creates branches named land / bldg / ptaxsim
    list(
      tar_target(
        sales_prepped,
        make_sfha_timing_vars(
          sales_joined,
          method = method,
          min_analysis_year = 2009,
          min_price = 5000
        )
      ),

      tar_target(
        repeat_sales,
        make_repeat_sales(sales_prepped)
      ),

      tar_target(
        df_prep,
        make_df_prep(repeat_sales, pin_muni_key_tbl)
      ),

      tar_target(
        df_prep_filled,
        fill_missing_muni_by_nbhd_zip(df_prep)
      ),

      tar_target(
        df_prep_final,
        final_df_prep_filters(df_prep_filled)
      ),

      tar_target(
        df_prep_final_rds,
        {
          dir.create(sales_out_dir, recursive = TRUE, showWarnings = FALSE)
          out_file <- file.path(sales_out_dir, paste0("df_prep_", out_stub, "_final.rds"))
          saveRDS(df_prep_final, out_file)
          out_file
        },
        format = "file"
      )
    )
  ),

  #
  # tar_target(
  #   sales_prepped_bldg,
  #   make_sfha_timing_vars(sales_joined,  method = "bldg", min_analysis_year = 2009, min_price = 5000)
  # ),
  #
  # tar_target(
  #   sales_prepped_land,
  #   make_sfha_timing_vars(sales_joined,  method = "land", min_analysis_year = 2009, min_price = 5000)
  # ),
  #
  #
  # tar_target(
  #   sales_prepped_ptaxsim_land,
  #   make_sfha_timing_vars(sales_joined,  method = "ptaxsim", min_analysis_year = 2009, min_price = 5000)
  # ),




  # --- Checking Work ----




  # tar_target(
  #   sfha_compare_pin10,
  #   sfha_indicator_final |>
  #     dplyr::select(
  #       pin10,
  #       sfha2018, sfha2024, sfha2026,
  #       lomr2018, lomr2024
  #     ) |>
  #     dplyr::left_join(
  #       sfha_indicator_final_buildings |>
  #         dplyr::select(
  #           pin10,
  #           bldg_sfha2018, bldg_sfha2024, bldg_sfha2026,
  #           bldg_lomr2018, bldg_lomr2024
  #         ),
  #       by = "pin10",
  #       relationship = "one-to-one"
  #     ) |>
  #     dplyr::mutate(
  #       # SFHA disagreements
  #       diff_sfha2018 = .data$sfha2018 != .data$bldg_sfha2018,
  #       diff_sfha2024 = .data$sfha2024 != .data$bldg_sfha2024,
  #       diff_sfha2026 = .data$sfha2026 != .data$bldg_sfha2026,
  #
  #       land_only_sfha2018 = (.data$sfha2018 == 1L & .data$bldg_sfha2018 == 0L),
  #       bldg_only_sfha2018 = (.data$sfha2018 == 0L & .data$bldg_sfha2018 == 1L),
  #
  #       land_only_sfha2024 = (.data$sfha2024 == 1L & .data$bldg_sfha2024 == 0L),
  #       bldg_only_sfha2024 = (.data$sfha2024 == 0L & .data$bldg_sfha2024 == 1L),
  #
  #       land_only_sfha2026 = (.data$sfha2026 == 1L & .data$bldg_sfha2026 == 0L),
  #       bldg_only_sfha2026 = (.data$sfha2026 == 0L & .data$bldg_sfha2026 == 1L),
  #
  #       # LOMR disagreements
  #       diff_lomr2018 = .data$lomr2018 != .data$bldg_lomr2018,
  #       diff_lomr2024 = .data$lomr2024 != .data$bldg_lomr2024,
  #
  #       land_only_lomr2018 = (.data$lomr2018 == 1L & .data$bldg_lomr2018 == 0L),
  #       bldg_only_lomr2018 = (.data$lomr2018 == 0L & .data$bldg_lomr2018 == 1L),
  #
  #       land_only_lomr2024 = (.data$lomr2024 == 1L & .data$bldg_lomr2024 == 0L),
  #       bldg_only_lomr2024 = (.data$lomr2024 == 0L & .data$bldg_lomr2024 == 1L)
  #     )
  # ),

  # tar_target(
  #   sfha_compare_summary,
  #   sfha_compare_pin10 |>
  #     dplyr::summarise(
  #       n_pin10 = dplyr::n(),
  #
  #       # SFHA: counts
  #       n_diff_sfha2018 = sum(.data$diff_sfha2018, na.rm = TRUE),
  #       n_diff_sfha2024 = sum(.data$diff_sfha2024, na.rm = TRUE),
  #       n_diff_sfha2026 = sum(.data$diff_sfha2026, na.rm = TRUE),
  #
  #       n_land_only_sfha2018 = sum(.data$land_only_sfha2018, na.rm = TRUE),
  #       n_bldg_only_sfha2018 = sum(.data$bldg_only_sfha2018, na.rm = TRUE),
  #
  #       n_land_only_sfha2024 = sum(.data$land_only_sfha2024, na.rm = TRUE),
  #       n_bldg_only_sfha2024 = sum(.data$bldg_only_sfha2024, na.rm = TRUE),
  #
  #       n_land_only_sfha2026 = sum(.data$land_only_sfha2026, na.rm = TRUE),
  #       n_bldg_only_sfha2026 = sum(.data$bldg_only_sfha2026, na.rm = TRUE),
  #
  #       # LOMR: counts
  #       n_diff_lomr2018 = sum(.data$diff_lomr2018, na.rm = TRUE),
  #       n_diff_lomr2024 = sum(.data$diff_lomr2024, na.rm = TRUE),
  #
  #       n_land_only_lomr2018 = sum(.data$land_only_lomr2018, na.rm = TRUE),
  #       n_bldg_only_lomr2018 = sum(.data$bldg_only_lomr2018, na.rm = TRUE),
  #
  #       n_land_only_lomr2024 = sum(.data$land_only_lomr2024, na.rm = TRUE),
  #       n_bldg_only_lomr2024 = sum(.data$bldg_only_lomr2024, na.rm = TRUE),
  #
  #       # SFHA: rates
  #       pct_diff_sfha2018 = n_diff_sfha2018 / n_pin10,
  #       pct_diff_sfha2024 = n_diff_sfha2024 / n_pin10,
  #       pct_diff_sfha2026 = n_diff_sfha2026 / n_pin10,
  #
  #       # LOMR: rates
  #       pct_diff_lomr2018 = n_diff_lomr2018 / n_pin10,
  #       pct_diff_lomr2024 = n_diff_lomr2024 / n_pin10
  #     )
  # ),





  # ---- Final dissertation prep inputs ----
  tar_target(pin_muni_key_file, "data/raw/pin_muni_key.csv", format = "file"),
  tar_target(muni_nicknames_file, "../Merriman RA/ptax/Necessary_Files/muni_shortnames.xlsx", format = "file"),
  tar_target(floodfactor_file, "data/processed/floodfactor_scores.csv", format = "file"),
  tar_target(manual_ff_scores_file, "data/processed/pins_with_some_addresses_forgoogle.xlsx", format = "file"),

  # ---- Read/assemble lookup tables ----
  tar_target(
    pin_muni_key_tbl,
    read_pin_muni_key(pin_muni_key_file, muni_nicknames_file, floodfactor_file)
  )

  #
  # # ---- Clean and keep Repeat sales subset ----
  # tar_target(
  #   repeat_sales_bldg,
  #   make_repeat_sales(sales_prepped_bldg)
  # ),
  #
  # # ---- Build df_prep (your final analysis dataset base) ----
  # tar_target(
  #   df_prep_bldg,
  #   make_df_prep(repeat_sales_bldg, pin_muni_key_tbl)
  # ),
  #
  # # ---- Fill missing muni/Triad/Township using nbhd+zip lookup ----
  # tar_target(
  #   df_prep_bldg_filled,
  #   fill_missing_muni_by_nbhd_zip(df_prep_bldg)
  # ),
  #
  # # ---- Final filters / flags ----
  # tar_target(
  #   df_prep_bldg_final,
  #   final_df_prep_filters(df_prep_bldg_filled)
  # ),
  #
  # #--- Parcel-based final steps
  #
  # # ---- Clean and keep Repeat sales subset ----
  # tar_target(
  #   repeat_sales_land,
  #   make_repeat_sales(sales_prepped_land)
  # ),
  #
  # # ---- Build df_prep (your final analysis dataset base) ----
  # tar_target(
  #   df_prep_land,
  #   make_df_prep(repeat_sales_land, pin_muni_key_tbl)
  # ),
  #
  # # ---- Fill missing muni/Triad/Township using nbhd+zip lookup ----
  # tar_target(
  #   df_prep_land_filled,
  #   fill_missing_muni_by_nbhd_zip(df_prep_land)
  # ),
  #
  # # ---- Final filters / flags ----
  # tar_target(
  #   df_prep_land_final,
  #   final_df_prep_filters(df_prep_land_filled)
  # ),
  #
  # # Write stable RDS outputs for Quarto + non-targets usage
  # tar_target(
  #   sales_prepped_land_rds,
  #   {
  #     dir.create(sales_out_dir, recursive = TRUE, showWarnings = FALSE)
  #     out_file <- file.path(sales_out_dir, "df_prep_land_final.rds")
  #     saveRDS(df_prep_land_final, out_file)
  #     out_file
  #   },
  #   format = "file"
  # ),
  #
  # tar_target(
  #   sales_prepped_bldg_rds,
  #   {
  #     dir.create(sales_out_dir, recursive = TRUE, showWarnings = FALSE)
  #     out_file <- file.path(sales_out_dir, "df_prep_bldg_final.rds")
  #     saveRDS(df_prep_bldg_final, out_file)
  #     out_file
  #   },
  #   format = "file"
  # )
)
