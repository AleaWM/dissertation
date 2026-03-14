# _targets.R
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("sf", "dplyr", "readr", "readxl", "stringr", "tidyr", "lubridate", "tibble")
)


sfha_methods <- tibble::tibble(
  method   = c("land", "bldg", # "ptaxsim",
    "risk500", "risk500_land"),
  out_stub = c("land", "bldg", # "ptaxsim",
    "risk500", "risk500_land")
)


sales_versions <- data.frame(
  sales_label = c( # "baseline",
    "updated",
    "2026"
  ),
  sales_file  = c(
    # "data/raw/Assessor_-_Parcel_Sales_20250709.csv",
    "data/raw/Assessor_-_Parcel_Sales_20251229.csv",
    "data/raw/Assessor_-_Parcel_Sales_20260308.csv"
  ),
  stringsAsFactors = FALSE
)

# helper functions for the spatial joins + rollups
source("R/sfha_targets_functions.R")
source("R/sales_data_functions.R")
source("R/indicator_final_functions.R")
source("R/q1_targets_functions.R")
# source("R/helper_pins_to_drop.R")
# source()

targets_out_dir <- "data/processed/targets"
sfha_out_dir    <- file.path(targets_out_dir, "sfha")
sales_out_dir   <- file.path(targets_out_dir, "sales")

list(
  # tar_target(
  # targets_dirs,
  # {
  #   dir.create(sfha_out_dir, recursive = TRUE, showWarnings = FALSE)
  #   TRUE
  # }
  # ),
  # --- External Data Items ----
  ## --- Manual recode parcels -----
  tar_target(drop_pins_file, "R/helper_pins_to_drop.R", format = "file"),
  # tar_target(firm_override_file, "R/helper_assign_FIRM_panels.R", format = "file"),

  tar_target(
    drop_parcels,
    {
      source(drop_pins_file, local = TRUE)
      drop_parcels
    }
  ),



  ## ---- Read/assemble lookup tables ----
  tar_target(pin_muni_key_file, "data/raw/pin_muni_key.csv", format = "file"),
  tar_target(muni_nicknames_file, "../Merriman RA/ptax/Necessary_Files/muni_shortnames.xlsx", format = "file"),
  tar_target(floodfactor_file, "data/processed/floodfactor_scores.csv", format = "file"),
  tar_target(manual_ff_scores_file, "data/processed/pins_with_some_addresses_forgoogle.xlsx", format = "file"),


  tar_target(
    # joined in during filling in nbhd code variables
    manual_flood_scores,

    readxl::read_xlsx(manual_ff_scores_file) |>
      mutate(pin = str_pad(pin, 14, "left", "0"),
        pin10 = str_sub(pin, 1, 10)) |>
      select(pin10, flood_factor_score, clean_name)
  ),

  # file where I manually changed the preliminary date and effective dates (NFHL didn't have preliminary dates)
  tar_target(
    firm_dates_file,
    "inputs/Cook_2026_download/S_FIRM_PAN.xlsx",
    format = "file"
  ),

  tar_target(
    firm_dates,
    readxl::read_xlsx(firm_dates_file) |>
      dplyr::select(
        FIRM_PAN,   # what the join will be based off of
        VERSION_ID,
        FIRM_ID,
        PRE_DATE, # mostly want this variable
        EFF_DATE
      )
  ),


  tar_target(
    pin_muni_key_tbl,
    read_pin_muni_key(pin_muni_key_file, muni_nicknames_file, floodfactor_file)
  ),

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

  # originally from Microsofts building footprints. Downloaded whole state of buildings,
  # clips it with Cook Border on the way in
  tar_target(
    buildings_raw,
    read_buildings(buildings_files, border = border, crs_out = sf::st_crs(border))
  ),

  tar_file(ptaxsim_db_file, "../Merriman RA/ptax/ptaxsim.db/ptaxsim-2023.0.0.db"),

  # county border used for clipping other shapefiles
  tar_target(
    cook_border_file,
    "inputs/Mapping_FIRMs/Cook_County_Border/Cook_County_Border.shp",
    format = "file"
  ),
  tar_target(
    border,
    read_border(cook_border_file, crs_out = 6454)
  ),

  tar_target(common_crs, sf::st_crs(border)),


  ## ----- PTAXSIM parcel centroids and parcel polygons ------
  # an option for all parcels without using 2018 and 2022 historic parcel geodatabases from online
  # also has information for which years the parcel existed
  # tables extracted from CCAO PTAXSIM database
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
        dplyr::filter(!sf::st_is_empty(geometry))
    }
  ),


  # technically is 10 digit land parcels, not 14-digit PIN
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

  # some parcels appear twice with broken up start and stop dates
  # keeps unique parcel with last (most recent) longitude and latitude and combines start and stop years
  tar_target(
    pin_centroids,
    pin_centroids_raw |>
      dplyr::group_by(pin10) |>
      dplyr::summarize(
        longitude  = dplyr::last(longitude),
        latitude   = dplyr::last(latitude),
        start_year = min(start_year, na.rm = TRUE),
        end_year   = max(end_year,   na.rm = TRUE)
      ) |>
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  ),


  # --- Spatial polygons for property parcels ---------------------------------
  # (Either downloaded  from Parcel Viewer backend data
  # or Cook County data portal (assessor's archived parcel polygons))

  # parcel polygons as of tax year 2018, downloaded from Cook Open Data
  # tar_target(
  #   parcels_2018_gdb,
  #   "inputs/Historical_Parcels_-_2018.gdb/parcels_2018.gdb",
  #   format = "file"
  # ),

  # parcel polygons as of tax year 2022, downloaded from Cook Open Data
  tar_target(
    parcels_2022_gdb,
    "inputs/Mapping_Firms/Historical_Parcels_-_2022.gdb/ccao_2022parcels.gdb",
    format = "file"
  ),

  # parcel polygons as of tax year 2025, downloaded from Cook County Parcel Viewer datasets
  tar_target(
    parcels_2025_gdb,
    "inputs/cook_parcels_2025.gpkg",
    format = "file"
  ),

  # parcel polygons as of tax year 2018, downloaded from Cook County Parcel Viewer datasets
  tar_target(
    parcels_2018_gdb,
    "inputs/cook_parcels_2018.gpkg",
    format = "file"
  ),

  tar_target(
    parcels_with_firms,
    readxl::read_xlsx("data/processed/parcel_centroids_FIRM_PAN.xlsx") |>
      select(pin10 = PIN10, FIRM_PAN) |> distinct() |> left_join(firm_dates)
  ),

  tar_target(
    indicator_combined,
    read_rds("data/processed/indicator_combined.RDS")
  ),

  # --- Spatial polygons from NFHL geodatabases (State or County datasets, depending on vintage) ---

  ## ---- Archived Polygons from Miyuki; 2018 NFHL ---------------------------
  # historic nfhl from 2018 (before FIRM updates)
  tar_target(
    nfhl_2018_gdb,
    "inputs/Mapping_FIRMs/NFHL_17_20180129.gdb/NFHL_17_20180129.gdb",
    format = "file"
  ),


  tar_target(
    nfhl_2026_gdb,
    "inputs/cook_2026_county_nfhl.gdb",
    format = "file"
  ),

  ## --- 2026 NFHL polygons -----------------------------------

  # now an official geodatabase since the maps became effective in northern cook in late January 2026
  # now no longer need the 2024 geodatabase
  tar_target(
    firm_panels_2026_shp,
    "inputs/cook_2026_county_nfhl.gdb/S_FIRM_Pan.shp",
    format = "file"
  ),


  # ------ Clean Parcel Data and Identify FIRM panel each parcel is in --------------
  # Uses PIN centroid and FIRM polygon because it is much faster and panels are large squares
  tar_target(
    firm_panels_2026,
    read_firm_panels_shp(
      shp_path = firm_panels_2026_shp,
      border   = border,
      crs_out  = sf::st_crs(border))
  ),

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
        dplyr::select(-dplyr::any_of(c("PCOMM", "PANEL", "SUFFIX", "ST_FIPS", "VERSION_ID",
          "SCALE", "PANEL_TYP", "PNP_REASON", "BASE_TYP", "DFIRM_ID", "EFF_DATE", "EFF_DAT",
          "geometry")))
      x
    }
  ),



  # --- read in parcel polygons that are used to identify parcels that overlap with SFHA ---
  tar_target(
    parcels_2018,
    read_parcels_2018(parcels_2018_gdb, crs_out = sf::st_crs(border), border = border)
  ),

  # still used for joining buildings for now
  tar_target(
    parcels_2022, # Used for 2024 NFHL SFHA areas and 2026 Pending FIRM areas
    read_parcels_2022(parcels_2022_gdb, crs_out = sf::st_crs(border), border = border)
  ),


  # parcel polygons as of tax year 2025, downloaded from Cook County Parcel Viewer datasets
  tar_target(
    parcels_2025,
    read_parcels_2025(parcels_2025_gdb, crs_out = sf::st_crs(border), border = border)
    #  sf::st_read("inputs/cook_parcels_2025.gpkg", layer = "parcels_2025", quiet = TRUE) |>
    # sf::st_transform(border)
  ),


  # --- Hazard polygons for Cook County; multiple vintages  ---
  # State wide geodatabases are clipped using county border when read in
  tar_target(
    sfha_2018_poly,
    read_nfhl_sfha(nfhl_2018_gdb, layer = "S_Fld_Haz_Ar", border = border, sfha_tf_field = "SFHA_TF")
  ),

  tar_target(
    sfha_2026_poly,
    read_nfhl_sfha(nfhl_2026_gdb, layer = "S_FLD_HAZ_AR", border = border, sfha_tf_field = "SFHA_TF")
  ),


  tar_target(
    lomr_2018_poly,
    read_nfhl_lomr(nfhl_2018_gdb, layer = "S_LOMR", border = border)
  ),

  tar_target(
    lomr_2026_poly,
    read_nfhl_lomr(nfhl_2026_gdb, layer = "S_LOMR", border = border)
  ),


  ## --- Expanded Risk zone; 1 in 500 year flood area according to FEMA  ----
  tar_target(
    risk500_2018_poly,
    read_nfhl_1in500(nfhl_2018_gdb, layer = "S_Fld_Haz_Ar", border = border, sfha_tf_field = "SFHA_TF")
  ),

  tar_target(
    risk500_2026_poly,
    read_nfhl_1in500(nfhl_2026_gdb, layer = "S_FLD_HAZ_AR", border = border, sfha_tf_field = "SFHA_TF")
  ),



  # --- Identify Parcels in Hazard Zones -----------------------

  ## --- Using 2018 and 2022 Archived Parcel Geodatabases -------------
  tar_target(
    parcels_sfha_2018,
    join_parcels_to_zone(parcels_2018, sfha_2018_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    parcels_sfha_2026,
    join_parcels_to_zone(parcels_2025, sfha_2026_poly, id_field = "DFIRM_ID")
  ),


  tar_target(
    parcels_lomr_2018,
    join_parcels_to_zone(parcels_2018, lomr_2018_poly, id_field = "LOMR_ID")
  ),

  tar_target(
    parcels_lomr_2026,
    join_parcels_to_zone(parcels_2025, lomr_2026_poly, id_field = "LOMR_ID")
  ),


  ## ---- Using PTAXSIM polygons instead of parcel geodatabases  ----------
  tar_target(
    ptaxsim_sfha_2018,
    join_parcels_to_zone(ptaxsim_parcels_sf, sfha_2018_poly, id_field = "DFIRM_ID")
  ),


  tar_target(
    ptaxsim_sfha_2026,
    join_parcels_to_zone(ptaxsim_parcels_sf, sfha_2026_poly, id_field = "DFIRM_ID")
  ),


  ## ---- Building Polygons for Flag variables  ----

  ### ---- Combine building polygons and parcel polygons so that buildings have Parcel number
  tar_target(
    buildings_with_pin,
    assign_buildings_to_parcels(buildings_raw, parcels_2022, parcel_pin_field = "pin")
  ),

  ### ---- Join buildings to risk polygon: Keep intersecting parcels -------------------------------
  tar_target(
    buildings_sfha_2018,
    join_buildings_to_zone(buildings_with_pin, sfha_2018_poly, id_field = "DFIRM_ID")
  ),


  tar_target(
    buildings_sfha_2026,
    join_buildings_to_zone(buildings_with_pin, sfha_2026_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    buildings_lomr_2018,
    join_buildings_to_zone(buildings_with_pin, lomr_2018_poly, id_field = "LOMR_ID")
  ),

  tar_target(
    buildings_lomr_2026,
    join_buildings_to_zone(buildings_with_pin, lomr_2026_poly, id_field = "LOMR_ID")
  ),


  ## ---- Buildings in expanded Risk zones ----------------------------
  tar_target(
    buildings_risk500_2018,
    join_buildings_to_zone(buildings_with_pin, risk500_2018_poly, id_field = "DFIRM_ID")
  ),

  tar_target(
    buildings_risk500_2026,
    join_buildings_to_zone(buildings_with_pin, risk500_2026_poly, id_field = "DFIRM_ID")
  ),



  ## ---- Land Parcels that intersect expanded Risk zones ----------------------------
  tar_target(
    land_risk500_2018,
    join_parcels_to_zone(ptaxsim_parcels_sf, risk500_2018_poly, id_field = "DFIRM_ID")
  ),


  tar_target(
    land_risk500_2026,
    join_parcels_to_zone(ptaxsim_parcels_sf, risk500_2026_poly, id_field = "DFIRM_ID")
  ),



  # ---------- SFHA indicator files (this is “hits-only”) -------------
  tar_target(
    sfha_indicator_land_parcels,
    make_sfha_indicator_parcels(
      parcels_sfha_2018,
      parcels_sfha_2026,
      parcels_lomr_2018,
      parcels_lomr_2026
    )
  ),

  tar_target(
    ptaxsim_sfha_indicator_parcel_polygons,
    make_sfha_indicator_parcels(
      ptaxsim_sfha_2018,
      ptaxsim_sfha_2026,
      parcels_lomr_2018,
      parcels_lomr_2026
    )
  ),

  tar_target(
    sfha_indicator_buildings,
    make_sfha_indicator_buildings(
      buildings_sfha_2018,
      buildings_sfha_2026,
      buildings_lomr_2018,
      buildings_lomr_2026
    )
  ),

  tar_target(
    risk500_indicator_buildings,
    make_sfha_indicator_buildings(
      buildings_risk500_2018,
      buildings_risk500_2026,
      buildings_lomr_2018,
      buildings_lomr_2026
    )
  ),

  tar_target(
    risk500_indicator_land,
    make_sfha_indicator_parcels(
      land_risk500_2018,
      land_risk500_2026,
      parcels_lomr_2018,
      parcels_lomr_2026
    )
  ),

  ## --- Add flag variables ----------------------------
  tar_target(
    sfha_indicator_final_land,
    make_sfha_indicator_final(
      parcels_with_firms = parcels_with_firms,
      indicator_df       = sfha_indicator_land_parcels,
      zone_2018          = "FLD_ZONE_2018",
      zone_2026          = "FLD_ZONE_2026",
      lomr_date_col      = "lomr_date",
      out_prefix         = "land_sfha"
    )
  ),


  tar_target(
    ptaxsim_sfha_indicator_final,
    make_sfha_indicator_final(
      parcels_with_firms = parcels_with_firms,
      indicator_df       = ptaxsim_sfha_indicator_parcel_polygons,
      zone_2018          = "FLD_ZONE_2018",
      zone_2026          = "FLD_ZONE_2026",
      lomr_date_col      = "lomr_date",
      out_prefix         = "ptax_land_sfha"
    )
  ),


  tar_target(
    sfha_indicator_final_buildings,
    make_sfha_indicator_final(
      parcels_with_firms = parcels_with_firms,
      indicator_df       = sfha_indicator_buildings,
      zone_2018          = "bldg_FLD_ZONE_2018",
      zone_2026          = "bldg_FLD_ZONE_2026",
      lomr_date_col      = "lomr_date",
      out_prefix         = "bldg_sfha"
    )
  ),



  tar_target(
    risk500_indicator_final_buildings,
    make_sfha_indicator_final(
      parcels_with_firms,
      risk500_indicator_buildings,
      zone_2018 = "bldg_FLD_ZONE_2018",
      zone_2026 = "bldg_FLD_ZONE_2026",
      out_prefix = "risk500_bldg_"
    )
  ),



  tar_target(
    risk500_indicator_final_land,
    make_sfha_indicator_final(
      parcels_with_firms,
      risk500_indicator_land,
      zone_2018 = "FLD_ZONE_2018",
      zone_2026 = "FLD_ZONE_2026",
      out_prefix = "risk500_land_"
    )
  ),


  # ----- Make combined indicator file -------------------------------
  tar_target(
    sfha_compare_pin10_fourway,
    sfha_indicator_final_land |>
      dplyr::select(pin10, land_sfha2018, land_sfha2026, lomr_date, PRE_DATE, EFF_DATE, FIRM_PAN, VERSION_ID) |>
      dplyr::left_join(
        sfha_indicator_final_buildings |>
          dplyr::select(pin10, bldg_sfha2018, bldg_sfha2026, bldg_lomr_date = lomr_date),
        by = "pin10"
      ) |>
      # dplyr::left_join(
      #   ptaxsim_sfha_indicator_final |>
      #     dplyr::select(pin10, ptax_land_sfha2018, ptax_land_sfha2024, ptax_land_sfha2026),
      #   by = "pin10"
      # ) |>
      dplyr::left_join(
        risk500_indicator_final_buildings |>
          dplyr::select(pin10, risk500_bldg_2018, risk500_bldg_2026),
        by = "pin10")  |>
      dplyr::left_join(
        risk500_indicator_final_land |>
          dplyr::select(pin10, risk500_land_2018, risk500_land_2026),
        by = "pin10") |>

      group_by(pin10) |>
      slice_head(n = 1) |>
      ungroup()
  ),

  # ---- Prep Sales Data ----

  tarchetypes::tar_map(
    values = sales_versions,
    names  = sales_label,
    list(
      # --- Sales ingest (version-specific) ---
      tar_target(
        sales_csv_file,
        sales_file,
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

      # joins residential sales and the indicators for both parcel-based and building-based SFHA indicator variables
      tar_target(
        sales_joined, # has residential only sales now

        merge_sales_sfha(res_sales, sfha_compare_pin10_fourway)
      ),

      # checking sales_joined_updated: had missing firm panels but after the filtering below, none remained
      # sales_joined_updated |> fill_missing_panels() |>filter(is.na(FIRM_PAN) & res_c2 == TRUE & times_sold > 1 & sale_price > 5000 & !pin10  %in% drop_parcels) |>
      # fill_missing_firm_pan()|>  |> View()
      # BUT there were stil 53 observations missing land_sfha2018. So it is filling in the firm pans, but not before the firm_dates are merged in.
      # which means it will have a firm panel, but not the indicators that go with it.
      # or that it doesn't have a polygon that worked with the SFHA areas
      # 17 distinct parcels don't have their data
      # notes on each in pins_still_missing.R script

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
            df_prep_final,  # make df_prep includes keep_df_prep_filters and nbhd_fill functions
            make_df_prep(repeat_sales, pin_muni_key_tbl) |>
              left_join(manual_flood_scores, by = "pin10") |>
              mutate(flood_factor_score = ifelse(is.na(env_flood_fs_factor), flood_factor_score, env_flood_fs_factor),
                clean_name = ifelse(is.na(clean_name.x), clean_name.y, clean_name.x),
                high_ff_score = ifelse(env_flood_fs_factor > 4 | flood_factor_score > 4, TRUE, FALSE))
          ),


          # tar_target(
          #   df_prep_final_rds,
          #   {
          #     dir.create(sales_out_dir, recursive = TRUE, showWarnings = FALSE)
          #     out_file <- file.path(
          #       sales_out_dir,
          #       paste0("df_prep_", out_stub, "_", sales_label, "_arcgis.rds")
          #     )
          #     saveRDS(df_prep_final, out_file)
          #     out_file
          #   },
          #   format = "file"
          # ),

          tar_target(
            q1_model_inputs,
            add_q1_assumption_vars(
              df_prep_final
            )
          ),

          tar_target(
            q1_model_inputs_rds,
            {
              dir.create(sales_out_dir, recursive = TRUE, showWarnings = FALSE)
              out_file <- file.path(
                sales_out_dir,
                paste0("df_prep_", out_stub, "_", sales_label, "_arcgis_forQ1_assumptiontests.rds")
              )
              saveRDS(q1_model_inputs, out_file)
              out_file
            },
            format = "file"
          )
        )
    ))
  ) # ,
  # tar_target(
  #   combined_datasets,
  #   {
  #     updated_map <- c(
  #       bldg_updated = file.path(sales_out_dir, "df_prep_bldg_updated_arcgis_forQ1_assumptiontests.rds"),
  #       land_updated = file.path(sales_out_dir, "df_prep_land_updated_arcgis_forQ1_assumptiontests.rds"),
  #       risk500_updated = file.path(sales_out_dir, "df_prep_risk500_updated_arcgis_forQ1_assumptiontests.rds"),
  #       risk500_land_updated = file.path(sales_out_dir, "df_prep_risk500_land_updated_arcgis_forQ1_assumptiontests.rds")
  #     )
  #
  #     sales2026_map <- c(
  #       bldg_updated = file.path(sales_out_dir, "df_prep_bldg_2026_arcgis_forQ1_assumptiontests.rds"),
  #       land_updated = file.path(sales_out_dir, "df_prep_land_2026_arcgis_forQ1_assumptiontests.rds"),
  #       risk500_updated = file.path(sales_out_dir, "df_prep_risk500_2026_arcgis_forQ1_assumptiontests.rds"),
  #       risk500_land_updated = file.path(sales_out_dir, "df_prep_risk500_land_2026_arcgis_forQ1_assumptiontests.rds")
  #     )
  #
  #     list(
  #       updated = read_q1_dataset_bundle(updated_map),
  #       sales_2026 = read_q1_dataset_bundle(sales2026_map)
  #     )
  #   }
  # )
)
