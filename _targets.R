# _targets.R
library(targets)

tar_option_set(
  packages = c("sf", "dplyr", "readr", "readxl", "stringr", "tidyr", "lubridate")
)

# helper functions for the spatial joins + rollups
source("R/sfha_targets_functions.R")

targets_out_dir <- "data/processed/targets"
sfha_out_dir    <- file.path(targets_out_dir, "sfha")

list(
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

  # county border for clipping
  tar_target(
    cook_border_file,
    "inputs/Mapping_FIRMs/Cook_County_Border/Cook_County_Border.shp",
    format = "file"
  ),

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

  # SFHA polygons (include T and F, need to filter it!!)
  tar_target(
    prelim_sfha_shp,
    "inputs/FIRMDB_11182022_Cook-County_Illinois/S_Fld_Haz_Ar.shp",
    format = "file"
  ),

  # updated FIRM panels with their data. includes pending FIRMs for 2026
  tar_target(
    firm_panels_2026_shp,
    "inputs/Cook_2026_download/S_FIRM_Pan.shp",
    format = "file"
  ),


  # --- read base geometries ---
  tar_target(
    border,
    read_border(cook_border_file, crs_out = 6454)
  ),

  # tar_target(
  #   firm_panels_2018,
  #   read_firm_panels(nfhl_2018_gdb, layer = "S_FIRM_PAN", border = border, crs_out = sf::st_crs(border))
  # ),
  #
  # tar_target(
  #   firm_panels_2024,
  #   read_firm_panels(nfhl_state_2024_gdb, layer = "S_FIRM_PAN", border = border, crs_out = sf::st_crs(border))
  # ),

  tar_target(
    firm_panels_2026,
    read_firm_panels_shp(
      shp_path = firm_panels_2026_shp,
      border   = border,
      crs_out  = sf::st_crs(border)
    )
  ),


  # --- read in parcel polygons that are used to identify parcels that overlap with SFHA ---
  tar_target(
    parcels_2018,
    read_parcels_2018(parcels_2018_gdb, crs_out = sf::st_crs(border), border = border)
  ),

  tar_target(
    parcels_2022,
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

  tar_target(
    sfha_2026_poly,
    read_prelim_sfha(nfhl_state_2026_gdb, layer = "S_FLD_HAZ_AR",  border = border, sfha_tf_field = "SFHA_TF", crs_out = sf::st_crs(border))
  ),

  # preliminary FIRM  changes were not a geodatabase during initial data availability, only shapefile
  tar_target(
    sfha_prelim_poly,
    read_prelim_sfha(prelim_sfha_shp, border = border, crs_out = sf::st_crs(border))
  ),

  tar_target(
    lomr_2018_poly,
    read_nfhl_lomr(nfhl_2018_gdb, layer = "S_LOMR", border = border)
  ),

  tar_target(
    lomr_2024_poly,
    read_nfhl_lomr(nfhl_state_2024_gdb, layer = "S_LOMR", border = border)
  ),

  # --- spatial joins (expensive) ---
  tar_target(
    parcels_sfha_2018,
    join_parcels_to_zone(parcels_2018, sfha_2018_poly, id_field = "DFIRM_ID"),
    cue = tar_cue(mode = "thorough")
  ),

  tar_target(
    parcels_sfha_2024,
    join_parcels_to_zone(parcels_2022, sfha_2024_poly, id_field = "DFIRM_ID"),
    cue = tar_cue(mode = "thorough")
  ),

  tar_target(
    parcels_prelim_sfha,
    join_parcels_to_zone(parcels_2022, sfha_prelim_poly, id_field = "DFIRM_ID"),
    cue = tar_cue(mode = "thorough")
  ),

  tar_target(
    parcels_lomr_2018,
    join_parcels_to_zone(parcels_2018, lomr_2018_poly, id_field = "LOMR_ID"),
    cue = tar_cue(mode = "thorough")
  ),

  tar_target(
    parcels_lomr_2024,
    join_parcels_to_zone(parcels_2022, lomr_2024_poly, id_field = "LOMR_ID"),
    cue = tar_cue(mode = "thorough")
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
    sfha_indicator_pins_csv,
    {
      targets_dirs
      out <- file.path(sfha_out_dir, "sfha_indicator_pins.csv")
      readr::write_csv(sfha_indicator_pins, out)
      out
    },
    format = "file"
  ),

  # tar_target(
  #   parcels_withFIRMs_2018,
  #   {
  #     out <- assign_panel_to_parcels(
  #       parcels   = parcels_2018,
  #       panels    = firm_panels_2018,
  #       parcel_id = "pin10"
  #     )
  #     assert_panel_assignment(out, parcel_id = "pin10")
  #     out
  #   }
  # ),
  #
  # tar_target(
  #   parcels_withFIRMs_2024,
  #   {
  #     out <- assign_panel_to_parcels(
  #       parcels   = parcels_2022,
  #       panels    = firm_panels_2024,
  #       parcel_id = "pin10"
  #     )
  #     assert_panel_assignment(out, parcel_id = "pin10")
  #     out
  #   }
  # ),

  tar_target(
    parcels_withFIRMs_2026,
    {
      out <- assign_panel_to_parcels(
        parcels   = parcels_2022,
        panels    = firm_panels_2026,
        parcel_id = "pin10"
      )
      assert_panel_assignment(out, parcel_id = "pin10")
      out
    }
  ),

  # tar_target(
  #   parcels_withFIRMs_csv,
  #   {
  #     targets_dirs
  #
  #     out <- parcels_withFIRMs_2018 |>
  #
  #   dplyr::rename(panel_2018 = PANEL, dfirm_id_2018 = DFIRM_ID) |>
  #   dplyr::full_join(
  #     parcels_withFIRMs_2024 |>
  #       dplyr::rename(panel_2024 = PANEL, dfirm_id_2024 = DFIRM_ID),
  #     by = "pin10"
  #   ) |>
  #      dplyr::full_join(
  #         parcels_withFIRMs_2026 |>
  #           dplyr::rename(panel_2026 = PANEL, dfirm_id_2026 = DFIRM_ID),
  #         by = "pin10"
  #       ),
  #
  #     out_file <- file.path(sfha_out_dir, "parcels_withFIRMs.csv")
  #     readr::write_csv(out, out_file)
  #     out_file
  #   },
  #   format = "file"
  # ),



  # ============================================================
  # STAGE 2: Apply prelim coverage using parcels_wFIRMs + S_FIRM_PAN.xlsx
  # ============================================================

  tar_target(
    parcels_with_firms,
    readr::read_csv(parcels_withFIRMs_csv, show_col_types = FALSE) |>
      dplyr::transmute(
        pin10 = as.character(pin10),
        firm_pan = as.character(panel_2024) # or panel_2024, pick one
      ) |>
      dplyr::distinct(pin10, .keep_all = TRUE)
  ),


  tar_target(
    firm_pan_xlsx_file,
    "data/raw/S_FIRM_PAN.xlsx",
    format = "file"
  ),


  tar_target(
    prelim_old_firm_panels,
    {
      pan <- readxl::read_xlsx(firm_pan_xlsx_file)

      pan |>
        dplyr::filter(.data$SOURCE_DB == "PreliminaryFIRMDB") |>
        dplyr::mutate(
          old_firm_panel = dplyr::coalesce(
            as.character(.data$old_firm_panel),
            stringr::str_replace(as.character(.data$FIRM_PAN), "K$", "J")
          )
        ) |>
        dplyr::filter(!is.na(.data$old_firm_panel)) |>
        dplyr::pull(.data$old_firm_panel) |>
        unique()
    }
  ),

  tar_target(
    sfha_indicator_final,
    {
      # sfha_indicator_pins is "hits-only": missing means 0 for full-county vintages.
      # For prelim: compute NA/0/1 using prelim_covered.
      base <- parcels_with_firms |>
        dplyr::mutate(prelim_covered = as.integer(.data$firm_pan %in% prelim_old_firm_panels))

      base |>
        dplyr::left_join(sfha_indicator_pins, by = "pin10") |>
        dplyr::mutate(
          sfha2018 = dplyr::coalesce(as.integer(sfha2018), 0L),
          sfha2024 = dplyr::coalesce(as.integer(sfha2024), 0L),
          lomr2018 = dplyr::coalesce(as.integer(lomr2018), 0L),
          lomr2024 = dplyr::coalesce(as.integer(lomr2024), 0L),

          prelim_hit = dplyr::coalesce(as.integer(prelimsfha), 0L),
          prelimsfha = dplyr::case_when(
            prelim_covered == 0L ~ NA_integer_,
            prelim_covered == 1L & prelim_hit == 1L ~ 1L,
            prelim_covered == 1L & prelim_hit == 0L ~ 0L
          )
        ) |>
        dplyr::select(-prelim_hit)
    }
  ),

  tar_target(
    sfha_indicator_final_csv,
    {
      targets_dirs
      out <- file.path(sfha_out_dir, "sfha_indicator_final.csv")
      readr::write_csv(sfha_indicator_final, out)
      out
    },
    format = "file"
  )
)
