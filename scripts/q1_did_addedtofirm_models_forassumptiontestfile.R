# Step 3 of assumption test document. took out to run as background job.


library(tidyverse)
library(fixest)

library(didFF)   # for testing if functional form matters for the analysis. Tests if parallel trends assumption is sensitive to functional form (i.e. logged variable vs unlogged variable)
library(did)  # for step 3 and 4

bldg_sfha <- readRDS("data/processed/targets/sales/df_prep_bldg_v2026_03.RDS")

df_prep <- bldg_sfha

df_es_added <- df_prep |>
  filter(prelim_sfha_category %in% c("Added to prelim SFHA", "Never SFHA")) |>
  mutate(pin_num = as.numeric(pin)) |>
  dplyr::arrange(pin_num, sale_year, sale_date) |>
  dplyr::group_by(pin_num, sale_year, pre_date_chr) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup() |>
  mutate(treated = prelim_sfha_category == "Added to prelim SFHA") |>

  group_by(pin_num) |>
  mutate(
    g_add = if (any(addedto_prelim_sfha == 1, na.rm = TRUE)) {
      min(sale_year[addedto_prelim_sfha == 1], na.rm = TRUE)
    } else {
      0L
    }
  ) |>
  ungroup()


table(df_es_added$g_add)



# takes a lot longer than the removed & always in SFHA model above.
prelim_add_att_gt_out <- att_gt(yname = "log_price",
  tname = "sale_year",
  idname = "pin_num",
  gname = "g_add",
  clustervars = "pin10",
  data = df_es_added,
  allow_unbalanced_panel = TRUE,
  print_details = TRUE)

# save output and read it in in next chunk to deal with rendering time.
saveRDS(prelim_add_att_gt_out, file = "outputs/Q1_prelim_add_gt_out.rds")




prelim_add_att_gt_out <- read_rds("outputs/Q1_prelim_add_gt_out.rds")
summary(prelim_add_att_gt_out)
ggdid(prelim_add_att_gt_out)

agg_group <- aggte(prelim_add_att_gt_out, type = "group", na.rm = TRUE)
summary(agg_group)
ggdid(agg_group)
