# Run all Q2 assumption-test precomputation scripts from the project root.

# Reuse the existing prepared input; do not overwrite it just to remake plots.
# This producer writes the exact path read by scripts 01 and 02.
if (!file.exists("outputs/q2_models/bldg_sfha_q2_prepped.RDS")) {
  source("scripts/q2_FF_variable_checks/q2_FF_var_checks_00_prep_data.R")
}
source("scripts/q2_assumption_tests/01_create_q2_plot_objects.R")
source("scripts/q2_assumption_tests/02_create_q2_model_and_table_objects.R")
