# Q2 dissertation marginal effects scripts

Run these from the project root, not from inside the `scripts/` folder.

Recommended order:

```r
source("scripts/q2_dissertation_00_prep_data.R")
source("scripts/q2_dissertation_01_basic_twfe_models.R")
source("scripts/q2_dissertation_02_etwfe_models.R")
source("scripts/q2_dissertation_03_main_marginaleffects.R")
source("scripts/q2_dissertation_04_robustness_models.R")
```

Or run everything with:

```r
source("scripts/q2_dissertation_run_all.R")
```

Outputs go to:

```text
outputs/q2_marginaleffects/
outputs/q2_marginaleffects/tables/
outputs/q2_marginaleffects/marginaleffects/
outputs/q2_marginaleffects/etwfe/
outputs/q2_marginaleffects/diagnostics/
```

The scripts now also save dropped-term diagnostics for terms with missing or very large standard errors. Presentation tables omit those terms, and the companion Quarto file displays the dropped-term diagnostics in a tabset.

The marginal effects outputs are saved in log-point units. The earlier `pct_change`, `pct_low`, and `pct_high` variables are no longer created.

The companion Quarto file `dissertation_tables_Q2_marginaleffects_from_saved_outputs.qmd` loads these saved objects and does not re-estimate the heavy models during rendering.
