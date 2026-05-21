# Q2 Flood Factor model scripts

Run these from the project root, not from inside the `scripts/` folder.

Recommended order:

```r
source("scripts/q2_00_prep_data.R")
source("scripts/q2_01_run_ff_models.R")
source("scripts/q2_02_run_marginaleffects.R")
```

Outputs go to:

```text
outputs/q2_models/
outputs/q2_models/tables/
outputs/q2_models/marginaleffects/
outputs/q2_models/figures/
```

The Quarto document should load saved `.rds`, `.html`, `.csv`, or figure files from those folders instead of re-estimating models while rendering.

The script `q2_03_quarto_load_outputs_example.R` contains copy/paste examples for your Quarto document.
