# Reproducibility and model provenance

## Original source flow

1. `analysis/01-milestones/`: early simulation exploration and visual prototypes; code constructs `delivery_sim`.
2. `analysis/04-calibration-exploration/test-data-compare.R`: references `df_real`, `shape`, `rate` and `daily_controls`, compares real and gamma-simulated distributions, then adds route difficulty and organization labels.
3. `analysis/02-monthly-simulation/bag-control.R`: uses `daily_controls`, allocates packages to bags and expands each bag into `delivery_structure` using fixed seeds.
4. `delivery-final-1.R` and `delivery-final-2.R`: alternative generation variants using `rgamma`, random noise, difficulty and bag-progress effects. Version 1 explicitly makes envelopes slower/more variable; version 2 assigns different type offsets. Neither has been established as the exact run behind every exported image. Do not run both in sequence and assume the result is the original final dataset.
5. `analysis/03-visualizations/`: violin and weekly plots generally expect `delivery_final`; bag scatter expects `day_bag_summary`; pie chart expects `time_share`; other session objects may be needed.

## R packages

```r
install.packages(c("dplyr", "tidyr", "purrr", "tibble", "ggplot2", "ggforce", "scales", "patchwork"))
```

Run scripts from a clean R session only after reviewing dependencies. The original files were written as iterative R-session scripts rather than a single tested pipeline. Package versions were not supplied; no R execution was possible in the preparation environment. Some milestone scripts export PNGs into the working directory. Use a dedicated scratch directory.

## Interpretation

The interview describes collecting firsthand observations with voice memos, while the supplied source code explicitly generates simulated workflow data. Comments describe calibration to real observations, but the underlying data and calibration parameters are absent. The monthly results are modeled examples, not independent evidence of a package-type effect, learning effect, or superiority of either sorting strategy. The model encodes remaining-package and organization assumptions, which can generate the patterns shown. The original image exports remain unchanged; the selected copies are composited on white for consistent web legibility.

## Publication boundaries

No raw recordings, customer addresses, tracking numbers or identifiable operational data should be published. Future comparative work should include up-front sorting time and control or counterbalance route, volume, experience, package mix and order effects.
