# SY142-Game-Theory-1

A Game-Theoretic Simulation Framework for Teaching Strategic Competition in Health Service Markets.

This repository holds the complete R simulation code and data outputs for the paper. It models strategic competition between two asymmetric healthy-living centers as a 36-month repeated Prisoner's Dilemma, with an Axelrod-style tournament (22 strategies), Monte Carlo validation (n = 1000), and sensitivity analyses.

## Citation

Yilmaz, S.; Gunal, A.M. A Game-Theoretic Simulation Framework for Teaching Strategic Competition in Health Service Markets. Mathematics 2026, 14, x. https://doi.org/10.3390/mathXXXXXXX (DOI is a placeholder until the paper receives its final identifier.)

## Reproduce

Requires R 4.5.2 and the packages `ggplot2`, `patchwork`, `readxl`, `dplyr`, `tidyr`, and `scales`. Install them with `install.packages(c("ggplot2", "patchwork", "readxl", "dplyr", "tidyr", "scales"))`, then run the canonical pipeline with `Rscript R-Codes/RunAll.R`.

All analyses use a fixed seed, `set.seed(2026)`, defined in `TamSimulasyon.R`. Running the code with the same R version reproduces the committed `results/*.csv` files and figures exactly. Approximate runtime is 130-145 minutes on a standard desktop.

## Artifact map

`R-Codes/TamSimulasyon.R` is the core simulation engine and holds all model parameters (`sim_params`). `R-Codes/RunAll.R` is the canonical pipeline: it runs every analysis and writes all `results/*.csv`, including the illustrative `sample_simulation_monthly.csv`, which is taken from the first Monte-Carlo cooperative run (`results$mc_coop$monthly_data[[1]]`). `R-Codes/FiguresGeneration.R` generates plots from the committed CSVs, including the Figure-4 panels built from that illustrative sample. `R-Codes/GenerateFigures_Paper.R` produces the paper-specific figures (Figures 2-5). `R-Codes/YarisKodu.R` is a standalone tournament runner; it duplicates the `sim_params` block from `TamSimulasyon.R` (keep the two in sync) and runs 200 sims per matchup to match the published results.

## Key facts

The extended tournament evaluates 22 strategies across 484 matchups at 200 sims each, for 96,800 total runs. Monte Carlo validation uses n = 1000 for each scenario (cooperation and Nash). The Nash equilibrium (S2, S2) yields total NPV 16.25M TL and the cooperative outcome (S3, S3) yields total NPV 20.21M TL, a cooperation premium of +24.1%.

## Label map (code label to manuscript English term)

Display-only labels in `sim_params` now use English terms aligned with the manuscript:

| Field | English terms |
| --- | --- |
| `price_levels$strategy` | Destructive, Aggressive, Cooperative, Premium |
| `marketing_levels$strategy` | None, Basic, Active, Intensive |
| `investments$name` | Technology, Training, Decoration, Equipment |

Internal keys remain in Turkish and are intentionally left unchanged: `investments$code` (T, E, D, C), `shocks$shock_type` (for example `ekonomik_kriz`), `seasonal_factors$season_name` (for example `Yilbasi`), and the center names (`Merkez A`, `Merkez B`).

## License

MIT License. See the `LICENSE` file.
