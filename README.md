# SY142-Game-Theory-1

**A Game-Theoretic Simulation Framework for Teaching Strategic Competition in Health Service Markets**

Version 1.0 | R-based simulation framework

## Overview

This repository contains the complete simulation code, data outputs, and figure generation scripts for the game-theoretic simulation framework SY142-Game-Theory-1. The framework models strategic competition between two asymmetric healthy living centers as a 36-month repeated Prisoner's Dilemma, designed for teaching strategic management in health service markets.

## Citation

If you use this framework or data in your research, please cite:

> Yılmaz, S.; Günal, A.M. A Game-Theoretic Simulation Framework for Teaching Strategic Competition in Health Service Markets. *Mathematics* **2026**, *14*, x. https://doi.org/10.3390/mathXXXXXXX

## Repository Structure

```
SY142-Game-Theory-1/
├── R-Codes/
│   ├── TamSimulasyon.R           # Core simulation engine (all functions)
│   ├── RunAll.R                  # Main analysis script (runs all analyses)
│   ├── FiguresGeneration.R       # Figure generation (supplementary figures)
│   ├── GenerateFigures_Paper.R   # Paper-specific figures (Figures 2-5)
│   └── YarisKodu.R               # Tournament execution code
├── data/
│   ├── payoff_matrix.csv                 # 4×4 NPV payoff matrix (16 combinations)
│   ├── tournament_rankings.csv           # Base tournament rankings (10 strategies)
│   ├── tournament_full_matrix.csv        # Base tournament full matchup matrix
│   ├── extended_strategy_rankings.csv    # Extended tournament rankings (22 strategies)
│   ├── extended_strategy_matrix.csv      # Extended tournament full matchup matrix
│   ├── monte_carlo_coop_results.csv      # Monte Carlo results - Cooperation (n=1000)
│   ├── monte_carlo_nash_results.csv      # Monte Carlo results - Nash Eq. (n=1000)
│   ├── sample_simulation_monthly.csv     # Sample 36-month simulation output (53 variables)
│   ├── sensitivity_discount_rate.csv     # Sensitivity: discount rate (5 levels)
│   ├── sensitivity_shock_prob.csv        # Sensitivity: shock probability (11 levels)
│   ├── sensitivity_base_demand.csv       # Sensitivity: base demand (6 levels)
│   └── sensitivity_burnout.csv           # Sensitivity: burnout threshold (8 levels)
├── figures/
│   └── (generated figures in PNG and PDF)
├── README.md
└── LICENSE
```

## Requirements

- **R** version 4.5.2 or later
- **R packages**: ggplot2, patchwork, readxl (for student strategy input)

Install required packages:
```r
install.packages(c("ggplot2", "patchwork", "readxl"))
```

## Usage

### Running the Complete Analysis

```r
source("R-Codes/TamSimulasyon.R")
results <- run_comprehensive_analysis()
export_results_to_csv(results)
```

This executes:
1. Payoff matrix calculation (100 simulations × 16 strategy combinations)
2. Nash equilibrium identification
3. Pareto optimality assessment
4. Prisoner's Dilemma condition verification
5. Strategy tournament (10 strategies, 100 simulations per matchup)
6. Monte Carlo analysis (1,000 simulations each for cooperation and Nash scenarios)
7. Sensitivity analyses (discount rate, shock probability, base demand, burnout threshold)

### Running the Extended Tournament

```r
source("R-Codes/TamSimulasyon.R")
extended <- run_extended_strategy_analysis()
```

This evaluates 22 strategies with 200 simulations per matchup (96,800 total runs).

### Generating Paper Figures

```r
source("R-Codes/GenerateFigures_Paper.R")
```

Generates Figures 2–5 at 600 DPI in PNG and PDF formats.

## Model Parameters

| Parameter | Center A | Center B |
|-----------|----------|----------|
| Monthly Capacity | 115 clients | 88 clients |
| Fixed Cost | 45,000 TL/month | 32,000 TL/month |
| Brand Bonus | +4% market share | 0% |
| Discount Rate | 1.33%/month (16%/yr) | 1.58%/month (19%/yr) |
| Initial Satisfaction | 70 | 75 |
| Initial Reputation | 72 | 65 |

## Key Results

- **Nash Equilibrium**: (S2, S2) with total NPV = 16.25M TL
- **Cooperative Outcome**: (S3, S3) with total NPV = 20.21M TL
- **Cooperation Premium**: +24.1%
- **Tournament Winner**: Forgiving Tit-for-Tat (10.87M TL avg NPV)
- **Asymmetric PD**: Center A faces classical PD; Center B's rational choice aligns with cooperation

## Reproducibility

All analyses use a fixed random seed (2026). Running the code with the same R version should produce identical results. Approximate computation time: 130–145 minutes for the complete analysis on a standard desktop computer.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact

- **Salim Yılmaz** - salimyilmaz142@gmail.com & salim.yilmaz@acibadem.edu.tr
- Department of Healthcare Management, Acıbadem Mehmet Ali Aydınlar University, Istanbul, Türkiye
