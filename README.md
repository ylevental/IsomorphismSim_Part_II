# AntBoost

**An R Package Demonstrating the Isomorphism Between Boosting and Ant Colony Recruitment**

> Companion code for *"Isomorphic Functionalities between Ant Colony and Ensemble Learning: Part II — On the Strength of Weak Learnability and the Boosting Paradigm"* by Ernest Fokoué, Gregory Babbitt, and Yuval Levental (Rochester Institute of Technology).

---

## Overview

AntBoost implements both **AdaBoost** (with decision stumps) and **Ant Colony Adaptive Recruitment (ACAR)**, and provides simulation tools to demonstrate that these two systems are mathematically isomorphic:

| Boosting Concept | Ant Colony Analog |
|---|---|
| Iteration *t* | Recruitment wave *t* |
| Instance weights *D_t(i)* | Pheromone concentrations *τ_j(t)* |
| Weak learner on weighted data | Ant foraging guided by pheromone |
| Weight update via error | Pheromone update via site quality |
| Weighted majority vote | Quorum-based colony decision |

## Quick Start

### Prerequisites

```r
install.packages(c("ggplot2", "patchwork", "dplyr", "tidyr"))
```

### Generate All Manuscript Figures

From the repository root:

```bash
Rscript generate_all_figures.R
```

This produces `figures_boosting/` containing:

| File | Manuscript Reference | Description |
|---|---|---|
| `figure1_weak_learnability.pdf` | Figure 1 (Section 3.2) | Exponential convergence with γ |
| `figure2_weight_pheromone.pdf` | Figure 2 (Section 3.3) | Weight ↔ pheromone visual isomorphism |
| `figure3_margin_quorum.pdf` | Figure 3 (Section 3.4) | Boosting margin ↔ quorum margin |
| `figure4_convergence.pdf` | Figure 4 (Section 3.5) | Statistical equivalence of convergence |
| `figure5_noise_robustness.pdf` | Figure 5 (Section 3.6) | Identical degradation under noise |
| `table1_performance.csv` | Table 1 (Section 3.5) | Accuracy comparison data |

### Use as a Package

```r
# Install from local source
install.packages(".", repos = NULL, type = "source")

library(AntBoost)

# Run AdaBoost
dat <- generate_data(n = 300, p = 5, noise = 0.1)
res <- adaboost(dat$X, dat$y, M = 100)

# Run ACAR
acar_res <- acar(c(10, 5, 3, 2, 1), n_ants = 100, n_waves = 50)

# Generate all figures
generate_all_figures("my_figures/")
```

## Package Structure

```
AntBoost/
├── R/
│   ├── adaboost.R          # AdaBoost implementation
│   ├── acar.R              # Ant Colony Adaptive Recruitment
│   ├── data_generation.R   # Synthetic data generation
│   ├── experiments.R       # Simulation experiments (Figures 1, 4, 5)
│   └── visualize.R         # Plotting functions + generate_all_figures()
├── generate_all_figures.R  # Standalone runner (no install needed)
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
└── README.md
```

## Key Functions

| Function | Purpose |
|---|---|
| `adaboost(X, y, M)` | Train AdaBoost ensemble |
| `predict_adaboost(result, X)` | Predict with AdaBoost |
| `acar(qualities, ...)` | Simulate ant colony recruitment |
| `calculate_margins(result, X, y)` | Compute boosting margins |
| `calculate_quorum_margin(result)` | Compute ant quorum margin |
| `weak_learnability_experiment()` | Run Figure 1 simulation |
| `convergence_experiment()` | Run Figure 4 simulation |
| `noise_experiment()` | Run Figure 5 simulation |
| `generate_all_figures(dir)` | Generate all manuscript figures |

## Citation

```bibtex
@article{fokoue2026boosting,
  title   = {Isomorphic Functionalities between Ant Colony and Ensemble Learning:
             Part {II} --- On the Strength of Weak Learnability and the
             Boosting Paradigm},
  author  = {Fokou{\'e}, Ernest and Babbitt, Gregory and Levental, Yuval},
  journal = {arXiv preprint},
  year    = {2026}
}
```

## License

MIT
