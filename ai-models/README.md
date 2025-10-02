# Bioinfo Lab - AI Models
This repository contains R scripts for building and evaluating classification and survival models using tabular biomedical data.

## Contents
- `classification_models.R`: Script for training and evaluating classification models.
- `survival_models.R`: Script for training and evaluating survival analysis models.
- `scripts/`: Folder containing helper R scripts (e.g., `split_data.R`, `build_models.R`).
- `models/`: Output directory where trained models and metrics are saved.
- `install_packages.R`: Script to install required R packages.

## Requirements
- R (version ≥ 4.0 recommended)
- Required R packages: `parallel`, `qs2`, `varPro` (and any others used in helper scripts)
- TSV-formatted input data files

You can install the required packages by running:

```r
Rscript install_packages.R
```

## Usage
### Classification Models
Train and evaluate a classification model:

```sh
Rscript classification_models.R -i <input_file>
```

**Arguments:**
- `-i` Path to input data file (TSV format)
- `-h` Show help message

Outputs:
- Trained models (`initial_class_model.qs2`, `reduced_class_model.qs2`) in `models/`
- Metrics table (`metrics_all_classification.tsv`) in `models/`

### Survival Models
Train and evaluate a survival analysis model:

```sh
Rscript survival_models.R -i <input_file>
```

**Arguments:**
- `-i` Path to input data file (TSV format)
- `-h` Show help message

Outputs:
- Trained models (`initial_survival_model.qs2`, `reduced_survival_model.qs2`) in `models/`
- Metrics table (`metrics_all_survival.tsv`) in `models/`

## Notes
- The scripts automatically create the `models/` directory if it does not exist.
- Helper scripts must be placed in the `scripts/` directory.
- Input files must be properly formatted TSV files with appropriate columns (see script comments for details).
- For Linux users, ensure you have the necessary system libraries to install R packages.

## License
This repository is provided for academic and research purposes.
