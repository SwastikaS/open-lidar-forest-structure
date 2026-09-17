# Terrestrial LiDAR scripts

This folder contains the R scripts for measuring individual-tree structure from isolated terrestrial laser-scanning point clouds.

Run all scripts from the project root. Detailed methods, validation results and limitations are documented in [`docs/tls_workflow.md`](../../docs/tls_workflow.md).

## Setup

Install the required packages before running the workflow:

```r
source("scripts/00_setup.R")
```

The raw `.las` or `.laz` files belong under:

```text
data/raw/tls/
```

Reference measurements used by the validation scripts belong under:

```text
data/reference/tls/
```

## Single-tree demonstration

The first five scripts demonstrate the method with the isolated European beech tree `WL12`.

1. `01_inspect_tls_tree.R`
   Reads the point cloud, checks its dimensions and creates front and side projections.

2. `02_extract_dbh_slice.R`
   Calculates height above the tree base and extracts the stem cross-section around 1.3 m.

3. `03_fit_dbh_circle.R`
   Cleans the DBH cross-section, fits a robust circle and reports geometric diagnostics.

4. `04_estimate_stem_taper.R`
   Tracks the lower stem upwards, estimates diameter at selected heights and calculates taper, displacement and lean.

5. `05_visualise_stem_structure.R`
   Creates the lower-stem profile and stem-centre visualisation.

Run them in order:

```r
source("scripts/tls/01_inspect_tls_tree.R")
source("scripts/tls/02_extract_dbh_slice.R")
source("scripts/tls/03_fit_dbh_circle.R")
source("scripts/tls/04_estimate_stem_taper.R")
source("scripts/tls/05_visualise_stem_structure.R")
```

## Batch processing and validation

Scripts 06–10 apply and evaluate the workflow across the available isolated-tree dataset.

6. `06_batch_tree_heights.R`
   Calculates height for every TLS tree and compares the results with the available reference table.

7. `07_batch_dbh_pilot.R`
   Tests the hybrid DBH method on a small group of trees before full batch processing.

8. `08_batch_dbh_all_trees.R`
   Applies the hybrid DBH method to all available tree point clouds and records successful, inspect and failed outcomes.

9. `09_validate_batch_results.R`
   Summarises DBH accuracy by quality category, creates the inspection queue and exports the validation figure.

10. `10_validate_stem_taper.R`
    Estimates stem diameter at 1.3, 2, 4 and 6 m, calculates validation statistics and separates geometric fit quality from disagreement with published TLS-derived measurements.

Run the batch scripts in order:

```r
source("scripts/tls/06_batch_tree_heights.R")
source("scripts/tls/07_batch_dbh_pilot.R")
source("scripts/tls/08_batch_dbh_all_trees.R")
source("scripts/tls/09_validate_batch_results.R")
source("scripts/tls/10_validate_stem_taper.R")
```

Scripts 06–10 can take several minutes because they repeatedly read and process the point clouds. Warnings about points flagged `withheld` come from flags stored in the source LAZ files and do not necessarily indicate processing failure.

## Reusable processing functions

Reusable TLS functions are stored in:

```text
R/tls_processing.R
```

The scripts and Shiny application source this file so that the main measurement calculations are maintained in one place.

## Interactive application

Launch the local application from the project root with:

```r
shiny::runApp("app")
```

The application processes isolated-tree `.las` or `.laz` files and provides downloadable measurements and diagnostic plots.

## Outputs

Generated results are stored under:

```text
outputs/tls/
├── figures/
├── tables/
├── vectors/
├── rasters/
└── models/
```

Do not place raw point clouds in the output folders or commit large `.las` and `.laz` files to GitHub.
