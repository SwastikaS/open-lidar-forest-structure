# Terrestrial LiDAR scripts

This folder contains the R scripts for measuring individual-tree structure from isolated terrestrial laser-scanning point clouds.

Run all scripts from the project root. Detailed methods, validation results and limitations are documented in [docs/tls_workflow.md](../../docs/tls_workflow.md).

## Setup

Install the required packages before running the workflow:

```r
source("scripts/00_setup.R")
```

Raw `.las` and `.laz` files belong under:

```text
data/raw/tls/
```

Reference measurements used by the validation scripts belong under:

```text
data/reference/tls/
```

## Single-tree demonstration

Scripts 01–05 demonstrate the stem-measurement method with the isolated European beech tree `WL12`.

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

Run these scripts in order:

```r
source("scripts/tls/01_inspect_tls_tree.R")
source("scripts/tls/02_extract_dbh_slice.R")
source("scripts/tls/03_fit_dbh_circle.R")
source("scripts/tls/04_estimate_stem_taper.R")
source("scripts/tls/05_visualise_stem_structure.R")
```

## Stem batch processing and validation

Scripts 06–10 apply and evaluate the stem-measurement workflow across the available isolated-tree dataset.

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

Run the stem batch scripts in order:

```r
source("scripts/tls/06_batch_tree_heights.R")
source("scripts/tls/07_batch_dbh_pilot.R")
source("scripts/tls/08_batch_dbh_all_trees.R")
source("scripts/tls/09_validate_batch_results.R")
source("scripts/tls/10_validate_stem_taper.R")
```

These scripts can take several minutes because they repeatedly read and process the point clouds.

Warnings about points flagged `withheld` originate from flags stored in the source LAZ files and do not necessarily indicate processing failure.

## Crown-structure development and validation

Scripts 11–18 develop and validate crown measurements from the horizontal projection of each segmented tree point cloud.

11. `11_inspect_crown_structure.R`
    Inspects the crown-related reference variables and creates a vertical point-density profile for the demonstration tree.

12. `12_calculate_crown_metrics.R`
    Calculates initial crown-width and projected convex-hull measurements for `WL12`.

13. `13_compare_crown_area_methods.R`
    Compares alternative crown-width and projected-area calculations. This is a methodological exploration rather than the final validation workflow.

14. `14_validate_crown_metrics.R`
    Applies the initial crown-base-filtered method across all trees. This is retained as a development diagnostic and is not the final crown-validation method.

15. `15_inspect_withheld_points.R`
    Audits LAS withheld flags. It shows that all points in `AD18` carry the withheld flag, so these points must not be removed automatically.

16. `16_finalise_crown_validation.R`
    Corrects the interpretation of `crown_length_r` and summarises the crown-base-filtered experiment. This remains a development diagnostic rather than the final method.

17. `17_inspect_WL20_crown.R`
    Investigates the principal disagreement from the filtered method and demonstrates that the crown-base cutoff excluded genuine lower branches.

18. `18_validate_full_crown_projection.R`
    Implements the final crown-width workflow using the complete XY projection of each segmented tree. This is the current crown-validation script.

Run the final crown validation from the project root:

```r
source("scripts/tls/18_validate_full_crown_projection.R")
```

The preferred crown diameter is calculated from the complete projected convex hull. Across all 59 trees, its mean absolute difference from the published second crown-diameter variable was 0.181 m, its root mean square difference was 0.241 m, and all estimates were within 1 m.

Projected convex-hull area is retained as an experimental structural metric. It is not considered a validated reproduction of the published `crown_area` variable.

## Reusable processing functions

Reusable TLS functions are stored in:

```text
R/tls_processing.R
```

The scripts and Shiny application source this file so that the main measurement calculations are maintained in one place.

The reusable engine currently provides functions for:

- robust circle fitting;
- hybrid DBH estimation;
- tree-height calculation;
- lower-stem tracking;
- diameter estimation at selected heights;
- taper, displacement and lean calculation;
- crown-width and projected convex-hull calculation;
- single-tree and batch processing.

## Interactive application

Launch the local TLS Tree Structure Analyser from the project root:

```r
shiny::runApp("app")
```

The application supports single-tree and batch processing. It reports tree height, DBH, basal area, lower-stem structure and crown dimensions and provides downloadable CSV results and diagnostic plots.

Each input file must contain one already segmented tree, include the tree base and use metres for its XYZ coordinates.

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

Important final crown outputs include:

- `tls_full_crown_projection_validation.csv`;
- `tls_full_crown_projection_accuracy_summary.csv`;
- `tls_full_crown_projection_batch_summary.csv`;
- `tls_full_crown_projection_inspection_queue.csv`;
- `tls_full_crown_projection_validation.png`.

Do not place raw point clouds in the output folders or commit large `.las` and `.laz` files to GitHub.