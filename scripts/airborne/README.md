# Airborne LiDAR workflow

This module demonstrates the extraction of forest canopy structure from a height-normalised airborne LiDAR point cloud using R.

## Workflow

Run the scripts from the project root in numerical order:

1. `01_inspect_point_cloud.R` — load and inspect the LAS attributes, classifications and returns.
2. `02_canopy_metrics.R` — calculate site- and grid-level canopy metrics and generate a 1 m canopy height model.
3. `03_detect_treetops.R` — identify canopy maxima and assess sensitivity to detection-window size.
4. `04_segment_tree_crowns.R` — segment individual trees and delineate crown polygons.
5. `05_biomass_predictors.R` — derive grid-level structural predictors relevant to biomass modelling.
6. `06_export_figures.R` — produce publication- and README-ready figures.

## Demonstration dataset

The workflow uses `Megaplot.laz`, a small height-normalised forest point cloud distributed with the R package `lidR`.

- Area: 5.16 ha
- Points: 81,590
- Point density: 1.58 points m⁻²
- Maximum return height: 29.97 m
- P95 canopy height: 23.05 m
- Returns above 2 m: 85.73%
- Detected treetops: 558
- Delineated crowns: 535

## Outputs

The workflow produces:

- a 1 m canopy height model;
- 20 m canopy-structure rasters;
- detected treetop points;
- individual crown polygons;
- tree-level height and crown metrics;
- grid-level structural predictors for future biomass modelling.

Generated figures, rasters, tables and vectors are stored under `outputs/airborne/`.

## Important limitation

The outputs are structural indicators derived from LiDAR. They are not direct estimates of above-ground biomass. Producing biomass estimates requires field- or externally calibrated biomass observations and a validated predictive model.