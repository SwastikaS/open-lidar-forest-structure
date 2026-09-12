# Terrestrial LiDAR workflow

This module demonstrates individual-tree measurement from a terrestrial laser-scanning point cloud. The current validated prototype processes the isolated beech tree `WL12` and estimates tree height, diameter at breast height (DBH), lower-stem taper, basal area, stem displacement and lean.

## Workflow

Run the scripts from the project root in numerical order:

1. `01_inspect_tls_tree.R` — inspect the point cloud and reference measurements.
2. `02_extract_dbh_slice.R` — extract the stem cross-section at 1.3 m above the tree base.
3. `03_fit_dbh_circle.R` — clean the cross-section and estimate DBH using robust circle fitting.
4. `04_estimate_stem_taper.R` — track the stem upwards and estimate diameter at multiple heights.
5. `05_visualise_stem_structure.R` — visualise the reconstructed lower-stem trajectory.

## Demonstration tree

- Tree: `WL12`
- Species: *Fagus sylvatica*
- Calculated height: 25.599 m
- Estimated DBH: 44.339 cm
- Reference DBH: 43.588 cm
- Absolute DBH error: 0.751 cm
- Basal area: 0.154 m²
- Lower-stem lean: 4.276°

## Important limitation

The current workflow is a validated single-tree prototype, not yet a general automated TLS-processing application. The stem-tracking thresholds were developed for the demonstration tree and must be tested and adapted across different species, tree sizes and point-cloud conditions.

Generated figures and tables are stored under `outputs/tls/`.