# Terrestrial LiDAR workflow

## What this workflow does

This workflow uses a terrestrial laser-scanning (TLS) point cloud of an individual tree to calculate:

- total tree height;
- diameter at breast height (DBH);
- basal area;
- stem diameter at different heights;
- lower-stem taper;
- stem displacement;
- lower-stem lean.

The current example uses an isolated European beech tree named `WL12`.

## Data source

The point cloud comes from the following open dataset:

Bornand, A. (2023). *Individual tree TLS point clouds for tree volume estimation*. EnviDat.
https://doi.org/10.16904/envidat.403

The dataset contains individual-tree point clouds and reference tree measurements. The trees were scanned under leaf-off conditions using terrestrial laser scanning.

## Workflow steps

### 1. Inspect the point cloud

The first script examines:

- the number of points;
- the X, Y and Z coordinate ranges;
- the point density;
- the minimum and maximum elevations;
- the overall dimensions of the tree.

The `WL12` point cloud contains approximately 1.42 million points.

The file has no geographic coordinate reference system because its coordinates describe the tree within a local measurement system. The coordinate units are metres.

### 2. Calculate tree height

The lowest point is treated as the tree base.

Tree height is calculated as:

`highest Z value − lowest Z value`

The calculated height of `WL12` is 25.599 m. This matches the reference tree height supplied with the dataset.

### 3. Extract the DBH section

DBH is normally measured at 1.3 m above the tree base.

The workflow extracts a thin horizontal section between 1.25 and 1.35 m. Looking at this section from above produces a two-dimensional view of the stem circumference.

Points unusually far from the main stem are removed before fitting the circle.

### 4. Estimate DBH

A robust circle is fitted to the cleaned stem points. Robust fitting reduces the influence of stray points, noise and small branches.

The diameter of the fitted circle is used as the estimated DBH.

| Measurement | Result |
|---|---:|
| Estimated DBH | 44.339 cm |
| Reference diameter at 1.3 m | 43.588 cm |
| Absolute error | 0.751 cm |
| Percentage error | 1.72% |
| Circle-fit RMSE | 10.31 mm |
| Circumference coverage | 100% |

### 5. Track the stem upwards

The workflow begins with the reliable fitted circle at 1.3 m and moves upwards in 0.1 m steps.

At each height, it:

1. extracts a thin horizontal section;
2. searches near the previously fitted stem position;
3. selects points close to the expected stem surface;
4. fits a new circle;
5. rejects sudden or unrealistic changes caused by branches or noise.

This produces a series of stem centres and diameters between 1.3 and 6 m.

### 6. Calculate stem structure

The accepted circles are used to describe the shape and position of the lower stem.

| Measurement | Result |
|---|---:|
| Calculated tree height | 25.599 m |
| Estimated DBH | 44.339 cm |
| Diameter at 6 m | 38.483 cm |
| Basal area at 1.3 m | 0.154 m² |
| Mean taper | 1.246 cm m⁻¹ |
| Lower-stem displacement | 0.351 m |
| Lower-stem lean | 4.276° |
| Mean absolute diameter error | 0.517 cm |
| Accepted tracking heights | 48 |

## Which parts use established methods?

The following steps are commonly used in TLS forest measurement:

- measuring DBH at 1.3 m;
- extracting horizontal stem sections;
- fitting circles or cylinders to stem points;
- calculating tree height from the point-cloud height range;
- fitting several stem sections to describe taper and lean.

## Which parts are experimental?

The exact upward-tracking rules in this project are part of our current prototype.

These include:

- the 0.1 m tracking interval;
- the search distance around the previous circle;
- the minimum number of required points;
- the permitted change in radius;
- the permitted movement of the stem centre;
- the circle-fit quality threshold.

These settings worked well for `WL12`, but they have not yet been tested across all available trees. They should therefore be described as prototype settings rather than a universal method.

## Current limitations

The workflow begins with an already separated individual-tree point cloud. It does not yet detect and separate individual trees from a complete forest-plot point cloud.

The current results are based on one demonstration tree. A successful result for one tree does not prove that the same settings will work for every species, stem size or point-cloud condition.

## Next development stage

The next stage will:

1. run the measurements across all available individual-tree point clouds;
2. connect each point cloud with its reference measurements;
3. compare estimated and reference height and diameter;
4. record successful and failed fits;
5. identify settings that work across different trees and species;
6. convert the workflow into reusable R functions;
7. develop a simple interface for uploading and processing individual-tree files.

## Outputs

Generated results are stored under:

- `outputs/tls/figures/`
- `outputs/tls/tables/`
- `outputs/tls/vectors/`
- `outputs/tls/rasters/`
- `outputs/tls/models/`

The processing scripts are stored under:

- `scripts/tls/`

## References

Bornand, A. (2023). *Individual tree TLS point clouds for tree volume estimation*. EnviDat.
https://doi.org/10.16904/envidat.403

Terryn, L. et al. (2023). Analysing individual 3D tree structure using the R package `ITSMe`. *Methods in Ecology and Evolution*.
https://doi.org/10.1111/2041-210X.14033

Liang, X. et al. (2018). International benchmarking of terrestrial laser scanning approaches for forest inventories. *ISPRS Journal of Photogrammetry and Remote Sensing*, 144, 137–179.
https://doi.org/10.1016/j.isprsjprs.2018.06.021