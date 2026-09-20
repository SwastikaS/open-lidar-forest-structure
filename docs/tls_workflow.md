# Terrestrial LiDAR workflow

## Purpose

This workflow processes isolated individual-tree terrestrial laser-scanning (TLS) point clouds to calculate:

- total tree height;
- diameter at breast height (DBH);
- basal area;
- stem diameter at selected heights;
- lower-stem taper;
- stem displacement and lean;
- geometric quality diagnostics.

The analysis began with the European beech tree `WL12` and was subsequently applied across all 59 available tree point clouds.

## Data source

The workflow uses the open dataset:

> Bornand, A. (2023). *Individual tree TLS point clouds for tree volume estimation*. EnviDat.
> <https://doi.org/10.16904/envidat.403>

The trees were scanned under leaf-off conditions during winter 2020/2021 with a Leica BLK360. Each `.laz` file contains one already segmented tree. The coordinates use a local system in metres rather than a geographic coordinate reference system.

The accompanying parameter table contains tree height and stem-diameter measurements derived from the TLS data. These values support method comparison but are not independent field measurements.

## Input requirements

The current workflow assumes that:

- each input file contains one isolated tree;
- X, Y and Z coordinates are expressed in metres;
- the point cloud includes the tree base and top;
- enough stem-surface points are present near the required heights;
- branches and noise do not completely obscure the main stem.

Plot-level tree detection and segmentation are outside the present workflow.

## Processing method

### 1. Inspect the point cloud

The inspection stage records point count, coordinate ranges, vertical extent and point density. The `WL12` example contains approximately 1.42 million points and spans 25.599 m vertically.

### 2. Calculate tree height

Tree height is calculated from the vertical extent of the isolated point cloud:

```text
tree height = maximum Z − minimum Z
```

This approach requires the tree base and highest return to be present. It does not perform terrain normalisation because each file already contains a locally referenced individual tree.

### 3. Estimate DBH

DBH is measured at 1.3 m above the lowest point. The batch workflow uses a hybrid method:

1. locate a lower-stem cross-section near 0.3 m;
2. fit a robust circle to the candidate stem points;
3. track the stem centre and radius upwards in 0.1 m steps;
4. accept the fitted circle at 1.3 m when tracking reaches breast height;
5. use a direct 1.3 m circle fit only when tracking fails and the direct fit passes additional quality checks.

The robust circle fit reduces the influence of stray points, small branches and noise. The estimated DBH is twice the fitted radius.

### 4. Estimate stem taper

The stem is tracked upwards using consecutive horizontal cross-sections. At each step, the workflow searches near the preceding circle, fits a new circle and rejects implausible jumps in centre position or radius.

Diameters are extracted at 1.3, 2, 4 and 6 m. Accepted stem centres are also used to calculate lower-stem displacement and lean.

### 5. Assign quality flags

Geometric fit quality and agreement with the published TLS-derived values are treated separately.

`quality_flag` describes the geometry of the fitted circle:

- `acceptable`: passes the automatic geometric checks;
- `inspect`: produces an estimate but requires visual review;
- `failed`: does not return a defensible estimate.

`comparison_flag` describes agreement with the published TLS-derived value:

- `below review threshold`: absolute difference is less than 5 cm;
- `large disagreement: investigate`: absolute difference is at least 5 cm;
- `no comparison available`: either value is missing.

The 5 cm threshold is an investigation rule, not proof that either measurement is correct.

## Demonstration tree: WL12

The `WL12` point cloud provides a clear example of the workflow.

| Measurement | Result |
| --- | ---: |
| Calculated tree height | 25.599 m |
| Estimated DBH | 44.339 cm |
| Published TLS-derived DBH | 43.588 cm |
| Absolute DBH difference | 0.751 cm |
| Circle-fit RMSE at 1.3 m | 10.31 mm |
| Circumference completeness | 100% |
| Diameter at 6 m | 38.483 cm |
| Basal area at 1.3 m | 0.154 m² |
| Mean taper | 1.246 cm m⁻¹ |
| Lower-stem displacement | 0.351 m |
| Lower-stem lean | 4.276° |

## Batch validation

### Tree height

All 59 trees were processed successfully for height.

| Statistic | Result |
| --- | ---: |
| Mean error | −0.216 m |
| Mean absolute error | 0.217 m |
| Root mean square error | 0.467 m |

The negative mean error indicates a small overall tendency to underestimate height.

### DBH

Of the 59 trees:

- 53 trees (89.8%) produced DBH estimates;
- 38 trees (64.4%) passed automatic quality control;
- 15 trees (25.4%) were marked `inspect`;
- 6 trees (10.2%) failed without receiving a DBH estimate.

Among the 38 quality-approved DBH estimates:

| Statistic | Result |
| --- | ---: |
| Mean error | 0.34 cm |
| Mean absolute error | 0.52 cm |
| Root mean square error | 0.78 cm |
| Within 1 cm | 89.5% |
| Within 2 cm | 92.1% |
| Within 5 cm | 100% |

The 15 estimates marked `inspect` had a mean absolute error of 4.47 cm and an RMSE of 10.51 cm. The poorer performance of this group demonstrates why automated outputs should not be treated as equally reliable.

### Stem diameters at multiple heights

Diameter estimates were compared with the published TLS-derived values at 1.3, 2, 4 and 6 m.

| Height | Valid comparisons | All MAE | All RMSE | Acceptable comparisons | Acceptable MAE | Acceptable RMSE |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 1.3 m | 53 | 1.64 cm | 5.63 cm | 38 | 0.52 cm | 0.78 cm |
| 2.0 m | 53 | 1.39 cm | 3.39 cm | 35 | 0.72 cm | 1.11 cm |
| 4.0 m | 53 | 1.17 cm | 2.92 cm | 39 | 0.65 cm | 1.90 cm |
| 6.0 m | 52 | 1.41 cm | 3.03 cm | 34 | 0.50 cm | 0.74 cm |

Eight measurements differed from the published values by at least 5 cm. Seven were already classified `inspect` by the geometric checks. AD11 at 4 m passed the geometric checks but differed by 11.51 cm. Visual inspection showed one clear ring in both 10 cm and 20 cm cross-sections, so the reason for that disagreement remains unresolved.

The comparison queue is therefore complementary to geometric quality control: it identifies unusual agreement patterns without redefining a geometrically sound circle as a failed fit.

![TLS stem-taper validation](../outputs/tls/figures/tls_stem_taper_validation.png)

## Crown-structure estimation and validation

Crown dimensions were calculated from the complete horizontal projection of each already segmented individual-tree point cloud. The published `crown_base_height` was not used to remove lower points because this excluded genuine lower branches for some trees and reduced the estimated crown width.

The workflow calculates:

- the mean of the original X and Y extents;
- an orientation-adjusted mean extent after principal component analysis;
- the maximum distance between projected convex-hull vertices;
- a preferred crown diameter calculated as the mean farthest distance associated with the convex-hull vertices;
- the area of the complete two-dimensional convex hull.

The preferred crown-diameter method was evaluated against the published `crown_dia2` measurements for all 59 trees.

| Crown-width result | Value |
| ------------------ | ----: |
| Successful comparisons | 59 |
| Mean error | 0.118 m |
| Mean absolute error | 0.181 m |
| Root mean square error | 0.241 m |
| Correlation | 0.995 |
| Differences within 0.5 m | 56 of 59 |
| Differences within 1 m | 59 of 59 |

The mean original X–Y extent had an MAE of 0.342 m and an RMSE of 0.434 m. The PCA-rotated mean extent had an MAE of 0.326 m and an RMSE of 0.412 m. The preferred convex-hull-derived diameter therefore provided the closest overall agreement with the published second crown-diameter variable.

### Crown vertical extent

The published variable `crown_length_r` followed the complete vertical extent of the TLS point cloud extremely closely:

- mean error: 0.011 m;
- mean absolute error: 0.024 m;
- root mean square error: 0.067 m;
- correlation: 0.9998.

It is therefore treated as a TLS vertical-extent diagnostic in this project. It is not interpreted as field-derived crown length or as the distance from the field-measured crown base to the tree top.

### Projected crown area

The complete projected convex-hull area was retained as an experimental structural metric. It was systematically larger than the published `crown_area` variable, with a mean absolute difference of 34.49 m² and a mean absolute percentage difference of approximately 109.9%.

This disagreement indicates that the two area variables are not methodologically equivalent. The application therefore reports this measurement explicitly as `projected_convex_hull_area_m2` and labels it experimental. It should not be interpreted as a validated reproduction of the published crown-area measurement.

### Crown-processing assumptions

The crown calculations assume that:

- each input file contains one already segmented tree;
- the complete tree projection represents the tree crown;
- no major neighbouring-tree or off-tree artefacts remain in the file;
- X, Y and Z coordinates are expressed in metres.

The LAS withheld flag was also audited. Only `AD18` contained withheld points, and every point in that file carried the flag. Removing withheld points would therefore remove the entire tree. The flag was treated as a file-level export characteristic rather than a point-level filtering rule.

The final crown-validation workflow is implemented in:

```r
source("scripts/tls/18_validate_full_crown_projection.R")

## Interactive TLS application

The repository includes a local Shiny application for processing isolated `.las` or `.laz` trees. It provides summary measurements, geometric diagnostics, point-cloud projections, cross-section plots and downloadable CSV results.

Install the required packages through the project setup script and launch the application from the project root:

```r
source("scripts/00_setup.R")
shiny::runApp("app")
```

The application runs locally in a web-browser window. Shiny provides the interface, while the reusable calculations are stored in `R/tls_processing.R`.

## Scripts and outputs

Run the TLS scripts from the project root in numerical order. Script-level instructions are available in [`scripts/tls/README.md`](../scripts/tls/README.md).

The main validation script is:

```r
source("scripts/tls/10_validate_stem_taper.R")
```

Generated files are organised under:

- `outputs/tls/figures/` — diagnostic and validation figures;
- `outputs/tls/tables/` — tree-level results, summaries and review queues;
- `outputs/tls/vectors/`, `rasters/` and `models/` — reserved for later spatial and modelling outputs.

Important taper-validation outputs include:

- `tls_stem_taper_validation.csv`;
- `tls_stem_taper_comparison_flags.csv`;
- `tls_stem_taper_comparison_review.csv`;
- `tls_taper_accuracy_by_height.csv`;
- `tls_taper_accuracy_by_height_and_quality.csv`;
- `tls_stem_taper_validation.png`.

## Limitations

- The workflow begins with already segmented individual-tree point clouds.
- Height depends on complete capture of the tree base and top.
- Stem fitting can be affected by occlusion, branches, irregular stems and incomplete circumference sampling.
- Tracking and quality thresholds were developed and evaluated on this dataset and may require adjustment for other scanners, species, forest conditions and point densities.
- Published comparison diameters are derived from the same TLS data and do not provide independent field validation.
- Results marked `inspect` require visual review; failed fits should not be assigned a diameter automatically.
- Biomass and woody volume are not estimated by this workflow. Those tasks require validated allometry or a separate quantitative structure model.

## Methodological basis

The workflow uses established TLS measurement ideas—horizontal stem sections, robust circle fitting, sequential stem tracking and geometric quality checks—combined in a project-specific prototype. It is not an implementation of TreeQSM or a claim of universal performance.

Bornand et al. evaluated reconstructive and allometric approaches for individual-tree volume estimation and provide the source data used here. The associated published stem measurements were derived through a separate TLS-processing workflow, so differences between the two methods should be investigated rather than treated automatically as errors.

## References

Bornand, A. (2023). *Individual tree TLS point clouds for tree volume estimation*. EnviDat. <https://doi.org/10.16904/envidat.403>

Bornand, A., Rehush, N., Morsdorf, F., Thürig, E., & Abegg, M. (2023). Individual tree volume estimation with terrestrial laser scanning: Evaluating reconstructive and allometric approaches. *Agricultural and Forest Meteorology*, 341, 109654. <https://doi.org/10.1016/j.agrformet.2023.109654>

Terryn, L. et al. (2023). Analysing individual 3D tree structure using the R package `ITSMe`. *Methods in Ecology and Evolution*. <https://doi.org/10.1111/2041-210X.14033>

Liang, X. et al. (2018). International benchmarking of terrestrial laser scanning approaches for forest inventories. *ISPRS Journal of Photogrammetry and Remote Sensing*, 144, 137–179. <https://doi.org/10.1016/j.isprsjprs.2018.06.021>
