# Airborne LiDAR workflow

## What this workflow does

This workflow uses an airborne LiDAR point cloud to measure forest canopy structure.

It produces:

- canopy-height maps;
- average and upper-canopy height measurements;
- detected treetops;
- estimated tree-crown boundaries;
- structural variables that could later be used in a biomass model.

The analysis is completed in R using `lidR`, `terra` and `sf`.

## Workflow steps

### 1. Inspect the point cloud

The first script checks:

- the number and density of LiDAR points;
- the coordinate reference system;
- the minimum and maximum heights;
- ground and non-ground classifications;
- the number of laser returns.

The example dataset is already height-normalised. Ground points have a height of 0 m, so the point heights represent distance above the ground.

### 2. Calculate canopy measurements

The workflow calculates canopy measurements for the whole site and within 20 × 20 m grid cells.

These include:

- mean height;
- height variation;
- median height;
- height percentiles such as P95;
- maximum height;
- percentage of returns above 2 m;
- canopy relief ratio.

P95 is the height below which 95% of the LiDAR returns occur. It is often more reliable than the absolute maximum for describing the upper canopy.

### 3. Create a canopy height model

A canopy height model is a raster in which each pixel represents vegetation height above the ground.

The workflow creates a 1 m canopy height model and fills small gaps using values from neighbouring pixels.

### 4. Detect treetops

Treetops are detected by finding local high points in the canopy height model.

Search windows of 3, 5 and 7 m are compared. The 5 m window gives the most balanced result for this example.

A detected high point is only a possible tree apex. It is not a confirmed tree stem.

### 5. Delineate tree crowns

The Dalponte–Coomes algorithm grows a crown area around each detected treetop.

The workflow then calculates:

- tree height;
- mean return height;
- crown area;
- equivalent crown diameter;
- a simple crown-volume proxy.

Crown boundaries are estimates. Neighbouring trees may sometimes be merged, while one large crown may occasionally be divided into several crowns.

### 6. Produce structural predictors

The final script produces 20 m grid-level measurements of canopy height, density and vertical variation.

These measurements can later be compared with field or GEDI biomass observations to build a biomass model.

They are not direct biomass estimates.

## Demonstration results

| Measurement | Result |
|---|---:|
| Surveyed area | 5.16 ha |
| LiDAR points | 81,590 |
| Point density | 1.58 points m⁻² |
| P95 canopy height | 23.05 m |
| Maximum return height | 29.97 m |
| Returns above 2 m | 85.73% |
| Detected treetops | 558 |
| Delineated crowns | 535 |
| Mean delineated-tree height | 22.09 m |
| Mean crown area | 78.85 m² |

## Limitations

No field-mapped trees were available for this example. Therefore:

- the detected treetops cannot be directly checked against real stem locations;
- small or hidden understory trees may not be detected;
- neighbouring crowns may be merged;
- individual crowns may be divided incorrectly;
- the selected detection settings are exploratory;
- the structural measurements must not be reported as measured biomass.

## References

- Roussel, J.-R. et al. (2020). `lidR`: An R package for analysis of airborne laser scanning data. *Remote Sensing of Environment*, 251, 112061. https://doi.org/10.1016/j.rse.2020.112061
- Popescu, S.C. and Wynne, R.H. (2004). Seeing the trees in the forest: using lidar and multispectral data fusion with local filtering and variable window size for estimating tree height. *Photogrammetric Engineering & Remote Sensing*, 70, 589–604. https://doi.org/10.14358/PERS.70.5.589
- Dalponte, M. and Coomes, D.A. (2016). Tree-centric mapping of forest carbon density from airborne laser scanning and hyperspectral data. *Methods in Ecology and Evolution*, 7, 1236–1245. https://doi.org/10.1111/2041-210X.12575