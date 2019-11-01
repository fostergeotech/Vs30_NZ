# New Zealand Vs30 mapping

Kevin Foster

VERSION 18.12  -  Published version ([Earthquake Spectra](https://earthquakespectra.org/doi/full/10.1193/121118EQS281M))

*If you want to USE this work,* you can download the publication and electronic supplements, including the model itself as GeoTIF format raster files, [at my website](https://fostergeotech.com/Vs30map). (You can also get it from the official publication linked above, but you may not have subscription access).

*If you want to INSPECT my work, or adapt pieces of it for your own projects,* that's what this repo is for. Note that the necessary input files (Geology map and digital elevation models for New Zealand) are too large to host here, and in any case it will take you a fair bit of time and effort to get my code running on your system.

All codes have hardcoded working directory information. Other users will need to edit working directory strings accordingly. Unless indicated otherwise (in a few scripts), `~/VsMap/` is the working directory.

This codebase is not a "one-click" operation but interested users should be able to make use of most/parts of it with some persistence. Please contact me with questions.

## Organization

R files contained in `R/` represent the vast bulk of research workflow. Some image processing tasks and other miscellaneous tasks are done by shell scripts in `VsMap/sh/`. Important scripts are highlighted here, roughly in the order in which they need to be run for the published modeling workflow:

 | File                                                   | Description                                                                                                                                                                                                                                                                                         |
 |--------------------------------------------------------|-----------------------------------------------------------------------|
 |  `R/install.packages.R                          `      |  List of all external libraries/packages used in R. If you plan to run these codes you can save a little time by using this install script first.                                                                                                                                                   |
 |  `R/functions.R                                 `      |  Miscellaneous helper functions. A relic of early template code provided by Thompson. Some functions are still used; some are not.                                                                                                                                                                  |
 |  `R/geodetic.R                                  `      |  Convenience functions for converting to/from input/output coordinate reference systems.                                                                                                                                                                                                            |
 |  `R/loadDEM.R                                   `      |  Digital elevation model processing                                                                                                                                                                                                                                                                 |
 |  `sh/DEMprocessing.sh                           `      |  Digital elevation model processing                                                                                                                                                                                                                                                                 |
 |  `R/tileSlopes.R                                `      |  Stitch together tiled slope rasters created by `DEMprocessing.sh`                                                                                                                                                                                                                                  |
 |  `sh/hillshades.sh                              `      |  Produce hillshades. (Resampled later by an R script)                                                                                                                                                                                                                                               |
 |  `R/processQmap.R                               `      |  Load QMAP and assign additional metadata to polygons based on text parsing                                                                                                                                                                                                                         |
 |  `R/rasterizeQmap.R                             `      |  Convert NZ geology map from polygons to rasters                                                                                                                                                                                                                                                    |
 |  `R/vspr.R                                      `      |  Vs30 data preprocessing. Run once before R/modelsUpdate.R and AGAIN after modelsUpdate.R                                                                                                                                                                                                           |
 |  `R/modelsUpdate.R                              `      |  Bayesian updating of geology and terrain models                                                                                                                                                                                                                                                    |
 |  `R/vspr.R                                      `      |  See note above                                                                                                                                                                                                                                                                                     |
 |  `R/slopePlotDetail.R                           `      |  Slope-Vs30 plots                                                                                                                                                                                                                                                                                   |
 |  `R/makeRaster_AhdiGeoCats.R                    `      |  Rasterize geology categories                                                                                                                                                                                                                                                                       |
 |  `R/makeRaster_AhdiAK*                          `      |  Geology rasters                                                                                                                                                                                                                                                                                    |
 |  `R/maps2.R                                     `      |  Produces hybrid (slope-modified) geology-based models.                                                                                                                                                                                                                                             |
 |  `R/makeRaster_YongCA*                          `      |  Terrain rasters                                                                                                                                                                                                                                                                                    |
 |  `R/makeRaster_ice_water_DEM_hillshade_etc.R    `      |  As filename indicates. These rasters are all needed for the `R/MAP_all*.R` scripts                                                                                                                                                                                                                 |
 |  `R/tileTiffs.R                                 `      |  For many models, output raster tiles are produced in parallel. This script is used to stitch the TIF files together. Tiled (intermediary) TIFs are stored in `VsMap/tmp` dir.                                                                                                                      |
 |  `R/fitVariogram*.R                             `      |  Scripts for fitting variograms to terrain- and/or geology-based model residuals                                                                                                                                                                                                                    |
 |  `R/krige.R                                     `      |  Regression kriging                                                                                                                                                                                                                                                                                 |
 |  `R/mvn_all.R                                   `      |  Multivariate normal (MVN) method (Worden et al. 2018)                                                                                                                                                                                                                                              |
 |  `R/makeRaster_weightedGeostatsModels.R         `      |  Combine geology- and terrain-based geostats models.                                                                                                                                                                                                                                                |
 |  `R/figs.R                                      `      |  Various figures                                                                                                                                                                                                                                                                                    |
 |  `R/makeFigs.R                                  `      |  Uses ImageMagick to combine maps and legend bars produced by MAP_all*.R scripts to produce final figures for publication.                                                                                                                                                                          |
 |  `sh/makeFigs.sh                                `      |  Same as above, for a few specialized figures, e.g. the "six-pane" figures used for overview of generating geology- and terrain-based models.                                                                                                                                                       |
 |  `sh/convert-final-maps--make-prj-files.sh      `      |  Produce standalone GIS projection metadata files to be distributed alongside final maps (because maps are in a NZ projection---NZGD00, aka EPSG:2193---and some users have reported difficulty importing the GeoTIFF files without this supplemental metadata).                                    |
 |  `sh/convert-final-maps-to-UTM.sh               `      |  Resample final maps using nearest-neighbor sampling to the more standard UTM59S projection (EPSG:32759)                                                                                                                                                                                            |



Other folders:
  * Rdata files are written in `VsMap/Rdata`
      * `vspr.Rdata` velocity data input, saved as `spatialPointsDataFrame` (dataframe object with spatial coordinates as provided by package `sp`)
      * variogram_*.Rdata` various variogram objects saved as `variogramModel` object as provided by package `gstat`
  * `VsMap/out` contains output files: includes (among others)
      * scatter plots, histograms, etc. used for assessing model and most figures included in Earthquake Spectra draft.
      * PNG files containing colored maps with hillshades, legends, etc as shown in submitted Earthquake Spectra manuscript.
  * Some (mostly larger) files are saved in ~/big_noDB folder. These are big input files, either DEM (digital elevation model) or the QMAP (geology map), or various intermediate rasters derived from these (e.g. Iwahashi & Pike style terrain maps).
  * Most input data is included in `VsMap/in` folder. This includes Vs30 input data, some versions of geologic map, etc.
  * `Vs30_tables` contains auto-generated tables with various subsets of the collection of input Vs30 data. This table is created by `vspr_write.R` following the import of Vs30 data and the application of various sorting, overlay and model tasks. These were useful in assessing the performance of text search scripts used to parse geology map metadata and assign simplified geologic categories.

## Dependencies

Developed on a multiprocessor Linux machine. (Parallel processing is used only in the MVN implementation via the R `parallel` library). Requires [GDAL](https://gdal.org/) and the associated `rgdal` R package.

Note that R dependencies are listed in one file, `R/install.packages.R` as mentioned above.

Publication map generation is done in R; however, models are created and saved in GeoTIFF format. I used imageMagick in a few scripts for figure production/editing.

`flowchart_main.gv` and `flowchart_preprocessing.gv` are partially complete flowcharts of code structure and can be compiled with [GraphViz](https://graphviz.org/) (as in `flowchart.sh`)
