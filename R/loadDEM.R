#==============================================================================
# Reading in DEM data, downsampling, generating slope files -------------------


# INPUT FILES:
#
# ~/big_noDB/topo/topo_orig_NZMG/?i/*.tif (original DEM files)

# OUTPUT FILES:
    #
    # DEM Rdata:
        # ~/VsMap/Rdata/nzsi_9c_DEM.Rdata
        # ~/VsMap/Rdata/nzsi_30c_DEM.Rdata
        # ~/VsMap/Rdata/nzni_9c_DEM.Rdata
        # ~/VsMap/Rdata/nzni_30c_DEM.Rdata
    # DEM TIF:
        # ~/big_noDB/topo/nzsi_9c_DEM.tif
        # ~/big_noDB/topo/nzsi_30c_DEM.tif
        # ~/big_noDB/topo/nzni_9c_DEM.tif
        # ~/big_noDB/topo/nzni_30c_DEM.tif
    # Slope Rdata:
        # VsMap/Rdata/nzsi_9c_slp.Rdata
        # VsMap/Rdata/nzsi_30c_slp.Rdata
        # VsMap/Rdata/nzni_9c_slp.Rdata
        # VsMap/Rdata/nzni_30c_slp.Rdata
    # Slope TIF:
        # ~/big_noDB/topo/nzsi_9c_slp.tif
        # ~/big_noDB/topo/nzsi_30c_slp.tif
        # ~/big_noDB/topo/nzni_9c_slp.tif
        # ~/big_noDB/topo/nzni_30c_slp.tif

rm(list = ls())


library(raster)
library(rgdal)
library(sp)
library(ggplot2)
source("~/VsMap/R/geodetic.R")
setwd("~/big_noDB")

# The following needs to only be run ONCE to generate the downsampled 270m
# and 900m (~9c and ~30c) topography maps. The LRIS topographic data is
# stored in tiled GeoTIFF format and is opened using the "mosaic" command
# then saved as a single RasterLayer object.

elv0105si <- raster("./topo/topo_orig_NZMG/si/0001-0005.tif")
elv0204si <- raster("./topo/topo_orig_NZMG/si/0002-0004.tif")
elv0303si <- raster("./topo/topo_orig_NZMG/si/0003-0003.tif")
elv0405si <- raster("./topo/topo_orig_NZMG/si/0004-0005.tif")
elv0504si <- raster("./topo/topo_orig_NZMG/si/0005-0004.tif")
elv0602si <- raster("./topo/topo_orig_NZMG/si/0006-0002.tif")
elv0703si <- raster("./topo/topo_orig_NZMG/si/0007-0003.tif")
elv0801si <- raster("./topo/topo_orig_NZMG/si/0008-0001.tif")
elv0902si <- raster("./topo/topo_orig_NZMG/si/0009-0002.tif")
nzsi <- mosaic(elv0105si,elv0204si,elv0303si,elv0405si,elv0504si,elv0602si,elv0703si,elv0801si,elv0902si,fun=min)

elv0101ni <- raster("./topo/topo_orig_NZMG/ni/0001-0001.tif")
elv0202ni <- raster("./topo/topo_orig_NZMG/ni/0002-0002.tif")
elv0305ni <- raster("./topo/topo_orig_NZMG/ni/0003-0005.tif")
elv0403ni <- raster("./topo/topo_orig_NZMG/ni/0004-0003.tif")
elv0504ni <- raster("./topo/topo_orig_NZMG/ni/0005-0004.tif")
elv0606ni <- raster("./topo/topo_orig_NZMG/ni/0006-0006.tif")
elv0705ni <- raster("./topo/topo_orig_NZMG/ni/0007-0005.tif")
elv0804ni <- raster("./topo/topo_orig_NZMG/ni/0008-0004.tif")
nzni <- mosaic(elv0101ni,elv0202ni,elv0305ni,elv0403ni,elv0504ni,elv0606ni,elv0705ni,elv0804ni,fun=min)




nzsi_9c <- nzsi
res(nzsi_9c) <- 270 # downsample from 25x25m resolution to 270m (~9 arc-second) resolution.
# caution - the resample() commands take quite a long time!
nzsi_9c <- resample(nzsi, nzsi_9c, method="bilinear")

nzsi_30c <- nzsi
res(nzsi_30c) <- 900 # downsample from 25x25m resolution to 900m (~30 arc-second) resolution.
# caution - the resample() commands take quite a long time!
nzsi_30c <- resample(nzsi, nzsi_30c, method="bilinear")


nzni_9c <- nzni
res(nzni_9c) <- 270 # downsample from 25x25m resolution to 270m (~9 arc-second) resolution.
# caution - the resample() commands take quite a long time!
nzni_9c <- resample(nzni, nzni_9c, method="bilinear")

nzni_30c <- nzni
res(nzni_30c) <- 900 # downsample from 25x25m resolution to 900m (~30 arc-second) resolution.
# caution - the resample() commands take quite a long time!
nzni_30c <- resample(nzni, nzni_30c, method="bilinear")




setwd("~/VsMap/")

save(nzsi_9c,   file = "Rdata/nzsi_9c_DEM.Rdata")
save(nzsi_30c,  file = "Rdata/nzsi_30c_DEM.Rdata")
save(nzni_9c,   file = "Rdata/nzni_9c_DEM.Rdata")
save(nzni_30c,  file = "Rdata/nzni_30c_DEM.Rdata")


load(file="Rdata/nzsi_9c_DEM.Rdata")
load(file="Rdata/nzsi_30c_DEM.Rdata")
load(file="Rdata/nzni_9c_DEM.Rdata")
load(file="Rdata/nzni_30c_DEM.Rdata")




# Create spatial grid dataframes
nzsi_9c.sgdf  <- as(nzsi_9c,  'SpatialGridDataFrame')
nzsi_30c.sgdf <- as(nzsi_30c, 'SpatialGridDataFrame')
nzni_9c.sgdf  <- as(nzni_9c,  'SpatialGridDataFrame')
nzni_30c.sgdf <- as(nzni_30c, 'SpatialGridDataFrame')

# save as TIF too
writeRaster(nzsi_9c,  filename = "~/big_noDB/topo/nzsi_9c_DEM.tif",  format="GTiff", overwrite=TRUE)
writeRaster(nzsi_30c, filename = "~/big_noDB/topo/nzsi_30c_DEM.tif", format="GTiff", overwrite=TRUE)
writeRaster(nzni_9c,  filename = "~/big_noDB/topo/nzni_9c_DEM.tif",  format="GTiff", overwrite=TRUE)
writeRaster(nzni_30c, filename = "~/big_noDB/topo/nzni_30c_DEM.tif", format="GTiff", overwrite=TRUE)

# Compute slopes

slp_nzsi_30c <- terrain(nzsi_30c, opt='slope', unit = 'tangent', neighbors = 8)
slp_nzsi_9c  <- terrain(nzsi_9c,  opt='slope', unit = 'tangent', neighbors = 8)
slp_nzni_30c <- terrain(nzni_30c, opt='slope', unit = 'tangent', neighbors = 8)
slp_nzni_9c  <- terrain(nzni_9c,  opt='slope', unit = 'tangent', neighbors = 8)

# Create spatial grid dataframes - slope
slp_nzsi_9c.sgdf  <- as(slp_nzsi_9c,  'SpatialGridDataFrame')
slp_nzsi_30c.sgdf <- as(slp_nzsi_30c, 'SpatialGridDataFrame')
slp_nzni_9c.sgdf  <- as(slp_nzni_9c,  'SpatialGridDataFrame')
slp_nzni_30c.sgdf <- as(slp_nzni_30c, 'SpatialGridDataFrame')


save(slp_nzsi_9c,   slp_nzsi_9c.sgdf,  file = "Rdata/nzsi_9c_slp.Rdata")
save(slp_nzsi_30c,  slp_nzsi_30c.sgdf, file = "Rdata/nzsi_30c_slp.Rdata")
save(slp_nzni_9c,   slp_nzni_9c.sgdf,  file = "Rdata/nzni_9c_slp.Rdata")
save(slp_nzni_30c,  slp_nzni_30c.sgdf, file = "Rdata/nzni_30c_slp.Rdata")

# Save as TIF too
writeRaster(slp_nzsi_9c, filename  = "~/big_noDB/topo/nzsi_9c_slp.tif",  format="GTiff", overwrite=TRUE)
writeRaster(slp_nzsi_30c, filename = "~/big_noDB/topo/nzsi_30c_slp.tif", format="GTiff", overwrite=TRUE)
writeRaster(slp_nzni_9c, filename  = "~/big_noDB/topo/nzni_9c_slp.tif",  format="GTiff", overwrite=TRUE)
writeRaster(slp_nzni_30c, filename = "~/big_noDB/topo/nzni_30c_slp.tif", format="GTiff", overwrite=TRUE)

