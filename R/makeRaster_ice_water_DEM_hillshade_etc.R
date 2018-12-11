# makeRaster_ice_water_DEM_hillshade_etc.R
#
# This script need only be run once - used in virtually all downstream mapping scripts.
# 
rm(list=ls())
setwd("~/VsMap")
library(raster)
library(spatstat)
source("R/geodetic.R")

# INDEX (for deciding what's Ice and what's Water... and for limits of land)
# indexRaster <- raster("~/big_noDB/models/INDEX_NZGD00_allNZ.tif")

# # classify water, ice, ocean:
# load("~/big_noDB/geo/QMAP_Seamless_July13K_NZGD00.Rdata")
# unitLookup <- data.frame(map_NZGD00$INDEX, map_NZGD00$groupID_AhdiAK)
# 
# isOcean <- indexRaster==0
# isOcean[isOcean==0] <- NA
# writeRaster(x = isOcean, filename = "~/big_noDB/models/isOcean_NZGD00.tif")
# 
# gID <- subs(indexRaster, unitLookup)
# #levels(gID)
# isIce <- gID==16
# isIce[isIce==0] <- NA
# writeRaster(x = isIce, filename = "~/big_noDB/models/isIce_NZGD00.tif")
# 
# isWater <- gID==4
# isWater[isWater==0] <- NA
# writeRaster(x = isWater, filename = "~/big_noDB/models/isWater_NZGD00.tif")
# 
# 
# 
# 
# 
# 
# # GEO
# geo_allNZ_NZGD00 <- raster("~/big_noDB/models/geo_NZGD00_allNZ_AhdiAK.tif")
# 
# 
# # DEM one-time processing
# # copied here from QCAM_SCEC_GEESDV_data.R
# # which is deprecated
# #
#

mergeNorthAndSouth <- F # change to "true" to re-merge north and south island DEM TIFFs for downstream reprocessing (hillshades, etc etc)

if(mergeNorthAndSouth) {
  
  reProject <- F # change to TRUE to regenerate plane-projection (NZGD00) DEM TIFFs.
  
  if(reProject) {
    siDEM <- raster("~/big_noDB/topo/DEM_all_si.tif") #NZMG
    siDEM00 <- projectRaster(siDEM, crs = crsNZGD00())
    writeRaster(x = siDEM00, format = "GTiff", filename = "~/big_noDB/topo/DEM_all_si_NZGD00.tif")
    niDEM <- raster("~/big_noDB/topo/DEM_all_ni.tif") #NZMG
    niDEM00 <- projectRaster(niDEM, crs = crsNZGD00())
    writeRaster(x = niDEM00, format = "GTiff", filename = "~/big_noDB/topo/DEM_all_ni_NZGD00.tif")
  } else {
    siDEM00 <- raster("~/big_noDB/topo/DEM_all_si_NZGD00.tif")
    niDEM00 <- raster("~/big_noDB/topo/DEM_all_ni_NZGD00.tif")
  }
  
  # DEM00   <- mosaic(siDEM00, niDEM00, fun="min") # this doesn't work; did using gdal_merge.py , below:
  
  # Combining north and south island. Only need to run this once:
  # ... BUT the tiles overlap and gdal_mergy.py does NOT handle the merge as I would like.
  # ... the order in which the two arguments are provided to gdal_merge.py determines which
  # ... value is selected in overlap region. To get around it, I must
  # ... apply a lower xmax to south island tif first:
  xmax_si <- 1724000 # this longitude passes roughly halfway between picton and wellington peninsula.
  exts_si <- extent(siDEM00)
  exts_si[2] <- xmax_si
  siDEM00 <- crop(siDEM00,exts_si)
  writeRaster(siDEM00,filename = "~/big_noDB/topo/DEM_all_si_NZGD00_crop.tif")
  
  # NOW, I can run gdal_merge without "chopping off" a portion of the north island.
  unlink("~/big_noDB/topo/DEM_all_NZGD00.tif") # unlink (delete) TIFF first, because GDAL sometimes retains information when writing over existing TIFF files. I want to avoid this.
  system2(command = "gdal_merge.py",args = c("-o", "~/big_noDB/topo/DEM_all_NZGD00.tif",
                                             "~/big_noDB/topo/DEM_all_ni_NZGD00.tif",
                                             "~/big_noDB/topo/DEM_all_si_NZGD00_crop.tif"))
}

#
#
#
# 
# # hillshades
# DEM00  <- raster("~/big_noDB/topo/DEM_all_NZGD00.tif") # original resolution
# DEM    <- crop(DEM00,geo_allNZ_NZGD00)
# DEM    <- resample(DEM,geo_allNZ_NZGD00)
# isOcean <- raster("~/big_noDB/models/isOcean_NZGD00.tif")
# isWater <- raster("~/big_noDB/models/isWater_NZGD00.tif")
# isOW <- or(isOcean, isWater)
# DEM[isOW] <- NA
# writeRaster(DEM, filename = "~/big_noDB/models/DEM_all_NZGD00_resampled.tif", format="GTiff", overwrite=T)
# Slope  <- terrain(DEM, opt='slope')
# Aspect <- terrain(DEM, opt='aspect')
# 
# # swiss hillshade
# Shade1 <- hillShade(Slope, Aspect, angle=30, direction=225)
# Shade2 <- hillShade(Slope, Aspect, angle=30, direction=270)
# Shade3 <- hillShade(Slope, Aspect, angle=30, direction=315)
# Shade4 <- hillShade(Slope, Aspect, angle=30, direction=360)
# masterShade <- 0.25*sum(Shade1,Shade2,Shade3,Shade4)
# writeRaster(masterShade, filename = "~/big_noDB/models/hillshadeA_NZGD00.tif", format="GTiff", overwrite=T)
# 
# 
# a <- 0.5
# b <- 2
# DEMnrm <- DEM/maxValue(DEM)
# ms2    <- (masterShade^b+(a*DEMnrm)^b)^(1/b)
# writeRaster(ms2, filename = "~/big_noDB/models/hillshadeB_NZGD00.tif", format="GTiff", overwrite=T)
# 
# 
#  
# plot(Shade1,      col=grey.colors(100,start=0,end=1))
# plot(masterShade, col=grey.colors(100,start=0,end=1))
# plot(ms2,         col=grey.colors(100,start=0.4,end=1,gamma = 3.1))
# 
# 
# 
# 
