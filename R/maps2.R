# This script depends on rasterized QMAP with polygon IDs, as produced in rasterizeQmap.R

# To copy the files from Hypocentre to (e.g.) Microfunk, use:
#
# cd ~/VsMap/Rdata
# scp kmf76@hypocentre:/home/kmf76/VsMap/Rdata/hyb_NZGD00_allNZ.Rdata ./
rm(list=ls())



#########################################################################
# specify which model to use.
# NOTE!!!!!!!! #######################################
#    GEOMODEL must be *the model on which* HYBMODEL was based!
#########################################################################


# GEOMODEL <- "AhdiAK"  # done
GEOMODEL <- "AhdiAK_noQ3"
# HYBMODEL <- "AhdiAK_KaiAll_hyb09c"  # done, and no longer using this one.
HYBMODEL <- "AhdiAK_noQ3_hyb09c"

library("tools")
library("rgdal")
library("raster")
library ("parallel")

setwd("~/VsMap/")
source("R/geodetic.R")
source("R/functions.R")
source("R/maps2_fns.R")
source("R/classifyThings.R")
source("R/models.R")

load("~/big_noDB/geo/QMAP_Seamless_July13K_NZGD00.Rdata")
mapDF <- as.data.frame(map_NZGD00)

# Use lookup table to select numeric values in place of factors
hybLookup <- do.call(what = paste0(HYBMODEL, "_lookup"), 
                     args = list())


# store a lookup table for finding hybrid groups in worker processes
mapIdx <- mapDF$INDEX
mapGrp <- as.character(mapDF[,paste0("groupID_", HYBMODEL)])
mapGeo <- mapDF[,paste0("Vs30_", GEOMODEL)]
idxGrpDF <- data.frame(mapIdx, mapGrp)
idxGeoDF <- data.frame(mapIdx, mapGeo)

# The following two lines are important. added 20180112. Forces "NA" values in ocean (where index value is zero).
# (Actually... since the operative command in the scripts in maps2_fns.R is subs(), and subsWithNA=T by default,
# this might not be needed. Oh well.)
idxGrpDF <- rbind(idxGrpDF, c(0,"00_WATER"))
idxGeoDF <- rbind(idxGeoDF, c(0,NA))



############################################################################
# GET ALL NZ INDEX MAPS ####################################################
allNZfiles <- list.files(path="Rdata/", pattern=glob2rx("INDEX_NZGD00_allNZ_x*y*.Rdata"))

# for testing
# allNZfiles <- allNZfiles[85:87]

# Make cluster for parallel to use
numCores <- parallel::detectCores() - 2
clustah <- makeCluster(numCores)
# big parallel loops
# to debug, run:
# idx <- 85 # x05y04 contains chch
# rasterLoopGeo(indexFile = allNZfiles[idx], idxGeoDF,
#               GEOMODEL)
# rasterLoopHyb(indexFile = allNZfiles[idx], idxGrpDF,
#               GEOMODEL, HYBMODEL)

parLapply(clustah,X = allNZfiles,fun = rasterLoopGeo,
                                        idxGeoDF, GEOMODEL)
# parLapply(clustah,X = allNZfiles,fun = rasterLoopHyb,
#                                         idxGrpDF, GEOMODEL, HYBMODEL)
# release cluster
stopCluster(clustah)








################################################################################################################
################################################################################################################
## The following was used for stitching tiles together into one large raster, STORED AS AN RDATA FILE.    ######
## My new approach is using tileTiffs.R to generate TIFF rasters which are less problematic when working  ######
## with R, because they can be accessed randomly from disk without loading entire file to memory.         ######
################################################################################################################
################################################################################################################


# # stitch together all hybrid NZGD00 maps into one structure
# for(x in 0:10) { # 11 
#   for (y in 0:15) { # 16
#     load(sprintf("~/VsMap/Rdata/hyb_NZGD00_allNZ_x%02dy%02d_%s.Rdata", x,  y, HYBMODEL))
#     assign(x = sprintf("HybX%02dY%02d", x, y),value = hybRaster)
#   }
# }

# str <- ""
# for (x in 0:10) {  # 11
#   for (y in 0:15) {  # 16
#     str <- paste0(str, sprintf("HybX%02dY%02d, ", x, y))
#   }
# }








# Hyb_allNZ_NZGD00 <- mosaic(
#   HybX00Y00, HybX00Y01, HybX00Y02, HybX00Y03, HybX00Y04, HybX00Y05, HybX00Y06, HybX00Y07, 
#   HybX00Y08, HybX00Y09, HybX00Y10, HybX00Y11, HybX00Y12, HybX00Y13, HybX00Y14, HybX00Y15, 
#   HybX01Y00, HybX01Y01, HybX01Y02, HybX01Y03, HybX01Y04, HybX01Y05, HybX01Y06, HybX01Y07, 
#   HybX01Y08, HybX01Y09, HybX01Y10, HybX01Y11, HybX01Y12, HybX01Y13, HybX01Y14, HybX01Y15, 
#   HybX02Y00, HybX02Y01, HybX02Y02, HybX02Y03, HybX02Y04, HybX02Y05, HybX02Y06, HybX02Y07, 
#   HybX02Y08, HybX02Y09, HybX02Y10, HybX02Y11, HybX02Y12, HybX02Y13, HybX02Y14, HybX02Y15, 
#   HybX03Y00, HybX03Y01, HybX03Y02, HybX03Y03, HybX03Y04, HybX03Y05, HybX03Y06, HybX03Y07, 
#   HybX03Y08, HybX03Y09, HybX03Y10, HybX03Y11, HybX03Y12, HybX03Y13, HybX03Y14, HybX03Y15, 
#   HybX04Y00, HybX04Y01, HybX04Y02, HybX04Y03, HybX04Y04, HybX04Y05, HybX04Y06, HybX04Y07, 
#   HybX04Y08, HybX04Y09, HybX04Y10, HybX04Y11, HybX04Y12, HybX04Y13, HybX04Y14, HybX04Y15, 
#   HybX05Y00, HybX05Y01, HybX05Y02, HybX05Y03, HybX05Y04, HybX05Y05, HybX05Y06, HybX05Y07,
#   HybX05Y08, HybX05Y09, HybX05Y10, HybX05Y11, HybX05Y12, HybX05Y13, HybX05Y14, HybX05Y15, 
#   HybX06Y00, HybX06Y01, HybX06Y02, HybX06Y03, HybX06Y04, HybX06Y05, HybX06Y06, HybX06Y07, 
#   HybX06Y08, HybX06Y09, HybX06Y10, HybX06Y11, HybX06Y12, HybX06Y13, HybX06Y14, HybX06Y15, 
#   HybX07Y00, HybX07Y01, HybX07Y02, HybX07Y03, HybX07Y04, HybX07Y05, HybX07Y06, HybX07Y07, 
#   HybX07Y08, HybX07Y09, HybX07Y10, HybX07Y11, HybX07Y12, HybX07Y13, HybX07Y14, HybX07Y15, 
#   HybX08Y00, HybX08Y01, HybX08Y02, HybX08Y03, HybX08Y04, HybX08Y05, HybX08Y06, HybX08Y07, 
#   HybX08Y08, HybX08Y09, HybX08Y10, HybX08Y11, HybX08Y12, HybX08Y13, HybX08Y14, HybX08Y15, 
#   HybX09Y00, HybX09Y01, HybX09Y02, HybX09Y03, HybX09Y04, HybX09Y05, HybX09Y06, HybX09Y07, 
#   HybX09Y08, HybX09Y09, HybX09Y10, HybX09Y11, HybX09Y12, HybX09Y13, HybX09Y14, HybX09Y15, 
#   HybX10Y00, HybX10Y01, HybX10Y02, HybX10Y03, HybX10Y04, HybX10Y05, HybX10Y06, HybX10Y07, 
#   HybX10Y08, HybX10Y09, HybX10Y10, HybX10Y11, HybX10Y12, HybX10Y13, HybX10Y14, HybX10Y15,
#   fun=min)
# Hyb_allNZ_NZGD00.sgdf <- as(Hyb_allNZ_NZGD00, "SpatialGridDataFrame")
# 
# 
# save(Hyb_allNZ_NZGD00, Hyb_allNZ_NZGD00.sgdf,
#      file=sprintf("~/VsMap/Rdata/hyb_NZGD00_allNZ_%s.Rdata",HYBMODEL))
# 
# 
# 
# 
# 




# # stitch together all geo NZGD00 maps into one structure
# for(x in 0:10) { # 11 
#   for (y in 0:15) { # 16
#     load(sprintf("~/VsMap/Rdata/geo_NZGD00_allNZ_x%02dy%02d_%s.Rdata", x,  y, GEOMODEL))
#     assign(x = sprintf("geoX%02dY%02d", x, y),value = geoRaster)
#   }
# }
# 
# 
# 
# 
# geo_allNZ_NZGD00 <- mosaic(
#   geoX00Y00, geoX00Y01, geoX00Y02, geoX00Y03, geoX00Y04, geoX00Y05, geoX00Y06, geoX00Y07, 
#   geoX00Y08, geoX00Y09, geoX00Y10, geoX00Y11, geoX00Y12, geoX00Y13, geoX00Y14, geoX00Y15, 
#   geoX01Y00, geoX01Y01, geoX01Y02, geoX01Y03, geoX01Y04, geoX01Y05, geoX01Y06, geoX01Y07, 
#   geoX01Y08, geoX01Y09, geoX01Y10, geoX01Y11, geoX01Y12, geoX01Y13, geoX01Y14, geoX01Y15, 
#   geoX02Y00, geoX02Y01, geoX02Y02, geoX02Y03, geoX02Y04, geoX02Y05, geoX02Y06, geoX02Y07, 
#   geoX02Y08, geoX02Y09, geoX02Y10, geoX02Y11, geoX02Y12, geoX02Y13, geoX02Y14, geoX02Y15, 
#   geoX03Y00, geoX03Y01, geoX03Y02, geoX03Y03, geoX03Y04, geoX03Y05, geoX03Y06, geoX03Y07, 
#   geoX03Y08, geoX03Y09, geoX03Y10, geoX03Y11, geoX03Y12, geoX03Y13, geoX03Y14, geoX03Y15, 
#   geoX04Y00, geoX04Y01, geoX04Y02, geoX04Y03, geoX04Y04, geoX04Y05, geoX04Y06, geoX04Y07, 
#   geoX04Y08, geoX04Y09, geoX04Y10, geoX04Y11, geoX04Y12, geoX04Y13, geoX04Y14, geoX04Y15, 
#   geoX05Y00, geoX05Y01, geoX05Y02, geoX05Y03, geoX05Y04, geoX05Y05, geoX05Y06, geoX05Y07,
#   geoX05Y08, geoX05Y09, geoX05Y10, geoX05Y11, geoX05Y12, geoX05Y13, geoX05Y14, geoX05Y15, 
#   geoX06Y00, geoX06Y01, geoX06Y02, geoX06Y03, geoX06Y04, geoX06Y05, geoX06Y06, geoX06Y07, 
#   geoX06Y08, geoX06Y09, geoX06Y10, geoX06Y11, geoX06Y12, geoX06Y13, geoX06Y14, geoX06Y15, 
#   geoX07Y00, geoX07Y01, geoX07Y02, geoX07Y03, geoX07Y04, geoX07Y05, geoX07Y06, geoX07Y07, 
#   geoX07Y08, geoX07Y09, geoX07Y10, geoX07Y11, geoX07Y12, geoX07Y13, geoX07Y14, geoX07Y15, 
#   geoX08Y00, geoX08Y01, geoX08Y02, geoX08Y03, geoX08Y04, geoX08Y05, geoX08Y06, geoX08Y07, 
#   geoX08Y08, geoX08Y09, geoX08Y10, geoX08Y11, geoX08Y12, geoX08Y13, geoX08Y14, geoX08Y15, 
#   geoX09Y00, geoX09Y01, geoX09Y02, geoX09Y03, geoX09Y04, geoX09Y05, geoX09Y06, geoX09Y07, 
#   geoX09Y08, geoX09Y09, geoX09Y10, geoX09Y11, geoX09Y12, geoX09Y13, geoX09Y14, geoX09Y15, 
#   geoX10Y00, geoX10Y01, geoX10Y02, geoX10Y03, geoX10Y04, geoX10Y05, geoX10Y06, geoX10Y07, 
#   geoX10Y08, geoX10Y09, geoX10Y10, geoX10Y11, geoX10Y12, geoX10Y13, geoX10Y14, geoX10Y15,
#   fun=min)
# geo_allNZ_NZGD00.sgdf <- as(geo_allNZ_NZGD00, "SpatialGridDataFrame")
# 
# 
# save(geo_allNZ_NZGD00, geo_allNZ_NZGD00.sgdf,
#      file=sprintf("~/VsMap/Rdata/geo_NZGD00_allNZ_%s.Rdata",GEOMODEL))

