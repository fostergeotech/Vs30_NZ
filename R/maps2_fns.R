#maps2_fns.R
#


getHybFromGeoAndSlopeAndIndex <- function(
    geoRaster, slpRasterNZGD00, indexRaster, HYBMODEL, idxGrpDF) {
  
  source("R/models.R") # contains hybrid models
  hybRaster <- geoRaster  #hyb will be modified starting from geo
  
  # Use the model's hybrid lookup dataframe to decide which polygons to modify
  hybDF <- get(paste0(MODEL, "_hybDF")) # e.g. AhdiAK_KaiAll_hyb09c_hybDF
  
  # create a raster containing groups
  grpRaster <- subs(indexRaster, idxGrpDF, subsWithNA=T)
  lvlz      <- levels(grpRaster)[[1]]
  
  
  for(i in 1:nrow(hybDF)) { # loop over hybrid units
    # i <- 1  # testing
    groupID  <- as.character(hybDF$slopeUnits[i])
    groupIDn <- lvlz$ID[which(lvlz$mapGrp==groupID)]
    pixelsInGroup <- Which(grpRaster == groupIDn, cells=T)
    
    if(length(pixelsInGroup) > 0) { # if there are pixels to modify...
      slp09c <- slpRasterNZGD00[pixelsInGroup]
      data  <- data.frame(slp09c)
      hybFn <- paste0(HYBMODEL, "_set_Vs30_hyb")
      Vs30out <- do.call(what = hybFn, 
                         args = list(data = data,
                                     groupID = groupID))
      hybRaster[pixelsInGroup] <- Vs30out
    }
  }
  return(hybRaster)
}




rasterLoopGeo <- function(indexFile, idxGeoDF, GEOMODEL) {  # this function will be parallelized below...
  library(raster)
  source("R/functions.R")
  source("R/maps2_fns.R")
  base <- tools::file_path_sans_ext(indexFile)  # e.g. "INDEX_NZGD00_allNZ_x01y02"
  base <- substr(base, start=7, stop=999999)  # e.g. "NZGD00_allNZ_x01y02"
  
  base <- paste0(base, "_", GEOMODEL)  # identify the model that is used

  load(paste0("Rdata/",indexFile))  # variable is named indexRaster
  geoRaster <- subs(indexRaster, idxGeoDF, subsWithNA=T)
  
  
  # make PNG and TIF - not needed -------------------------
  # writeRaster(geoRaster, filename=paste0("img/Msgeo_",base,".tif"), format = "GTiff", overwrite=TRUE)
  # png(paste0("out/Msgeo_",base,".png"), width = 8, height = 8, units = "in", res = 300)
  # plot(geoRaster)
  # dev.off()
  
  
  save(geoRaster, file=paste0("Rdata/geo_",base,".Rdata"))
  #load(file=paste0("Rdata/Msgeo_",base,".Rdata"))
  base2 <- substr(base, start=8, stop=999999)  # e.g. "allNZ_x01y02"
  geoRasterWGS84 <- makeTIFandXYZ_WGS84(geoRaster,
                                        TIFname=paste0("/tmp/geo_WGS84_",base2,".tif"),
                                        XYZname=paste0("img/geo_WGS84_",base2,".xyz"))
  save(geoRasterWGS84, file = paste0("Rdata/geo_WGS84_",base2,".Rdata"))
  #load(                file = paste0("Rdata/geo_WGS84_",base2,".Rdata"))
}








rasterLoopHyb <- function(indexFile, idxGrpDF, GEOMODEL, HYBMODEL) {
  
  # indexFile refers to the INDEX raster file.
  # idxGrpDF is the table lookup allowing to generate an intermediate group raster
  # GEOMODEL is required to parse a string identifying the previously-generated 
  #       geology raster which the hybrid raster will modify.
  # HYBMODEL is of course for applying the hybrid slope changes.
  
  setwd("~/VsMap/")
  library(raster)
  source("R/functions.R")
  source("R/maps2_fns.R") # needed for parallelization. Alternative is to export
  
  
  base <- tools::file_path_sans_ext(indexFile)  # e.g. "INDEX_NZGD00_allNZ_x01y02"
  base <- substr(base, start=7, stop=999999)   # e.g. "NZGD00_allNZ_x01y02"
  
  # load the geology raster:
  baseGeo <- paste0(base, "_", GEOMODEL)
  load(file=paste0("Rdata/geo_",baseGeo,".Rdata"))
  
  # This requires slope tiles to have been created first---in tileSlopes.R
  load(file = paste0("Rdata/slp_", base, ".Rdata"))
  
  # load the index raster:
  load(file = paste0("Rdata/INDEX_",base,".Rdata"))
  
  # don't need a TIF of the slope..
  # writeRaster(slpRasterNZGD00, filename=paste0("img/slp_",base,".tif"), format = "GTiff", overwrite=TRUE)
  
  # the actual work:
  hybRaster <- getHybFromGeoAndSlopeAndIndex(
      geoRaster, slpRasterNZGD00, indexRaster, HYBMODEL, idxGrpDF)
  
  
  baseHyb <- paste0(base, "_", HYBMODEL)
  save(hybRaster, file=paste0("Rdata/hyb_", baseHyb, ".Rdata"))
  writeRaster(hybRaster, filename=paste0("img/hyb_",baseHyb,".tif"), format = "GTiff", overwrite=TRUE)
  
  baseHyb <- substr(baseHyb, start=8, stop=999999)  # e.g. "allNZ_x01y02"
  hybRasterWGS84 <- makeTIFandXYZ_WGS84(hybRaster,
                                      TIFname=paste0("/tmp/hyb_WGS84_",baseHyb,".tif"),
                                      XYZname=paste0("img/hyb_WGS84_",baseHyb,".xyz"))
  save(hybRasterWGS84, file = paste0("Rdata/hyb_WGS84_",baseHyb,".Rdata"))
}

