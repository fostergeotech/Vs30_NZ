#krige_fns.R
library(raster)
library(gstat)




krigeRaster <- function(inpIndexRaster, inpModelRaster, 
                        vspr, resName, variogram, krgMaxDist, krgNmax, useMaxDist, useNmax) {
  
  lnObsPred <- stdDev <- inpModelRaster
  
  NAs <- is.na(inpModelRaster)
  
  if (!all(as.vector(NAs))) { # catches the case where entire input raster is NAs.
    
    
    iALL_na <- Which(!is.na(inpIndexRaster), cells=TRUE)
    xy <- xyFromCell(inpModelRaster, iALL_na, spatial = TRUE)

    # This sets the variable being kriged.
    # Note it's the residuals, not the estimates themselves!
    # formula <- vspr[[resName]] ~ 1
    if(identical(resName,      "res_AhdiAK_noQ3_hyb09c")) {
      #vspr$thisResidual <- vspr$res_AhdiAK_noQ3_hyb09c
      vspr$thisResidual <- (vspr$res_AhdiAK_noQ3_hyb09c / vspr$stDv_AhdiAK_noQ3_hyb09c) # using NORMALIZED residuals now.  (20180926)
    } else if(identical(resName, "res_YongCA_noQ3")) {
      #vspr$thisResidual <-   vspr$res_YongCA_noQ3
      vspr$thisResidual <-   (vspr$res_YongCA_noQ3 / vspr$stDv_YongCA_noQ3) # using NORMALIZED residuals now. (20180926)
    }
    
    
    if(useMaxDist & useNmax) {
      ko <- krige(formula   = thisResidual~1, locations = vspr, newdata   = xy, model     = variogram, beta      =  0  , nmax      = krgNmax, maxdist   = krgMaxDist )@data}
    else {if(useMaxDist & !useNmax) {
      ko <- krige(formula   = thisResidual~1, locations = vspr, newdata   = xy, model     = variogram, beta      =  0  , maxdist   = krgMaxDist )@data}
    else {if(!useMaxDist & useNmax) {
      ko <- krige(formula   = thisResidual~1, locations = vspr, newdata   = xy, model     = variogram, beta      =  0  , nmax      = krgNmax)@data}
    else {if(!useMaxDist & !useNmax) {
      ko <- krige(formula   = thisResidual~1, locations = vspr, newdata   = xy, model     = variogram, beta      =  0)@data}
    }}}
    
    
    
    
    lnObsPred[iALL_na] <- ko$var1.pred
    stdDev[iALL_na] <- sqrt(ko$var1.var)
  }
  return(c(lnObsPred=lnObsPred, stdDev=stdDev))
}


krigeNZGD00toWGS84loop <- function(fileName) {
  setwd("~/VsMap/")
  library(raster)
  source("R/functions.R")
  source("R/maps2_fns.R")
  
  base <- tools::file_path_sans_ext(fileName)  # e.g. "KRIGE_NZGD00_allNZ_x01y02"
  base <- substr(base, start=7, stop=999999)  # e.g. "NZGD00_allNZ_x01y02"
  
  load(file=paste0("Rdata/KRIGE_",base,".Rdata"))
  
  Hyb_WGS84_NZ <- makeTIFandXYZ_WGS84(Hyb,
                                      TIFname=paste0("/tmp/hyb_WGS84_",base,".tif"),
                                      XYZname=paste0("img/hyb_WGS84_",base,".xyz"))
  save(Hyb_WGS84_NZ, file = paste0("Rdata/hyb_WGS84_",base,".Rdata"))
}




doTheKrigingThing <- function(index,xVec,yVec,vsprSubset,
                              variogram, krgMaxDist, krgNmax, useMaxDist, useNmax,
                              MODEL, resampleFlag,
                              variogramVersion) {
  source("R/krige_fns.R")
  source("R/functions.R")
  
  
  xTileNum <- xVec[index]
  yTileNum <- yVec[index]

  # note for testing: x05y05 includes Christchurch.
  xyTile <- sprintf("x%02dy%02d",xTileNum,yTileNum)
  # load(paste0("Rdata/","_NZGD00_allNZ_",  xyTile,"_",MODEL,".Rdata"))
  # load(paste0("Rdata/INDEX_NZGD00_allNZ_",xyTile,".Rdata"))
  modelRaster <- raster(switch(MODEL,
                               AhdiAK                  = "~/big_noDB/models/geo_NZGD00_allNZ_AhdiAK.tif",
                               AhdiAK_noQ3             = "~/big_noDB/models/geo_NZGD00_allNZ_AhdiAK_noQ3.tif",
                               AhdiAK_noQ3_hyb09c      = "~/big_noDB/models/hyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                               YongCA                  = "~/big_noDB/models/Yong2012_Cali_Vs30.tif",
                               YongCA_noQ3             = "~/big_noDB/models/YongCA_noQ3.tif",
                               AhdiYongWeighted1       = "~/big_noDB/models/AhdiYongWeighted1.tif"))
  
  # new 20180926 - need to use stDv raster too, because variogram is now normalized.
  stDevRaster <- raster(switch(MODEL,
                               AhdiAK                  = "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK.tif",
                               AhdiAK_noQ3             = "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK_noQ3.tif",
                               AhdiAK_noQ3_hyb09c      = "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                               YongCA                  = "~/big_noDB/models/YongCA_sigma.tif",
                               YongCA_noQ3             = "~/big_noDB/models/YongCA_noQ3_sigma.tif",
                               AhdiYongWeighted1       = "~/big_noDB/models/AhdiYongWeighted1_sigma.tif"))
  
  fullExtents <- extent(modelRaster)
  wth <- ncol(modelRaster)
  hgt <- nrow(modelRaster)
  nTilesX <- 11
  nTilesY <- 16
  wth1 <- wth/nTilesX
  hgt1 <- hgt/nTilesY
  if(!( # if the raster cannot be subdivided perfectly, there's something wrong with resolution/tiling scheme.
    floor(hgt1)==hgt1 && floor(wth1)==wth1)) {stop("ERROR - raster is not readily subdivided into equal sized tiles.")}
  
  cropLim_x_0 <- wth1*xTileNum + 1 #fullExtents[1]
  cropLim_x_1 <- cropLim_x_0 + wth1 - 1
  cropLim_y_0 <- hgt1*yTileNum + 1 #fullExtents[3]
  cropLim_y_1 <- cropLim_y_0 + hgt1 - 1
  cropLim_row_0 <- hgt - cropLim_y_1 + 1
  cropLim_row_1 <- cropLim_row_0 + hgt1 - 1
  cropLim_col_0 <- cropLim_x_0
  cropLim_col_1 <- cropLim_x_1
  
  cropLims <- extent(modelRaster, # giving modelRaster as first argument means remaining 4 arguments are row and col references.
                     cropLim_row_0,
                     cropLim_row_1,
                     cropLim_col_0,
                     cropLim_col_1)
  
  modelTile <- crop(modelRaster,cropLims)
  stDevTile <- crop(stDevRaster,cropLims)
  rm(modelRaster,stDevRaster) # save memory
  
  indexAll <-  raster("~/big_noDB/models/INDEX_NZGD00_allNZ.tif")
  indexRaster <- crop(indexAll, cropLims)
  rm(indexAll)
  
  
  if (resampleFlag) {
    # Note - resampling the MODEL at low resolution with bilinear interpolation
    # yielded a few unstable pixels in testing. Not clear how this happens. 
    # For now, if resampling is done here,
    # make it nearest-neighbor!!
    # (obviously, the INDEX raster MUST ALWAYS be interpolated nearest-neighbor.)
    inpModelRaster <- resampleRaster(modelTile,              TheMethod="ngb", newRes_m=newRes)
    
    inpIndexRaster <- resampleRaster(indexRaster, TheMethod="ngb", newRes_m=newRes)
    inpStDevRaster <- resampleRaster(stDevTile,   TheMethod="ngb", newRes_m=newRes)
  } else {
    inpIndexRaster <- indexRaster
    inpModelRaster <- modelTile
    inpStDevRaster <- stDevTile
  }
  
  krigedOutput <- krigeRaster(inpIndexRaster, inpModelRaster, 
                              vspr      = vsprSubset, 
                              resName   = paste0("res_", MODEL),
                              variogram = variogram,
                              krgMaxDist, krgNmax, useMaxDist, useNmax)
  
  NAs <- is.na(inpModelRaster)
  #krigeVs30 <- inpModelRaster*exp(krigedOutput$lnObsPred)  # actual Vs30 prediction
  krigeVs30 <- inpModelRaster*exp(krigedOutput$lnObsPred * inpStDevRaster)  # actual Vs30 prediction - now adjusted using input standard deviation because variogram is normalized.
  resid     <- krigedOutput$lnObsPred # interpolated residual surface, STILL normalized.
  stDev     <- krigedOutput$stdDev  # std Dev (kriging stDv only!)
  resid[NAs] <- NA
  stDev[NAs] <- NA
  
  for (prefix in c("krige", "resid", "stDev")) {
    rast <- switch(prefix,
                   krige = krigeVs30,
                   resid = resid,
                   stDev = stDev)
    fileMain <- paste0(prefix,"_NZGD00_allNZ_",xyTile,"_",MODEL,"_",variogramVersion)
    print(fileMain)
    writeRaster(rast, filename=paste0("tmp/",fileMain,".tif"), format = "GTiff", overwrite=TRUE)
  }
}



