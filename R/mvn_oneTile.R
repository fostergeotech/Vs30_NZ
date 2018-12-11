# mvn_oneTile.R

# Where appropriate, equations are referenced by number in the draft PDF of Worden et al.
# PDF title: worden-al-2017--20170719.pdf
# saved in this directory for future reference.
#
# shorthand: "Wea" = Worden et al.

mvn_oneTile <- function(index,xVec,yVec,vsprSubset,vg,MODEL) {
  library(gstat)
  library(Matrix)
  library(raster)
  
  source("R/mvn_params.R")
  source("R/functions.R")
  source("R/mvnRaster.R")
  
  
  
  
  
  
  
  xTileNum <- xVec[index]
  yTileNum <- yVec[index]
  # note for testing: x04y02 (index 85) includes Christchurch.
  xyTile <- sprintf("x%02dy%02d",xTileNum,yTileNum)
  load(sprintf("Rdata/INDEX_NZGD00_allNZ_%s.Rdata",xyTile))
  exts <- extent(indexRaster)
  
  wholeModel <- raster(switch(MODEL,
    AhdiAK                  = "~/big_noDB/models/geo_NZGD00_allNZ_AhdiAK.tif",
    AhdiAK_noQ3             = "~/big_noDB/models/geo_NZGD00_allNZ_AhdiAK_noQ3.tif",
    AhdiAK_noQ3_hyb09c      = "~/big_noDB/models/hyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
    YongCA                  = "~/big_noDB/models/Yong2012_Cali_Vs30.tif",
    YongCA_noQ3             = "~/big_noDB/models/YongCA_noQ3.tif",
    AhdiYongWeighted1       = "~/big_noDB/models/AhdiYongWeighted1.tif"))       
  
  wholeStdDev <- raster(switch(MODEL,
    AhdiAK                  = "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK.tif",
    AhdiAK_noQ3             = "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK_noQ3.tif",
    AhdiAK_noQ3_hyb09c      = "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
    YongCA                  = "~/big_noDB/models/YongCA_sigma.tif",
    YongCA_noQ3             = "~/big_noDB/models/YongCA_noQ3_sigma.tif",
    AhdiYongWeighted1       = "~/big_noDB/models/AhdiYongWeighted1_sigma.tif"))       
  
  inpRaster <- crop(wholeModel, exts)
  inpStdDev <- crop(wholeStdDev, exts)
  
  
  if (resampleFlag) {
    # Note - resampling the MODEL at low resolution with bilinear interpolation
    # yielded a few unstable pixels in testing. Not clear how this happens. 
    # For now, if resampling is done here,
    # make it nearest-neighbor!!
    # (obviously, the INDEX raster MUST ALWAYS be interpolated nearest-neighbor.)
    inpModelRaster  <- resampleRaster(inpRaster,   TheMethod = "ngb",  newRes_m = newRes)
    inpIndexRaster  <- resampleRaster(indexRaster, TheMethod = "ngb",  newRes_m = newRes)
    inpStdDevRaster <- resampleRaster(inpStdDev,   TheMethod = "ngb",  newRes_m = newRes)
  } else {
    inpIndexRaster  <- indexRaster
    inpModelRaster  <- inpRaster
    inpStdDevRaster <- inpStdDev
  }
  
  vspr <- vsprSubset
  variogram  <- vg
  mvnOutput <- mvnRaster(inpIndexRaster, inpModelRaster, inpStdDevRaster,
                               vspr, variogram, MODEL, covReducPar)
  mvnVs30 <- inpModelRaster*exp(mvnOutput$lnObsPred)
  # KrigGeoVs30 <- geoModelRaster*exp(krigedGeoOutput$lnObsPred)
  
  save(mvnOutput, mvnVs30, 
       file = sprintf("Rdata/MVNRM_NZGD00_allNZ_%s_%s_noisy%s_minDist%03.1fkm_%s_crp%03.1f.Rdata",
                      xyTile,MODEL,
                      substr(as.character(useNoisyMeasurements),1,1),
                      minThreshold/1000,
                      vgName,covReducPar))
  
  for (prefix in c("MVkrg", "MVres", "MVsdv")) {
    rast <- switch(prefix,
                   MVkrg = mvnVs30,
                   MVres = mvnOutput$lnObsPred,
                   MVsdv = mvnOutput$stdDev)
    writeRaster(rast, filename=paste0("tmp/",prefix,"_NZGD00_allNZ_",xyTile,"_",MODEL,
                                      "_noisy",substr(as.character(useNoisyMeasurements),1,1),
                                      sprintf("_minDist%03.1fkm",minThreshold/1000),
                                      "_",vgName,sprintf("_crp%03.1f",covReducPar),
                                      ".tif"), format = "GTiff", overwrite=TRUE)
  }
  
}




