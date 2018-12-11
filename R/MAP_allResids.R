# This script is based on QCAMmaps_Vs30_allNZgeo_tryResample.R
#
# ALL interpolated residual surfaces (i.e., both Kriged and MVN)

rm(list=ls())
setwd("~/VsMap")


# library(viridis)
library(RColorBrewer)
library(raster)
source("R/functions.R")
source("R/MAP_NZGD00_regions.R") # contains regions
source("R/MAPparams.R")
load("Rdata/vspr.Rdata")

mapIDvec <- c(  
              ## old ## "AY1_MVN",
              ## old ## "AY1_KRG",
              ## old ## "AY1_MVN_check", # ADDED AY1_MVN_check on 20180316 - solely for verifying that I generated the MVN residual raster correctly.
              ## old ##                  # (it was created initially within the MVN method; taking the ratio (or exp(log-log)) of the input and 
              ## old ##                  # output MVN rasters should yield same result!)
              ## old ## "AY1_MVN_norm",  # normalized MVN residual
              ## old ## "AY1_MVN_noisyT_minDist0_v2",
              ## old ## "AY1_MVN_noisyF_minDist2.5km_v2",
              ## old ## "AY1_MVN_noisyF_minDist2.5km_v1"
              ## old ## "AY1_MVN_noisyT_minDist0_v2_norm",
              ## old ## "AY1_MVN_noisyF_minDist2.5km_v2_norm",
              ## old ## "AY1_MVN_noisyF_minDist2.5km_v1_norm"
              ## old ## "AY1_KRG_v4",
              # "AY1_MVN_noisyT_minDist0_v3_crp1.5",
              # "AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN",
              # "AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1",
              # "AY1_MVN_noisyT_minDist0_v3_crp0.0",
              # "AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN",
              # "AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1",
              # "AY1_MVN_noisyF_minDist0_v3_crp1.5",
              # "AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN",
              # "AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1",
              # "AY1_MVN_noisyF_minDist0_v3_crp0.0",
              # "AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN",
              # "AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1",
              # "AY1_KRG_v5",
              "AhdiAK_noQ3_hyb09c_KRG_v6_normSigma",
              "YongCA_noQ3_KRG_v7_normSigma",
              "AhdiAK_noQ3_hyb09c_KRG_v6",
              "YongCA_noQ3_KRG_v7",
              "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma", # these are not terribly useful, because the normalized maps aren't smooth (since there's still the CRP in play).
              "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma", # these are not terribly useful, because the normalized maps aren't smooth (since there's still the CRP in play).
              "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5",
              "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5"
              # "AhdiAK_noQ3_hyb09c_KRG_v8",
              # "YongCA_noQ3_KRG_v9"
)
regionVec <- names(extList)
# regionVec <- c("CH") # testing


# output
maxpix          = 1e7  # plot() for rasters is complicated. Using maxpix with massive
                       # rasters doesn't NECESSARILY result in quicker plotting (1e7
                       # was not working in my experience) but if raster is resampled
                       # beforehand, maxpixels works as expected and doesn't cause
                       # much/any delay.
makeMask        = F



# getVs30 points
vsSubset <- subsetVsPoints(vspr)$noQ3


# color stuff
nModCol     <- 101
nResCol     <- 7
# modelCol        = colorRampPalette(brewer.pal(11,"PuOr"))(nModCol)
modelColDiscrete <- c(
  "#4A6FE3",
  "#8E9EEA",
  "#C7CEF5",
  "#FFFFFF",
  "#F9C2CB",
  "#E9849A",
  "#D33F6A"
)
modelCol        = colorRampPalette(modelColDiscrete)(nModCol)
if(grayScaleWhiteWater) {
  modelCol <- darkenRGB(modelCol)
}
residCol = modelCol


# random plotting stuff
nGrayCol <- 100
bgcol = rgb(1,1,1)
txtcol = rgb(0,0,0)

CEX = 3


for(mapSize in mapSizeVec)  {
  # mapSize <- "six" # testing
  
  for(region in regionVec) {
  # for(region in c("CH")) { # testing
    # region <- "NI"
    # region <- "CH"
  
    pxWidth         = switch(mapSize,poster=3300,three=1113,six=1972)
    pxHeight        = switch(mapSize,poster=4200,three=1113,six=1972)
    
    margins <- switch(mapSize,poster=c(7,7,7,7),three=c(2,2,2,2),six=c(2,2,2,2))
    
    # these are the current resampling dimensions. I chose these by measuring map pane size manually for the 
    # chosen raster output size, 3300x4200.
    rasterX <- switch(mapSize,poster=2698,three=900,six=1800)
    rasterY <- switch(mapSize,poster=3632,three=900,six=1800)
    
    hillshade        <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_hillshadeA_NZGD00.tif",rasterX,rasterY,region))
    isOcean          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isOcean_NZGD00.tif",rasterX,rasterY,region))
    isWater          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isWater_NZGD00.tif",rasterX,rasterY,region))
    isIce            <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isIce_NZGD00.tif",rasterX,rasterY,region))
    
    for (mapID in mapIDvec) {
    # mapID <- mapIDvec[1] # testing
      
      if(identical(mapID,"AY1_MVN_check")) {
        modelRastNumerator <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1.tif", 
                                             rasterX, rasterY, region))
        modelRastDenominator <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1.tif", 
                                               rasterX, rasterY, region))
        modelRast <- log(modelRastNumerator) - log(modelRastDenominator)
      } else {
        if(mapID %in% c(
          "AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN",
          "AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1",
          "AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN",
          "AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1",
          "AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN",
          "AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1",
          "AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN",
          "AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1",
          "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma",
          "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma"
        )) {
          resid <- raster(sprintf(switch(mapID,
                                 ## old ## AY1_MVN_norm                               = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1.tif",
                                 ## old ## AY1_MVN_noisyT_minDist0_v2_norm            = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif",
                                 AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
                                 AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
                                 AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
                                 AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
                                 AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif",
                                 AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif",
                                 AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif",
                                 AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif",
                                 AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif",
                                 YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif"
                                 ),
                                  rasterX, rasterY, region))
          stdv     <- raster(sprintf(switch(mapID,
                                 ## old ## AY1_MVN_norm                               = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1.tif",
                                 ## old ## AY1_MVN_noisyT_minDist0_v2_norm            = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif",
                                 AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
                                 AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
                                 AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif",
                                 AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif",
                                 AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_sigma.tif",
                                 AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_sigma.tif",
                                 AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_sigma.tif",
                                 AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_sigma.tif",
                                 AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = "~/big_noDB/models/RESAMP_%04dx%04d_%s_sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                                 YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_noQ3_sigma.tif"
                                           ),
                                  rasterX, rasterY, region))
          modelRast <- resid/stdv
          # modelRast <- stdv/resid # experiment
        } else if(mapID %in% c(
          "AhdiAK_noQ3_hyb09c_KRG_v6",
          "YongCA_noQ3_KRG_v7"
        )) {
          # adding special case here - kriging resid surface is already normalized
          # so need to "un-normalize" it
          resid <- raster(sprintf(switch(mapID,
                                         AhdiAK_noQ3_hyb09c_KRG_v6 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
                                         YongCA_noQ3_KRG_v7        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_resid_NZGD00_allNZ_YongCA_noQ3_v7.tif"
          ),
          rasterX, rasterY, region))
          stdv <- raster(sprintf(switch(mapID,
                                        AhdiAK_noQ3_hyb09c_KRG_v6 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                                        YongCA_noQ3_KRG_v7        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_noQ3_sigma.tif"
          ),
          rasterX, rasterY, region))
          modelRast <- resid * stdv
        } else {
          modelRast <- raster(sprintf(switch(mapID,
                          ## old ##                    AY1_KRG          = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_resid_NZGD00_allNZ_AhdiYongWeighted1.tif",
                          ## old ##                    AY1_MVN          = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1.tif",
                          ## old ## AY1_MVN_noisyT_minDist0_v2          = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif",
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v2      = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist2.5km_v2.tif",
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v1      = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist2.5km_v1.tif",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5   = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0   = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5   = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0   = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif",
                          AY1_KRG_v4                          = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_resid_NZGD00_allNZ_AhdiYongWeighted1_v4.tif",
                          AY1_KRG_v5                          = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_resid_NZGD00_allNZ_AhdiYongWeighted1_v5.tif",
                          AhdiAK_noQ3_hyb09c_KRG_v6_normSigma = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
                          YongCA_noQ3_KRG_v7_normSigma        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_resid_NZGD00_allNZ_YongCA_noQ3_v7.tif",
                          AhdiAK_noQ3_hyb09c_KRG_v8           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v8.tif",
                          YongCA_noQ3_KRG_v9                  = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_resid_NZGD00_allNZ_YongCA_noQ3_v9.tif",
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif",
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_resid_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif"
          ), rasterX, rasterY, region))
        }
      }
      if(makeMask) {
        MVNmask <- raster(sprintf(switch(mapID,
                                         AY1_KRG       = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_AhdiYongWeighted1.tif",
                                         AY1_MVN       = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1.tif",
                                         AY1_MVN_check = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1.tif",
                                         AY1_MVN_norm  = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1.tif"
        ), rasterX, rasterY, region))
      }
      
      modelRast <- setMinMax(modelRast)
      print(sprintf("%s  -  Max: %06.3f Min: %06.3f",mapID, maxValue(modelRast),minValue(modelRast)))
      
      extents <- extList[[region]]
  
      
      zlims <- switch(mapID,
                          ## old ## AY1_KRG                                      = c(-2,2),
                          ## old ## AY1_MVN                                      = c(-2,2),
                          ## old ## AY1_MVN_check                                = c(-2,2),
                          ## old ## AY1_MVN_norm                                 = c(-3,3),
                          ## old ## AY1_MVN_noisyT_minDist0_v2                   = c(-2,2),
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v2               = c(-2,2),
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v1               = c(-2,2),
                          ## old ## AY1_MVN_noisyT_minDist0_v2_norm              = c(-3,3),
                          AY1_MVN_noisyT_minDist0_v3_crp1.5            = c(-2,2),
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN    = c(-8,8),
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1    = c(-5,5),
                          AY1_MVN_noisyT_minDist0_v3_crp0.0            = c(-2,2),
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN    = c(-8,8),
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1    = c(-5,5),
                          AY1_MVN_noisyF_minDist0_v3_crp1.5            = c(-3,3),
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN    = c(-10,10),
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1    = c(-5,5),
                          AY1_MVN_noisyF_minDist0_v3_crp0.0            = c(-3,3),
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN    = c(-10,10),
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1    = c(-5,5),
                          AY1_KRG_v4                                   = c(-2,2),
                          AY1_KRG_v5                                   = c(-2,2),
                          AhdiAK_noQ3_hyb09c_KRG_v6_normSigma          = c(-5,5),
                          YongCA_noQ3_KRG_v7_normSigma                 = c(-5,5),
                          AhdiAK_noQ3_hyb09c_KRG_v6                    = c(-1,1),
                          YongCA_noQ3_KRG_v7                           = c(-1,1),
                          AhdiAK_noQ3_hyb09c_KRG_v8                    = c(-5,5),
                          YongCA_noQ3_KRG_v9                           = c(-5,5),
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = c(-5,5),
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma        = c(-5,5),
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5 = c(-1,1),
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5        = c(-1,1)
                      
      )
      zscale <- seq(zlims[1],zlims[2], 
                                      switch(mapID,
                                                    ## old ## AY1_KRG            = 0.5,
                                                    ## old ## AY1_MVN            = 0.5,
                                                    ## old ## AY1_MVN_check      = 0.5,
                                                    ## old ## AY1_MVN_norm       = 0.5,
                          ## old ## AY1_MVN_noisyT_minDist0_v2                   = 0.5,
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v2               = 0.5,
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v1               = 0.5,
                          ## old ## AY1_MVN_noisyT_minDist0_v2_norm              = 0.5,
                          AY1_MVN_noisyT_minDist0_v3_crp1.5            = 0.5,
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN       = 2,
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1       = 1,
                          AY1_MVN_noisyT_minDist0_v3_crp0.0            = 0.5,
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN       = 2,
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1       = 1,
                          AY1_MVN_noisyF_minDist0_v3_crp1.5            = 1,
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN       = 20,
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1       = 1,
                          AY1_MVN_noisyF_minDist0_v3_crp0.0            = 1,
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN       = 20,
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1       = 1,
                          AY1_KRG_v4                                   = 0.5,
                          AY1_KRG_v5                                   = 0.5,
                          AhdiAK_noQ3_hyb09c_KRG_v6_normSigma                    = 0.5,
                          YongCA_noQ3_KRG_v7_normSigma                           = 0.5,
                          AhdiAK_noQ3_hyb09c_KRG_v6                              = 0.5,
                          YongCA_noQ3_KRG_v7                                     = 0.5,
                          AhdiAK_noQ3_hyb09c_KRG_v8                    = 0.5,
                          YongCA_noQ3_KRG_v9                           = 0.5,
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = 0.5,
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma = 0.5,
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5 = 0.5,
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5        = 0.5
                                      ))
      rscale <- seq(zlims[1],zlims[2],length.out = nResCol)
      
      
      # # only plot the Vs30 points contained within in the raster extents
      coordsD <- coordinates(vsSubset)
      coX <- coordsD[,1]
      coY <- coordsD[,2]
      exM <- extents
      whichVs <- which((coX >= exM[1]) &
                         (coX <= exM[2]) &
                         (coY >= exM[3]) &
                         (coY <= exM[4]))
      vs <- vsSubset[whichVs,]
      coordsD <- coordinates(vs)
      
      
      modelForResidualPoints = switch(mapID,
                                      ## old ## AY1_MVN                    = "AhdiYongWeighted1",
                                      ## old ## AY1_KRG                    = "AhdiYongWeighted1",
                                      ## old ## AY1_MVN_check              = "AhdiYongWeighted1",
                                      ## old ## AY1_MVN_norm               = "AhdiYongWeighted1",
                          ## old ## AY1_MVN_noisyT_minDist0_v2             = "AhdiYongWeighted1",
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v2         = "AhdiYongWeighted1",
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v1         = "AhdiYongWeighted1",
                          ## old ## AY1_MVN_noisyT_minDist0_v2_norm        = "AhdiYongWeighted1",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5      = "AhdiYongWeighted1",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN = "AhdiYongWeighted1",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1 = "AhdiYongWeighted1",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0      = "AhdiYongWeighted1",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN = "AhdiYongWeighted1",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1 = "AhdiYongWeighted1",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5      = "AhdiYongWeighted1",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN = "AhdiYongWeighted1",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1 = "AhdiYongWeighted1",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0      = "AhdiYongWeighted1",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN = "AhdiYongWeighted1",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1 = "AhdiYongWeighted1",
                          AY1_KRG_v4                             = "AhdiYongWeighted1",
                          AY1_KRG_v5                             = "AhdiYongWeighted1",
                          AhdiAK_noQ3_hyb09c_KRG_v6_normSigma              = "AhdiAK_noQ3_hyb09c",
                          YongCA_noQ3_KRG_v7_normSigma                     = "YongCA_noQ3",
                          AhdiAK_noQ3_hyb09c_KRG_v6                        = "AhdiAK_noQ3_hyb09c",
                          YongCA_noQ3_KRG_v7                               = "YongCA_noQ3",
                          AhdiAK_noQ3_hyb09c_KRG_v8              = "AhdiAK_noQ3_hyb09c",
                          YongCA_noQ3_KRG_v9                     = "YongCA_noQ3",
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = "AhdiAK_noQ3_hyb09c",
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma = "YongCA_noQ3",
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5 = "AhdiAK_noQ3_hyb09c",
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5        = "YongCA_noQ3"
      )
      vsD <- as.data.frame(vs)[,paste0("res_", modelForResidualPoints)]
      if(mapID %in% c(
        "AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1",
        "AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1",
        "AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1",
        "AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1",
        "AhdiAK_noQ3_hyb09c_KRG_v6_normSigma",
        "YongCA_noQ3_KRG_v7_normSigma",
        "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma",
        "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma"
      )) {
        # For most maps vsD (point color) is just ln(obs/pred),
        # but for the normalized version it's ln(obs/pred)/sigma.
        # (sigma = "stDv_xxxx" in the data structure.)
        vsD <- vsD/as.data.frame(vs)[,paste0("stDv_", modelForResidualPoints)]
        # vsD <- as.data.frame(vs)[,paste0("stDv_", modelForResidualPoints)]/vsD # experiment
      } else {if(endsWith(mapID,"_normMVN")) {
        # For normalizing by MVN sigma, divide by lnMeasUncer!
        # (This should be zero for the case noise=F, but because the residual surface approaches
        # infinity anyways there's no utility in comparing colors and the point overlay is turned off.)
        vsD <- vsD/as.data.frame(vs)[,"lnMeasUncer"]
        # vsD <- as.data.frame(vs)[,paste0("stDv_", modelForResidualPoints)]/vsD # experiment
      }}
      
      
      
      
      #### THESE ARE WRONG!!! ######
      # Dcol <- modelCol[as.numeric(cut(vsD,breaks=nModCol))] # choose colors
      # labels <- cut(vsD,breaks=nModCol)
      
      #### THESE ARE RIGHT!!! ######
      Dcol <- modelCol[as.numeric(cut(vsD,breaks=seq(zlims[1],zlims[2],length=nModCol+1)))] # choose colors
      labels <- cut(vsD,breaks=seq(zlims[1],zlims[2],length=nModCol+1))
      
      
      
      

            
      if(mapSize!="poster") {
        LARGE <- 0.6
        MED   <- 0.5
        SMALL <- 0.4 } else {
        LARGE <- 1.1
        MED   <- 1
        SMALL <- 0.8
      }
      
      sizes <- rep(LARGE, nrow(vs))
      
      q1s <- vs$QualityFlag=="Q1"
      q2s <- vs$QualityFlag=="Q2"
      q3s <- vs$QualityFlag=="Q3"
      used <- vs$QualityFlag!="Q3"
      McG <- vs$DataSource=="McGannCPT"
      Wth <- vs$DataSource=="Wotherspoon201711"
      
      sizes[q3s] <- SMALL
      sizes[q2s] <- MED
      
      shapes <- vector(length=nrow(vs))
      shapes[q1s] <- 23
      shapes[q2s] <- 22
      shapes[q3s] <- 21
      shapes[McG] <- 24
      shapes[Wth] <- 25
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      fileName =  paste0("out/maps/resid_",region,"_",mapID,"_",mapSize,".png")
      legName  =  paste0("out/maps/resid_",mapID,"_",mapSize,"_legend.png")
      
      png(fileName, width = pxWidth, height = pxHeight)
  
  
  
      # first par
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, bty="n")
  
      # plot hillshade first, then overlay
      plot(hillshade,
           col = grey.colors(nGrayCol, start=0, end=1), legend=F,
           xlab="NZGD2000 datum (metres)", ylab="NZGD2000 datum (metres)",
           main=switch(mapSize,three=NULL,poster=switch(mapID, 
                          ## old ## AY1_KRG    = "Interpolated/extrapolated residual from Kriging",
                          ## old ## AY1_MVN    = "Interpolated/extrapolated residual from MVN",
                          ## old ## AY1_MVN_check    = "Interpolated/extrapolated residual from MVN",
                          ## old ## AY1_MVN_norm     = "Interpolated/extrapolated residual from MVN, normalized by sigma",
                          ## old ## AY1_MVN_noisyT_minDist0_v2                  = "Interpolated/extrapolated residual from MVN",
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v2              = "Interpolated/extrapolated residual from MVN",
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v1              = "Interpolated/extrapolated residual from MVN",
                          ## old ## AY1_MVN_noisyT_minDist0_v2_norm             = "Interpolated/extrapolated residual from MVN, normalized by sigma",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5           = "Interpolated/extrapolated residual from MVN (crp=1.5)",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN      = "Interpolated/extrapolated residual from MVN (crp=1.5), normalized by MVN sigma",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1      = "Interpolated/extrapolated residual from MVN (crp=1.5), normalized by baseline sigma",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0           = "Interpolated/extrapolated residual from MVN (crp=0.0)",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN      = "Interpolated/extrapolated residual from MVN (crp=0.0), normalized by MVN sigma",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1      = "Interpolated/extrapolated residual from MVN (crp=0.0), normalized by baseline sigma",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5           = "Interpolated/extrapolated residual from MVN (crp=1.5)",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN      = "Interpolated/extrapolated residual from MVN (crp=1.5), normalized by MVN sigma",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1      = "Interpolated/extrapolated residual from MVN (crp=1.5), normalized by baseline sigma",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0           = "Interpolated/extrapolated residual from MVN (crp=0.0)",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN      = "Interpolated/extrapolated residual from MVN (crp=0.0), normalized by MVN sigma",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1      = "Interpolated/extrapolated residual from MVN (crp=0.0), normalized by baseline sigma",
                          AY1_KRG_v4 = "Interpolated/extrapolated residual from Kriging",
                          AY1_KRG_v5 = "Interpolated/extrapolated residual from Kriging",
                          AhdiAK_noQ3_hyb09c_KRG_v6_normSigma                      = "Interpolated/extrapolated residual from Kriging (geology-based model), normalized by baseline sigma",
                          YongCA_noQ3_KRG_v7_normSigma                             = "Interpolated/extrapolated residual from Kriging (terrain-based model), normalized by baseline sigma",
                          AhdiAK_noQ3_hyb09c_KRG_v6                                = "Interpolated/extrapolated residual from Kriging (geology-based model)",
                          YongCA_noQ3_KRG_v7                                       = "Interpolated/extrapolated residual from Kriging (terrain-based model)",
                          AhdiAK_noQ3_hyb09c_KRG_v8                      = "Interpolated/extrapolated residual from Kriging (geology-based model)",
                          YongCA_noQ3_KRG_v9                             = "Interpolated/extrapolated residual from Kriging (terrain-based model)",
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = "Interpolated/extrapolated residual from MVN (crp=1.5), normalized by baseline sigma",
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma        = "Interpolated/extrapolated residual from MVN (crp=1.5), normalized by baseline sigma",
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5 = "Interpolated/extrapolated residual from MVN (crp=1.5)",
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5        = "Interpolated/extrapolated residual from MVN (crp=1.5)"
           )),
           sub  =  switch(mapID,
                          ## old ## AY1_KRG                                      = "",
                          ## old ## AY1_MVN                                      = "",
                          ## old ## AY1_MVN_noisyT_minDist0_v2                   = "variogram v2  -  noise=T   -   minDist=0km",
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v2               = "variogram v2  -  noise=T   -   minDist=0km",
                          ## old ## AY1_MVN_noisyF_minDist2.5km_v1               = "variogram v2  -  noise=T   -   minDist=0km",
                          ## old ## AY1_MVN_noisyT_minDist0_v2_norm              = "variogram v2  -  noise=T   -   minDist=0km",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5            = "variogram v3  -  noise=T   -   minDist=1.5km",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN       = "variogram v3  -  noise=T   -   minDist=1.5km",
                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1       = "variogram v3  -  noise=T   -   minDist=1.5km",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0            = "variogram v3  -  noise=T   -   minDist=0km",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN       = "variogram v3  -  noise=T   -   minDist=0km",
                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1       = "variogram v3  -  noise=T   -   minDist=0km",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5            = "variogram v3  -  noise=F   -   minDist=1.5km",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN       = "variogram v3  -  noise=F   -   minDist=1.5km",
                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1       = "variogram v3  -  noise=F   -   minDist=1.5km",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0            = "variogram v3  -  noise=F   -   minDist=0km",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN       = "variogram v3  -  noise=F   -   minDist=0km",
                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1       = "variogram v3  -  noise=F   -   minDist=0km",
                          AY1_KRG_v4                                   = "variogram v4",
                          AY1_KRG_v5                                   = "variogram v5",
                          AhdiAK_noQ3_hyb09c_KRG_v6_normSigma                    = "variogram v6",
                          YongCA_noQ3_KRG_v7_normSigma                           = "variogram v7",
                          AhdiAK_noQ3_hyb09c_KRG_v6                              = "variogram v6",
                          YongCA_noQ3_KRG_v7                                     = "variogram v7",
                          AhdiAK_noQ3_hyb09c_KRG_v8                    = "variogram v8",
                          YongCA_noQ3_KRG_v9                           = "variogram v9",
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = "variogram v6",
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma        = "variogram v7",
                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5           = "variogram v6",
                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5                  = "variogram v7",
                          ""),
           cex=CEX, # cex = symbol size for scatter plots
           cex.sub = 0.8
           ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           )
  
  
      # Add water, ocean, ice
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isOcean , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           )
  
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isWater , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           )
  
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isIce   , col = rgb(1.0, 1.0, 1), alpha=0.8, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           )
  
  
      # map
  
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(modelRast, col = modelCol, zlim=zlims,
           #xaxt=seq(from=130, to=180, by=1)*1e4, yaxt='n',
           add=T,
           alpha=0.7,
           # alpha=1, # for troubleshooting
           legend=F # specify detailed legend stuff in another call.
           ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           )
      if(mapSize=="poster") {
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
        plot(modelRast, legend.only=T,
             col=modelCol, zlim=zlims,
             legend.width=1, legend.shrink=0.75,
             axis.args=list(at    = zscale,
                            labels= zscale,
                            cex.axis=1),
             legend.args=list(text=switch(mapID,
                                          ## old ## AY1_MVN_norm                              = 'Normalised residual surface, ln(obs./pred.)/sigma',
                                          ## old ## AY1_MVN_noisyT_minDist0_v2_norm           = 'Normalised residual surface, ln(obs./pred.)/sigma',
                                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN    = 'Normalised residual surface, ln(obs./pred.)/sigma_MVN',
                                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN    = 'Normalised residual surface, ln(obs./pred.)/sigma_MVN',
                                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN    = 'Normalised residual surface, ln(obs./pred.)/sigma_MVN',
                                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN    = 'Normalised residual surface, ln(obs./pred.)/sigma_MVN',
                                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AhdiAK_noQ3_hyb09c_KRG_v6_normSigma                    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          YongCA_noQ3_KRG_v7_normSigma                           = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AhdiAK_noQ3_hyb09c_KRG_v8                    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          YongCA_noQ3_KRG_v9                           = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma        = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          'Interpolated residual surface, ln(obs./pred.)'
                                          ),
                              side=4,
                              #font=2,
                              line=3.5,
                              cex=CEX)
             ,maxpixels = maxpix
             ,ext=extents
             ,axes=switch(mapSize,poster=T,three=F,six=F)
             )
      }
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
  
  
      if(makeMask) {
        # add uncertainty-mask
        # must be done incrementally (not sure how to set partial alpha per pixel!)
        # There is some fine-tuning to be done with the masking. To help with this, some
        # control parameters:
        nLevels <- 20
        alphaLevelSeq <- seq(from=0, to=0.6, length.out=nLevels)  # 0.6 comes from maxValue() applied to the MVN and KRG sigma rasters!
        alphaExp <- 1.8
        grayC <- rgb(0.7,0.7,0.7)
        for(alphaLevel in alphaLevelSeq) {
          thisMask <- MVNmask>alphaLevel
          thisMask[thisMask==0] <- NA
          par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
          plot(thisMask, col=grayC, alpha=alphaLevel^alphaExp, add=T, legend=F)
        }
      }
  
  
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      
      
      # # # Legend for residuals  # THIS DOESN'T WORK - waiting for stackoverflow response
      # # # https://stackoverflow.com/questions/48160409/scale-on-color-bar-is-not-behaving-multi-layer-raster-plot
      # # plot(modelRast, legend.only=T, horizontal=T, col=residCol, 
      # #      # legend.width=1, legend.shrink=0.75, 
      # #      zlims = rlims,
      # #      axis.args=list(
      # #               at    = rscale,
      # #               labels= as.character(round(rscale,1)),#,
      # #               cex.axis=1),
      # #      legend.args=list(
      # #               text='residual = ln(obs./pred.)',
      # #               # side=1,
      # #               # #font=2,
      # #               cex=CEX))
      # #
      # # # Instead, use this discrete option for now.
      # scaleLabels0 <- round(rscale,1)
      # scaleLabels1 <- as.character(scaleLabels0)
      # scaleLabels1[scaleLabels0==min(scaleLabels0)] <- paste0(scaleLabels1[scaleLabels0==min(scaleLabels0)], " (overpred.)")
      # scaleLabels1[scaleLabels0==max(scaleLabels0)] <- paste0(scaleLabels1[scaleLabels0==max(scaleLabels0)], " (underpred.)")
      # legend("topright",legend = scaleLabels1, 
      #        # horiz=T,
      #        fill=residCol, 
      #        title='residual = ln(obs./pred.)')
      
      
      
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      
      
      
      # plotting Vs30 points...................
      
      
      # plot points
      
      # For normalized residuals with noise=0, the ratio tends toward infinity.
      # In this case the plotted residual points aren't useful because they can't be compared against
      # background color visually. So, I don't plot the points for those cases.
      if(!(identical(mapID,"AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN")) &
         !(identical(mapID,"AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN"))) {
      
        # Only show Q3 points for AhdiAK geology model...
        if(identical(mapID, "AhdiAK")) {
          points(x = coordsD[q3s,], pch=shapes[q3s], bg=Dcol[q3s], col="black", cex=sizes[q3s])}
        points(x = coordsD[McG,], pch=shapes[McG], bg=Dcol[McG], col="black", cex=sizes[McG])
        points(x = coordsD[Wth,], pch=shapes[Wth], bg=Dcol[Wth], col="black", cex=sizes[Wth])
        points(x = coordsD[q2s,], pch=shapes[q2s], bg=Dcol[q2s], col="black", cex=sizes[q2s])
        points(x = coordsD[q1s,], pch=shapes[q1s], bg=Dcol[q1s], col="black", cex=sizes[q1s])
        # text(x = coordsD, labels=labels) # this line just for troubleshooting
        # text(x = coordsD, labels=paste0(coordsD[,1]," ",coordsD[,2]), cex=0.4) # this line just for troubleshooting
        
        
      if(mapSize=="poster") {
          
          
          # legend....
          # for AhdiAK, include Q3. For all other maps, don't.
          switch(mapID,
                 AhdiAK = {
                   legend(x="topleft",
                          legend = c("Kaiser_Q1","Kaiser_Q2","Kaiser_Q3 (not used)","McGann (resampled)","Wotherspoon"),
                          pt.bg = c(residCol[22], residCol[11], residCol[88], residCol[44], residCol[66]), # arbitrary colors
                          pch=c(23,22,21,24,25),
                          pt.cex=c(LARGE, MED, SMALL, LARGE, LARGE),
                          title = "Vs30 data: quality designation"#,
                          #horiz=T
                   )
                 },
                 {
                   legend(x="topleft",
                          legend = c("Kaiser_Q1","Kaiser_Q2","McGann (resampled)","Wotherspoon"),
                          pt.bg = c(residCol[44], residCol[22], residCol[88], residCol[44]), # arbitrary colors
                          pch=c(23,22,24,25),
                          pt.cex=c(LARGE, MED, LARGE, LARGE),
                          title = "Vs30 data: quality designation"#,
                          #horiz=T
                   )
                 }
          )
            
        }
        
      }
      # draw scale & north arrow
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
      drawScale(region)
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
      northarrow(region)


      dev.off()
    
      if(mapSize=="three") {
        png(legName, width=450,height=pxHeight)
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol)
        plot.new()
        plot(modelRast, legend.only=T,
             col=modelCol, zlim=zlims,
             legend.width=1, legend.shrink=0.75,
             axis.args=list(at    = zscale,
                            labels= zscale,
                            cex.axis=1),
             legend.args=list(text=switch(mapID,
                                          ## old ## AY1_MVN_norm                              = 'Normalised residual surface, ln(obs./pred.)/sigma',
                                          ## old ## AY1_MVN_noisyT_minDist0_v2_norm           = 'Normalised residual surface, ln(obs./pred.)/sigma',
                                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normMVN    = 'Normalised residual surface, ln(obs./pred.)/sigma_MVN',
                                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normMVN    = 'Normalised residual surface, ln(obs./pred.)/sigma_MVN',
                                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normMVN    = 'Normalised residual surface, ln(obs./pred.)/sigma_MVN',
                                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normMVN    = 'Normalised residual surface, ln(obs./pred.)/sigma_MVN',
                                          AY1_MVN_noisyT_minDist0_v3_crp1.5_normAY1    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AY1_MVN_noisyT_minDist0_v3_crp0.0_normAY1    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AY1_MVN_noisyF_minDist0_v3_crp1.5_normAY1    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AhdiAK_noQ3_hyb09c_KRG_v6_normSigma                    = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          YongCA_noQ3_KRG_v7_normSigma                           = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma        = 'Normalised residual surface, ln(obs./pred.)/sigma_baseline',
                                          'Interpolated residual surface, ln(obs./pred.)'
                                          ),
                              side=4,
                              #font=2,
                              line=3.0,
                              cex=CEX)
             ,maxpixels = maxpix
             ,ext=extents
             ,axes=switch(mapSize,poster=T,three=F,six=F)
             ,smallplot=c(0.32,0.48,0.075,0.925)
        )
        dev.off()
      }
      
    } # for mapID
  } # for mapSize
} # for region
