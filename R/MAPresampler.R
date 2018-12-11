# MAPresampler.R
#
# Batch resampling for many maps.
#

rm(list=ls())
setwd("~/VsMap/")

library(raster)


tifList <- c(

  # INDEX TIFF
  # "~/big_noDB/models/INDEX_NZGD00_allNZ.tif",

  # SLOPE TIFF (specifically, the 09c one used for Hybrid model generation)
  # "~/big_noDB/topo/slp_NZMG/slp_NZGD00_allNZ_fromHybRdataFiles.tif",
  # "~/big_noDB/models/slopeWeighting1_w0.tif", # this is the weighting factor applied in makeRaster_AhdiYongWeighted1_KRG_slopeWeighting1.R

  # CATEGORY TIFFS....
  # "~/big_noDB/models/AhdiGeoCats.tif",
  # "~/big_noDB/topo/terrainCats/IwahashiPike_NZ_100m_16.tif",

  # Vs30 TIFFS.......
  ## old ## "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
  ## old ## "~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
  ## old ## "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1.tif",
  ## old ## "~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_AhdiYongWeighted1.tif",
  ## old ## "~/big_noDB/models/AhdiYongWeighted1_KRG_slopeWeighting1_Vs30.tif",
  ## old ## "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif",

  ## old ## "~/big_noDB/models/AhdiYongWeighted1.tif",
  ## old ## "~/big_noDB/models/AhdiYongWeighted1_MVN_slopeWeighting1_Vs30.tif",
  ## old ## "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
  ## old ## "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
  ## old ## "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif",
  ## old ## "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif",
  ## old ## "~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_AhdiYongWeighted1_v4.tif",
  ## old ## "~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_AhdiYongWeighted1_v5.tif",
  ## old ## "~/big_noDB/models/AhdiYongWeighted1_KRG_slopeWeighting1_Vs30_v5.tif",
  ## old ## "~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v8.tif",
  ## old ## "~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_YongCA_noQ3_v9.tif",
  ## old ## "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v8_crp1.5.tif",
  ## old ## "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v9_crp1.5.tif",
  
  # "~/big_noDB/models/geo_NZGD00_allNZ_AhdiAK.tif",
  # "~/big_noDB/models/geo_NZGD00_allNZ_AhdiAK_noQ3.tif",
  # "~/big_noDB/models/hyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
  # "~/big_noDB/models/Yong2012_Cali_Vs30.tif",
  # "~/big_noDB/models/YongCA_noQ3.tif",

  # "~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
  # "~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_YongCA_noQ3_v7.tif",
   # "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif",
  # "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif",
  
  # SIGMA TIFFS.......
  ## old ## "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
  ## old ## "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
  ## old ## "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1.tif",
  ## old ## "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_AhdiYongWeighted1.tif",
  ## old ## "~/big_noDB/models/AhdiYongWeighted1_KRG_slopeWeighting1_sigma.tif",
  ## old ## "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif",

  ## old ## "~/big_noDB/models/AhdiYongWeighted1_sigma.tif",
  ## old ## "~/big_noDB/models/AhdiYongWeighted1_MVN_slopeWeighting1_sigma.tif",
  ## old ## "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
  ## old ## "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
  ## old ## "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif",
  ## old ## "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif",
  ## old ## "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_AhdiYongWeighted1_v4.tif",
  ## old ## "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_AhdiYongWeighted1_v5.tif",
  ## old ## "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v8.tif",
  ## old ## "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_YongCA_noQ3_v9.tif",
  ## old ## "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v8_crp1.5.tif",
  ## old ## "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v9_crp1.5.tif",
    
  # "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK.tif",
  # "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK_noQ3.tif",
  # "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
  # "~/big_noDB/models/YongCA_sigma.tif",
  # "~/big_noDB/models/YongCA_noQ3_sigma.tif"#,
  
  # "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
  # "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_YongCA_noQ3_v7.tif",
  # "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif",
  # "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif",

  
  # (for MVN and Kriged models), RESIDUAL SURFACE TIFFS...........
  ## old ## "~/big_noDB/models/MVN_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
  ## old ## "~/big_noDB/models/KRG_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
  ## old ## "~/big_noDB/models/MVN_resid_NZGD00_allNZ_AhdiYongWeighted1.tif"
  ## old ## "~/big_noDB/models/KRG_resid_NZGD00_allNZ_AhdiYongWeighted1.tif",
  ## old ## "~/big_noDB/models/MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif",
  
  ## old ## "~/big_noDB/models/MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
  ## old ## "~/big_noDB/models/MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
  ## old ## "~/big_noDB/models/MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif",
  ## old ## "~/big_noDB/models/MVN_resid_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif",
  ## old ## "~/big_noDB/models/KRG_resid_NZGD00_allNZ_AhdiYongWeighted1_v4.tif",
  ## old ## "~/big_noDB/models/KRG_resid_NZGD00_allNZ_AhdiYongWeighted1_v5.tif",
  ## old ## "~/big_noDB/models/KRG_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v8.tif",
  ## old ## "~/big_noDB/models/KRG_resid_NZGD00_allNZ_YongCA_noQ3_v9.tif"
  ## old ## "~/big_noDB/models/MVN_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v8_crp1.5.tif",
  ## old ## "~/big_noDB/models/MVN_resid_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v9_crp1.5.tif"
  
  # "~/big_noDB/models/KRG_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
  # "~/big_noDB/models/KRG_resid_NZGD00_allNZ_YongCA_noQ3_v7.tif",
  # "~/big_noDB/models/MVN_resid_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif"#,
  # "~/big_noDB/models/MVN_resid_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif",
  
  
  # New weighted combinations of KRG and MVN models:
  "~/big_noDB/models/AhdiYongWeightedKRG_Vs30.tif",
  "~/big_noDB/models/AhdiYongWeightedMVN_nTcrp1.5_Vs30.tif",
  "~/big_noDB/models/AhdiYongWeightedKRG_sigma.tif",
  "~/big_noDB/models/AhdiYongWeightedMVN_nTcrp1.5_sigma.tif"#,
  
  
  
  # # HILLSHADE AND WATER TIFFS....
  # "~/big_noDB/models/hillshadeA_NZGD00.tif"     ,
  # not used # "~/big_noDB/models/hillshadeB_NZGD00.tif"     ,
  # "~/big_noDB/models/isOcean_NZGD00.tif"        ,
  # "~/big_noDB/models/isWater_NZGD00.tif"        ,
  # "~/big_noDB/models/isIce_NZGD00.tif"          ,


  # # ELEVATION TIFFS....
  # "~/big_noDB/topo/DEM_all_NZGD00_100m.tif",
  # "~/big_noDB/topo/DEM_all_NZGD00.tif",
  # "~/big_noDB/models/DEM_all_NZGD00_resampled.tif"
)


source("R/MAP_NZGD00_regions.R")



for(size in c("three","six","poster")) {
# for(size in c("three")) {
# for(size in c("poster")) {
# for(size in c("six")) {
  if(size=="poster") {
    # this is the size used for all MAP_ scripts at the moment.
    # It yields a 1:1 pixel representation for output PNG of size 3300x4200.
    nrow <- 3632
    ncol <- 2698
  } else {if(size=="three") {
    # These dims are for small maps for publications
    nrow <- 900
    ncol <- 900
  } else {if(size=="six") {
    nrow <- 1800
    ncol <- 1800
  }}}
  
  
  
  
  
  
  
  for(region in names(extList)) {
  # for(region in c("CH","NZ")) { # testing
  # for(region in c("NZ")) { # testing
  # for(region in c("NAP", "WHA", "BLN", "WPT", "GM")) {
  # for(region in c("NI", "SI", "AKL", "WEL", "NEL","NELURB")) {
  # for(region in c("NAP")) {
    for(rast in tifList) {
  
      # if(!identical(rast,"~/big_noDB/models/AhdiGeoCats.tif")) {} # temporary for testing
      # else {
        extents <- extList[[region]]
        resamp <- raster(extents, nrow=nrow, ncol=ncol)
    
        Rast <- raster(rast)
        
        if(is.factor(Rast)) {
          resampRast <- resample( Rast, resamp, method="ngb")  # must use nearest-neighbor interpolation for categorical data.  
          
          # the process of resampling removes the RAT (raster attribute table, i.e. output of levels()) from categorical rasters! must re-add it manually:
          levels(resampRast) <- levels(Rast) # data.frame(ID=1:17, j=as.character(1:17))
          
        } else {
          resampRast <- resample( Rast, resamp, method="bilinear")  # can use bilinear for continuous data.
        }
        
        # save newly resampled raster
        p <- dirname(rast)
        b <- basename(rast)
        bb <- sprintf("RESAMP_%04dx%04d_%s_%s",ncol,nrow,region,b)
        outName <- file.path(p,bb)
        writeRaster(resampRast,outName,format="GTiff", overwrite=T)
      # }
    }
  }
}
