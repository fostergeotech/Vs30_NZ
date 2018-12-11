# for large files of the type produced by models for all NZ.
# raster library seems incapable of dealing with these when they are R objects.

# Recent examples:
# > plot(geo_allNZ_NZGD00)
      # Error in file(fn, "rb") : cannot open the connection
      # In addition: Warning message:
      #   In file(fn, "rb") :
      #   cannot open file '/tmp/Rtmp7ZTe34/raster/r_tmp_2017-08-30_230100_3491_20779.gri': No such file or directory
# > writeRaster(isOcean, filename = "~/big_noDB/geo/NZGD00_isOcean.tif", format = "GTiff")
      # Error in file(fn, "rb") : cannot open the connection
      # In addition: Warning message:
      #   In file(fn, "rb") :
      #   cannot open file '/tmp/Rtmp4fS2As/raster/r_tmp_2017-08-31_183303_31818_24385.gri': No such file or directory
#
# Hoping that by handling these as GTiffs from the start I can avoid this issue.
# Update: yes, this fixes the problems.

rm(list=ls())
setwd("~/VsMap")

library(parallel)
library(raster)

writeOneTile <- function(i,xrange,yrange,inpPath,inpBaseName,inpSuffix,varName,tmpDir) {
  library(raster)
  xVec <- rep(xrange, each=length(yrange))
  yVec <- rep(yrange, length(xrange))
  xytile <- sprintf("x%02dy%02d",xVec[i],yVec[i])
  load(paste0(inpPath, inpBaseName, xytile, inpSuffix))
  tile <- get(varName)
  tileFname <- paste0(tmpDir, "tile", xytile, ".tif")
  writeRaster(tile, filename = tileFname, format = "GTiff", overwrite=T)
  return(tileFname)
}


tileTiffs <- function(xrange,yrange,inpPath,inpBaseName,inpSuffix,varName,tmpDir) {
  fnames <- character(length = 0)
  iVec <- 1:(length(xrange)*length(yrange))
  PARALLEL = T
  if(!PARALLEL) {
    fnames <- lapply(X = iVec, FUN = writeOneTile, xrange,yrange,inpPath,inpBaseName,inpSuffix,varName,tmpDir)
  } else {
    nCores <- parallel::detectCores()
    clustah <- makeCluster(nCores-2)
    clusterExport(cl=clustah, varlist = list("writeRaster"))
    fnames <- parLapply(cl=clustah, X = iVec, FUN = writeOneTile, xrange,yrange,inpPath,inpBaseName,inpSuffix,varName,tmpDir)
    stopCluster(clustah)
  }
  sargs <- c("-o", outpFileName, "-n", "-1.69999999999999994e+308", "-a_nodata", "-1.69999999999999994e+308", fnames)
  unlink(outpFileName) # must do this beforehand - otherwise gdal_merge retains the extents of previous file
  system2(command = "gdal_merge.py",
          args = sargs)
  unlink(fnames)
}


# whichOnes <- "AhdiAK"
# whichOnes <- "INDEX"
# whichOnes <- "AhdiAK_noQ3"
# whichOnes <- "AhdiAK_noQ3_hyb09c"
# whichOnes <- "slopeForHybrid"
# whichOnes <- "oldModel17.3"
# whichOnes <- "AhdiAK_noQ3_hyb09c_KRG_Vs30"
# whichOnes <- "AhdiAK_noQ3_hyb09c_KRG_StDv"
# whichOnes <- "AhdiAK_noQ3_hyb09c_KRG_Resid"
# whichOnes <- "AhdiAK_noQ3_hyb09c_MVN_Vs30"
# whichOnes <- "AhdiAK_noQ3_hyb09c_MVN_StDv"
# whichOnes <- "AhdiAK_noQ3_hyb09c_MVN_Resid"
# whichOnes <- "AhdiYongWeighted1_KRG_Vs30"
# whichOnes <- "AhdiYongWeighted1_KRG_StDv"
# whichOnes <- "AhdiYongWeighted1_KRG_Resid"
# whichOnes <- "AhdiYongWeighted1_MVN_Vs30"
# whichOnes <- "AhdiYongWeighted1_MVN_StDv"
# whichOnes <- "AhdiYongWeighted1_MVN_Resid"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyT_dist0_v2_Vs30"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyT_dist0_v2_StDv"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyT_dist0_v2_Resid"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_Vs30"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_StDv"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_Resid"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_Vs30"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_StDv"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_Resid"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_Vs30"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_StDv"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_Resid"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_Vs30"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_StDv"
# whichOnes <- "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_Resid"
# whichOnes <- "AhdiYongWeighted1_KRG_v4_Vs30"
# whichOnes <- "AhdiYongWeighted1_KRG_v4_StDv"
# whichOnes <- "AhdiYongWeighted1_KRG_v4_Resid"
# whichOnes <- "AhdiYongWeighted1_KRG_v5_Vs30"
# whichOnes <- "AhdiYongWeighted1_KRG_v5_StDv"
# whichOnes <- "AhdiYongWeighted1_KRG_v5_Resid"



whichOnes <- c(
               # "AhdiYongWeighted1_KRG_Vs30"   ,
               # "AhdiYongWeighted1_KRG_StDv"   ,
               # "AhdiYongWeighted1_KRG_Resid"  ,
               # "AhdiYongWeighted1_MVN_Vs30"   ,
               # "AhdiYongWeighted1_MVN_StDv"   ,
               # "AhdiYongWeighted1_MVN_Resid"  ,
               # "AhdiYongWeighted1_MVN_noisyT_dist0_v2_Vs30"  ,
               # "AhdiYongWeighted1_MVN_noisyT_dist0_v2_StDv"  ,
               # "AhdiYongWeighted1_MVN_noisyT_dist0_v2_Resid" ,
               # "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_Vs30"  ,
               # "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_StDv"  ,
               # "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_Resid" ,
               # "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_Vs30"  ,
               # "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_StDv"  ,
               # "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_Resid" ,
               
               # "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_Vs30"  ,
               # "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_StDv"  ,
               # "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_Resid"
               
               # "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_Vs30"  ,
               # "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_StDv"  ,
               # "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_Resid" ,
               
               # "AhdiYongWeighted1_KRG_v4_Vs30"  ,
               # "AhdiYongWeighted1_KRG_v4_StDv"  ,
               # "AhdiYongWeighted1_KRG_v4_Resid" ,
               
               # "AhdiYongWeighted1_KRG_v5_Vs30"  ,
               # "AhdiYongWeighted1_KRG_v5_StDv"  ,
               # "AhdiYongWeighted1_KRG_v5_Resid"
               
  
  "AhdiAK_noQ3_hyb09c_KRG_v6_Vs30"        ,
  "AhdiAK_noQ3_hyb09c_KRG_v6_StDv"        ,
  "AhdiAK_noQ3_hyb09c_KRG_v6_Resid"       ,
  
  "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v6_crp1.5_Vs30"        ,
  "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v6_crp1.5_StDv"        ,
  "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v6_crp1.5_Resid"       #,
  
  # "YongCA_noQ3_KRG_v7_Vs30"               ,
  # "YongCA_noQ3_KRG_v7_StDv"               ,
  # "YongCA_noQ3_KRG_v7_Resid"             # ,
  # 
  # "YongCA_noQ3_MVN_noisyT_dist0_v7_crp1.5_Vs30"        ,
  # "YongCA_noQ3_MVN_noisyT_dist0_v7_crp1.5_StDv"        ,
  # "YongCA_noQ3_MVN_noisyT_dist0_v7_crp1.5_Resid"
  # 
  # "AhdiAK_noQ3_hyb09c_KRG_v8_Vs30"        ,
  # "AhdiAK_noQ3_hyb09c_KRG_v8_StDv"        ,
  # "AhdiAK_noQ3_hyb09c_KRG_v8_Resid"       ,
  # 
  # "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v8_crp1.5_Vs30"        ,
  # "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v8_crp1.5_StDv"        ,
  # "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v8_crp1.5_Resid"       #,
  
  # "YongCA_noQ3_KRG_v9_Vs30"               ,
  # "YongCA_noQ3_KRG_v9_StDv"               ,
  # "YongCA_noQ3_KRG_v9_Resid"            # ,
  # 
  # "YongCA_noQ3_MVN_noisyT_dist0_v9_crp1.5_Vs30"        ,
  # "YongCA_noQ3_MVN_noisyT_dist0_v9_crp1.5_StDv"        ,
  # "YongCA_noQ3_MVN_noisyT_dist0_v9_crp1.5_Resid"
)



for(i in whichOnes) {
  switch(i,
         AhdiAK = {
           inpPath      <- "~/VsMap/Rdata/"
           inpBaseName  <- "geo_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK.Rdata"
           xrange       <- 0:10
           yrange       <- 0:15
           outpFileName <- "~/big_noDB/models/geo_NZGD00_allNZ_AhdiAK.tif"
           varName      <- "geoRaster"
           tmpDir       <- "~/VsMap/tmp/"},
         INDEX = {
           inpPath      <- "~/VsMap/Rdata/"
           inpBaseName  <- "INDEX_NZGD00_allNZ_"
           inpSuffix    <- ".Rdata"
           xrange       <- 0:10
           yrange       <- 0:15
           outpFileName <- "~/big_noDB/models/INDEX_NZGD00_allNZ.tif"
           varName      <- "indexRaster"
           tmpDir       <- "~/VsMap/tmp/"},
         AhdiAK_noQ3 = {
           inpPath      <- "~/VsMap/Rdata/"
           inpBaseName  <- "geo_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3.Rdata"
           xrange       <- 0:10
           yrange       <- 0:15
           outpFileName <- "~/big_noDB/models/geo_NZGD00_allNZ_AhdiAK_noQ3.tif"
           varName      <- "geoRaster"
           tmpDir       <- "~/VsMap/tmp/"},
         AhdiAK_noQ3_hyb09c = {
           inpPath      <- "~/VsMap/Rdata/"
           inpBaseName  <- "hyb_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c.Rdata"
           xrange       <- 0:10
           yrange       <- 0:15
           outpFileName <- "~/big_noDB/models/hyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif"
           varName      <- "hybRaster"
           tmpDir       <- "~/VsMap/tmp/"},
         slopeForHybrid = {
           inpPath      <- "~/VsMap/Rdata/"
           inpBaseName  <- "slp_NZGD00_allNZ_"
           inpSuffix    <- ".Rdata"
           xrange       <- 0:10
           yrange       <- 0:15
           outpFileName <- "~/big_noDB/topo/slp_NZMG/slp_NZGD00_allNZ_fromHybRdataFiles.tif"
           varName      <- "slpRasterNZGD00"
           tmpDir       <- "~/VsMap/tmp/"},
         oldModel17.3 = {
           # # This config is used for producing the kriged model corresponding to the git
           # # commit 667ec330640dfe89bfd92c92104a7d44b9741664
           # # Author: kevinUCeqEng <kevin.foster@pg.canterbury.ac.nz>
           # #   Date:   Thu May 4 12:38:33 2017 +1200
           # #
           # # "final 17.3 version before update geology classification"
           # #
           # # (It is the version of the map that was used by GMsim team until recently.)
           # # This raster will only be used to compare old to new.
           # #
           # # To produce the KRIGE_NZGD00_allNZ_x**y**.Rdata files corresponding to this old commit,
           # # need to check out the old commit and re-run these files:
           # #
           # # processQmap.R
           # # vspr.R
           # # analysisKF.R
           # # rasterizeQmap.R
           # # maps2.R
           # # krige.R
           # #
           # # In maps2.R, I had to make a small edit to the function
           # # named getHybFromGeoAndSlope. In this function, I added Which(...cells=T) to each of the three
           # # hybrid selection statements in that function. This problem is fixed later on an Aug 27 2017 commit,
           # # but that was after the model defs changed.
           # #
           inpPath      <- "~/Vs30-mapping_OLD/Rdata/"
           inpBaseName  <- "KRIGE_NZGD00_allNZ_"
           inpSuffix    <- ".Rdata"
           xrange       <- 0:9
           yrange       <- 0:9
           outpFileName <- "~/big_noDB_NEW/models/KRIGE_NZGD00_allNZ_201703.tif"
           varName      <- "KrigHybVs30"
           tmpDir       <- "~/Vs30-mapping_NEW/tmp/"
           # # .... after all of that, I'm not at all confident that the model produced is actually the "right" 201703 model.
         },


         #######################################################################################################
         ####                                                                                               ####
         ####           These (Rdata-based) versions are superseded. New way is direct to tiff.             ####
         ####                                                                                               ####
         #######################################################################################################
         # KRIGE = {
         #   inpPath      <- "~/VsMap/Rdata/"
         #   inpBaseName  <- "KRIGE_NZGD00_allNZ_"
         #   inpSuffix    <- "_AhdiAK_noQ3_hyb09c.Rdata"
         #   xrange       <- 0:10
         #   yrange       <- 0:15
         #   outpFileName <- "~/big_noDB/models/krgHyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif"
         #   varName      <- "KrigVs30"
         #   tmpDir       <- "~/VsMap/tmp/"},
         # MVNRM = {
         #   inpPath      <- "~/VsMap/Rdata/"
         #   inpBaseName  <- "MVNRM_NZGD00_allNZ_"
         #   inpSuffix    <- "_AhdiAK_KaiAll_hyb09c.Rdata"
         #   xrange       <- 0:10
         #   yrange       <- 0:15
         #   outpFileName <- "~/big_noDB/models/MVNRM_NZGD00_allNZ_AhdiAK_KaiAll_hyb09c.tif"
         #   varName      <- "mvnVs30"
         #   tmpDir       <- "~/VsMap/tmp/"},
         #######################################################################################################
         ####                                                                                               ####
         ####           New way is below.................                                                   ####
         ####                                                                                               ####
         #######################################################################################################
         AhdiAK_noQ3_hyb09c_KRG_Vs30 = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "krige_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_KRG_StDv = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "stDev_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_KRG_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "resid_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_resid_NZGD00_allNZ",inpSuffix)
         },
         
         AhdiAK_noQ3_hyb09c_MVN_Vs30 = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_MVN_StDv = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_MVN_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },
         
         AhdiYongWeighted1_KRG_Vs30  = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "krige_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_KRG_StDv  = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "stDev_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_KRG_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "resid_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_resid_NZGD00_allNZ",inpSuffix)
         },
         
         AhdiYongWeighted1_MVN_Vs30  = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_StDv  = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyT_dist0_v2_Vs30 = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyT_dist0_v2_StDv = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyT_dist0_v2_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_Vs30 = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_StDv = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_Vs30 = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_StDv = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_Vs30 = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_StDv = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_Vs30 = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_StDv = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },
         
         AhdiYongWeighted1_KRG_v4_Vs30  = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "krige_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_v4.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_KRG_v4_StDv  = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "stDev_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_v4.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_KRG_v4_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "resid_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_v4.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_resid_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_KRG_v5_Vs30  = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "krige_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_v5.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_KRG_v5_StDv  = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "stDev_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_v5.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiYongWeighted1_KRG_v5_Resid = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "resid_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiYongWeighted1_v5.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_resid_NZGD00_allNZ",inpSuffix)
         },
         
         
         
         # New stuff - 2018-09 - performing geostatistics on input models instead of combined, per BB request

         AhdiAK_noQ3_hyb09c_KRG_v6_Vs30      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "krige_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_v6.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v6_crp1.5_Vs30      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_KRG_v7_Vs30      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "krige_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_v7.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_Vs30_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_MVN_noisyT_dist0_v7_crp1.5_Vs30      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         
         
         AhdiAK_noQ3_hyb09c_KRG_v6_StDv      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "stDev_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_v6.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v6_crp1.5_StDv      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_KRG_v7_StDv      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "stDev_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_v7.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_stDv_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_MVN_noisyT_dist0_v7_crp1.5_StDv      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         
         
         AhdiAK_noQ3_hyb09c_KRG_v6_Resid      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "resid_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_v6.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_resid_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v6_crp1.5_Resid      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_KRG_v7_Resid      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "resid_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_v7.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_resid_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_MVN_noisyT_dist0_v7_crp1.5_Resid      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },


         
         
         
         # New stuff - 2018-10 - performing geostatistics on input models instead of combined, per BB request...
         # but also using just noQ3noCanterbury-based variograms. (v8 and v9).
         
         AhdiAK_noQ3_hyb09c_KRG_v8_Vs30      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "krige_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_v8.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_Vs30_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v8_crp1.5_Vs30      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v8_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_KRG_v9_Vs30      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "krige_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_v9.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_Vs30_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_MVN_noisyT_dist0_v9_crp1.5_Vs30      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVkrg_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_hyb09c_noisyT_minDist0.0km_v9_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ",inpSuffix)
         },
         
         
         AhdiAK_noQ3_hyb09c_KRG_v8_StDv      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "stDev_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_v8.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_stDv_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v8_crp1.5_StDv      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v8_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_KRG_v9_StDv      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "stDev_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_v9.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_stDv_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_MVN_noisyT_dist0_v9_crp1.5_StDv      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVsdv_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_hyb09c_noisyT_minDist0.0km_v9_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_stDv_NZGD00_allNZ",inpSuffix)
         },
         
         
         AhdiAK_noQ3_hyb09c_KRG_v8_Resid      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "resid_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_v8.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_resid_NZGD00_allNZ",inpSuffix)
         },
         AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v8_crp1.5_Resid      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v8_crp1.5.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_KRG_v9_Resid      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "resid_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_v9.tif"
           outpFileName <- paste0("~/big_noDB/models/KRG_resid_NZGD00_allNZ",inpSuffix)
         },
         YongCA_noQ3_MVN_noisyT_dist0_v9_crp1.5_Resid      = {
           inpPath      <- "~/VsMap/tmp/"
           inpBaseName  <- "MVres_NZGD00_allNZ_"
           inpSuffix    <- "_YongCA_noQ3_v9.tif"
           outpFileName <- paste0("~/big_noDB/models/MVN_resid_NZGD00_allNZ",inpSuffix)
         }
         
         
         
         
         
         
         
         
                  
  )


  if(i %in% c("AhdiAK",
              "INDEX",
              "AhdiAK_noQ3",
              "AhdiAK_noQ3_hyb09c",
              "slopeForHybrid",
              "oldModel17.3"))
    {
      tileTiffs(xrange,yrange,inpPath,inpBaseName,inpSuffix,varName,tmpDir)
    }
    else
      {
      if(i %in% c(
        "AhdiAK_noQ3_hyb09c_KRG_Vs30"  ,   # Kriging and MVN scripts now dump output tiles as TIFFs instead of Rdata,
        "AhdiAK_noQ3_hyb09c_KRG_StDv"  ,   # in ~/VsMap/tmp . So they can be handled by a simple system2("gdal_merge.py")
        "AhdiAK_noQ3_hyb09c_KRG_Resid" ,   # call, below.
        "AhdiAK_noQ3_hyb09c_MVN_Vs30"  ,
        "AhdiAK_noQ3_hyb09c_MVN_StDv"  ,
        "AhdiAK_noQ3_hyb09c_MVN_Resid" ,
        "AhdiYongWeighted1_KRG_Vs30"   ,
        "AhdiYongWeighted1_KRG_StDv"   ,
        "AhdiYongWeighted1_KRG_Resid"  ,
        "AhdiYongWeighted1_MVN_Vs30"   ,
        "AhdiYongWeighted1_MVN_StDv"   ,
        "AhdiYongWeighted1_MVN_Resid"  ,
        "AhdiYongWeighted1_MVN_noisyT_dist0_v2_Vs30"  ,
        "AhdiYongWeighted1_MVN_noisyT_dist0_v2_StDv"  ,
        "AhdiYongWeighted1_MVN_noisyT_dist0_v2_Resid" ,
        "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_Vs30"  ,
        "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_StDv"  ,
        "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp1.5_Resid" ,
        "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_Vs30"  ,
        "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_StDv"  ,
        "AhdiYongWeighted1_MVN_noisyT_dist0_v3_crp0.0_Resid" ,
        "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_Vs30"  ,
        "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_StDv"  ,
        "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp1.5_Resid" ,
        "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_Vs30"  ,
        "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_StDv"  ,
        "AhdiYongWeighted1_MVN_noisyF_dist0_v3_crp0.0_Resid" ,
        "AhdiYongWeighted1_KRG_v4_Vs30"   ,
        "AhdiYongWeighted1_KRG_v4_StDv"   ,
        "AhdiYongWeighted1_KRG_v4_Resid"  ,
        "AhdiYongWeighted1_KRG_v5_Vs30"   ,
        "AhdiYongWeighted1_KRG_v5_StDv"   ,
        "AhdiYongWeighted1_KRG_v5_Resid"  ,
        
        # new stuff - 2018-09 - doing geostats on geo and terrain models separately, per BB request
        
        "AhdiAK_noQ3_hyb09c_KRG_v6_Vs30"        ,
        "AhdiAK_noQ3_hyb09c_KRG_v6_StDv"        ,
        "AhdiAK_noQ3_hyb09c_KRG_v6_Resid"       ,
        "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v6_crp1.5_Vs30"        ,
        "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v6_crp1.5_StDv"        ,
        "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v6_crp1.5_Resid"       ,
        "YongCA_noQ3_KRG_v7_Vs30"               ,
        "YongCA_noQ3_KRG_v7_StDv"               ,
        "YongCA_noQ3_KRG_v7_Resid"              ,
        "YongCA_noQ3_MVN_noisyT_dist0_v7_crp1.5_Vs30"        ,
        "YongCA_noQ3_MVN_noisyT_dist0_v7_crp1.5_StDv"        ,
        "YongCA_noQ3_MVN_noisyT_dist0_v7_crp1.5_Resid"       ,
        

        # New stuff - 2018-10 - performing geostatistics on input models instead of combined, per BB request...
        # but also using just noQ3noCanterbury-based variograms. (v8 and v9).
        
        
        "AhdiAK_noQ3_hyb09c_KRG_v8_Vs30"        ,
        "AhdiAK_noQ3_hyb09c_KRG_v8_StDv"        ,
        "AhdiAK_noQ3_hyb09c_KRG_v8_Resid"       ,
        "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v8_crp1.5_Vs30"        ,
        "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v8_crp1.5_StDv"        ,
        "AhdiAK_noQ3_hyb09c_MVN_noisyT_dist0_v8_crp1.5_Resid"       ,
        "YongCA_noQ3_KRG_v9_Vs30"               ,
        "YongCA_noQ3_KRG_v9_StDv"               ,
        "YongCA_noQ3_KRG_v9_Resid"              ,
        "YongCA_noQ3_MVN_noisyT_dist0_v9_crp1.5_Vs30"        ,
        "YongCA_noQ3_MVN_noisyT_dist0_v9_crp1.5_StDv"        ,
        "YongCA_noQ3_MVN_noisyT_dist0_v9_crp1.5_Resid"
        
        
        
      ))
        {
          tiffNames <- Sys.glob(paths = paste0(inpPath,inpBaseName,"*",inpSuffix))
          sargs <- c("-o", outpFileName, "-n", "-1.69999999999999994e+308", "-a_nodata", "-1.69999999999999994e+308", tiffNames)
          unlink(outpFileName) # must do this beforehand - otherwise gdal_merge retains the extents of previous file
          # system2("which", args = c("gdal_merge.py"))  # this was used for troubleshooting
          # system2("which", args = c("python"))         # this was used for troubleshooting
          system2(command = "gdal_merge.py", args = sargs)
        }
      }
}




