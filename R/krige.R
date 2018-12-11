################################################
# Make map of kriging error

# to download all tiles from Hypocentre and save time:
#
# cd ~/VsMap/img/
# scp kmf76@hypocentre:/home/kmf76/VsMap/img/krigeHyb_WGS84_allNZ_x0\?y0\?.tif ./
# scp kmf76@hypocentre:/home/kmf76/VsMap/img/krigeHyb_WGS84_allNZ_x0\?y0\?.xyz ./
# scp kmf76@hypocentre:/home/kmf76/VsMap/img/residHyb_WGS84_allNZ_x0\?y0\?.tif ./
# scp kmf76@hypocentre:/home/kmf76/VsMap/img/residHyb_WGS84_allNZ_x0\?y0\?.xyz ./
# scp kmf76@hypocentre:/home/kmf76/VsMap/img/stDevHyb_WGS84_allNZ_x0\?y0\?.tif ./
# scp kmf76@hypocentre:/home/kmf76/VsMap/img/stDevHyb_WGS84_allNZ_x0\?y0\?.xyz ./
# cd ../Rdata/
# scp kmf76@hypocentre:/home/kmf76/VsMap/Rdata/KRIGE_WGS84_allNZ_x0\?y0\?.Rdata ./
# scp kmf76@hypocentre:/home/kmf76/VsMap/Rdata/KRIGE_NZGD00_allNZ.Rdata ./

rm(list=ls())
setwd('~/VsMap/')




# SET THE MODEL TO KRIGE:
# MODEL    <- "AhdiYongWeighted1"
# variogramVersion <- "v5"
# or....
MODEL    <- "AhdiAK_noQ3_hyb09c"
variogramVersion <- "v6"
# # or....
# MODEL    <- "YongCA_noQ3"
# variogramVersion <- "v7"

# MODEL    <- "AhdiAK_noQ3_hyb09c"
# variogramVersion <- "v8"
# or....
# MODEL    <- "YongCA_noQ3"
# variogramVersion <- "v9"



# kriging parameters needed to overcome singular covariance matrix issue:
useMaxDist <- F
useNmax    <- F
# notes on the above:
#  maxdist = 105km corresponds to 99% total variance.
#  maxdist = 70km corresponds to 95%.
#  maxdist = 55km corresponds to 90%.
# Look at results in out/krigTweaks to see how these parameters affect results.
# Looks like maxDist = 55km and/or Nmax=32 result in stable, indistinguishable 
# stdDev outputs for Christchurch. Vs30 also indistinguishable. Kriged residual looks 
# slightly smoother for maxDist=55 than for nmax=32. Based on the above, I'll use maxdist=70km
# and no nmax for "production" kriging.

krgMaxDist <- 70000  # meters (corresponds to effective range for 99% total variance)
krgNmax    <- 32      # max number of nearby points to use




library(sp)
library(raster)
library(gstat)
library(parallel)
source("R/functions.R")
source("R/krige_fns.R")
source("R/models.R")
load("Rdata/vspr.Rdata")


# see fitVariogram.R for the script used to generate and save this one:
# (or, LATEST version, fitVariogram_geologyAndTerrain.R)

load(sprintf("Rdata/variogram_%s_%s.Rdata",MODEL,variogramVersion)) # variable name is "variogram"  



# resampling resolution - in meters - for testing only
resampleFlag <- F
newRes <- 800  # new resolution if resampling to be done


# subsetting Vs30 datastructure for kriging because of singular covariance matrix problem!
# this issue is discussed here:
#    http://gis.stackexchange.com/questions/200722/gstat-krige-error-covariance-matrix-singular-at-location-917300-3-6109e06-0
# it's possible that I have issues related to duplicate observations too---for example, this is mentioned here:
#    http://gis.stackexchange.com/questions/222192/r-gstat-krige-covariance-matrix-singular-at-location-5-88-47-4-0-skipping/222237
# revisit this issue a later iteration
vsprSubset <- subsetVsPoints(vspr)$noQ3

zt <- 0.1 # zero threshold
if(nrow(zerodist(vsprSubset, zero = zt)) > 0) {
  warning("Vs30 dataframe used for Kriging contains duplicate points - this will result in a singular covariance matrix. Automatically removing duplicate points now. (Manual review might be good idea.).")
  vsprSubset <- vsprSubset[-zerodist(vsprSubset, zero=zt)[,1],]
}

# remove points where MODEL predictions don't exist
naRows <- which(is.na(vsprSubset[[paste0("res_",MODEL)]]))
if(length(naRows)>0) {
  warning("Some entries in vspr dataframe don't contain model predictions for this model. (Probably because they're near the coast, where raster-based predictors such as slope and convexity require a neighborhood of cells for their definition).")
  vsprSubset <- vsprSubset[-naRows,]
}



# vectors for iteration
yVec <- rep(0:15,11)
xVec <- c()
for (i in 0:10) { xVec <- c(xVec, rep(i, 16))}


# # for testing before parLapply...
# index <- 85  # note: 85 = x05y04 - contains chch
# doTheKrigingThing(index,xVec,yVec,vsprSubset,variogram,
#                   krgMaxDist, krgNmax, useMaxDist, useNmax, MODEL, resampleFlag, variogramVersion)
# # apply(X=as.array(c(69,70,85,86)), MARGIN=1,FUN=doTheKrigingThing,
# #       xVec,yVec,vsprSubset,variogram,
# #       krgMaxDist, krgNmax, useMaxDist, useNmax, MODEL, resampleFlag)


numCores <- detectCores()-2 # 32 cores  total - aim for 30
clustah <- makeCluster(numCores)
parLapply(clustah,
          X = 1:176,
          # X = c(69,70,85,86),
          fun = doTheKrigingThing,
          xVec = xVec, yVec = yVec, vsprSubset=vsprSubset,
          variogram=variogram, krgMaxDist, krgNmax,
          useMaxDist, useNmax, MODEL, resampleFlag,
          variogramVersion)
stopCluster(clustah)



# # load(file = "Rdata/hyb_NZGD00_allNZ_x05y04_AhdiAK_KaiAll_hyb09c.Rdata") # for comparison
# load(file = paste0("Rdata/KRIGE_NZGD00_allNZ_x05y04_",MODEL,".Rdata"))
# # plot(hybRaster)
# 
# png(sprintf("out/krigTweaks/krgVs30_%1dnmax%02d_%1dmaxdist%03dkm.png",useNmax,krgNmax,useMaxDist,krgMaxDist/1000))
# plot(KrigVs30)
# dev.off()
# 
# png(sprintf("out/krigTweaks/krgstDv_%1dnmax%02d_%1dmaxdist%03dkm.png",useNmax,krgNmax,useMaxDist,krgMaxDist/1000))
# plot(krigedOutput$stdDev)
# dev.off()
# 
# png(sprintf("out/krigTweaks/krgRes_%1dnmax%02d_%1dmaxdist%03dkm.png",useNmax,krgNmax,useMaxDist,krgMaxDist/1000))
# plot(krigedOutput$lnObsPred)
# dev.off()
# 
# 
# 
# 
# # using rasterVis for better comparisons
# # https://oscarperpinan.github.io/rastervis/FAQ.html
# 
# # p <- brick(c(hybRaster, KrigVs30))
# # library(rasterVis)
# # png("out/hybAndKrg.png",width = 2800, height = 1500) # need to give explicit width & height or else resulting image will not be full-res.
# # rasterVis::levelplot(p, maxpixels = 1e9, 
# #                      pretty=T, cuts=100)#, # make scale line up nicely and make color scale continuous
# #                      # xlab=NULL, ylab=NULL, scales=list(draw=FALSE),  # suppress axis labels and ticks
# #                      # names.attr=c("hyb", "krg"))
# # dev.off()
# 
# 






############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
## stitch together all kriged NZGD00 maps into one structure
## NOTE: this is not used anymore. Instead, use tileTiffs.R! It's easier and better to put the raster in a large TIFF file than a large Rdata file.
############################################################################################################################################################
############################################################################################################################################################


# for(x in 0:10) {
#   for (y in 0:15) {
#     load(sprintf("~/VsMap/Rdata/KRIGE_NZGD00_allNZ_x%02dy%02d_%s.Rdata", x, y, MODEL))
#     assign(x = sprintf("krigeX%02dY%02d", x, y),value = KrigVs30)
#     assign(x = sprintf("residX%02dY%02d", x, y),value = krigedOutput$lnObsPred)
#     assign(x = sprintf("stDevX%02dY%02d", x, y),value = krigedOutput$stdDev)
#   }
# }
# 
# # str <- ""
# # for (x in 0:10) {
# #   for (y in 0:15) {
# #     str <- paste0(str, sprintf("krigeX%02dY%02d, ", x, y))
# #   }
# # }
# 
# krige_allNZ_NZGD00 <- mosaic(
#   krigeX00Y00, krigeX00Y01, krigeX00Y02, krigeX00Y03, krigeX00Y04, krigeX00Y05, krigeX00Y06, krigeX00Y07,
#   krigeX00Y08, krigeX00Y09, krigeX00Y10, krigeX00Y11, krigeX00Y12, krigeX00Y13, krigeX00Y14, krigeX00Y15,
#   krigeX01Y00, krigeX01Y01, krigeX01Y02, krigeX01Y03, krigeX01Y04, krigeX01Y05, krigeX01Y06, krigeX01Y07,
#   krigeX01Y08, krigeX01Y09, krigeX01Y10, krigeX01Y11, krigeX01Y12, krigeX01Y13, krigeX01Y14, krigeX01Y15,
#   krigeX02Y00, krigeX02Y01, krigeX02Y02, krigeX02Y03, krigeX02Y04, krigeX02Y05, krigeX02Y06, krigeX02Y07,
#   krigeX02Y08, krigeX02Y09, krigeX02Y10, krigeX02Y11, krigeX02Y12, krigeX02Y13, krigeX02Y14, krigeX02Y15,
#   krigeX03Y00, krigeX03Y01, krigeX03Y02, krigeX03Y03, krigeX03Y04, krigeX03Y05, krigeX03Y06, krigeX03Y07,
#   krigeX03Y08, krigeX03Y09, krigeX03Y10, krigeX03Y11, krigeX03Y12, krigeX03Y13, krigeX03Y14, krigeX03Y15,
#   krigeX04Y00, krigeX04Y01, krigeX04Y02, krigeX04Y03, krigeX04Y04, krigeX04Y05, krigeX04Y06, krigeX04Y07,
#   krigeX04Y08, krigeX04Y09, krigeX04Y10, krigeX04Y11, krigeX04Y12, krigeX04Y13, krigeX04Y14, krigeX04Y15,
#   krigeX05Y00, krigeX05Y01, krigeX05Y02, krigeX05Y03, krigeX05Y04, krigeX05Y05, krigeX05Y06, krigeX05Y07,
#   krigeX05Y08, krigeX05Y09, krigeX05Y10, krigeX05Y11, krigeX05Y12, krigeX05Y13, krigeX05Y14, krigeX05Y15,
#   krigeX06Y00, krigeX06Y01, krigeX06Y02, krigeX06Y03, krigeX06Y04, krigeX06Y05, krigeX06Y06, krigeX06Y07,
#   krigeX06Y08, krigeX06Y09, krigeX06Y10, krigeX06Y11, krigeX06Y12, krigeX06Y13, krigeX06Y14, krigeX06Y15,
#   krigeX07Y00, krigeX07Y01, krigeX07Y02, krigeX07Y03, krigeX07Y04, krigeX07Y05, krigeX07Y06, krigeX07Y07,
#   krigeX07Y08, krigeX07Y09, krigeX07Y10, krigeX07Y11, krigeX07Y12, krigeX07Y13, krigeX07Y14, krigeX07Y15,
#   krigeX08Y00, krigeX08Y01, krigeX08Y02, krigeX08Y03, krigeX08Y04, krigeX08Y05, krigeX08Y06, krigeX08Y07,
#   krigeX08Y08, krigeX08Y09, krigeX08Y10, krigeX08Y11, krigeX08Y12, krigeX08Y13, krigeX08Y14, krigeX08Y15,
#   krigeX09Y00, krigeX09Y01, krigeX09Y02, krigeX09Y03, krigeX09Y04, krigeX09Y05, krigeX09Y06, krigeX09Y07,
#   krigeX09Y08, krigeX09Y09, krigeX09Y10, krigeX09Y11, krigeX09Y12, krigeX09Y13, krigeX09Y14, krigeX09Y15,
#   krigeX10Y00, krigeX10Y01, krigeX10Y02, krigeX10Y03, krigeX10Y04, krigeX10Y05, krigeX10Y06, krigeX10Y07,
#   krigeX10Y08, krigeX10Y09, krigeX10Y10, krigeX10Y11, krigeX10Y12, krigeX10Y13, krigeX10Y14, krigeX10Y15,
#   fun=min)
# krige_allNZ_NZGD00.sgdf <- as(krige_allNZ_NZGD00, "SpatialGridDataFrame")
# 
# 
# resid_allNZ_NZGD00 <- mosaic(
#   residX00Y00, residX00Y01, residX00Y02, residX00Y03, residX00Y04, residX00Y05, residX00Y06, residX00Y07,
#   residX00Y08, residX00Y09, residX00Y10, residX00Y11, residX00Y12, residX00Y13, residX00Y14, residX00Y15,
#   residX01Y00, residX01Y01, residX01Y02, residX01Y03, residX01Y04, residX01Y05, residX01Y06, residX01Y07,
#   residX01Y08, residX01Y09, residX01Y10, residX01Y11, residX01Y12, residX01Y13, residX01Y14, residX01Y15,
#   residX02Y00, residX02Y01, residX02Y02, residX02Y03, residX02Y04, residX02Y05, residX02Y06, residX02Y07,
#   residX02Y08, residX02Y09, residX02Y10, residX02Y11, residX02Y12, residX02Y13, residX02Y14, residX02Y15,
#   residX03Y00, residX03Y01, residX03Y02, residX03Y03, residX03Y04, residX03Y05, residX03Y06, residX03Y07,
#   residX03Y08, residX03Y09, residX03Y10, residX03Y11, residX03Y12, residX03Y13, residX03Y14, residX03Y15,
#   residX04Y00, residX04Y01, residX04Y02, residX04Y03, residX04Y04, residX04Y05, residX04Y06, residX04Y07,
#   residX04Y08, residX04Y09, residX04Y10, residX04Y11, residX04Y12, residX04Y13, residX04Y14, residX04Y15,
#   residX05Y00, residX05Y01, residX05Y02, residX05Y03, residX05Y04, residX05Y05, residX05Y06, residX05Y07,
#   residX05Y08, residX05Y09, residX05Y10, residX05Y11, residX05Y12, residX05Y13, residX05Y14, residX05Y15,
#   residX06Y00, residX06Y01, residX06Y02, residX06Y03, residX06Y04, residX06Y05, residX06Y06, residX06Y07,
#   residX06Y08, residX06Y09, residX06Y10, residX06Y11, residX06Y12, residX06Y13, residX06Y14, residX06Y15,
#   residX07Y00, residX07Y01, residX07Y02, residX07Y03, residX07Y04, residX07Y05, residX07Y06, residX07Y07,
#   residX07Y08, residX07Y09, residX07Y10, residX07Y11, residX07Y12, residX07Y13, residX07Y14, residX07Y15,
#   residX08Y00, residX08Y01, residX08Y02, residX08Y03, residX08Y04, residX08Y05, residX08Y06, residX08Y07,
#   residX08Y08, residX08Y09, residX08Y10, residX08Y11, residX08Y12, residX08Y13, residX08Y14, residX08Y15,
#   residX09Y00, residX09Y01, residX09Y02, residX09Y03, residX09Y04, residX09Y05, residX09Y06, residX09Y07,
#   residX09Y08, residX09Y09, residX09Y10, residX09Y11, residX09Y12, residX09Y13, residX09Y14, residX09Y15,
#   residX10Y00, residX10Y01, residX10Y02, residX10Y03, residX10Y04, residX10Y05, residX10Y06, residX10Y07,
#   residX10Y08, residX10Y09, residX10Y10, residX10Y11, residX10Y12, residX10Y13, residX10Y14, residX10Y15,
#   fun=min)
# resid_allNZ_NZGD00.sgdf <- as(resid_allNZ_NZGD00, "SpatialGridDataFrame")
# 
# 
# stDev_allNZ_NZGD00 <- mosaic(
#   stDevX00Y00, stDevX00Y01, stDevX00Y02, stDevX00Y03, stDevX00Y04, stDevX00Y05, stDevX00Y06, stDevX00Y07,
#   stDevX00Y08, stDevX00Y09, stDevX00Y10, stDevX00Y11, stDevX00Y12, stDevX00Y13, stDevX00Y14, stDevX00Y15,
#   stDevX01Y00, stDevX01Y01, stDevX01Y02, stDevX01Y03, stDevX01Y04, stDevX01Y05, stDevX01Y06, stDevX01Y07,
#   stDevX01Y08, stDevX01Y09, stDevX01Y10, stDevX01Y11, stDevX01Y12, stDevX01Y13, stDevX01Y14, stDevX01Y15,
#   stDevX02Y00, stDevX02Y01, stDevX02Y02, stDevX02Y03, stDevX02Y04, stDevX02Y05, stDevX02Y06, stDevX02Y07,
#   stDevX02Y08, stDevX02Y09, stDevX02Y10, stDevX02Y11, stDevX02Y12, stDevX02Y13, stDevX02Y14, stDevX02Y15,
#   stDevX03Y00, stDevX03Y01, stDevX03Y02, stDevX03Y03, stDevX03Y04, stDevX03Y05, stDevX03Y06, stDevX03Y07,
#   stDevX03Y08, stDevX03Y09, stDevX03Y10, stDevX03Y11, stDevX03Y12, stDevX03Y13, stDevX03Y14, stDevX03Y15,
#   stDevX04Y00, stDevX04Y01, stDevX04Y02, stDevX04Y03, stDevX04Y04, stDevX04Y05, stDevX04Y06, stDevX04Y07,
#   stDevX04Y08, stDevX04Y09, stDevX04Y10, stDevX04Y11, stDevX04Y12, stDevX04Y13, stDevX04Y14, stDevX04Y15,
#   stDevX05Y00, stDevX05Y01, stDevX05Y02, stDevX05Y03, stDevX05Y04, stDevX05Y05, stDevX05Y06, stDevX05Y07,
#   stDevX05Y08, stDevX05Y09, stDevX05Y10, stDevX05Y11, stDevX05Y12, stDevX05Y13, stDevX05Y14, stDevX05Y15,
#   stDevX06Y00, stDevX06Y01, stDevX06Y02, stDevX06Y03, stDevX06Y04, stDevX06Y05, stDevX06Y06, stDevX06Y07,
#   stDevX06Y08, stDevX06Y09, stDevX06Y10, stDevX06Y11, stDevX06Y12, stDevX06Y13, stDevX06Y14, stDevX06Y15,
#   stDevX07Y00, stDevX07Y01, stDevX07Y02, stDevX07Y03, stDevX07Y04, stDevX07Y05, stDevX07Y06, stDevX07Y07,
#   stDevX07Y08, stDevX07Y09, stDevX07Y10, stDevX07Y11, stDevX07Y12, stDevX07Y13, stDevX07Y14, stDevX07Y15,
#   stDevX08Y00, stDevX08Y01, stDevX08Y02, stDevX08Y03, stDevX08Y04, stDevX08Y05, stDevX08Y06, stDevX08Y07,
#   stDevX08Y08, stDevX08Y09, stDevX08Y10, stDevX08Y11, stDevX08Y12, stDevX08Y13, stDevX08Y14, stDevX08Y15,
#   stDevX09Y00, stDevX09Y01, stDevX09Y02, stDevX09Y03, stDevX09Y04, stDevX09Y05, stDevX09Y06, stDevX09Y07,
#   stDevX09Y08, stDevX09Y09, stDevX09Y10, stDevX09Y11, stDevX09Y12, stDevX09Y13, stDevX09Y14, stDevX09Y15,
#   stDevX10Y00, stDevX10Y01, stDevX10Y02, stDevX10Y03, stDevX10Y04, stDevX10Y05, stDevX10Y06, stDevX10Y07,
#   stDevX10Y08, stDevX10Y09, stDevX10Y10, stDevX10Y11, stDevX10Y12, stDevX10Y13, stDevX10Y14, stDevX10Y15,
#   fun=min)
# stDev_allNZ_NZGD00.sgdf <- as(stDev_allNZ_NZGD00, "SpatialGridDataFrame")
# 
# 
# 
# 
# save(krige_allNZ_NZGD00, krige_allNZ_NZGD00.sgdf,
#      resid_allNZ_NZGD00, resid_allNZ_NZGD00.sgdf,
#      stDev_allNZ_NZGD00, stDev_allNZ_NZGD00.sgdf,
#      file=sprintf("~/VsMap/Rdata/KRIGE_NZGD00_allNZ_%s.Rdata", MODEL))
# #load(file=sprintf("~/VsMap/Rdata/KRIGE_NZGD00_allNZ_%s.Rdata", MODEL))
# 
# 
# 
