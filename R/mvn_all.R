# mvn_all.R

# Run all MVN interpolation for NZ.

rm(list=ls())
setwd('~/VsMap/')
source("R/mvn_params.R")  # CALL THIS FIRST

library(sp)
library(raster)
library(gstat)
library(parallel)
library(rgeos)

source("R/functions.R")
source("R/mvn_oneTile.R")


load("Rdata/vspr.Rdata")

# see fitVariogram.R for the script used to generate and save this one:
# (or, most recent version - fitVariogram_geologyAndTerrain3_allTogether.R)
load(sprintf("Rdata/variogram_%s_%s.Rdata",MODEL, vgName)) # variable name is "variogram"

vg <- variogram

# ################ BEGIN DIAGNOSTIC BLOCK #################################
# # The following is just for diagnostics. Overwrites variogram
# # to force zero nugget - for diagnosing MVN issues.
# psill=0.3
# range=20e3
# nugget=0
# kappa = 0.9
# vg <- vgm(psill  = psill,
#     model  = "Mat",
#     range  = range,
#     nugget = nugget,
#     kappa  = kappa)
# variogram <- vg
# #
# #
# ################ END DIAGNOSTIC BLOCK ###################################


# subsetting Vs30 datastructure for kriging because of singular covariance matrix problem!
# this issue is discussed here:
#    http://gis.stackexchange.com/questions/200722/gstat-krige-error-covariance-matrix-singular-at-location-917300-3-6109e06-0
# it's possible that I have issues related to duplicate observations too---for example, this is mentioned here:
#    http://gis.stackexchange.com/questions/222192/r-gstat-krige-covariance-matrix-singular-at-location-5-88-47-4-0-skipping/222237
# revisit this issue a later iteration
# vsprSubset <- subsetVsPoints(vspr)$noQ3noMcGann
vsprSubset <- subsetVsPoints(vspr)$noQ3
# vsprNoMcGann <- subset(vspr, vspr$DataSource %in% c("CantSMS", "Wotherspoon"))
# vsprSubset <- vsprNoMcGann


cullZeroDist <- !useNoisyMeasurements # if no noisy, must cull to 0.1 m to avoid singular covariance matrix. (0.1m is consistent with kriging models.)
if(cullZeroDist) {
  zt <- 0.1 # zero threshold
  if(nrow(zerodist(vsprSubset, zero = zt)) > 0) {
    warning("Vs30 dataframe used for Kriging contains duplicate points - this will result in a singular covariance matrix. Automatically removing duplicate points now. (Manual review might be good idea.).")
    vsprSubset <- vsprSubset[-zerodist(vsprSubset, zero=zt)[,1],]
  }
}

# remove points where MODEL predictions don't exist
naRows <- which(is.na(vsprSubset[[paste0("res_",MODEL)]]))
if(length(naRows)>0) {
  warning("Some entries in vspr dataframe don't contain model predictions for this model. (Probably because they're near the coast, where raster-based predictors such as slope and convexity require a neighborhood of cells for their definition).")
  vsprSubset <- vsprSubset[-naRows,]
}


save(vsprSubset,file = "Rdata/mvnTroubleshooting_vsprSubset_normal.Rdata")
if(minThreshold > 0) {
  # prune data according to a minimum threshold distance (set in mvn_params.R)
  # (this makes the zerodist operation above unnecessary)
  # Useful for troubleshooting but not needed as long as noisy
  # measurements are used - this tends to bang the covariance matrix into shape.
  # Keep an eye on warnings returned by mvn.R to be sure.
  distMat <- gWithinDistance(vsprSubset, dist = minThreshold, byid = T)
  priority <- colSums(distMat)
  tooClose <- length(which(priority>1)) # how many are within this distance
  while(tooClose > 1) {
    idx <- which(priority==max(priority))
    idx <- idx[1]
    vsprSubset <- vsprSubset[-idx,]
    distMat <- gWithinDistance(vsprSubset, dist = minThreshold, byid = T)
    priority <- colSums(distMat)
    tooClose <- length(which(priority>1)) # how many are within this distance
  }
  save(vsprSubset,file = "Rdata/mvnTroubleshooting_vsprSubset_6km.Rdata")
  load("Rdata/mvnTroubleshooting_vsprSubset_6km.Rdata")
}


# vectors for iteration
yVec <- rep(0:15,11)
xVec <- c()
for (i in 0:10) { xVec <- c(xVec, rep(i, 16))}





######################################################################
############# choose one: test version or parLapply()
######################################################################

# ######### (a) ########################################
# # for testing before parLapply...
# # index <- 11
# index <- 85  # x05y04 contains Chch
# # # #
# mvn_oneTile(index,xVec,yVec,vsprSubset,vg,MODEL)
# # apply(X=array(1:176), MARGIN=1, FUN = mvn_oneTile, xVec,yVec,vsprSubset,vg,MODEL)





# # # testing - plot outputs:
# #
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__normal.Rdata") # current MVN
# plot(mvnOutput$lnObsPred, zlim=c(-1,3), col=rainbow(4000))
# points(vsprSubset, pch=21, bg=rainbow(4000)[as.numeric(cut(vsprSubset$res_AhdiYongWeighted1,
#                                                            breaks=seq(from=-1,to=3,length.out = 4000)))])
# plot(mvnVs30, zlim=c(0,1000), col=rainbow(1000))
# points(vsprSubset, pch=21, bg=rainbow(1000)[vsprSubset$Vs30])
# 
# # Diagnostic: force zero nugget, no noisy measurements.
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__zeroNugget_noNoisyMeas.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__zeroNugget_noNoisyMeas__weirdResults.Rdata")
# # results: weird circular artifacts, eventually identified as resulting from non positive definite cov matrix
# 
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__zeroNugget_noNoisyMeas__weirdResults.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__v2_yesNoisyMeas.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__v3_yesNoisyMeas.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__v3_noNoisyMeas.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__v4_noNoisyMeas.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__v4_yesNoisyMeas.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__normal.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__v1_newCorrFn_noNoisyMeas.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__v1_newCorrFn_yesNoisyMeas.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__v1_newCorrFn_6km_nearPD_noNoisyMeas.Rdata")
# load("Rdata/MVNRM_NZGD00_allNZ_x05y04_AhdiYongWeighted1__v1_newCorrFn_6km_noNearPD_noNoisyMeas.Rdata")
# 
# 
# plot(mvnOutput$lnObsPred)
# plot(mvnOutput$stdDev)
# plot(mvnVs30, zlim=c(0,1000))





# # ######### (b) ########################################
# # or, to do all...
numCores <- detectCores()-leaveFreeCores # 32 cores  total - aim for 30
# # numCores <- 2
clustah <- makeCluster(numCores)
clusterExport(cl=clustah,varlist = paramsList)
#
parLapply(clustah,X = 1:176,fun = mvn_oneTile,
          xVec,yVec,vsprSubset,vg,MODEL)
# parLapply(clustah,X = c(69,70,85,86),fun = mvn_oneTile, # four tiles around Chch
#           xVec,yVec,vsprSubset,vg,MODEL)
stopCluster(clustah)

