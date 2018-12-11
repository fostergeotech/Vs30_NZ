rm(list=ls())
setwd("~/VsMap")
library(gstat)
library(ggplot2)
library(colorRamps)
source("R/models.R")
source("R/functions.R")
load("Rdata/vspr.Rdata")


# choosing a variogram for the following model:
MODEL <- "AhdiYongWeighted1"

includeTitle <- F # "false" for publication figures. "true" for review etc.


# Experimenting with Cressie style variogram
cressieTF <- F
# cressieTF <- F

# Normalized? Y/N
# Should variogram be generated with residuals res = ln(Vs30_obs/Vs30_pred), (F)
# or with normalized residuals res_norm  = res / stdDev?                     (T)
normResFlags <- c(T,F)
# These are overwritten in the blocks below for "variogram version"

# lagwidth or boundaries
lagwidths = c(0.2,0.5,1,2,5,10,20,30,50,75,100,120)*1e3 # metres
# lagwidths = c(1,5,10,20,50,120)*1e3 # metres
cutoffDists <- c(50e3,100e3,200e3,500e3) # metres
# cutoffDists <- c(50e3,200e3) # metres
# boundaries <- 2^seq(1,10,by=0.2)
# boundaries <- c(0.5,1,1.5,2,3,5,8,13,21,34,50,75,100,150,200,250,300,400,500)
# boundaries <- c(boundaries[boundaries<cutoffDist],cutoffDist)


# manual variogram fit params
# 
# ######## version 1 DO NOT CHANGE ########
# psill=0.3
# range=20e3
# nugget=0.01
# kappa = 0.9
# vgName <- "v2"
# #########################################
# Formerly, variogram versions v2 and
# higher were just "Testing" versions
# used while troubleshooting MVN. I've
# now fixed MVN, and will choose new
# parameters for v2 in an effort to
# select a more appropriate variogram.
# (20180408)
######## v2 #############################
# psill=0.1
# nugget=0
# kappa = 0.5
# range=10e3
# vgName <- "v2"
#########################################
# ######## v3 #(FINAL: normalized for MVN)#
# psill=5
# nugget=0
# kappa = 0.5
# range=30e3
# vgName <- "v3"
# normResFlags <- c(T)
# #########################################
# ######## v4 (KRIGING) ###################
# # Fit sill to highest variogram ordinates
# psill=0.3
# nugget=0
# kappa = 0.5
# range=30e3
# normResFlags <- c(F)
# vgName <- "v4"
# #########################################
######## v5 (FINAL: KRIGING) ############
# Fit sill to variance of residuals, which is 0.092.
psill=0.092
nugget=0
kappa = 0.5
range=7e3
normResFlags <- c(F)
vgName <- "v5"
#########################################



dat <- subsetVsPoints(vspr)
# quick n dirty loop to remove NA points
for(subsetName1 in names(dat)) {
  subset1 <- dat[[subsetName1]]
  subset2 <- subset1[!is.na(subset1$res_AhdiYongWeighted1),]
  dat[[subsetName1]] <- subset2
}

for(cutoffDist in cutoffDists) {
  # cutoffDist <- cutoffDists[1] # testing
  for(normResFlag in normResFlags) {
    # normResFlag <- normResFlags[2] # testing
    maxGamma <- switch(as.character(normResFlag),
                       "TRUE" = 7,
                       "FALSE" = 0.35)
    for(lagwidth in lagwidths) {
      # lagwidth <- lagwidths[8] # testing
      # for(modelSubset in c("Kaiser_all", "allData", "McGann", "noQ3", "noQ3noMcGann")) {
      for(modelSubset in c("noQ3noMcGann")) {
        # modelSubset <- "noQ3noMcGann" #testing
                           
        
        dat_subset <- dat[[modelSubset]]
        
        dat_subset$normalizedResidual <- dat_subset$res_AhdiYongWeighted1 / dat_subset$stDv_AhdiYongWeighted1
        
        if(nrow(dat_subset)>=2) {  # need a minimum of 1 pair i.e. 2 datapoints for the empirical variogram.
          # ev = empirical variogram
          if(normResFlag) {
            ev   <- variogram(normalizedResidual~1,data = dat_subset,   cutoff=cutoffDist, width=lagwidth, cressie=cressieTF)
          } else {
            ev   <- variogram(res_AhdiYongWeighted1~1,data = dat_subset,   cutoff=cutoffDist, width=lagwidth, cressie=cressieTF)
          }
          vgdf <- ev[,c("dist","gamma","np")]
    
          # evp = empirical variogram plot
          evp  <- ggplot() +
            geom_point(data = vgdf, aes(x=dist/1000, y=gamma))
        }
        distanceVec <- exp(seq(log(10), log(cutoffDist), length.out = 100))
        fitVarManual <- vgm(psill  = psill,
                            model  = "Mat",
                            range  = range,
                            nugget = nugget,
                            kappa  = kappa)
        
        vgmTheo_manual <- variogramLine(fitVarManual, dist_vector = distanceVec)
        
        ylimm <- c(0, maxGamma)
        
        zzzzz <- evp + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(ylimm) + xlab("dist, km") + ylab("V(u)")
        if(includeTitle) {zzzzz <- zzzzz + ggtitle(paste0("Empirical and theoretical variogram name: ",vgName),
                                                   subtitle = sprintf("Data: %s   Lag width: %03.1fkm  Normalised: %s", modelSubset, lagwidth/1000, normResFlag))}
        zzzzz
        ggsave(filename = sprintf("out/plots/vgram_emp+theo_%s_%s_dat_%s_cressie%s_norm%s_cut%03.0fkm_mG%03.1f_lag%03.1fkm.png",
                                  MODEL, vgName, modelSubset, 
                                  substr(as.character(cressieTF),1,1), 
                                  substr(as.character(normResFlag),1,1),
                                  cutoffDist/1000, # cutoff distance in km
                                  maxGamma,
                                  lagwidth/1000
                                  ),
               height = ifelse(includeTitle, 3, 2),
               width = 6)
      }
    }
  }
}

variogram <- fitVarManual
save(variogram, file = paste0("Rdata/variogram_", MODEL, "_", vgName,".Rdata"))

