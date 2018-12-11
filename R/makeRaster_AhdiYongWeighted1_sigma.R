#  makeRaster_AhdiYongWeighted1_sigma.R
# 
# Creates a sigma raster by SRSS of AhdiAK_noQ3_hyb09c and YongCA_noQ3.
# MUST CREATE THE MEAN MAP FIRST i.e. run makeRaster_AhdiYongWeighted1.R


rm(list=ls())

library(raster)
YongSig  <- raster("~/big_noDB/models/YongCA_noQ3_sigma.tif")
AhdiSig  <- raster("~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif")
AhdiYong <- raster("~/big_noDB/models/AhdiYongWeighted1.tif")
YongMu   <- raster("~/big_noDB/models/YongCA_noQ3.tif")
AhdiMu   <- raster("~/big_noDB/models/hyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif")


sig1sq <- YongSig^2
sig2sq <- AhdiSig^2
mu1    <- log(YongMu)
mu2    <- log(AhdiMu)
mu     <- log(AhdiYong)

# Reference: https://en.wikipedia.org/wiki/Mixture_distribution#Moments
sigsq <- 0.5 * ( (( mu1 - mu)^2) + sig1sq +
                 (( mu2 - mu)^2) + sig2sq)
sig <- sigsq^0.5





writeRaster(sig, "~/big_noDB/models/AhdiYongWeighted1_sigma.tif", format="GTiff", overwrite=T)
