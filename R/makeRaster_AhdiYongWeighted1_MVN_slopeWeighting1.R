#  makeRaster_AhdiYongWeighted1_MVN_slopeWeighting1.R
# 
# Generate Vs30 and sigma rasters representing the weighted combination (mixture) of 
# two models: AhdiYongWeighted1 and AhdiYongWeighted1_MVN.
# The weighting coefficients are chosen from a piecewise-linear function of the
# log10(slope09c). At flat slopes e.g. Christchurch, MVN receives full weighting.
# At steep slopes, e.g. 1:1, MVN receives zero weighting.

rm(list=ls())

library(raster)
MVN0_Vs30  <- raster("~/big_noDB/models/AhdiYongWeighted1.tif")
MVN1_Vs30  <- raster("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif")
MVN0_sigma <- raster("~/big_noDB/models/AhdiYongWeighted1_sigma.tif")
MVN1_sigma <- raster("~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif")


###########################################################################################################
# The following creates the weighting factor raster, w0. I save it so don't have to regenerate.
###########################################################################################################
          # slp        <- raster("~/big_noDB/topo/slp_NZMG/slp_NZGD00_allNZ_fromHybRdataFiles.tif")
          # lslp <- log10(slp)
          # rm(slp)  # save memory
          # w0   <- 0.25*(lslp + 4)  # w0 is the weighting factor associated with AhdiYongWeighted1. This line performs the linear transform so that w0(-4)=0 and w0(0)=1.
          # rm(lslp) # save memory
          # w0[w0<0] <- 0 # range of w0 is from 0 to 1.
          # w0[w0>1] <- 1 # range of w0 is from 0 to 1.
          # w1 <- 1 - w0  # w1 is the weighting factor associated with AhdiYongWeighted1_MVN.
w0 <- raster("~/big_noDB/models/slopeWeighting1_w0.tif")  
w1 <- 1 - w0


logK0 <- log(MVN0_Vs30)
rm(MVN0_Vs30) # save memory
logK1 <- log(MVN1_Vs30)
rm(MVN1_Vs30) # save memory


logVs30_interp <- (logK0*w0 + logK1*w1) # interpolated Vs30
Vs30_interp <- exp(logVs30_interp)

sigsq <-  (w0 * ((( logK0 - logVs30_interp)^2) + (MVN0_sigma^2)))  +  # Weighted mixture
          (w1 * ((( logK1 - logVs30_interp)^2) + (MVN1_sigma^2)))
sig <- sigsq^0.5


writeRaster(Vs30_interp, "~/big_noDB/models/AhdiYongWeighted1_MVN_slopeWeighting1_Vs30.tif", format="GTiff", overwrite=T)
writeRaster(sig,         "~/big_noDB/models/AhdiYongWeighted1_MVN_slopeWeighting1_sigma.tif", format="GTiff", overwrite=T)

# uncomment the next line if needed to regenerate w0 tiff:
# writeRaster(w0,          "~/big_noDB/models/slopeWeighting1_w0.tif", format="GTiff",overwrite=T)