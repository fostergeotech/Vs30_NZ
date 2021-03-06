# makeRaster_AhdiAK_sigma.R
# 
# Creates a sigma raster

rm(list=ls())

library(raster)

source("R/MODEL_AhdiAK.R")

Ahdi  <- raster("~/big_noDB/models/AhdiGeoCats.tif")

# 00_ICE and 00_WATER are not included in AhdiAK_lookup(),
# so I add them manually - thus the first two NA entries:
subsTable <- data.frame(groupNum  = seq(1,17), 
                        sigmaVals = c(NA, NA, AhdiAK_lookup()$stDv_AhdiAK))

Sigma <- subs(x = Ahdi, y = subsTable)

writeRaster(Sigma, "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK.tif", format="GTiff", overwrite=T)
