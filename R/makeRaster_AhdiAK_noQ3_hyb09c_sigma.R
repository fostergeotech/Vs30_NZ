# makeRaster_AhdiAK_noQ3_hyb09c_sigma.R
# 
# Creates a sigma raster

rm(list=ls())

library(raster)

source("R/MODEL_AhdiAK_noQ3_hyb09c.R")

Ahdi  <- raster("~/big_noDB/models/AhdiGeoCats.tif")

# 00_ICE and 00_WATER are not included in AhdiAK_noQ3_hyb09c_lookup(),
# so I add them manually - thus the first two NA entries:
subsTable <- data.frame(groupNum  = seq(1,17), 
                        sigmaVals = c(NA, NA, AhdiAK_noQ3_hyb09c_lookup()$stDv_AhdiAK_noQ3))

hybSigma <- subs(x = Ahdi, y = subsTable)

writeRaster(hybSigma, "~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif", format="GTiff", overwrite=T)
