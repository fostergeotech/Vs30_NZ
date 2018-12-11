#  makeRaster_AhdiYongWeighted1.R
# 
# Creates a model raster by combining AhdiAK_noQ3_hyb09c and YongCA_noQ3.

rm(list=ls())

library(raster)
Yong <- raster("~/big_noDB/models/YongCA_noQ3.tif")
Ahdi <- raster("~/big_noDB/models/hyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif")

lY <- log(Yong)
lA <- log(Ahdi)

lAY <- ((lY + lA)*0.5)

AhdiYong <- exp(lAY)

writeRaster(AhdiYong, "~/big_noDB/models/AhdiYongWeighted1.tif", format="GTiff", overwrite=T)
