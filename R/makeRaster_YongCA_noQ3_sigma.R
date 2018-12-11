#  makeRaster_YongCA_noQ3_sigma.R
# 
# Creates a model raster providing sigma (ln std dev) values for YongCA_noQ3 model.
# Sigma values are taken directly from those in MODEL_YongCA_noQ3.R
# 
rm(list=ls())

library(raster)
IPcats <- raster("~/big_noDB/topo/terrainCats/IwahashiPike_NZ_100m_16.tif")
source("R/IP_levels.R")
source("R/MODEL_YongCA_noQ3.R")
# plot(IPcats)

YongCA_noQ3_sigma <- IPcats
subsTable <- data.frame(groupNum = seq(1,16), sigmaVals = YongCA_noQ3_lookup()$stDv_YongCA_noQ3)
YongCA_noQ3_sigma <- subs(YongCA_noQ3_sigma, subsTable)
writeRaster(YongCA_noQ3_sigma, "~/big_noDB/models/YongCA_noQ3_sigma.tif", format="GTiff", overwrite=T)
