#  makeRaster_YongCA_sigma.R
# 
# Creates a model raster providing sigma (ln std dev) values for YongCA model.
# Sigma values are taken directly from those in MODEL_YongCA.R
# 
rm(list=ls())

library(raster)
IPcats <- raster("~/big_noDB/topo/terrainCats/IwahashiPike_NZ_100m_16.tif")
source("R/IP_levels.R")
source("R/MODEL_YongCA.R")
# plot(IPcats)

YongCA_sigma <- IPcats
subsTable <- data.frame(groupNum = seq(1,16), sigmaVals = YongCA_lookup()$stDv_YongCA)
YongCA_sigma <- subs(YongCA_sigma, subsTable)
writeRaster(YongCA_sigma, "~/big_noDB/models/YongCA_sigma.tif", format="GTiff", overwrite=T)
