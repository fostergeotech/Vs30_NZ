# AhdiGeologyCategoriesRaster.R
# 
# Take as input the QMAP INDEX raster (~/big_noDB/models/INDEX_NZGD00_allNZ.tif)
# and generate a map with categorical data giving Ahdi geology classifications.
# 
# committed before shutdown on 20171206
# 
rm(list=ls())

idxFileName    <- "~/big_noDB/models/INDEX_NZGD00_allNZ.tif"   # INPUT
geoFileName    <- "~/big_noDB/models/AhdiGeoCats.tif"          # OUTPUT

library(raster)

idx <- raster(x = idxFileName) # load index file


# the following is for testing - crop input raster to smaller size (chch) 
# set flag to 0 for production, 1 for testing
# need to examine output to ensure levels/factors are aligned with the right geology categories.
if(0) {
  xmn = 1512000 
  xmx = 1614400 
  ymn = 5109600 
  ymx = 5212000 
  crp <- c(xmn,xmx,ymn,ymx)
  idx2 <-idx
  idx <- crop(idx2, crp)
  # the index file has zeroes in water.
  idx[Which(idx==0)] <- NA
  plot(idx)
}





# load Qmap data
load(file = "~/big_noDB/geo/QMAP_Seamless_July13K_NZGD00.Rdata")


# create key/value pairs for creating geology category raster.
idxVec <- map_NZGD00$INDEX
geoVec <- map_NZGD00$groupID_AhdiAK
geoLevels <- levels(geoVec)
numGeoVec <- data.frame(n = 1:length(geoLevels), groupID=sort(geoLevels))
convertDF1 <- data.frame(index = idxVec, groupID = geoVec)
convertDF2 <- merge(x = convertDF1, y=numGeoVec, by="groupID")
convertDF3 <- convertDF2[,c("index","n")]

# the following were used to verify that this operation did what i wanted:
# cdf3 <- convertDF2[,c(1,3)]
# unique(cdf3) # should have same groupID and n as numGeoVec.


# perform value substitution and save output.
# the substitution operation is performed TWICE. once by setting the
# raster values directly to integers; then by substituting the integers with
# their group ID strings. This is because I could not find a way of reordering
# the factor levels in the way I wanted. This impacts the order in which
# legend values are displayed in figures.
geo <- subs(x = idx, y = convertDF3,
            by = "index", which = "n")#, 
            # filename = geoFileName, overwrite = T)
geo2 <- subs(x = geo, y = numGeoVec, by="n", which="groupID")

writeRaster(geo2, filename=geoFileName, overwrite = T)
# Note: the .aux.xml file created with the TIFF file is where R finds levels(geo)!


