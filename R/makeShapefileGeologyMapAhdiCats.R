# makeShapefileGeologyMapAhdiCats.R

library(sp)
library(rgdal)

setwd("~/VsMap")
load("~/big_noDB/geo/QMAP_Seamless_July13K_NZGD00.Rdata")
simpleMapCats <- map_NZGD00[,"groupID_AhdiAK"]
colnames(simpleMapCats@data)[1] <- "cat"

# convert factors to integers - shapefile format can't handle categorical.
simpleMapCats2 <- simpleMapCats
simpleMapCats2$cat <- as.character(simpleMapCats$cat)

# Ne
rgdal::writeOGR(obj = simpleMapCats2,
                     dsn = "/home/kmf76/big_noDB/geo/FosterGeoCats_18-04",
                     layer = "cat",
                     driver = "ESRI Shapefile",check_exists = T, overwrite_layer = T,
                     verbose = T)

