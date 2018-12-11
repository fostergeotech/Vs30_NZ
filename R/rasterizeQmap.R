# RasterizeQmap
#
#
# This assigns ID values corresponding to each polygon---then
# another script will assign Vs30 based on polygon ID and table lookups.
# The advantage is only needing to perform "rasterize" operation once, 
# then assign Vs30 values in a less costly operation whenever the geology-based
# estimates are updated.

# This script uses parallel library and 28 cores---to be run on Hypocentre.
# 
# To run on non-parallel environment I think it's a matter 
# of just changing the makeCluster() argument from 28 to 1.

# To copy the raster files from Hypocentre to Microfunk (and avoid lots of processing),
# issue the following shell commands:
#
# cd ~/VsMap/Rdata/
# scp kmf76@hypocentre:/home/kmf76/VsMap/Rdata/INDEX_NZGD00_allNZ_x\?\?y\?\?.Rdata ./





rm(list=ls())

setwd("~/VsMap/")
library(rgdal)
library(raster)
library(rgeos)
library(Grid2Polygons) 
library(spatstat)
library(parallel)
source("R/functions.R")
source("R/geodetic.R")
source("R/classifyThings.R")
load("Rdata/vspr.Rdata")
load("~/big_noDB/geo/QMAP_Seamless_July13K_NZGD00.Rdata")









      # NZGD00
      # xLims <- c(1470000,
      #            1620000)
      # yLims <- c(5100000,
      #            5250000)
# NZGD00 corresponding to CHCH WGS84 limits
xLims <- c(1480245.147,  # 171d 45m 00s E
           1620377.653)  # 173d 15m 00s E
yLims <- c(5099599.388,  # 44d 15m 00s S
           5239154.347)  # 43d 00m 00s S

rasterizedCant <- rasterizeQmap(Qmap = map_NZGD00.Cant,
                                xLims=xLims, yLims=yLims,
                                ncol=600, nrow=600)
save(rasterizedCant,
     file="~/VsMap/Rdata/INDEX_NZGD00_Canterbury.Rdata")

xLimsNZ <- c(1000000,
             2126400) # xmax-xmin=2126400 = 1024*100*11
yLimsNZ <- c(4700000,
             6338400) #ymax-ymin=1638400  = 1024*100*16
xTotalWidth <- (xLimsNZ[2]-xLimsNZ[1])
yTotalWidth <- (yLimsNZ[2]-yLimsNZ[1])
xTiles  <- 11
yTiles  <- 16
n <- xTiles*yTiles

xTileWidth  <- xTotalWidth / xTiles
yTileWidth  <- yTotalWidth / yTiles
ncol <- xTileWidth/100 # i.e. 100m resolution
nrow <- yTileWidth/100 # i.e. 100m resolution

# loop prep: preallocating
xmin <- numeric(n)
ymin <- numeric(n)
xmax <- numeric(n)
ymax <- numeric(n)
fileName <- character(n)

i <- 0  #counter

# loop to populate vectors for parallelization
for (xT in 0:(xTiles-1)) {
  for (yT in 0:(yTiles-1)) {
    i <- i+1
    xmin[i] <- xLimsNZ[1] + (xT*xTileWidth)
    xmax[i] <- xmin[i] + xTileWidth
    ymin[i] <- yLimsNZ[1] + (yT*yTileWidth)
    ymax[i] <- ymin[i] + yTileWidth
    fileName[i] <- paste0("~/VsMap/Rdata/INDEX_NZGD00_allNZ",
                       "_x",sprintf(fmt = "%02i", xT),
                       "y", sprintf(fmt = "%02i", yT),
                       ".Rdata")
    print(fileName)
    print(c(xmin, xmax, ymin, ymax))
  }
}


# Now dat parallel magic
vec <- 1:n

rasterizeParallel <- function(n, Qmap, 
                              xMinList, xMaxList, 
                              yMinList, yMaxList, 
                              ncol, nrow,
                              fileName) {
  source("R/functions.R")
  
  indexRaster <- rasterizeQmap(Qmap = Qmap,
                          xLims=c(xMinList[n], xMaxList[n]),
                          yLims=c(yMinList[n], yMaxList[n]),
                          ncol=ncol,
                          nrow=nrow)
  save(indexRaster,
       file=fileName[n])
}


clustah <- makeCluster(28)
parLapply(cl = clustah,X = vec,fun = rasterizeParallel, Qmap=map_NZGD00,
          xMinList=xmin,
          xMaxList=xmax,
          yMinList=ymin,
          yMaxList=ymax,
          ncol=ncol, nrow=nrow,
          fileName=fileName
)
stopCluster(clustah)





# this loop loads all rasters just to verify 
# for (i in 0:9) {
#   for (j in 0:9) {
#     strname <- paste0("x",sprintf("%02.0f", i),"y",sprintf("%02.0f",j))
#     load(file = paste0("~/VsMap/Rdata/INDEX_NZGD00_allNZ_",strname,".Rdata"))
#     assign(x = strname, Msgeo_NZGD00_NZ)
#   }}

