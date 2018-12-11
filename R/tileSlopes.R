#tileSlopes.R
#
# Runs over NZGD00 TIF files of slopes (created in DEMprocessing.sh) with 11x16 tile pattern
# so that slopes can be referenced properly when generating hybrid maps.

# really lazy about how this is implemented... copy-pasted loop from rasterizeQmap.R
# where 10x10 NZGD tiles were initially set up. The tile dimensions and nrow/ncol are
# intiialized in a for loop.

# INPUT FILES:
      # img/nzni_30c_slp_nnb_1s_NZGD00.tif")
      # img/nzsi_30c_slp_nnb_1s_NZGD00.tif")
# OUTPUT FILES:
      # for X in 0-10, Y in 0-15 (176 files total)
      # VsMap/Rdata/slp_NZGD00_allNZ_xXXyYY.Rdata

rm(list=ls())

library(raster)
source("R/geodetic.R")

setwd("~/VsMap")

ni <- raster(x = "img/nzni_30c_slp_nnb_1s_NZGD00.tif")
si <- raster(x = "img/nzsi_30c_slp_nnb_1s_NZGD00.tif")

# > extent(ni)
# class       : Extent 
# xmin        : 952945.5 
# xmax        : 2247060 
# ymin        : 4659935 
# ymax        : 6237859 
# > extent(si)
# class       : Extent 
# xmin        : 952945.5 
# xmax        : 2247060 
# ymin        : 4659935 
# ymax        : 6237859 


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
    fileName[i] <- paste0("~/VsMap/Rdata/slp_NZGD00_allNZ_",
                          "x",sprintf(fmt = "%02i", xT),
                          "y", sprintf(fmt = "%02i", yT),
                          ".Rdata")
    print(fileName[i])
    print(c(xmin[i], xmax[i], ymin[i], ymax[i]))
  }
}


vec <- 1:n

#for (img in vec) {
tileSlopeParallel <- function(img, xmin, ymin, xmax, ymax, 
                              nrow, ncol, ni, si, fileName) {
  library(raster)
  source("R/geodetic.R")
  
#  img <- vec[1]  
  templateRaster <- raster(xmn=xmin[img], xmx=xmax[img],
                           ymn=ymin[img], ymx=ymax[img],
                           crs=crsNZGD00(),
                           nrows=nrow,   ncols=ncol)
  
  slpRasterNZGD00_ni <- projectRaster(from=ni, to=templateRaster,
                                      method='ngb')
  slpRasterNZGD00_si <- projectRaster(from=si, to=templateRaster,
                                      method='ngb')
  

  slpRasterNZGD00    <- merge(slpRasterNZGD00_ni, slpRasterNZGD00_si)
  
  save(slpRasterNZGD00,
       file=fileName[img])
}

library(parallel)

# debug
# img <- 1
# tileSlopeParallel(img)

clustah <- makeCluster(detectCores()-2)  # leave a couple of cores free
parLapply(cl=clustah,X = vec, fun = tileSlopeParallel, 
          xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, nrow=nrow, ncol=ncol, ni=ni, si=si, fileName=fileName)
stopCluster(clustah)
