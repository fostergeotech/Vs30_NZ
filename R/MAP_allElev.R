# This script is based on QCAMmaps_Vs30_allNZgeo_tryResample.R
#
# ALL elevation maps.


rm(list=ls())
setwd("~/VsMap")
# background rasters like hillshade etc. were generated (mostly) by R/QCAM_SCEC_GEESDV_data_allNZgeo.R

library(colorspace)
library(spatstat)
library(raster)
source("R/functions.R")
source("R/MAP_NZGD00_regions.R") # contains regions
source("R/MAPparams.R")
load("Rdata/vspr.Rdata")


mapIDvec <- mapIDvec_elev

# getVs30 points
vsprS <- subsetVsPoints(vspr)
vsSubset <- vsprS$allData


# color stuff
nModCol     <- 300
modelCol        = colorspace::terrain_hcl(n=nModCol,gamma = )
# modelCol        = rainbow(nModCol, start=0, end=1)
GMTpaletteRGB <-  t(matrix(nrow = 3,data = c(
  51,	102,	0,      # 0m
  129,	195,	31,     # 100m
  255,	255,	204,    # 200m
  244,	189,	69,     # 400m
  102,	51,	  12,     # 500m
  102,	51,	  0,      # 600m
  178,  153,  127,    # 700m (interp)
  255,	255,	255)))  # 800m
GMTpaletteHex <- rgb2hex(GMTpaletteRGB)
modelCol <- colorRampPalette(GMTpaletteHex, alpha = T)(nModCol)


# random plotting stuff
nGrayCol <- 100
bgcol = rgb(1,1,1)
txtcol = rgb(0,0,0)
CEX = 3
zlims <- c(0,4000)
zscale <- seq(0,4000,250)


for(region in regionVec) {
  # region <- regionVec[1] # testing
  print(region)
  for(mapSize in mapSizeVec) {
    print(mapSize)
    #mapSize <- "three" # testing
    margins <- switch(mapSize,poster=c(7,7,7,7),  # bottom,left,top,right
                              three=c(2,2,2,2),six=c(2,2,2,2))
    # if(mapSize=="three",oma
    # output
    pxWidth         = switch(mapSize,poster=3300,three=1500,six=1972)
    pxHeight        = switch(mapSize,poster=4200,three=1500,six=1972)
    maxpix          = 1e7  # plot() for rasters is complicated. Using maxpix with massive
    # rasters doesn't NECESSARILY result in quicker plotting (1e7
    # was not working in my experience) but if raster is resampled
    # beforehand, maxpixels works as expected and doesn't cause
    # much/any delay.
    makeMask        = F
    
    # these are the current resampling dimensions. I chose these by measuring map pane size manually for the 
    # chosen raster output size, 3300x4200.
    rasterX <- switch(mapSize,poster=2698,three=900,six=1800)
    rasterY <- switch(mapSize,poster=3632,three=900,six=1800)
    hillshade        <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_hillshadeA_NZGD00.tif",rasterX,rasterY,region))
    isOcean          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isOcean_NZGD00.tif",rasterX,rasterY,region))
    isWater          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isWater_NZGD00.tif",rasterX,rasterY,region))
    isIce            <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isIce_NZGD00.tif",rasterX,rasterY,region))
    
    for (mapID in mapIDvec) {
      print(mapID)
      # mapID <- mapIDvec[1]     # testing
      
      modelRast <- raster(sprintf(switch(mapID,
                                         elevation = "~/big_noDB/models/RESAMP_%04dx%04d_%s_DEM_all_NZGD00_resampled.tif"
      ), rasterX, rasterY, region))
    
      
      
      extents <- extList[[region]]
      
      
      
      
      fileName =  paste0("out/maps/elev_",region,"_",mapID,"_",mapSize,".png")
      legName  =  paste0("out/maps/elev_",mapID,"_",mapSize,"_legend.png")
      
      png(fileName, width = pxWidth, height = pxHeight)
      sp=c(0.2,0.8,0.2,0.8)

  
      # first par
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, bty="n", plt=sp)

      # plot hillshade first, then overlay
      plot(hillshade,
           col = grey.colors(nGrayCol, start=0, end=1), legend=F,
           xlab="NZGD2000 datum (metres)", ylab="NZGD2000 datum (metres)",
           main=switch(mapID, elevation = switch(mapSize,poster="Elevation",three=NULL,six=NULL)),
           sub=switch(mapID, elevation = "Source: NZDEM 25 metre data - Land Resource Information Systems Portal, https://lris.scinfo.org.nz/"),
           cex=CEX, # cex = symbol size for scatter plots
           cex.sub = 0.8
           ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           # ,smallplot=c(0,1,0,1)
           )

      # Add water, ocean, ice
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T, plt=sp)
      plot(isOcean , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           # ,smallplot=c(0,1,0,1)
           )

      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T, plt=sp)
      plot(isWater , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           # ,smallplot=c(0,1,0,1)
           )

      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T, plt=sp)
      plot(isIce   , col = rgb(1.0, 1.0, 1), alpha=0.8, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           # ,smallplot=c(0,1,0,1)
           )


      # map

      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T, plt=sp)
      plot(modelRast, col = modelCol, zlim=zlims,
           #xaxt=seq(from=130, to=180, by=1)*1e4, yaxt='n',
           add=T,
           alpha=0.4,
           legend=F # specify detailed legend stuff in another call.
           ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           # ,smallplot=c(0,1,0,1)
           )
      if(mapSize=="poster") { # only do legend for full size not 3 inch
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
        plot(modelRast, legend.only=T,
             col=modelCol, zlim=zlims,
             legend.width=switch(mapSize,poster=1,three=2),
             legend.shrink=switch(mapSize,poster=0.75,three=1),
             axis.args=list(at    = zscale,
                            labels= zscale,
                            cex.axis=1),
             legend.args=list(text='Elevation, metres',
                              side=4,
                              #font=2,
                              line=switch(mapSize,poster=3.5,three=0),
                              # line=1.5,
                              cex=CEX)
             ,maxpixels = maxpix
             ,ext=extents
             ,axes=switch(mapSize,poster=T,three=F,six=F)
             ,smallplot=switch(mapSize,poster=NULL,three=c(0.87,0.89,0.075,0.925)) # smallplot is a way of fine-tuning the legend location for small plots i.e. the 3 inch version.
             )
      }
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T, plt=sp)



      if(makeMask) {
        # add uncertainty-mask
        # must be done incrementally (not sure how to set partial alpha per pixel!)
        # There is some fine-tuning to be done with the masking. To help with this, some
        # control parameters:
        nLevels <- 10
        alphaLevelSeq <- seq(from=0, to=1, length.out=nLevels)
        alphaExp <- 1.8
        grayC <- rgb(0.8,0.8,0.8)
        for(alphaLevel in alphaLevelSeq) {
          thisMask <- MVNmask>alphaLevel
          thisMask[thisMask==0] <- NA
          par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
          plot(thisMask, col=grayC, alpha=alphaLevel^alphaExp, add=T, legend=F)
        }
      }

      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T, plt=sp)

      # draw scale & north arrow
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
      drawScale(region)
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
      northarrow(region)



      dev.off()
    
      if(fileName=="out/maps/elev_CH_elevation_three.png") { # plot a separate file with just legend.
        png(legName, width = 450, height = pxHeight)
        par(mfrow=c(1,1),  cex=CEX,   mar=c(2,2,2,2),   bg=bgcol, col=txtcol)
        plot.new()
        plot(modelRast, legend.only=T,
             col=modelCol, zlim=zlims,
             legend.width=switch(mapSize,poster=1,three=2), 
             legend.shrink=switch(mapSize,poster=0.75,three=1),
             axis.args=list(at    = zscale,
                            labels= zscale,
                            cex.axis=1),
             legend.args=list(text='Elevation, metres',
                              side=4,
                              #font=2,
                              line=switch(mapSize,poster=3.5,three=3),
                              # line=1.5,
                              cex=CEX)
             ,maxpixels = maxpix
             ,ext=extents
             ,axes=switch(mapSize,poster=T,three=F,six=F)
             ,smallplot=switch(mapSize,poster=NULL,three=c(0.32,0.48,0.075,0.925)) # smallplot is a way of fine-tuning the legend location for small plots i.e. the 3 inch version.
        )
        dev.off()
      } # if fileName...
      
    } # for mapID
  } # for mapSize
} # for region
