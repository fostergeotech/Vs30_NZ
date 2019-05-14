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

# getVs30 points
vsprS <- subsetVsPoints(vspr)
subsetNames <- c("McGann", "noQ3noMcGann", "noQ3noCanterbury", "McGannALL", "inset")

subsetName <- c("inset")

for(region in c("NZ","CH")) {
# for(region in c("NZ")) {
    
  mapSize <- "six"
  
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
  
  extents <- extList[[region]]
  
  margins <- switch(mapSize,poster=c(7,7,7,7),  # bottom,left,top,right
                            three=c(2,2,2,2),
                            six=c(2,2,2,2))
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
  
  
  
  
  
  if(mapSize=="three") {
    LARGE <- 0.6
    MED   <- 0.4
    SMALL <- 0.25
  } else {
      if(mapSize=="poster" || mapSize=="six") {
        LARGE <- 1.1
        MED   <- 0.8
        SMALL <- 0.5
      }
  }
  
  
  
  # sizes <- rep(LARGE, nrow(vs))
  # 
  # q1s <- vs$QualityFlag=="Q1"
  # q2s <- vs$QualityFlag=="Q2"
  # q3s <- vs$QualityFlag=="Q3"
  # used <- vs$QualityFlag!="Q3"
  # McG <- vs$DataSource=="McGannCPT"
  # Wth <- vs$DataSource=="Wotherspoon201711"
  # 
  # sizes[q3s] <- SMALL
  # sizes[q2s] <- MED
  # 
  # shapes <- vector(length=nrow(vs))
  # shapes[q1s] <- 23
  # shapes[q2s] <- 22
  # shapes[q3s] <- 21
  # shapes[McG] <- 24
  # shapes[Wth] <- 25
  
  
  
  
  modelRast <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_DEM_all_NZGD00_resampled.tif", rasterX, rasterY, region))
  
  
  fileName =  paste0("out/maps/dataLocs_elev_",region,"_",subsetName,"_",mapSize,".png")
  legName  =  paste0("out/maps/dataLocs_elev_",mapSize,"_legend.png")
  
  png(fileName, width = pxWidth, height = pxHeight)
  sp=c(0.2,0.8,0.2,0.8)
  
  
  # first par
  par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, bty="n", plt=sp)
  
  # plot hillshade first, then overlay
  plot(hillshade,
       col = grey.colors(nGrayCol, start=0, end=1), legend=F,
       xlab="", #"NZGD2000 datum (metres)",
       ylab="", #"NZGD2000 datum (metres)",
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
  #
  # 1 1 1 1 1 1
  #
  par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T, plt=sp)
  plot(isWater , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
       ,ext=extents
       ,axes=switch(mapSize,poster=T,three=F,six=F)
       # ,smallplot=c(0,1,0,1)
       )
  #
  # 2 2 2 2 2 2
  #
  par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T, plt=sp)
  plot(isIce   , col = rgb(1.0, 1.0, 1), alpha=0.8, add=T,  legend=F ,maxpixels = maxpix
       ,ext=extents
       ,axes=switch(mapSize,poster=T,three=F,six=F)
       # ,smallplot=c(0,1,0,1)
       )
  #
  # 3 3 3 3 3 3
  #
  #       # map
  #
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
  #
  # 4 4 4 4 4 4
  #
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
  
  
  
  
  
  
  
  
  
  
  
  
  if(subsetName!="inset") {
    vsSubset <- vsprS[[subsetName]]
    
    
    # Removing points outside map domain:
    
    # coordsD <- coordinates(vsSubset)
    # coX <- coordsD[,1]
    # coY <- coordsD[,2]
    # exM <- extents
    # whichVs <- which((coX >= exM[1]) &
    #                    (coX <= exM[2]) &
    #                    (coY >= exM[3]) &
    #                    (coY <= exM[4]))
    # vs2 <- vsSubset[whichVs,]
    # vs <- vs2
    # coordsD <- coordinates(vs)
    
    # for coloring by Vs30 (or whatever)
    # vsD <- vs$Vs30
    # Dcol <- modelCol[as.numeric(cut(vsD,breaks=seq(zlims[1],zlims[2],length=nModCol+1)))] # choose colors
    
    # # plot points  
    # points(x = coordsD[McG,], pch=shapes[McG], bg=Dcol[McG], col="black", cex=sizes[McG])
    # points(x = coordsD[Wth,], pch=shapes[Wth], bg=Dcol[Wth], col="black", cex=sizes[Wth])
    # points(x = coordsD[q2s,], pch=shapes[q2s], bg=Dcol[q2s], col="black", cex=sizes[q2s])
    # points(x = coordsD[q1s,], pch=shapes[q1s], bg=Dcol[q1s], col="black", cex=sizes[q1s])
    
    # just plot all points as circles:
    points(x = coordsD, bg="black", cex = SMALL)
    
  } else {
    # These weren't used at all:
    # points(x = coordinates(vsprS[["McGannALL"]]),    bg="black", cex = SMALL, pch=21)
    # points(x = coordinates(vsprS[["KaiserQ3"]]),     bg="black", cex = SMALL, pch=22)
   
    # Unused legend elements commented out below:

    legendVec <- c(
                   #"McGann (not used)",
                   "McGann (resampled)",
                   "Surface wave",
                   #"Kaiser Q3 (not used)",
                   "Kaiser Q2",
                   "Kaiser Q1"
                   )
    colorVec <- c(
                  #"black",
                  "white",
                  "cyan",
                  #"black",
                  "blue",
                  "yellow"
                  )
    shapeVec <- c(
                  #21,
                  21,
                  21,
                  #22,
                  22,
                  22
                  )
    #cexVec <- c(rep(SMALL,5),MED)
    cexVec <- c(SMALL,rep(MED,3))
    points(x = coordinates(vsprS[["McGann"]]),       bg="white", cex = SMALL, pch=21)
    points(x = coordinates(vsprS[["Wotherspoon"]]),  bg="cyan",  cex = MED, pch=21)
    points(x = coordinates(vsprS[["KaiserQ2"]]),  bg="blue", cex = MED, pch=22)
    points(x = coordinates(vsprS[["KaiserQ1"]]),  bg="yellow", cex = MED, pch=22)
    legend(x=switch(region,CH="bottomright",NZ="topleft"),
           legend = legendVec,
           pt.bg  = colorVec,
           pch    = shapeVec,
           pt.cex = cexVec
           # title = ""#,
           #horiz=T
    )
  }
  
  
  
  
  
  
  
  
  
  
  
  
  # draw scale & north arrow
  par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
  drawScale(region)
  par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
  northarrow(region)
  
  
  # Inset box
  if(region=="NZ") {
    rect(xleft=extList$CH[1],xright = extList$CH[2],
         ybottom = extList$CH[3],ytop = extList$CH[4],
         lwd=3)
    text(x=extList$CH[2], y=mean(extList$CH[3:4]), 
         labels="Panel (b)",
         pos=4)
  }
  
  
  dev.off()
  
  # if(fileName=="out/maps/elev_CH_elevation_three.png") { # plot a separate file with just legend.
  #   png(legName, width = 450, height = pxHeight)
  #   par(mfrow=c(1,1),  cex=CEX,   mar=c(2,2,2,2),   bg=bgcol, col=txtcol)
  #   plot.new()
  #   plot(modelRast, legend.only=T,
  #        col=modelCol, zlim=zlims,
  #        legend.width=switch(mapSize,poster=1,three=2), 
  #        legend.shrink=switch(mapSize,poster=0.75,three=1),
  #        axis.args=list(at    = zscale,
  #                       labels= zscale,
  #                       cex.axis=1),
  #        legend.args=list(text='Elevation, metres',
  #                         side=4,
  #                         #font=2,
  #                         line=switch(mapSize,poster=3.5,three=3),
  #                         # line=1.5,
  #                         cex=CEX)
  #        ,maxpixels = maxpix
  #        ,ext=extents
  #        ,axes=switch(mapSize,poster=T,three=F,six=F)
  #        ,smallplot=switch(mapSize,poster=NULL,three=c(0.32,0.48,0.075,0.925)) # smallplot is a way of fine-tuning the legend location for small plots i.e. the 3 inch version.
  #   )
  #   dev.off()
  # } # if fileName...
}
