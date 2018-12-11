rm(list=ls())
setwd("~/VsMap")


# library(viridis)
library(RColorBrewer)
library(raster)
source("R/functions.R")
source("R/MAP_NZGD00_regions.R") # contains regions
source("R/MAPparams.R")
load("Rdata/vspr.Rdata")

mapIDvec <- c(  
              "slope_09c_hyb",
              "w0"
              )
regionVec <- names(extList)
# regionVec <- c("CH") # testing

# output
maxpix          = 1e7  # plot() for rasters is complicated. Using maxpix with massive
                       # rasters doesn't NECESSARILY result in quicker plotting (1e7
                       # was not working in my experience) but if raster is resampled
                       # beforehand, maxpixels works as expected and doesn't cause
                       # much/any delay.
makeMask        = F



# getVs30 points
vsprS <- subsetVsPoints(vspr)
vsSubset <- vsprS$allData


# color stuff
nModCol     <- 100
modelCol        = colorRampPalette(brewer.pal(11,"Spectral"))(nModCol)



# random plotting stuff
nGrayCol <- 100
bgcol = rgb(1,1,1)
txtcol = rgb(0,0,0)
CEX = 3


for(region in regionVec) {
  
  for (mapID in mapIDvec) {
    # mapID <- "slope_09c_hyb"  
    
    for(mapSize in mapSizeVec) {
      # these are the current resampling dimensions. I chose these by measuring map pane size manually for the 
      # chosen raster output size, 3300x4200.
      rasterX <- switch(mapSize,poster=2698,three=900,six=1800)
      rasterY <- switch(mapSize,poster=3632,three=900,six=1800)

      pxWidth         = switch(mapSize,poster=3300,three=1113,six=1972)
      pxHeight        = switch(mapSize,poster=4200,three=1113,six=1972)
      
      margins <- switch(mapSize,poster=c(7,7,7,7),three=c(2,2,2,2),six=c(2,2,2,2))
            
      # region <- "CH"
      hillshade        <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_hillshadeA_NZGD00.tif",rasterX,rasterY,region))
      isOcean          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isOcean_NZGD00.tif",rasterX,rasterY,region))
      isWater          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isWater_NZGD00.tif",rasterX,rasterY,region))
      isIce            <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isIce_NZGD00.tif",rasterX,rasterY,region))
      
      
      modelRast0 <- raster(sprintf(switch(mapID,
                                         slope_09c_hyb      = "~/big_noDB/topo/slp_NZMG/RESAMP_%04dx%04d_%s_slp_NZGD00_allNZ_fromHybRdataFiles.tif",
                                         w0                 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_slopeWeighting1_w0.tif"
      ), rasterX, rasterY, region))
      
      modelRast <- switch(mapID,
                          slope_09c_hyb = log10(modelRast0), # use LOG10 of slope.
                          w0            = modelRast0         # the weighting factor is already generated from log10(slope) so it is plotted as-is.
                          )
      
      print(sprintf("log10(%s)  -  Max: %06.3f Min: %06.3f",mapID, maxValue(modelRast),minValue(modelRast)))
      
      extents <- extList[[region]]
  
      
      zlims <- switch(mapID, slope_09c_hyb = c(-5,1),
                             w0            = c(0,1))
      zscale <- seq(zlims[1],zlims[2],switch(mapID, 
                                             slope_09c_hyb = 1,
                                             w0            = 0.2))
      
      
      
      fileName =  paste0("out/maps/",switch(mapID,
                                            slope_09c_hyb = "slp09c_",
                                            w0            = "geostats_weighting1_"),
                         region,"_",mapID,"_",mapSize,".png")
      legName  =  paste0("out/maps/",switch(mapID,
                                            slope_09c_hyb = "slp09c_",
                                            w0            = "geostats_weighting1_"),
                                    mapID,"_",mapSize,"_legend.png")
      
      png(fileName, width = pxWidth, height = pxHeight)
  
  
  
      # first par
      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, bty="n")
  
      # plot hillshade first, then overlay
      plot(hillshade,
           col = grey.colors(nGrayCol, start=0, end=1), legend=F,
           xlab="NZGD2000 datum (metres)", ylab="NZGD2000 datum (metres)",
           main=switch(mapSize,three=NULL,poster=switch(mapID, 
                              slope_09c_hyb  = "log_10 of 09c resolution topographic slope",
                              w0             = "Weighting factor used for modifying geostatistics"
                       )),
           sub=switch(mapID,
                      slope_09c_hyb  = "slope resolved at 9 arc-seconds",
                      w0             = "Weighting factor is derived from the topographic slope resolved at 9 arc-seconds"
                      ),
           cex=2, # cex = symbol size for scatter plots
           cex.sub = 0.8
           ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F)
           )
  
  
      # Add water, ocean, ice
      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isOcean , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F)
           )
  
      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isWater , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F)
           )
  
      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isIce   , col = rgb(1.0, 1.0, 1), alpha=0.8, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F)
           )
  
  
      # map
  
      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(modelRast, col = modelCol, zlim=zlims,
           #xaxt=seq(from=130, to=180, by=1)*1e4, yaxt='n',
           add=T,
           alpha=0.7,
           legend=F # specify detailed legend stuff in another call.
           ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F)
           )
      if(mapSize!="three") { # only plot legend for full size; otherwise do it as a separate file.
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
        plot(modelRast, legend.only=T,
             col=modelCol, zlim=zlims,
             legend.width=1, legend.shrink=0.75,
             axis.args=list(at    = zscale,
                            labels= zscale,
                            cex.axis=1),
             legend.args=list(text=switch(mapID, 
                                          slope_09c_hyb = 'log_{10}(topographic slope)',
                                          w0            = "weighting factor"),
                              side=4,
                              #font=2,
                              line=3.5,
                              cex=3)
             ,maxpixels = maxpix
             ,ext=extents
             ,axes=switch(mapSize,poster=T,three=F)
             )
      }
  
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
  
      # draw scale & north arrow
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
      drawScale(region)
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
      northarrow(region)



      dev.off()
    
      if(fileName=="out/maps/slp09c_CH_slope_09c_hyb_three.png" |
         fileName=="out/maps/geostats_weighting1_CH_w0_three.png") {
          png(legName, width = 450, height=pxHeight)
          par(mfrow=c(1,1),  cex=CEX,   mar=c(2,2,2,2),   bg=bgcol, col=txtcol)
          plot.new()
          plot(modelRast, legend.only=T,
               col=modelCol, zlim=zlims,
               legend.width=1, legend.shrink=1,
               axis.args=list(at    = zscale,
                              labels= zscale,
                              cex.axis=1),
               legend.args=list(text=switch(mapID, 
                                            slope_09c_hyb = 'log_{10}(topographic slope)',
                                            w0            = "weighting factor"),
                                side=4,
                                #font=2,
                                line=3.0,
                                cex=3)
               ,maxpixels = maxpix
               ,ext=extents
               ,smallplot=c(0.32,0.48,0.075,0.925)
          )
          dev.off()
      }
    } # for mapSize
  } # for mapID
} # for region
