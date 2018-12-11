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
load("Rdata/vspr.Rdata")

mapIDvec <- c("hillshade")  

regionVec <- names(extList)
# regionVec <- c("NEL", "NELURB", "NZ")
# regionVec <- c("NZ")

# output
pxWidth         = 3300
pxHeight        = 4200
maxpix          = 1e7  # plot() for rasters is complicated. Using maxpix with massive
                       # rasters doesn't NECESSARILY result in quicker plotting (1e7
                       # was not working in my experience) but if raster is resampled
                       # beforehand, maxpixels works as expected and doesn't cause
                       # much/any delay.
makeMask        = F

# these are the current resampling dimensions. I chose these by measuring map pane size manually for the 
# chosen raster output size, 3300x4200.
rasterX <- 2698
rasterY <- 3632


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
modelCol <- colorRampPalette(GMTpaletteHex)(nModCol)


# random plotting stuff
nGrayCol <- 100
bgcol = rgb(1,1,1)
txtcol = rgb(0,0,0)
margins <- c(7,7,7,7)
CEX = 3
zlims <- c(0,4000)
zscale <- seq(0,4000,250)


for(region in regionVec) {
  hillshade        <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_hillshadeA_NZGD00.tif",rasterX,rasterY,region))
  isOcean          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isOcean_NZGD00.tif",rasterX,rasterY,region))
  isWater          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isWater_NZGD00.tif",rasterX,rasterY,region))
  isIce            <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isIce_NZGD00.tif",rasterX,rasterY,region))
  
  for (mapID in mapIDvec) {
      
    
    
    extents <- extList[[region]]
    
    # # only plot the Vs30 points contained within in the raster extents
    coordsD <- coordinates(vsSubset)
    coX <- coordsD[,1]
    coY <- coordsD[,2]
    exM <- extents
    whichVs <- which((coX >= exM[1]) &
                       (coX <= exM[2]) &
                       (coY >= exM[3]) &
                       (coY <= exM[4]))
    vs2 <- vsSubset[whichVs,]
    vs <- vs2
    coordsD <- coordinates(vs)
    vsD <- vs$Vs30
    Dcol <- modelCol[as.numeric(cut(vsD,breaks=nModCol))] # choose colors
    
    LARGE <- 1.1
    MED   <- 1
    SMALL <- 0.8
    
    sizes <- rep(LARGE, nrow(vs))
    
    q1s <- vs$QualityFlag=="Q1"
    q2s <- vs$QualityFlag=="Q2"
    q3s <- vs$QualityFlag=="Q3"
    used <- vs$QualityFlag!="Q3"
    McG <- vs$DataSource=="McGannCPT"
    Wth <- vs$DataSource=="Wotherspoon201711"
    
    sizes[q3s] <- SMALL
    sizes[q2s] <- MED
    
    shapes <- vector(length=nrow(vs))
    shapes[q1s] <- 23
    shapes[q2s] <- 22
    shapes[q3s] <- 21
    shapes[McG] <- 24
    shapes[Wth] <- 25
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    fileName =  paste0("out/maps/hillshade_",region,"_",mapID,".png")

    png(fileName, width = pxWidth, height = pxHeight)



    # first par
    par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, bty="n")

    # plot hillshade first, then overlay
    plot(hillshade,
         col = grey.colors(nGrayCol, start=0, end=1), legend=F,
         xlab="NZGD2000 datum (metres)", ylab="NZGD2000 datum (metres)",
         main=switch(mapID, AhdiAK = "Geology-only Vs30 model: AhdiAK",
                            AhdiAK_noQ3 = "Geology-only Vs30 model: AhdiAK_noQ3 (updated with noQ3 data)",
                            AhdiAK_noQ3_hyb09c = "Hybrid model: AhdiAK_noQ3_hyb09c",
                            AhdiAK_noQ3_hyb09c_KRG  = "Kriging-interpolated hybrid model: AhdiAK_noQ3_hyb09c_KRG",
                            AhdiAK_noQ3_hyb09c_MVN  = "MVN (multivariate normal) interpolated hybrid model: AhdiAK_noQ3_hyb09c_MVN",
                            YongCA = "Terrain-based model: YongCA",
                            YongCA_noQ3 = "Terrain-based model: YongCA_noQ3 (updated with noQ3 data)",
                            AhdiYongWeighted1 = "Weighted combination of AhdiAK_noQ3_hyb09c and YongCA_noQ3: AhdiYongWeighted1",
                            AhdiYongWeighted1_MVN = "Multivariate normal interpolated Vs30 model: AhdiYongWeighted1_MVN",
                            AhdiYongWeighted1_KRG = "Kriging interpolated Vs30 model"
                            ),
         sub=switch(mapID,
                    AhdiAK = "Geology based on Ahdi (2017, Alaska); no bayesian updating",
                    AhdiAK_noQ3 = "Geology based on Ahdi (2017, Alaska); bayesian updating with noQ3 (all data except Kaiser Q3) NZ data",
                    AhdiAK_noQ3_hyb09c = "Hybrid model based on AhdiAK_noQ3, with slope-based modifications",
                    YongCA = "Iwahashi & Pike's (2007) terrain categories with Yong et al.'s (2012) California-based Vs30 values",
                    YongCA_noQ3 = "Iwahashi & Pike's (2007) terrain categories with Yong et al.'s (2012) California-based Vs30 values; Bayesian updating with  noQ3 (all data except Kaiser Q3 data)",
                    ""),
         cex=CEX, # cex = symbol size for scatter plots
         cex.sub = 0.8
         ,maxpixels = maxpix
         ,ext=extents
         )




    par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)


    # Add water, ocean, ice
    par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
    plot(isOcean , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
         ,ext=extents
         )

    par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
    plot(isWater , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
         ,ext=extents
         )

    par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
    plot(isIce   , col = rgb(1.0, 1.0, 1), alpha=0.8, add=T,  legend=F ,maxpixels = maxpix
         ,ext=extents
         )

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

    par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)


    
    # # plotting Vs30 points...................
    # 
    # 
    # # plot points
    # # Only show Q3 points for AhdiAK geology model...
    # if(identical(mapID, "AhdiAK")) {
    #   points(x = coordsD[q3s,], pch=shapes[q3s], bg=Dcol[q3s], col="black", cex=sizes[q3s])}
    # points(x = coordsD[McG,], pch=shapes[McG], bg=Dcol[McG], col="black", cex=sizes[McG])
    # points(x = coordsD[Wth,], pch=shapes[Wth], bg=Dcol[Wth], col="black", cex=sizes[Wth])
    # points(x = coordsD[q2s,], pch=shapes[q2s], bg=Dcol[q2s], col="black", cex=sizes[q2s])
    # points(x = coordsD[q1s,], pch=shapes[q1s], bg=Dcol[q1s], col="black", cex=sizes[q1s])
    # 
    # # legend....
    # # for AhdiAK, include Q3. For all other maps, don't.
    # switch(mapID,
    #        AhdiAK = {
    #          legend(x="topleft",
    #                 legend = c("Kaiser_Q1","Kaiser_Q2","Kaiser_Q3 (not used)","McGann (resampled)","Wotherspoon"),
    #                 pt.bg = c(modelCol[1], modelCol[22], modelCol[44], modelCol[11], modelCol[33]), # arbitrary colors
    #                 pch=c(23,22,21,24,25),
    #                 pt.cex=c(LARGE, MED, SMALL, LARGE, LARGE),
    #                 title = "Vs30 data: quality designation"#,
    #                 #horiz=T
    #          )
    #        },
    #        {
    #          legend(x="topleft",
    #                 legend = c("Kaiser_Q1","Kaiser_Q2","McGann (resampled)","Wotherspoon"),
    #                 pt.bg = c(modelCol[1], modelCol[22], modelCol[11], modelCol[33]), # arbitrary colors
    #                 pch=c(23,22,24,25),
    #                 pt.cex=c(LARGE, MED, LARGE, LARGE),
    #                 title = "Vs30 data: quality designation"#,
    #                 #horiz=T
    #          )
    #        }
    # )
    
    dev.off()
  
  } # for mapID
} # for region
