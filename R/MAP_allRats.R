# This script is based on QCAMmaps_Vs30_allNZgeo_tryResample.R
#
# ALL ratios:
#   *  AhdiBayes                AhdiAK_noQ3                             over       AhdiAK
#   *  AhdiHyb                  AhdiAK_noQ3_hyb09c                      over       AhdiAK_noQ3
#   *  AhdiBOTH                 AhdiAK_noQ3_hyb09c                      over       AhdiAK
#   *  AhdiBOTHsigma            AhdiAK_noQ3_hyb09c_sigma                MINUS      AhdiAK_sigma   !!! SIGMA IS ALREADY LOGNORMAL !!!
#   *  YongBayes                YongCA_noQ3                             over       YongCA
#   *  YongBayesSigma           YongCA_noQ3_sigma                       MINUS      YongCA_sigma   !!! SIGMA IS ALREADY LOGNORMAL !!!
#   *  YongAhdi                 YongCA_noQ3                             over       AhdiAK_noQ3_hyb09c
#   *  YongAhdiPrior            YongCA                                  over       AhdiAK
#   *  KRGweighted_fullKRG      AhdiYongWeighted1_KRG_slopeWeighting1   over       AhdiYongWeighted1_KRG
#   *  MVNweighted_fullMVN      AhdiYongWeighted1_MVN_slopeWeighting1   over       AhdiYongWeighted1_MVN
#   *  fullMVN_MVNcrp1.5        AhdiYongWeighted1_MVN                   over       AhdiYongWeighted1_MVN_crp1.5
#   *  MVNslpWtd_MVNcrp1.5      AhdiYongWeighted1_MVN_slopeWeighting1   over       AhdiYongWeighted1_MVN_crp1.5
#   *  MVN_noiseT_crp0         AhdiYongWeighted1_MVN_crp0_noisyT       over       AhdiYongWeighted1_MVN_crp0_noisyF
#   *  MVN_noiseT_crp1.5       AhdiYongWeighted1_MVN_crp1.5_noisyT     over       AhdiYongWeighted1_MVN_crp1.5_noisyF
#
# Added 2018-10 - now geostatistics are done on the constituent input models rather than on the AhdiYongWeighted model.
#   *  AhdiMVN_KRGv6              AhdiAK_noQ3_hyb09c_MVN_v6_T_1.5         over       AhdiAK_noQ3_hyb09c_KRG_v6
#   *  YongMVN_KRGv7              YongCA_noQ3_MVN_v7_T_1.5                over       YongCA_noQ3_KRG_v7
#   *  AhdiMVN_KRGv8              AhdiAK_noQ3_hyb09c_MVN_v8_T_1.5         over       AhdiAK_noQ3_hyb09c_KRG_v8
#   *  YongMVN_KRGv9              YongCA_noQ3_MVN_v9_T_1.5                over       YongCA_noQ3_KRG_v9
#
# (All map outputs are log(ratio), not the ratio itself.)



rm(list=ls())
setwd("~/VsMap")


# library(viridis)
library(RColorBrewer)
library(raster)
source("R/functions.R")
source("R/MAP_NZGD00_regions.R") # contains regions
source("R/MAPparams.R")
load("Rdata/vspr.Rdata")

mapIDvec <- mapIDvec_rats # from MAPparams.R




# getVs30 points
vsprS <- subsetVsPoints(vspr)
vsSubset <- vsprS$allData





# random plotting stuff
nGrayCol <- 100
bgcol = rgb(1,1,1)
txtcol = rgb(0,0,0)
CEX = 3


for(region in regionVec) {
  # region <- "CH"
  for(mapSize in mapSizeVec) {
    #mapSize <- "three"

    margins <- switch(mapSize,poster=c(7,7,7,7),three=c(2,2,2,2),six=c(2,2,2,2))
    pxWidth         = switch(mapSize,poster=3300,three=1113,six=1972)
    pxHeight        = switch(mapSize,poster=4200,three=1113,six=1972)
    # these are the current resampling dimensions. I chose these by measuring map pane size manually for the
    # chosen raster output size, 3300x4200.
    rasterX <- switch(mapSize,poster=2698,three=900,six=1800)
    rasterY <- switch(mapSize,poster=3632,three=900,six=1800)
    hillshade        <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_hillshadeA_NZGD00.tif",rasterX,rasterY,region))
    isOcean          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isOcean_NZGD00.tif",rasterX,rasterY,region))
    isWater          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isWater_NZGD00.tif",rasterX,rasterY,region))
    isIce            <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isIce_NZGD00.tif",rasterX,rasterY,region))

    for (mapID in mapIDvec) {
      # mapID <- "AhdiHyb"
      # mapID <- "YongAhdiPrior"
      
      
      # for special case - added in response to BB request for ratio scale (rather than log scale):
      realRatio <- mapID %in% ratioNotLog

      numerator <- raster(sprintf(switch(mapID,
                                          AhdiBayes      = "~/big_noDB/models/RESAMP_%04dx%04d_%s_geo_NZGD00_allNZ_AhdiAK_noQ3.tif",
                                          AhdiHyb        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_hyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                                          AhdiBOTH       = "~/big_noDB/models/RESAMP_%04dx%04d_%s_hyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                                          AhdiBOTHsigma  = "~/big_noDB/models/RESAMP_%04dx%04d_%s_sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                                          YongBayes      = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_noQ3.tif",
                                          YongBayesSigma = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_noQ3_sigma.tif",
                                          YongAhdi       = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_noQ3.tif",
                                          YongAhdiPrior  = "~/big_noDB/models/RESAMP_%04dx%04d_%s_Yong2012_Cali_Vs30.tif",
                                          KRGweighted_fullKRG   = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_KRG_slopeWeighting1_Vs30_v5.tif",
                                          MVNweighted_fullMVN   = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_MVN_slopeWeighting1_Vs30.tif",
                                          fullMVN_MVNcrp1.5     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
                                          MVNslpWtd_MVNcrp1.5   = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_MVN_slopeWeighting1_Vs30.tif",
                                          MVN_noiseT_crp0      = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
                                          MVN_noiseT_crp1.5    = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
                                          AhdiMVN_KRGv6           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif",
                                          YongMVN_KRGv7           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif",
                                          AhdiMVN_KRGv8           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v8_crp1.5.tif",
                                          YongMVN_KRGv9           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v9_crp1.5.tif"
      ), rasterX, rasterY, region))
      denominator <- raster(sprintf(switch(mapID,
                                         AhdiBayes      = "~/big_noDB/models/RESAMP_%04dx%04d_%s_geo_NZGD00_allNZ_AhdiAK.tif",
                                         AhdiHyb        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_geo_NZGD00_allNZ_AhdiAK_noQ3.tif",
                                         AhdiBOTH       = "~/big_noDB/models/RESAMP_%04dx%04d_%s_geo_NZGD00_allNZ_AhdiAK.tif",
                                         AhdiBOTHsigma  = "~/big_noDB/models/RESAMP_%04dx%04d_%s_sig_NZGD00_allNZ_AhdiAK.tif",
                                         YongBayes      = "~/big_noDB/models/RESAMP_%04dx%04d_%s_Yong2012_Cali_Vs30.tif",
                                         YongBayesSigma = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_sigma.tif",
                                         YongAhdi       = "~/big_noDB/models/RESAMP_%04dx%04d_%s_hyb_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                                         YongAhdiPrior  = "~/big_noDB/models/RESAMP_%04dx%04d_%s_geo_NZGD00_allNZ_AhdiAK.tif",
                                         KRGweighted_fullKRG    = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_Vs30_NZGD00_allNZ_AhdiYongWeighted1_v5.tif",
                                         MVNweighted_fullMVN    = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
                                         fullMVN_MVNcrp1.5      = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
                                         MVNslpWtd_MVNcrp1.5    = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
                                         MVN_noiseT_crp0       = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif",
                                         MVN_noiseT_crp1.5     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif",
                                         AhdiMVN_KRGv6           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
                                         YongMVN_KRGv7           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_Vs30_NZGD00_allNZ_YongCA_noQ3_v7.tif",
                                         AhdiMVN_KRGv8           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v8.tif",
                                         YongMVN_KRGv9           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_Vs30_NZGD00_allNZ_YongCA_noQ3_v9.tif"
      ), rasterX, rasterY, region))

      if(mapID=="AhdiBOTHsigma" || mapID=="YongBayesSigma") {
        modelRast <- numerator - denominator   # for the sigma plot ONLY... sigma is already lognormal!
      } else {
        modelRast <- log(numerator) - log(denominator)  
      }
      
      print(sprintf("%s  -  Max: %06.3f Min: %06.3f",mapID, maxValue(modelRast),minValue(modelRast)))

      extents <- extList[[region]]


      zlims <- switch(mapID, AhdiBayes = c(-1.2,1.2),
                             AhdiHyb   = c(-0.12,0.12),
                             AhdiBOTH  = c(-1.2,1.2),
                             AhdiBOTHsigma  = c(-0.6,0.6),
                             YongBayes = c(-1.2,1.2),
                             YongBayesSigma = c(-0.6,0.6),
                             YongAhdi  = c(-1.5,1.5),
                             YongAhdiPrior  = c(-1.5,1.5),
                             KRGweighted_fullKRG    = c(-1.2,1.2),
                             MVNweighted_fullMVN    = c(-1.2,1.2),
                             fullMVN_MVNcrp1.5      = c(-1.2,1.2),
                             MVNslpWtd_MVNcrp1.5    = c(-1.2,1.2),
                             MVN_noiseT_crp0       = c(-1.2,1.2),
                             MVN_noiseT_crp1.5     = c(-1.2,1.2),
                             AhdiMVN_KRGv6           = c(-0.8,0.8),
                             YongMVN_KRGv7           = c(-0.8,0.8),
                             AhdiMVN_KRGv8           = c(-1.2,1.2),
                             YongMVN_KRGv9           = c(-1.2,1.2)
      )
      # zscale <- seq(zlims[1],zlims[2],switch(mapID, AhdiBayes = 0.2,
      #                                               AhdiHyb = 0.02,
      #                                               YongBayes = 0.2,
      #                                               YongAhdi = 0.5,
      #                                               KRGweighted_fullKRG = 0.2,
      #                                               MVNweighted_fullMVN = 0.2,
      #                                               fullMVN_MVNcrp1.5   = 0.1,
      #                                               MVNslpWtd_MVNcrp1.5 = 0.1,
      #                                               MVN_noiseT_crp0    = 0.1,
      #                                               MVN_noiseT_crp1.5  = 0.1
      #               ))
      
      
      zscale <- seq(zlims[1],zlims[2],length.out = 13)
      if(realRatio) {
        zscaleLabels <- sprintf("% 2.2f",exp(zscale))
      } else {
        zscaleLabels <- switch(mapID, AhdiHyb = sprintf("% 2.2f",zscale),
                                    sprintf("% 2.1f",zscale))
      }

      print(zscale)
      print(zscaleLabels)


      # nModCol     <- 100
      # modelCol        = colorRampPalette(brewer.pal(11,"Spectral"))(nModCol)
      # modelCol        = brewer.pal(7,"Spectral")

      nModCol = (length(zscaleLabels)+1)/2
      modelCol        = colorRampPalette(brewer.pal(11,"RdBu"))(nModCol)
      if(grayScaleWhiteWater) {
        modelCol <- darkenRGB(modelCol)
      }
      # modelCol <- c("#B24848",
      #               "#CD7B7C",
      #               "#E3ACAC",
      #               "#F4D9D9",
      #               "#FFFFFF",
      #               "#DDDEF2",
      #               "#B5B6E2",
      #               "#8A8CD0",
      #               "#5C60C1")
      # nModCol     <- length(modelCol)


      # Pick descriptive string for plot title & axis
      dataStr = switch(mapID,
                              AhdiBayes    = "log(posterior/prior)",
                              AhdiHyb      = "Geology & slope / posterior geology", # realRatio
                              AhdiBOTH     = "Geology & slope / prior geology", # realRatio
                              AhdiBOTHsigma= "(Geology & slope) - (prior geology) σ",
                              YongBayes    = "posterior/prior", # realRatio
                              YongBayesSigma="posterior σ - prior σ",
                              YongAhdi     = "posterior terrain / slope + geology", # realRatio
                             YongAhdiPrior = "log(terrain-based prior / geology-based prior)",
                       KRGweighted_fullKRG = "log(kriging, slope-weighted / kriging)",
                       MVNweighted_fullMVN = "log(MVN, slope-weighted / MVN)",
                       fullMVN_MVNcrp1.5   = "log(MVN, crp=0.0 / MVN, crp=1.5)",
                       MVNslpWtd_MVNcrp1.5 = "log(MVN, slope-weighted / MVN, crp=1.5)",
                       MVN_noiseT_crp0    = "log(MVN, noisy inputs / MVN, precise inputs) for crp=0",
                       MVN_noiseT_crp1.5  = "log(MVN, noisy inputs / MVN, precise inputs) for crp=1.5",
                       AhdiMVN_KRGv6        = "MVN model / kriged model", # realRatio
                       YongMVN_KRGv7        = "MVN model / kriged model", # realRatio
                       AhdiMVN_KRGv8        = "MVN model / kriged model", # realRatio
                       YongMVN_KRGv9        = "MVN model / kriged model" # realRatio
      )




      fileName =  paste0("out/maps/ratio_",region,"_",mapID,"_",mapSize,".png")
      legName  =  paste0("out/maps/ratio_",mapID,"_",mapSize,"_legend.png")

      png(fileName, width = pxWidth, height = pxHeight)



      # first par
      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, bty="n")

      # plot hillshade first, then overlay
      plot(hillshade,
           col = grey.colors(nGrayCol, start=0, end=1), legend=F,
           xlab="NZGD2000 datum (metres)", ylab="NZGD2000 datum (metres)",
           main=switch(mapSize,three=NULL,poster=dataStr),
           sub=switch(mapID,
                      AhdiBayes     = "",
                      AhdiHyb       = "Hybrid model based on slopes from 09c (9 arcseconds) resolution DEM sampling",
                      AhdiBOTH      = "Hybrid model based on slopes from 09c (9 arcseconds) resolution DEM sampling",
                      AhdiBOTHsigma = "Hybrid model based on slopes from 09c (9 arcseconds) resolution DEM sampling",
                      YongBayes     = "YongCA model: priors from Yong (2012) for California; conditioning on noQ3 NZ dataset",
                      YongBayesSigma= "YongCA model: priors from Yong (2012) for California; conditioning on noQ3 NZ dataset",
                      YongAhdi      = "Comparing 'final' versions of hybrid AhdiAK and posterior Yong model: log(Yong2012_noQ3/AhdiAK_noQ3_hyb09c",
                      YongAhdiPrior = "Comparing prior geology and terrain models: log(Yong2012/AhdiAK)",
                      ""
                      ),
           cex=2, # cex = symbol size for scatter plots
           cex.sub = 0.8
           ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           )


      # Add water, ocean, ice
      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isOcean , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           )

      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isWater , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           )

      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isIce   , col = rgb(1.0, 1.0, 1), alpha=0.8, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
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
           ,axes=switch(mapSize,poster=T,three=F,six=F)
           )
      if(mapSize=="poster") { # only do color bar for poster.
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
        plot(modelRast, legend.only=T,
             col=modelCol, zlim=zlims,
             legend.width=1, legend.shrink=0.75,
             axis.args=list(at    = zscale,
                            labels= zscaleLabels,
                            cex.axis=1),
             legend.args=list(text=dataStr,
                              side=4,
                              #font=2,
                              line=3.5,
                              cex=3)
             ,maxpixels = maxpix
             ,ext=extents
             ,axes=switch(mapSize,poster=T,three=F,six=F)
             )
      }
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)

      # if(makeMask) {
      #   # add uncertainty-mask
      #   # must be done incrementally (not sure how to set partial alpha per pixel!)
      #   # There is some fine-tuning to be done with the masking. To help with this, some
      #   # control parameters:
      #   nLevels <- 10
      #   alphaLevelSeq <- seq(from=0, to=1, length.out=nLevels)
      #   alphaExp <- 1.8
      #   grayC <- rgb(0.8,0.8,0.8)
      #   for(alphaLevel in alphaLevelSeq) {
      #     thisMask <- MVNmask>alphaLevel
      #     thisMask[thisMask==0] <- NA
      #     par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      #     plot(thisMask, col=grayC, alpha=alphaLevel^alphaExp, add=T, legend=F)
      #   }
      # }

      par(mfrow=c(1,1),  cex=3,   mar=margins,   bg=bgcol, col=txtcol, new=T)



      # plotting Vs30 points...................


      # # plot points
      # # Only show Q3 points for AhdiAK geology model...
      # if(identical(mapID, "AhdiAK")) {
      #   points(x = coordsD[q3s,], pch=shapes[q3s], bg=Dcol[q3s], col="black", cex=sizes[q3s])}
      # points(x = coordsD[McG,], pch=shapes[McG], bg=Dcol[McG], col="black", cex=sizes[McG])
      # points(x = coordsD[Wth,], pch=shapes[Wth], bg=Dcol[Wth], col="black", cex=sizes[Wth])
      # points(x = coordsD[q2s,], pch=shapes[q2s], bg=Dcol[q2s], col="black", cex=sizes[q2s])
      # points(x = coordsD[q1s,], pch=shapes[q1s], bg=Dcol[q1s], col="black", cex=sizes[q1s])

      # # legend....
      # # for AhdiAK, include Q3. For all other maps, don't.
      # switch(mapID,
      #        AhdiAK = {
      #          legend(x="topleft",
      #                 legend = c("Kaiser_Q1","Kaiser_Q2","Kaiser_Q3 (not used)","McGann (resampled)","Wotherspoon"),
      #                 pt.bg = c(modelCol[1], modelCol[22], modelCol[44], modelCol[11], modelCol[33]), # arbitrary colors
      #                 pch=c(23,22,21,24,25),
      #                 pt.cex=c(1.1, 1, 0.8, 1.1, 1.1),
      #                 title = "Vs30 data: quality designation"#,
      #                 #horiz=T
      #          )
      #        },
      #        {
      #          legend(x="topleft",
      #                 legend = c("Kaiser_Q1","Kaiser_Q2","McGann (resampled)","Wotherspoon"),
      #                 pt.bg = c(modelCol[1], modelCol[22], modelCol[11], modelCol[33]), # arbitrary colors
      #                 pch=c(23,22,24,25),
      #                 pt.cex=c(1.1, 1, 1.1, 1.1),
      #                 title = "Vs30 data: quality designation"#,
      #                 #horiz=T
      #          )
      #        }
      # )


      # draw scale & north arrow
      if(!(publication &
           mapID %in% c(
             "AhdiBOTH",
             "YongBayes",
             "AhdiBOTHsigma",
             "YongBayesSigma",
             "AhdiMVN_KRGv6"
           ))) {
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
        drawScale(region)
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
        northarrow(region)
      }
      
      dev.off()

      if(region=="CH" & mapSize!="poster") { # just need one legend per map type
        png(legName, width = 450, height = pxHeight)
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol)
        plot.new()
        plot(modelRast, legend.only=T,
             col=modelCol, zlim=zlims,
             legend.width=1, legend.shrink=0.75,
             axis.args=list(at    = zscale,
                            labels= zscaleLabels,
                            cex.axis=1),
             legend.args=list(text=dataStr,
                              side=4,
                              #font=2,
                              line=switch(mapID,
                                          AhdiBayes=3,
                                          AhdiHyb=3,
                                          AhdiBOTH=3,
                                          YongAhdi=3,
                                          YongBayes=3,
                                          AhdiMVN_KRGv6=3,
                                          YongMVN_KRGv7=3,
                                          3),
                              # line=3,
                              cex=3)
             ,maxpixels = maxpix
             ,ext=extents
             ,smallplot=c(0.32,0.48,0.075,0.925)
        )
        dev.off()
      }


    } # for mapID
  } # for mapSize
} # for region
