# MAP_allSigma.R
#
# Makes map of lognormal sigma/standard deviation for each model.





rm(list=ls())
setwd("~/VsMap")

library(viridis)
library(raster)
library(colorspace)
source("R/functions.R")
source("R/MAP_NZGD00_regions.R") # contains regions
source("R/MAPparams.R")
load("Rdata/vspr.Rdata")

mapIDvec <- mapIDvec_sigma # from MAPparams

# getVs30 points
vsprS <- subsetVsPoints(vspr)
vsSubset <- vsprS$allData


# color stuff
nModCol     <- 100
modelCol        = viridis(n = nModCol,  begin = 0.05, end = 1, direction= -1, option = "B")
# modelCol        = rainbow(nModCol, start=0, end=1)

nResCol  <- 7
residCol <- colorspace::diverge_hcl(n = nResCol) # this is a DISCRETE color scale!
nContinuousColors <- 99
residColContinuous <- colorspace::diverge_hcl(n = nContinuousColors) # this is a CONTINUOUS color scale! For the cut() operation and assigning colors to residuals.
rlims <- c(-1.8,1.8)
rscale <- seq(rlims[1],rlims[2],length.out = nResCol)


# random plotting stuff
nGrayCol <- 100
bgcol = rgb(1,1,1)
txtcol = rgb(0,0,0)
CEX = 3
zlims <- c(0,1)
zscale <- seq(0,1,0.1)


for(region in regionVec) {
# for(region in c("CH")) {
  # region <- regionVec[1]   # testing
  
  for(mapSize in mapSizeVec) {
  # for(mapSize in c("six")) { #testing
  # mapSize <- "three" # testing
    pxWidth         = switch(mapSize,poster=3300,three=1113,six=1972)
    pxHeight        = switch(mapSize,poster=4200,three=1113,six=1972)

    # these are the current resampling dimensions. I chose these by measuring map pane size manually for the 
    # chosen raster output size, 3300x4200.
    rasterX <- switch(mapSize,poster=2698,three=900,six=1800)
    rasterY <- switch(mapSize,poster=3632,three=900,six=1800)
    
    margins <- switch(mapSize,poster=c(17,7,7,7),three=c(2,2,2,2),six=c(2,2,2,2))
    
    hillshade        <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_hillshadeA_NZGD00.tif",rasterX,rasterY,region))
    isOcean          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isOcean_NZGD00.tif",rasterX,rasterY,region))
    isWater          <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isWater_NZGD00.tif",rasterX,rasterY,region))
    isIce            <- raster(sprintf("~/big_noDB/models/RESAMP_%04dx%04d_%s_isIce_NZGD00.tif",rasterX,rasterY,region))
    
    
    for (mapID in mapIDvec) {
      # mapID <- mapIDvec[1]  # testing
      
      if(mapID %in% c(
        # special cases: produce non-normalized sigma for kriging models
        # (and NORMALIZED sigma for MVN models)
        "AhdiAK_noQ3_hyb09c_KRG_v6",
        "YongCA_noQ3_KRG_v7",
        "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma",
        "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma"
      )) {
        r1 <- raster(sprintf(switch(mapID,
                                           AhdiAK_noQ3_hyb09c_KRG_v6                                     = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
                                           YongCA_noQ3_KRG_v7                                            = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_YongCA_noQ3_v7.tif",
                                           AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma    = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif",
                                           YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma           = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif"
        ), rasterX, rasterY, region))
        stdv <- raster(sprintf(switch(mapID,
                                      AhdiAK_noQ3_hyb09c_KRG_v6                                  = "~/big_noDB/models/RESAMP_%04dx%04d_%s_sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                                      YongCA_noQ3_KRG_v7                                         = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_noQ3_sigma.tif",
                                      AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = "~/big_noDB/models/RESAMP_%04dx%04d_%s_sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                                      YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_noQ3_sigma.tif"
        ), rasterX, rasterY, region))
        if(mapID %in% c(
          "AhdiAK_noQ3_hyb09c_KRG_v6",
          "YongCA_noQ3_KRG_v7")) {
          modelRast <- r1 * stdv  
        } else if(mapID %in% c(
          "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma",
          "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma"
        )) {
          modelRast <- r1 / stdv  
        }
        
      } else {
        # ALL OTHER cases (i.e. not the special cases for normalizing)
        
        modelRast <- raster(sprintf(switch(mapID,
                                           ## old ## AhdiYongWeighted1_MVN   = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1.tif",
                                           ## old ## AhdiYongWeighted1_KRG   = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_AhdiYongWeighted1.tif",
                                           ## old ## AhdiYongWeighted1_KRG_slopeWeighting1 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_KRG_slopeWeighting1_sigma.tif",
                                           ## old ## AhdiYongWeighted1_MVN_noisyT_minDist0_v2 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v2.tif",
                                           AhdiAK                  = "~/big_noDB/models/RESAMP_%04dx%04d_%s_sig_NZGD00_allNZ_AhdiAK.tif",
                                           AhdiAK_noQ3             = "~/big_noDB/models/RESAMP_%04dx%04d_%s_sig_NZGD00_allNZ_AhdiAK_noQ3.tif",
                                           AhdiAK_noQ3_hyb09c      = "~/big_noDB/models/RESAMP_%04dx%04d_%s_sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif",
                                           YongCA                  = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_sigma.tif",
                                           YongCA_noQ3             = "~/big_noDB/models/RESAMP_%04dx%04d_%s_YongCA_noQ3_sigma.tif",
                                           AhdiYongWeighted1       = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_sigma.tif",
                                           AhdiYongWeighted1_MVN_slopeWeighting1 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeighted1_MVN_slopeWeighting1_sigma.tif",
                                           AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif",
                                           AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp0.0.tif",
                                           AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp1.5 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp1.5.tif",
                                           AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif",
                                           AhdiYongWeighted1_KRG_v4 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_AhdiYongWeighted1_v4.tif",
                                           AhdiYongWeighted1_KRG_v5 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_AhdiYongWeighted1_v5.tif",
                                           AhdiAK_noQ3_hyb09c_KRG_v6_normSigma = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
                                           YongCA_noQ3_KRG_v7_normSigma        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_YongCA_noQ3_v7.tif",
                                           AhdiAK_noQ3_hyb09c_KRG_v8 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v8.tif",
                                           YongCA_noQ3_KRG_v9        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_KRG_stDv_NZGD00_allNZ_YongCA_noQ3_v9.tif",
                                           AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5 = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif",
                                           YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5        = "~/big_noDB/models/RESAMP_%04dx%04d_%s_MVN_stDv_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif",
                                           AhdiYongWeightedMVN                              = "~/big_noDB/models/RESAMP_%04dx%04d_%s_AhdiYongWeightedMVN_nTcrp1.5_sigma.tif"
        ), rasterX, rasterY, region))
      
        
      }
      
      
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
      
      
      # If producing kriged maps, residual is essentially zero.
      # More sensible to show underlying model residuals, NOT
      # the residuals of the kriged/MVN model!
      modelForResidualPoints = switch(mapID,
                                      ## old ## AhdiYongWeighted1_MVN  = "AhdiYongWeighted1",
                                      ## old ## AhdiYongWeighted1_KRG  = "AhdiYongWeighted1",
                                      ## old ## AhdiAK_noQ3_hyb09c_MVN = "AhdiAK_noQ3_hyb09c",
                                      ## old ## AhdiAK_noQ3_hyb09c_KRG = "AhdiAK_noQ3_hyb09c",
                                      ## old ## AhdiYongWeighted1_KRG_slopeWeighting1 = "AhdiYongWeighted1",
                                      ## old ## AhdiYongWeighted1_MVN_noisyT_minDist0_v2 = "AhdiYongWeighted1",
                                      AhdiAK                 = "AhdiAK",
                                      AhdiAK_noQ3            = "AhdiAK_noQ3",
                                      AhdiAK_noQ3_hyb09c     = "AhdiAK_noQ3_hyb09c",
                                      YongCA                 = "YongCA",
                                      YongCA_noQ3            = "YongCA_noQ3",
                                      AhdiYongWeighted1      = "AhdiYongWeighted1",
                                      AhdiYongWeighted1_MVN_slopeWeighting1 = "AhdiYongWeighted1",
                                      AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5 = "AhdiYongWeighted1",
                                      AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0 = "AhdiYongWeighted1",
                                      AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp1.5 = "AhdiYongWeighted1",
                                      AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0 = "AhdiYongWeighted1",
                                      AhdiYongWeighted1_KRG_v4  = "AhdiYongWeighted1",
                                      AhdiYongWeighted1_KRG_v5  = "AhdiYongWeighted1",
                                      AhdiAK_noQ3_hyb09c_KRG_v6_normSigma  = "AhdiAK_noQ3_hyb09c",
                                      YongCA_noQ3_KRG_v7_normSigma         = "YongCA_noQ3",
                                      AhdiAK_noQ3_hyb09c_KRG_v6            = "AhdiAK_noQ3_hyb09c",
                                      YongCA_noQ3_KRG_v7                   = "YongCA_noQ3",
                                      AhdiAK_noQ3_hyb09c_KRG_v8  = "AhdiAK_noQ3_hyb09c",
                                      YongCA_noQ3_KRG_v9         = "YongCA_noQ3",
                                      AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = "AhdiAK_noQ3_hyb09c",
                                      YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma = "YongCA_noQ3",
                                      AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5 = "AhdiAK_noQ3_hyb09c",
                                      YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5 = "YongCA_noQ3",
                                      AhdiYongWeightedMVN = "AhdiYongWeighted1" # not used...
      )
      
      # for MVN with fuzzy inputs, it is illuminating to plot points with
      # color corresponding to measurement uncertainty! this is done here...
      MVNnoisy <- identical("AhdiYongWeighted1_MVN_noisyT",
                            substr(mapID,1,28)) # true or false
      if(MVNnoisy) {
        vsD <- as.data.frame(vs)[,"lnMeasUncer"]
        Dcol <- modelCol[as.numeric(cut(vsD,breaks=seq(
          zlims[1],zlims[2],length=nModCol+1)))] # choose colors
      } else {
        vsD <- as.data.frame(vs)[,paste0("res_", modelForResidualPoints)]
        Dcol <- residColContinuous[as.numeric(cut(vsD,breaks=seq(
          rlims[1],rlims[2],length=nContinuousColors+1)))] # choose colors
      }
      
      
      if(mapSize=="three") {
        LARGE <- 0.6
        MED   <- 0.5
        SMALL <- 0.4 } else {
          if(mapSize=="poster" || mapSize=="six") {
            LARGE <- 1.1
            MED   <- 1
            SMALL <- 0.8
      }}
      
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
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      fileName =  paste0("out/maps/sigma_",region,"_",mapID,"_",mapSize,".png")
      legName  =  paste0("out/maps/sigma_",mapSize,"_legend")
      
      png(fileName, width = pxWidth, height = pxHeight)
      
      
      
      # first par
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, bty="n")
      
      # plot hillshade first, then overlay
      plot(hillshade,
           col = grey.colors(nGrayCol, start=0, end=1), legend=F,
           xlab="NZGD2000 datum (metres)", ylab="NZGD2000 datum (metres)",
           main=switch(mapSize,three=NULL,six=NULL,poster=switch(mapID,
                       ## old ## AhdiYongWeighted1_MVN = "Multivariate normal interpolated model σ",
                       ## old ## AhdiYongWeighted1_KRG = "Kriging interpolated model σ",
                       ## old ## AhdiYongWeighted1_KRG_slopeWeighting1 = "Kriging interpolated model σ, with slope-based weighting applied",
                       ## old ## AhdiYongWeighted1_MVN_noisyT_minDist0_v2 = "Multivariate normal interpolated model σ",
                       AhdiAK = "Geology-only model σ: AhdiAK",
                       AhdiAK_noQ3 = "Geology-only model σ: AhdiAK_noQ3 (updated with noQ3 data)",
                       AhdiAK_noQ3_hyb09c = "Hybrid model σ: AhdiAK_noQ3_hyb09c",
                       YongCA = "Terrain-based model σ: YongCA",
                       YongCA_noQ3 = "Terrain-based model σ: YongCA_noQ3 (updated with noQ3 data)",
                       AhdiYongWeighted1 = "Combined σ for weighted model: AhdiAK_noQ3_hyb09c and YongCA_noQ3",
                       AhdiYongWeighted1_MVN_slopeWeighting1 = "MVN interpolated model σ, with slope-based weighting applied",
                       AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5 = "Multivariate normal interpolated model σ (covariance reduction parameter = 1.5)",
                       AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0 = "Multivariate normal interpolated model σ (covariance reduction parameter = 0.0)",
                       AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp1.5 = "Multivariate normal interpolated model σ (covariance reduction parameter = 1.5)",
                       AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0 = "Multivariate normal interpolated model σ (covariance reduction parameter = 0.0)",
                       AhdiYongWeighted1_KRG_v4 = "Kriging interpolated model σ",
                       AhdiYongWeighted1_KRG_v5 = "Kriging interpolated model σ",
                       AhdiAK_noQ3_hyb09c_KRG_v6_normSigma  = "Kriging interpolated model σ (normalised)",
                       YongCA_noQ3_KRG_v7_normSigma         = "Kriging interpolated model σ (normalised)",
                       AhdiAK_noQ3_hyb09c_KRG_v6            = "Kriging interpolated model σ",
                       YongCA_noQ3_KRG_v7                   = "Kriging interpolated model σ",
                       AhdiAK_noQ3_hyb09c_KRG_v8  = "Kriging interpolated model σ",
                       YongCA_noQ3_KRG_v9         = "Kriging interpolated model σ",
                       AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = "Multivariate normal interpolated model σ (covariance reduction parameter = 1.5) (normalised)",
                       YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma = "Multivariate normal interpolated model σ (covariance reduction parameter = 1.5) (normalised)",
                       AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5 = "Multivariate normal interpolated model σ (covariance reduction parameter = 1.5)",
                       YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5 = "Multivariate normal interpolated model σ (covariance reduction parameter = 1.5)",
                       AhdiYongWeightedMVN = "Final (50/50 weighted) model"
                       )),
           sub=switch(mapID,
                      ## old ## AhdiYongWeighted1_MVN_noisyT_minDist0_v2 = "variogram v2  -  noise=T   -   minDist=0km",
                      AhdiAK = "Geology based on Ahdi (2017, Alaska); no bayesian updating",
                      AhdiAK_noQ3 = "Geology based on Ahdi (2017, Alaska); bayesian updating with noQ3 (all data except Kaiser Q3) NZ data",
                      AhdiAK_noQ3_hyb09c = "Hybrid model based on AhdiAK_noQ3, with slope-based modifications",
                      YongCA = "Iwahashi & Pike's (2007) terrain categories with Yong et al.'s (2012) California-based Vs30 values. Sigma values are assumed.",
                      YongCA_noQ3 = "Iwahashi & Pike's (2007) terrain categories with Yong et al.'s (2012) California-based Vs30 values; Bayesian updating with  noQ3 (all data except Kaiser Q3 data)",
                      AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5 = "variogram v3  -  noise=T   -   minDist=0km",
                      AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0 = "variogram v3  -  noise=T   -   minDist=0km",
                      AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp1.5 = "variogram v3  -  noise=F   -   minDist=0km",
                      AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0 = "variogram v3  -  noise=F   -   minDist=0km",
                      AhdiYongWeighted1_KRG_v4 = "variogram v4",
                      AhdiYongWeighted1_KRG_v5 = "variogram v5",
                      AhdiAK_noQ3_hyb09c_KRG_v6_normSigma  = "variogram v6",
                      YongCA_noQ3_KRG_v7_normSigma         = "variogram v7",
                      AhdiAK_noQ3_hyb09c_KRG_v6            = "variogram v6",
                      YongCA_noQ3_KRG_v7                   = "variogram v7",
                      AhdiAK_noQ3_hyb09c_KRG_v8  = "variogram v8",
                      YongCA_noQ3_KRG_v9         = "variogram v9",
                      AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma = "variogram v6",
                      YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma = "variogram v7",
                      AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5 = "variogram v6",
                      YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5 = "variogram v7",
                      ""),
           cex=CEX, # cex = symbol size for scatter plots
           cex.sub = 0.8
           ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
      )
      
  
  
  
      # Add water, ocean, ice
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isOcean , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
      )
  
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isWater , col = waterCol, alpha=1, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
      )
  
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(isIce   , col = rgb(1.0, 1.0, 1), alpha=0.8, add=T,  legend=F ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
      )
      
      
      # map
      
      par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
      plot(modelRast, col = modelCol, zlim=zlims,
           #xaxt=seq(from=130, to=180, by=1)*1e4, yaxt='n',
           add=T,
           alpha=0.7,
           legend=F # specify detailed legend stuff in another call.
           ,maxpixels = maxpix
           ,ext=extents
           ,axes=switch(mapSize,poster=T,three=F,six=F)
      )
      if(mapSize=="poster") {
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
        plot(modelRast, legend.only=T,
             col=modelCol, zlim=zlims,
             legend.width=1, legend.shrink=0.75,
             axis.args=list(at    = zscale,
                            labels= zscale,
                            cex.axis=1),
             legend.args=list(text='lognormal σ',
                              side=4,
                              #font=2,
                              line=3.5,
                              cex=CEX)
             ,maxpixels = maxpix
             ,ext=extents
             ,axes=switch(mapSize,poster=T,three=F,six=F)
        )
      }
      
  
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
  
      
      if(mapSize=="poster") {
        
        # # Legend for residuals  # THIS DOESN'T WORK - waiting for stackoverflow response
        # # https://stackoverflow.com/questions/48160409/scale-on-color-bar-is-not-behaving-multi-layer-raster-plot
        # plot(modelRast, legend.only=T, horizontal=T, col=residCol, 
        #      # legend.width=1, legend.shrink=0.75, 
        #      zlims = rlims,
        #      axis.args=list(
        #               at    = rscale,
        #               labels= as.character(round(rscale,1)),#,
        #               cex.axis=1),
        #      legend.args=list(
        #               text='residual = ln(obs./pred.)',
        #               # side=1,
        #               # #font=2,
        #               cex=CEX))
        #
        # # Instead, use this discrete option for now.
        scaleLabels0 <- round(rscale,1)
        scaleLabels1 <- as.character(scaleLabels0)
        scaleLabels1[scaleLabels0==min(scaleLabels0)] <- paste0(scaleLabels1[scaleLabels0==min(scaleLabels0)], " (overpred.)")
        scaleLabels1[scaleLabels0==max(scaleLabels0)] <- paste0(scaleLabels1[scaleLabels0==max(scaleLabels0)], " (underpred.)")
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
        if(!MVNnoisy) {
          # If plotting MVN noisy data then I choose
          # input uncertainty for point overlay colors,
          # in which case color scale is same as background,
          # and this supplemental legend is not needed.
          legend("topright",legend = scaleLabels1, 
                 # horiz=T,
                 fill=residCol, 
                 title='residual = ln(obs./pred.)')
        }
        
        
        # plotting Vs30 points...................
      
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
        # plot points
        # Only show Q3 points for AhdiAK geology model...
        if(identical(mapID, "AhdiAK")) {
          points(x = coordsD[q3s,], pch=shapes[q3s], bg=Dcol[q3s], col="black", cex=sizes[q3s])}

        # legend....
        # for AhdiAK, include Q3. For all other maps, don't.
        switch(mapID,
               AhdiAK = {
                 legend(x="topleft",
                        legend = c("Kaiser_Q1","Kaiser_Q2","Kaiser_Q3 (not used)","McGann (resampled)","Wotherspoon"),
                        pt.bg = c(residCol[2], residCol[1], residCol[5], residCol[2], residCol[3]), # arbitrary colors
                        pch=c(23,22,21,24,25),
                        pt.cex=c(LARGE, MED, SMALL, LARGE, LARGE),
                        title = sprintf(
                          "Vs30 data: shape indicates quality designation. Color indicates residual from %s model",
                          modelForResidualPoints)#,
                        #horiz=T
                 )
               },
               {
                 legend(x="topleft",
                        legend = c("Kaiser_Q1","Kaiser_Q2","McGann (resampled)","Wotherspoon"),
                        pt.bg = c(residCol[2], residCol[1], residCol[5], residCol[2]), # arbitrary colors
                        pch=c(23,22,24,25),
                        pt.cex=c(LARGE, MED, LARGE, LARGE),
                        title = sprintf(
                          "Vs30 data: shape indicates quality designation. Color indicates residual from %s model",
                          modelForResidualPoints)#,
                        #horiz=T
                 )
               }
        )
      }
      
      if(MVNnoisy & mapSize!="six") {
        # If it's a noisy MVN map, then plot points. (But not for the 6-inch publication figures.)
        par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)
        # plot points
        # Only show Q3 points for AhdiAK geology model...
        points(x = coordsD[McG,], pch=shapes[McG], bg=Dcol[McG], col="black", cex=sizes[McG])
        points(x = coordsD[Wth,], pch=shapes[Wth], bg=Dcol[Wth], col="black", cex=sizes[Wth])
        points(x = coordsD[q2s,], pch=shapes[q2s], bg=Dcol[q2s], col="black", cex=sizes[q2s])
        points(x = coordsD[q1s,], pch=shapes[q1s], bg=Dcol[q1s], col="black", cex=sizes[q1s])
      }
      
      
      # draw scale & north arrow
      if(!(publication &
           mapID %in% c(
             "AhdiAK",
             "AhdiAK_noQ3_hyb09c",
             "YongCA_noQ3",
             "AhdiAK_noQ3_hyb09c_KRG_v6", # for two-pane kriging
             "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5" # for two-pane MVN
           ))) {
  			par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
  			drawScale(region)
  			par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol, new=T)#,
  			northarrow(region)
      }
      
      
      dev.off()
      
      if(fileName=="out/maps/sigma_CH_AhdiAK_three.png") { # just need one legend file.
                                                           # actually... two... making a taller one for the six-pane figures.
                                                           # ........... three. one more for 6-inch figures.
        for(fff in c("a","b","six")) {
          legendHeight <- switch(fff, a = pxHeight,
                                      b = pxHeight*2 + 30,
                                      six=2096)
          png(filename = switch(fff,
                                a = "out/maps/sigma_three_legend.png",
                                b = "out/maps/sigma_three_legendtall.png",
                                six = "out/maps/sigma_six_legend.png"), width = 450,height=legendHeight)
          par(mfrow=c(1,1),  cex=CEX,   mar=margins,   bg=bgcol, col=txtcol)
          plot.new()
          plot(modelRast, legend.only=T,
               col=modelCol, zlim=zlims,
               legend.width=1, legend.shrink=0.75,
               axis.args=list(at    = zscale,
                              labels= zscale,
                              cex.axis=1),
               legend.args=list(text='lognormal σ',
                                side=4,
                                #font=2,
                                line=3.0,
                                cex=CEX)
               ,maxpixels = maxpix
               ,ext=extents
               ,axes=switch(mapSize,poster=T,three=F,six=F)
               ,smallplot=c(0.32,0.48,0.075,0.925)
          )
          dev.off()
        }
      }
      
    } # for mapID
  } # for mapSize
} # for region
