# fitVariogram_geologyAndTerrain3_allTogether.R
# 
# 20181004 - copied this file m fitVariogram_geologyAndTerrain
# 
# Purpose - To quantitatively fit variograms to the three constituent
# clouds from subsets (McGann, noQ3noMcGann, noQ3noCanterbury), and generate
# pretty pictures for publication.
# 
# 
rm(list=ls())
setwd("~/VsMap")
library(gstat)
library(ggplot2)
library(colorRamps)
library(boot)
source("R/models.R")
source("R/functions.R")
load("Rdata/vspr.Rdata")

# for bootstrapping
meanbro <- function(data,b) {
  return(mean(data[b,]))
}
Rbootstrap <- 999 # use 99 for quick repeats, 999 or so for more precise.

includeTitle <- T # "false" for publication figures. "true" for review etc.
plotTheoreticalVariogram <- T
includeBinNos <- T # show number of summarized bins
cressieTF <- F
plotErrorBars <- T

fitDist <- 10000 # fitDist - the cutoff distance (metres) to use for fitting.

# cutoffDists <- c(50e3,100e3,200e3,500e3) # metres
# cutoffDists <- c(20e3,50e3,75e3,100e3, 1600e3) # metres
#                                       # 1600 kms is enough to include all data. For discussion purposes
# cutoffDists <- c(20e3,50e3) # metres
cutoffDists <- c(20e3) # metres
# cutoffDists <- c(200e3) # metres
# cutoffDists <- c(50e3,200e3) # metres


#########################################
## New versions: ########################
#########################################

# v6: GEO
# v7: TER
# (note, kriging and mvn variograms are now the same)

# varVersions <- c("v6")
# MODELS      <- c("AhdiAK_noQ3_hyb09c")
# varVersions <- c("v7")
# MODELS      <- c("YongCA_noQ3")
varVersions <- c("v6","v7")
MODELS      <- c("AhdiAK_noQ3_hyb09c", "YongCA_noQ3")

pickVariogramVersion <- function(vgName) {
  psill <- switch(vgName,
                  v6 = 1,
                  v7 = 1)
  nugget <- 0
  kappa  <- 0.5
  range  <- 2e3  # This is used as STARTING POINT for variogram fitting. (shouldn't affect result...)
  
  # normResFlags <- c("ByMod","ByRes","Both") # ByMod  =  normalize by underlying model standard deviation.
  #                                           # ByRes  =  normalize by stDv of all residuals.
  #                                           # Both   =  obvious
  normResFlags <- c("Both")
  return(list(psill=psill, nugget=nugget, kappa=kappa, range=range, normResFlags=normResFlags))
}

#########################################
#########################################








boundsList <- list()

firstBin   <- 250 # metres
boundsList[[1]]    <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.2)))
boundsList[[2]]    <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.3)))
boundsList[[3]]    <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.4)))
boundsList[[4]]    <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.5)))
boundsList[[5]]    <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.6)))
boundsList[[6]]    <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.7)))
boundsList[[7]]    <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.8)))
boundsList[[8]]    <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.9)))
boundsList[[9]]    <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 1.0)))
firstBin   <- 500 # metres
boundsList[[10]]   <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.2)))
boundsList[[11]]   <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.3)))
boundsList[[12]]   <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.4)))
boundsList[[13]]   <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.5)))
boundsList[[14]]   <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.6)))
boundsList[[15]]   <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.7)))
boundsList[[16]]   <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.8)))
boundsList[[17]]   <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 0.9)))
boundsList[[18]]   <- cumsum(firstBin * exp(seq(from = 0, to = 20, by = 1.0)))







# # for constant lag width... (not using for now)
# lagwidth = 500 # metres
# bounds <- seq(from = lagwidth,
#               to = cutoffDist*2,
#               by = lagwidth)












dat <- subsetVsPoints(vspr)
# quick n dirty loop to remove NA points
for(subsetName1 in names(dat)) {
  subset1 <- dat[[subsetName1]]
  subset2 <- subset1[!is.na(subset1$res_AhdiYongWeighted1),] #which resid isn't important here, it's just to find NAs.
  dat[[subsetName1]] <- subset2
}


for(i in 1:length(varVersions)) {
  # i <- 1
  vgName   <- varVersions[i]
  MODEL    <- MODELS[i]

  # vgName <- varVersions[1]   # testing
  x <- pickVariogramVersion(vgName)
  psill  <- x$psill
  nugget <- x$nugget
  kappa  <- x$kappa
  range  <- x$range
  normResFlags <- x$normResFlags
  
  
  for(cutoffD in 1:length(cutoffDists)) {
    # cutoffD <- 1 # testing
    cutoffDist <- cutoffDists[cutoffD]
    # cutoffDist <- cutoffDists[1] # testing
    for(normResFlag in normResFlags) {
      # normResFlag <- normResFlags[1] # testing
      if(identical(normResFlag,"ByMod")) {
        maxGamma <- 9 # 8
        labHt <- 8.5
      } else if (identical(normResFlag, "ByRes")) {
        maxGamma <- 30 # 25
        labHt <- 27.5
      } else if (identical(normResFlag, "Both")) {
        maxGamma <- 3.5 # 3
        labHt <- 3
      }
      
      
      
      subsetList <- switch(MODEL,
                           AhdiAK_noQ3_hyb09c = c("McGann",  "noQ3noCanterbury",  "noQ3noMcGann"),
                           YongCA_noQ3        = c("noQ3noCanterburyB",  "noQ3noMcGannB")) # B subsets: all cat 15 and 16 removed. See note in functions.R for more detail
      
      colorList <- switch(MODEL,
                          AhdiAK_noQ3_hyb09c = c("red",  "green",  "blue"),
                          YongCA_noQ3        = c("green",  "blue"))
      
      
      
      # evp = empirical variogram plot
      evp  <- ggplot() # + theme(axis.line = element_line())
      
      
      
      for(modelSubsetNo in 1:length(subsetList)) { 
        
        modelSubset <- subsetList[modelSubsetNo]
        
        # modelSubset <- "noQ3noMcGann" #testing
        
        # "best" binning schemes. Out of the 18 schemes generated above,
        # I selected short lists (in fitVariogram_geologyAndTerrain2) based on visual inspection.
        # Then I picked one for each. These are the ones shown here.
        # 
        # The subsets below should match the ones I selected in the spreadsheet
        # located in:
        # ~/VsMap/out/plots/20181003 (R99) Many log-spaced bins 95 percent confidence interval bootstrapping and identified best for fitting/notes-20181003.ods
        # 
        # And the very best is identified in folder names within this path:
        # /home/kmf76/Vs30map_clean/out/plots/20181005 Set of best fits for each subset and model/
        # 
        binningScheme <- switch(MODEL,
                                AhdiAK_noQ3_hyb09c = switch(
                                  modelSubset,
                                  McGann = 3,
                                  noQ3noCanterbury = 14,
                                  noQ3noMcGann = 8),
                                YongCA_noQ3 = switch(
                                  modelSubset,
                                  noQ3noCanterburyB = 16,
                                  noQ3noMcGannB = 16)
                                )

        bounds <- boundsList[[binningScheme]]
        boundsIncl0 <- c(0,bounds) # this is used for manually binning gamma values, required for computing error bars.
        
        # for showing bin boundaries...
        lineFrame <- data.frame(x     = boundsIncl0,
                                ymin  = rep(0,length(boundsIncl0)), 
                                ymax  = rep(maxGamma, length(boundsIncl0)))
        
        dat_subset <- dat[[modelSubset]]
        
        # Decide which residuals are used for generating variogram:
        if(identical(MODEL,"AhdiAK_noQ3_hyb09c")) {
          res  <- dat_subset$res_AhdiAK_noQ3_hyb09c
          stdv <- dat_subset$stDv_AhdiAK_noQ3_hyb09c
        } else if(identical(MODEL, "YongCA_noQ3")) {
          res  <- dat_subset$res_YongCA_noQ3
          stdv <- dat_subset$stDv_YongCA_noQ3
        }
        varAllRes  <- var(res)
        res_norm_1 <- res / stdv
        res_norm_2 <- res_norm_1 / var(res_norm_1)
        res_norm_3 <- res_norm_1 / sqrt(var(res_norm_1))  # WINNER WINNER CHICKEN DINNER
        
        if(identical(normResFlag, "ByMod")) {
          # normalizing BY THE MODEL STANDARD DEVIATION.
          dat_subset$normalizedResidual <- res_norm_1
        } else if(identical(normResFlag, "ByRes")) {
          # normalizing BY STANDARD DEVIATION OF ALL RESIDUALS.
          dat_subset$normalizedResidual <- res / sqrt(varAllRes)
        } else if(identical(normResFlag, "Both")) {
          # both - THIS IS WHAT BB WANTS
          dat_subset$normalizedResidual <- res_norm_3
        }
        
        
        
        if(nrow(dat_subset)>=2) {  # need a minimum of 1 pair i.e. 2 datapoints for the empirical variogram.
          # ev = empirical variogram
          ev    <- variogram(normalizedResidual~1,    data = dat_subset,   cutoff=cutoffDist, boundaries = bounds, cressie=cressieTF)
          evAll <- variogram(normalizedResidual~1,    data = dat_subset,   cutoff=cutoffDist, cloud = TRUE,        cressie=cressieTF)
          vgdf <- ev[,c("dist","gamma","np")]
          
          # Save items in list
          if(modelSubsetNo==1) {
            evList <- ev
          } else {
            evList <- rbind(evList, ev)
          }
          

          # error bars
          evAll$binNo       <- rep(0,nrow(evAll))

          vgdf$simpleSigma  <- rep(0,nrow(vgdf))
          vgdf$Q25          <- rep(0,nrow(vgdf))
          vgdf$Q75          <- rep(0,nrow(vgdf))
          vgdf$Q50          <- rep(0,nrow(vgdf))
          vgdf$Q025         <- rep(0,nrow(vgdf))
          vgdf$Q975         <- rep(0,nrow(vgdf))
          vgdf$boot16       <- rep(0,nrow(vgdf))
          vgdf$boot84       <- rep(0,nrow(vgdf))
          vgdf$boot025      <- rep(0,nrow(vgdf))
          vgdf$boot975      <- rep(0,nrow(vgdf))
          
          for(bin in 1:nrow(vgdf)) {
            # bin <- 1 # testing
            whichPts <- evAll$dist > boundsIncl0[bin] & evAll$dist <= boundsIncl0[bin+1]
            allPts   <- evAll[whichPts,]
            evAll$binNo[whichPts]   <- bin

            # median
            vgdf$Q50[bin]     <- quantile(allPts$gamma,probs=0.5)
            
            # bootstrapping
            meanFrame <- as.data.frame(allPts)[,"gamma", drop=F] # create a dataframe with 1 col
            if(nrow(meanFrame)>0) {
              boots <- boot(data = meanFrame,statistic = meanbro,R = Rbootstrap) # bootstrap
              boot1684   <- quantile(boots$t, probs = c(.16,.84))   # get quantiles
              boot025975 <- quantile(boots$t, probs = c(.025,.975)) # get quantiles  
            } else {
              boot1684   <- c(NA,NA)
              boot025975 <- c(NA,NA)
            }
            vgdf$boot16[bin] <- boot1684[1]
            vgdf$boot84[bin] <- boot1684[2]
            vgdf$boot025[bin] <- boot025975[1]
            vgdf$boot975[bin] <- boot025975[2]
          }
          
          # # bin boundaries
          # evp <- evp + geom_linerange(data = lineFrame, aes(x = x/1000, ymin = ymin, ymax = ymax),
          #                             size = 0.1)
          
          
          # # plot raw data
          # evp <- evp + geom_point(data = evAll, aes(x=dist/1000, y=gamma), alpha=0.1)
          
          
          if(plotErrorBars) {
            # Error bars from computed Q25 and Q75:

            # # Error bars with bootstrapped 16th and 84th percentile mean
            # evp <- evp + geom_errorbar(data=vgdf, aes(x=dist/1000, ymin=boot16, ymax=boot84), col="black", width=0.001*0.015*cutoffDist)
            
            # Error bars with bootstrapped 2.5th and 97.5th percentile mean
            evp <- evp + geom_errorbar(
              data=vgdf, aes(x=dist/1000, ymin=boot025, ymax=boot975),
              col=colorList[modelSubsetNo], 
              width=0.001*0.015*cutoffDist)
          }
          
          evp <- evp + geom_point(data = vgdf, aes(x=dist/1000, y=gamma), col=colorList[modelSubsetNo])
        }
        distanceVec <- exp(seq(log(10), log(cutoffDist), length.out = 100))
        fitVarManual <- vgm(psill  = psill,
                             model  = "Mat",
                             range  = range,
                             nugget = nugget,
                             kappa  = kappa)

        vgmTheo_manual <- variogramLine(fitVarManual, dist_vector = distanceVec)
        
        # Fit variogram. Note that (as discussed in fit.variogram documentation):
        #   - Weights proportional to N_j/h^2 (number of points in bin / square of distance) are
        #       used for fitting.  (fit.method = 7, default). THIS HEAVILY WEIGHTS THE LOWER 
        #       DISTANCES AND THEREFORE I DON'T NEED TO TAKE SPECIAL MEASURES TO SELECT A SHORTER 
        #       RANGE FOR FITTING!
        #   - there is only ONE parameter being fitted---the range.
        #   - (Kappa is NOT fitted by default. We're keeping kappa=0.5, which reduces to a simple exponential variogram function.)
        fitVarAuto_ALL   <- fit.variogram(object = ev,         model  = fitVarManual, fit.sills = F) # fit variogram to SAMPLE variogram (i.e. it is INFLUENCED BY BINNING.)
        vgmTheo_auto_ALL <- variogramLine(fitVarAuto_ALL, dist_vector = distanceVec)

        # extract (1) model effective range and (2) effective range (i.e. range where gamma = .95)          
        # For a discussion about difference between the two, see Diggle & Ribeiro 2007,
        # section 3.4.1 (the Matern family), p. 51. For kappa=0.5, the practical range (effective
        # range) is about 3 * the parametric range.
        parametricRange <- fitVarAuto_ALL$range[2]
        effectiveRange  <- approx(x = vgmTheo_auto_ALL$gamma,
                                  y = vgmTheo_auto_ALL$dist,
                                  xout = 0.95)$y
        
        
        if(plotTheoreticalVariogram) {
          evp <- evp + geom_line(data = vgmTheo_auto_ALL, aes(x=dist/1000,y=gamma), col=colorList[modelSubsetNo])
        }
        if(includeBinNos) {
          zzzzzzzzzz <- labHt - 0.5*(modelSubsetNo-1)
          print(zzzzzzzzzz) #WTF
          evp <- evp + geom_text(data = vgdf, aes(x=dist/1000, label=np), y=zzzzzzzzzz, 
                                     size = 2, angle = 90,
                                     col=colorList[modelSubsetNo])
        }
        
      }
      

      fitVarAuto_ALL2   <- fit.variogram(object = evList, model  = fitVarManual, fit.sills = F)
      vgmTheo_auto_ALL2 <- variogramLine(fitVarAuto_ALL2, dist_vector = distanceVec)
      
      if(plotTheoreticalVariogram) {
        evp <- evp + geom_line(data = vgmTheo_auto_ALL2,
                               aes(x=dist/1000,y=gamma), col="black")
      }
      
      parametricRangeAll <- fitVarAuto_ALL2$range[2]
      effectiveRangeAll  <- approx(x = vgmTheo_auto_ALL2$gamma,
                                y = vgmTheo_auto_ALL2$dist,
                                xout = 0.95)$y
      
            
      evp <- evp + xlab("dist, km") + ylab("V(u)")
      
      
      
      if(includeTitle) {evp <- evp + 
        ggtitle(paste0("Empirical and theoretical variograms for model: ",vgName, 
                       " (", switch(MODEL, AhdiAK_noQ3_hyb09c = "GEOLOGY",
                        YongCA_noQ3 = "TERRAIN"), ")"),
                subtitle = sprintf(paste0("Mod: %s   ",
                                          "Effective range: % 3.1fkm"),
                                   MODEL, effectiveRangeAll/1000)) +
        theme(plot.subtitle=element_text(size=8),
              panel.background = element_blank())} # , hjust=0.5, face="italic", color="black"))}
      
      evp <- evp + xlim(0,cutoffDist/1000) + ylim(0, maxGamma)
      
      # manual legend
      valList <- setNames(colorList, subsetList)
      evp <- evp +
        scale_colour_manual(name="Data subsets",
                          # limits=colorList, labels = subsetList,
                          values = valList) +
        theme(legend.position = "right")
      
      evp
      
      ggsave(filename = sprintf("out/plots/NEWNEWNEW/vgram_avg_%s_%s_cut%03.0fkm_R%d.png",
                                switch(MODEL, AhdiAK_noQ3_hyb09c = "GEOLOGY",
                                       YongCA_noQ3 = "TERRAIN"),
                                vgName, 
                                cutoffDist/1000,
                                Rbootstrap # cutoff distance in km
      ),
      height = ifelse(includeTitle, 3, 2),
      width = 6)
      
    }
  }
  variogram <- fitVarAuto_ALL2
  save(variogram, file = paste0("Rdata/variogram_", MODEL, "_", vgName, ".Rdata"))
}



