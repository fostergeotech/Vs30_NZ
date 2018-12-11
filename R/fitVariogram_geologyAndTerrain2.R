# fitVariogram_geologyAndTerrain2.R
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

varVersions <- c("v6")
MODELS      <- c("AhdiAK_noQ3_hyb09c")
varVersions <- c("v7")
MODELS      <- c("YongCA_noQ3")
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
        maxGamma <- 5 # 3
        labHt <- 4
      }
      
      
      # for(modelSubset in c("Kaiser_all", "allData", "McGann", "noQ3", "noQ3noMcGann")) {
      # for(modelSubset in c("McGann", "noQ3noCanterbury", "noQ3noMcGann")) {
      for(modelSubset in c("McGann",  "noQ3noCanterbury",  "noQ3noMcGann",
                           "McGannB", "noQ3noCanterburyB", "noQ3noMcGannB")) { # B subsets: all cat 15 and 16 removed. See note in functions.R for more detail
        
        # modelSubset <- "noQ3noMcGann" #testing
        
        # "best" binning schemes. Out of the 18 schemes generated above,
        # I selected the following short lists based on visual inspection.
        # The idea is to perform a fit for EACH binning scheme in the list
        # but only the "best" ones. I expect that the theoretical variogram 
        # will be insensitive to binning scheme (at least for geology 
        # model---the terrain model is more "volatile").
        # 
        # The subsets below should match the ones I selected in the spreadsheet
        # located in:
        # ~/VsMap/out/plots/20181003 (R99) Many log-spaced bins 95 percent confidence interval bootstrapping and identified best for fitting/notes-20181003.ods
        bestBinningSchemes <- switch(MODEL,
                                     AhdiAK_noQ3_hyb09c = switch(
                                       modelSubset,
                                       McGann = c(1,3,4,11,13,16),
                                       noQ3noCanterbury = c(3,8,11,14,17),
                                       noQ3noMcGann = c(3,8,11,14)),
                                     YongCA_noQ3 = switch(
                                       modelSubset,
                                       McGann = c(4,12),
                                       # noQ3noCanterbury = c(3,5,11,13,14,16),
                                       noQ3noCanterbury = c(3,14,16),
                                       noQ3noMcGann = c(5,6,7,8,9,11,13),
                                       McGannB = c(1,2,3,5),
                                       # noQ3noCanterburyB = c(6,7,16,17),
                                       noQ3noCanterburyB = c(6,16),
                                       # noQ3noMcGannB = c(2,5,6,7,15,16,17))
                                       noQ3noMcGannB = c(6,7,15,16))
                                     )
      
        for(binningScheme in bestBinningSchemes) {
        # for(binningScheme in 1) { # testing
          # binningScheme <- 1 # testing
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
            if(identical(normResFlag, "ByMod") || identical(normResFlag, "ByRes") || identical(normResFlag, "Both")) {
              ev    <- variogram(normalizedResidual~1,    data = dat_subset,   cutoff=cutoffDist, boundaries = bounds, cressie=cressieTF)
              evAll <- variogram(normalizedResidual~1,    data = dat_subset,   cutoff=cutoffDist, cloud = TRUE,        cressie=cressieTF)
              evFit    <- variogram(normalizedResidual~1,    data = dat_subset,   cutoff=fitDist, boundaries = bounds, cressie=cressieTF)
            } else {
              ev    <- variogram(res_AhdiYongWeighted1~1, data = dat_subset,   cutoff=cutoffDist, boundaries = bounds, cressie=cressieTF)
              evAll <- variogram(res_AhdiYongWeighted1~1, data = dat_subset,   cutoff=cutoffDist, cloud=TRUE,          cressie=cressieTF)
            }
            vgdf <- ev[,c("dist","gamma","np")]
            
  
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
  
              # computing the standard deviation. (valid only for symmetric distributions.
              # Since variogram data follow a chi-squared distribution with 1 DOF
              # (Diggle & Ribeiro 2007, sec 5.2.2, p 102), this won't work here! i.e., some
              # ymin values will be <0 which is silly.)
              vgdf$simpleSigma[bin] <- sqrt(var(allPts$gamma))
              
              # Computing quantiles instead:
              vgdf$Q25[bin]     <- quantile(allPts$gamma,probs=0.25)
              vgdf$Q75[bin]     <- quantile(allPts$gamma,probs=0.75)
              
              # Computing 5th and 95th percentiles:
              vgdf$Q05[bin]    <- quantile(allPts$gamma,probs=0.05)
              vgdf$Q95[bin]    <- quantile(allPts$gamma,probs=0.95)
              
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
            
            
            # evp = empirical variogram plot
            evp  <- ggplot() # + theme(axis.line = element_line())
            
            # bin boundaries
            evp <- evp + geom_linerange(data = lineFrame, aes(x = x/1000, ymin = ymin, ymax = ymax),
                                        size = 0.1)
            
            
            # # plot raw data
            # evp <- evp + geom_point(data = evAll, aes(x=dist/1000, y=gamma), alpha=0.1)
            
            
            if(plotErrorBars) {
              # Error bars from computed Q25 and Q75:
              # evp <- evp + geom_errorbar(data = vgdf, aes(x=dist/1000, ymin=Q25, ymax=Q75),col="red")
              
              # # Error bars using standard Tukey values for boxes & whiskers:
              # # (Can't get this to display properly at the moment.)
              # for(i in 1:nrow(vgdf)) {
              #   thisBin <- evAll[evAll$binNo==i,]
              #   # evp <- evp + geom_boxplot(data = thisBin, aes(x=vgdf$dist[i], y=gamma), col="red")
              #   # evp <- evp + geom_boxplot(data = thisBin, aes(group=disssst, y=gamma), col="red")
              #   evp <- evp + geom_boxplot(data = thisBin, aes(y=gamma), x=vgdf$dist[i]/1000, width = 2000, col="blue")
              # }
              
              # # Box plots with precalculated quantiles:
              # evp <- evp + geom_crossbar(data=vgdf, aes(x=dist/1000, y=Q50, ymin=Q25, ymax=Q75), col="green") +
              #   geom_linerange(data=vgdf, aes(x=dist/1000, ymin=Q75,  ymax=Q95), col="green") +
              #   geom_linerange(data=vgdf, aes(x=dist/1000, ymin=Q05, ymax=Q25),  col="green")
              
              # # Error bars with bootstrapped 16th and 84th percentile mean
              # evp <- evp + geom_errorbar(data=vgdf, aes(x=dist/1000, ymin=boot16, ymax=boot84), col="black", width=0.001*0.015*cutoffDist)
              
              # Error bars with bootstrapped 2.5th and 97.5th percentile mean
              evp <- evp + geom_errorbar(data=vgdf, aes(x=dist/1000, ymin=boot025, ymax=boot975), col="black", width=0.001*0.015*cutoffDist)
            }
            
            evp <- evp + geom_point(data = vgdf, aes(x=dist/1000, y=gamma))
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
            zzzzz <- evp + geom_line(data = vgmTheo_auto_ALL, aes(x=dist/1000,y=gamma), col="blue")
          } else {
            zzzzz <- evp
          }
  
          zzzzz <- zzzzz + xlab("dist, km") + ylab("V(u)")
          
          if(includeBinNos) {
            zzzzz <- zzzzz + geom_text(data = vgdf, aes(x=dist/1000, y=labHt, label=np),
                                       size = 2, angle = 90)
          }
          
          
          if(includeTitle) {zzzzz <- zzzzz + 
            ggtitle(paste0("Empirical and theoretical variogram name: ",vgName),
              subtitle = sprintf(paste0("Mod: %s   Dat: %s   Norm: %s   Binning scheme no. % 2.0f\n",
                                        "Effective range: % 3.1fkm"),
                                 MODEL, modelSubset, normResFlag, binningScheme, effectiveRange/1000)) +
            theme(plot.subtitle=element_text(size=8),
                  panel.background = element_blank())} # , hjust=0.5, face="italic", color="black"))}
          
          zzzzz <- zzzzz + xlim(0,cutoffDist/1000) + ylim(0, maxGamma)
          zzzzz
          
          ggsave(filename = sprintf("out/plots/NEWNEWNEW/vgram_emp+theo_%s_erb%s_%s_%s_dat_%s_bin%1.0f_cut%03.0fkm_R%d.png",
                                    switch(MODEL, AhdiAK_noQ3_hyb09c = "GEOLOGY",
                                                  YongCA_noQ3 = "TERRAIN"),
                                    plotErrorBars, MODEL, vgName, modelSubset, 
                                    binningScheme,
                                    cutoffDist/1000,
                                    Rbootstrap # cutoff distance in km
                                    ),
                 height = ifelse(includeTitle, 3, 2),
                 width = 6)
        }
      }
    }
  }
  # variogram <- fitVarManual
  # save(variogram, file = paste0("Rdata/variogram_", MODEL, "_", vgName, ".Rdata"))
}



