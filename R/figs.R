# #!/usr/bin/R --vanilla
#
# # figs.R
# #
# # Make the figures needed for publication only.
#



############################################
############################################
# BB1 ######################################
############################################
# For each unit the prior and posterior distributions,
# along with an empirical distribution of the measurements.
# Can be both in graphical form and also tabular

rm(list=ls())
setwd("~/VsMap")
library(ggplot2)
library(sp)
library(ggthemes)
library(plyr) # for join()
source("R/functions.R")
plts <- ggsave


gf <- function(v){sapply(v, function(i){paste0("G",substr(i,0,2))})}


load("Rdata/vspr.Rdata")
vsprSubsets <- subsetVsPoints(vspr)
vsprM     <- vsprSubsets$noQ3
nameStr  <- "noQ3"
vsprDF <- as.data.frame(vsprM)

brkVs30  = c(100*1:10, 2000)
labVs30  = as.character(brkVs30)
# remove "900" and "700"
labVs30[labVs30=='900'] <- ''
labVs30[labVs30=='700'] <- ''
mbrkVs30 = c(10*11:19, 10*21:29, 10*31:39, 10*41:49,
             10*seq(from=52,to=58,by=2),
             10*seq(from=62,to=68,by=2),
             10*seq(from=72,to=78,by=2),
             10*seq(from=82,to=88,by=2),
             10*seq(from=92,to=98,by=2),
             100*11:20)

source("R/models.R")
MODEL <- "AhdiAK"
groupIDname <- paste0("groupID_",MODEL)
vsprDF$justGroup <- vsprM[[groupIDname]]
levels(vsprDF$justGroup) <- factor(gf(levels(vsprDF$justGroup)))

# Use these to specify vertical lines between bins.
nBins    <- length(levels(vsprDF$justGroup))
binLines <- seq(0.5,nBins-0.5)  # 00_ICE and 00_WATER not used

# Open AhdiAK priors and posteriors (noQ3) info
source("R/models.R")
AhdiAK_priorsTable <- AhdiAK_lookup()
AhdiAK_posteriorsTable <- AhdiAK_noQ3_lookup()

AhdiAK_priorsTable$pp     <- rep("prior",     dim(AhdiAK_priorsTable)[1])
AhdiAK_priorsTable$shapes  <- rep("cross",     dim(AhdiAK_priorsTable)[1])
AhdiAK_posteriorsTable$pp <- rep("posterior", dim(AhdiAK_posteriorsTable)[1])
AhdiAK_posteriorsTable$shapes <- rep("circle", dim(AhdiAK_posteriorsTable)[1])

colnames(AhdiAK_priorsTable)[colnames(AhdiAK_priorsTable)=="Vs30_AhdiAK"] <- "medianVs30"
colnames(AhdiAK_priorsTable)[colnames(AhdiAK_priorsTable)=="stDv_AhdiAK"] <- "stdDevVs30"
colnames(AhdiAK_posteriorsTable)[colnames(AhdiAK_posteriorsTable)=="Vs30_AhdiAK_noQ3"] <- "medianVs30"
colnames(AhdiAK_posteriorsTable)[colnames(AhdiAK_posteriorsTable)=="stDv_AhdiAK_noQ3"] <- "stdDevVs30"

# bind
Ahdi_prior_noQ3_posterior <- rbind(AhdiAK_posteriorsTable,
                                   AhdiAK_priorsTable)
Ahdi_prior_noQ3_posterior$pp <- factor(Ahdi_prior_noQ3_posterior$pp)
Ahdi_prior_noQ3_posterior$groupID <- gf(Ahdi_prior_noQ3_posterior$groupID)

# explicitly set levels of "pp" so that prior representation plots to the left, and posterior plots to the right.
Ahdi_prior_noQ3_posterior$pp = factor(Ahdi_prior_noQ3_posterior$pp,levels(Ahdi_prior_noQ3_posterior$pp)[c(2,1)])


Vs30plotBase <- ggplot() +
  xlab("Geology categories") + theme_linedraw() + # theme_minimal() +
  theme(
    # text = element_text(family="Sans"),
    axis.text.x = element_text(
    angle=90, vjust=0.5, hjust=0),
    panel.grid.major.y = element_line(color="gray70", size=0.3),
    panel.grid.minor.y = element_line(color="gray85", size=0.15),
    panel.grid.major.x = element_blank(),
    panel.border       = element_blank(),
    legend.background  = element_blank(),
    legend.key = element_blank(),
    axis.ticks = element_blank(),
    legend.title=element_blank(),
    legend.position = c(0.1, 0.85)
  ) +
  # ggtitle("Bayesian updating: Geology-based model") +
  ylab(expression("V"[s30]*" (m/s)")) +
  scale_y_log10(breaks=brkVs30,
                labels = labVs30,
                minor_breaks = mbrkVs30) +
  expand_limits(y=c(100,2000))
xxx <- 1.1
Vs30plot <- Vs30plotBase +
  geom_jitter(data = vsprDF, # jitter representing data
              aes(x = justGroup, y = Vs30),
              width=0.2, height=0, alpha=0.3, size=1.2) +
  # geom_boxplot(data = vsprDF, # boxplots representing data
  #              aes(x = justGroup, y = Vs30),
  #              varwidth = F, # T,
  #              width = 0.1,
  #              color="green",
  #              outlier.size = 0.5) +
  scale_shape_identity() + # required to pass shape mappings to geom_linerange()
  geom_linerange(data = Ahdi_prior_noQ3_posterior,
                 aes(x = groupID,
                     ymax = exp(log(medianVs30) + stdDevVs30),
                     ymin = exp(log(medianVs30) - stdDevVs30),
                     color = pp
                 ),
                 position=position_dodge(width=xxx)) +
  geom_segment(aes(x    = binLines, y    = min(brkVs30),
                   xend = binLines, yend = max(brkVs30)),
               color = "gray70",
               size = 0.2) +
  geom_point(data = Ahdi_prior_noQ3_posterior,
             aes(x = groupID,
                 y = medianVs30,
                 color = pp, shape = shapes),
             position=position_dodge(width=xxx))




ExportPlot(filename=paste0("out/figs/BB1"),
           plot=Vs30plot, width = 6, height=3.5)





############################################
############################################
# BB1B #####################################
############################################
# For each unit the prior and posterior distributions,
# along with an empirical distribution of the measurements.
# Can be both in graphical form and also tabular
# SAME AS ABOVE BUT FOR TERRAIN CATEGORIES.

rm(list=ls())
setwd("~/VsMap")
library(ggplot2)
library(sp)
library(ggthemes)
library(plyr) # for join()
source("R/functions.R")
plts <- ggsave


tf <- function(v){sapply(v, function(i){paste0("T",substr(i,0,2))})}


load("Rdata/vspr.Rdata")
vsprSubsets <- subsetVsPoints(vspr)
vsprM     <- vsprSubsets$noQ3
nameStr  <- "noQ3"
vsprDF <- as.data.frame(vsprM)

brkVs30  = c(100*1:10, 2000)
labVs30  = as.character(brkVs30)
# remove "900" and "700"
labVs30[labVs30=='900'] <- ''
labVs30[labVs30=='700'] <- ''
mbrkVs30 = c(10*11:19, 10*21:29, 10*31:39, 10*41:49,
             10*seq(from=52,to=58,by=2),
             10*seq(from=62,to=68,by=2),
             10*seq(from=72,to=78,by=2),
             10*seq(from=82,to=88,by=2),
             10*seq(from=92,to=98,by=2),
             100*11:20)

source("R/models.R")
MODEL <- "YongCA"
groupIDname <- paste0("groupID_",MODEL)
vsprDF$justGroup <- vsprM[[groupIDname]]
levels(vsprDF$justGroup) <- factor(tf(levels(vsprDF$justGroup)))


# Use these to specify vertical lines between bins.
nBins    <- length(levels(vsprDF$justGroup))
binLines <- seq(0.5,nBins+0.5)

# Open AhdiAK priors and posteriors (noQ3) info
source("R/models.R")
YongCA_priorsTable <- YongCA_lookup()
YongCA_posteriorsTable <- YongCA_noQ3_lookup()

YongCA_priorsTable$pp     <- rep("prior",     dim(YongCA_priorsTable)[1])
YongCA_priorsTable$shapes     <- rep("cross",     dim(YongCA_priorsTable)[1])
YongCA_posteriorsTable$pp <- rep("posterior", dim(YongCA_posteriorsTable)[1])
YongCA_posteriorsTable$shapes <- rep("circle", dim(YongCA_posteriorsTable)[1])


colnames(YongCA_priorsTable)[colnames(YongCA_priorsTable)=="Vs30_YongCA"] <- "medianVs30"
colnames(YongCA_priorsTable)[colnames(YongCA_priorsTable)=="stDv_YongCA"] <- "stdDevVs30"
colnames(YongCA_posteriorsTable)[colnames(YongCA_posteriorsTable)=="Vs30_YongCA_noQ3"] <- "medianVs30"
colnames(YongCA_posteriorsTable)[colnames(YongCA_posteriorsTable)=="stDv_YongCA_noQ3"] <- "stdDevVs30"

# bind
Yong_prior_noQ3_posterior <- rbind(YongCA_posteriorsTable,
                                   YongCA_priorsTable)
Yong_prior_noQ3_posterior$pp <- factor(Yong_prior_noQ3_posterior$pp)
Yong_prior_noQ3_posterior$groupID <- tf(Yong_prior_noQ3_posterior$groupID)

# explicitly set levels of "pp" so that prior representation plots to the left, and posterior plots to the right.
Yong_prior_noQ3_posterior$pp = factor(Yong_prior_noQ3_posterior$pp,levels(Yong_prior_noQ3_posterior$pp)[c(2,1)])


Vs30plotBase <- ggplot() +
  xlab("Terrain categories") + theme_linedraw() + # theme_minimal() +
  theme(
    # text = element_text(family="Sans"),
    axis.text.x = element_text(
      angle=90, vjust=0.5, hjust=0),
    panel.grid.major.y = element_line(color="gray70", size=0.3),
    panel.grid.minor.y = element_line(color="gray85", size=0.15),
    panel.grid.major.x = element_blank(),
    panel.border       = element_blank(),
    legend.background  = element_blank(),
    legend.key = element_blank(),
    axis.ticks = element_blank(),
    legend.title=element_blank(),
    legend.position = c(0.1, 0.85)
  ) +
# ggtitle("Bayesian updating: Terrain-based model") +
  ylab(expression("V"[s30]*" (m/s)")) +
  scale_y_log10(breaks=brkVs30,
                labels = labVs30,
                minor_breaks = mbrkVs30) +
  expand_limits(y=c(100,2000))
xxx <- 1.1
Vs30plot <- Vs30plotBase +
  geom_jitter(data = vsprDF, # jitter representing data
              aes(x = justGroup, y = Vs30),
              width=0.2, height=0, alpha=0.3, size=1.2) +
  # geom_boxplot(data = vsprDF, # boxplots representing data
  #              aes(x = justGroup, y = Vs30),
  #              varwidth = F, # T,
  #              width = 0.1,
  #              color="green",
  #              outlier.size = 0.5) +
  scale_shape_identity() + # required to pass shape mappings to geom_linerange()
  geom_linerange(data = Yong_prior_noQ3_posterior,
                 aes(x = groupID,
                     ymax = exp(log(medianVs30) + stdDevVs30),
                     ymin = exp(log(medianVs30) - stdDevVs30),
                     color = pp
                 ),
                 position=position_dodge(width=xxx)) +
  geom_segment(aes(x    = binLines, y    = min(brkVs30),
                   xend = binLines, yend = max(brkVs30)),
               color = "gray70",
               size = 0.2) +
  geom_point(data = Yong_prior_noQ3_posterior,
             aes(x = groupID,
                 y = medianVs30,
                 color = pp, shape = shapes),
             position=position_dodge(width=xxx))




ExportPlot(filename=paste0("out/figs/BB1B"),
           plot=Vs30plot, width = 6, height=3.5)






# ####not used####        ############################################
# ####not used####        ############################################
# ####not used####        # FIG001 ###################################
# ####not used####        ############################################
# ####not used####
# ####not used####        rm(list=ls())
# ####not used####        setwd("~/VsMap")
# ####not used####        library(ggplot2)
# ####not used####        library(sp)
# ####not used####        source("R/functions.R")
# ####not used####        plts <- ggsave
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####        load("Rdata/vspr.Rdata")
# ####not used####        vsprSubsets <- subsetVsPoints(vspr)
# ####not used####        vsprM     <- vsprSubsets$McGann
# ####not used####        nameStr  <- "McGann"
# ####not used####        vsprDF <- as.data.frame(vsprM)
# ####not used####
# ####not used####        brkVs30  = c( 100*1:4)
# ####not used####        labVs30  = as.character(brkVs30)
# ####not used####        mbrkVs30 = c(10*11:19, 10*21:29, 10*31:39)#, 10*41:49)
# ####not used####
# ####not used####        source("R/models.R")
# ####not used####        MODEL <- "AhdiAK"
# ####not used####        groupIDname <- paste0("groupID_",MODEL)
# ####not used####        vsprDF$justGroup <- vsprM[[groupIDname]]
# ####not used####
# ####not used####        Vs30plotBase <- ggplot(vsprDF, aes(x = justGroup, y = Vs30)) +
# ####not used####          xlab("") + theme_tufte() +
# ####not used####          theme(axis.text.x = element_text(
# ####not used####            angle=90, vjust=0.5, hjust=0),
# ####not used####            panel.grid.major.y = element_line(color="gray80"),
# ####not used####            panel.grid.minor.y = element_line(color="gray80")
# ####not used####          ) +
# ####not used####          ggtitle(nameStr) +
# ####not used####          ylab("Vs30, m/s\n(log scale)") +
# ####not used####          scale_y_log10(breaks=brkVs30,
# ####not used####                        labels = labVs30,
# ####not used####                        minor_breaks = mbrkVs30) + expand_limits(y=c(100,500))
# ####not used####        Vs30plot <- Vs30plotBase +
# ####not used####          geom_jitter(width=0.3, height=0, alpha=0.2, size=0.7)
# ####not used####
# ####not used####        plts(filename=paste0("out/figs/FIG001.png"),
# ####not used####             plot=Vs30plot, width = 6, height=4, units = "in")
# ####not used####
# ####not used####
# ####not used####
# ####not used####        ############################################
# ####not used####        ############################################
# ####not used####        # BB3 ######################################
# ####not used####        ############################################
# ####not used####
# ####not used####        rm(list=ls())
# ####not used####        setwd("~/VsMap")
# ####not used####        library(ggplot2)
# ####not used####        library(sp)
# ####not used####        source("R/functions.R")
# ####not used####        plts <- ggsave
# ####not used####
# ####not used####
# ####not used####
# ####not used####        vsprFileName <- "Rdata/vspr.Rdata"
# ####not used####        load(vsprFileName)
# ####not used####        library(sp)
# ####not used####        library(ggplot2)
# ####not used####        source("R/functions.R")
# ####not used####        source("R/models.R")    # for allMODELs
# ####not used####        # source("R/classifyThings.R")
# ####not used####        # # obtain subsets of data
# ####not used####        vsprdf <- data.frame(vspr)
# ####not used####        vsprSubsets.df <- subsetVsPoints(vsprdf)
# ####not used####        # sgrd <- exp(seq(from = log(1e-4), to = log(1), len = 1000 ))
# ####not used####        slopeLimVec <- c(0.00001, 1)
# ####not used####        # maxVs30 <- max(vsprSubsets.df$allData$Vs30)
# ####not used####        # minVs30 <- min(vsprSubsets.df$allData$Vs30)
# ####not used####        # minVs30 <- 0
# ####not used####        # VsLimVec <- c(minVs30,maxVs30)
# ####not used####        # VsLimVec <- c(0,2000)
# ####not used####        # VsLimVecLog <- c(100,2000)
# ####not used####        # VsBreakVec <- seq(0,2000,100)
# ####not used####        # i <- 1
# ####not used####
# ####not used####
# ####not used####        for (nameStr in c("noQ3","noQ3noMcGann")) {
# ####not used####          vsprdf <- vsprSubsets.df[[nameStr]]
# ####not used####
# ####not used####          resYlim <- max(abs(vsprdf$res_AhdiAK))
# ####not used####          resYlim <- ceiling(resYlim/0.5)*0.5
# ####not used####          resYlim <- c(-resYlim, resYlim)
# ####not used####          resYlim <- c(-3,3)
# ####not used####
# ####not used####          slp <- "09c"
# ####not used####          plt <- ggplot() +
# ####not used####            geom_point(aes(slp09c, res_AhdiAK), data = vsprdf, alpha= 0.2)
# ####not used####          plt <- plt +
# ####not used####            facet_wrap(c("groupID_AhdiAK"), drop=FALSE) +
# ####not used####            scale_x_log10(limits=slopeLimVec) +
# ####not used####            ylim(resYlim) +
# ####not used####            ggtitle(paste0(nameStr,
# ####not used####                           ": residual versus ", slp, " slope"))
# ####not used####
# ####not used####          BB3 <- plt + theme_linedraw()
# ####not used####
# ####not used####          fn <- paste0("out/figs/BB3_",nameStr,".png")
# ####not used####          plts(filename=fn,
# ####not used####                     plot=BB3, width = 6, height=6, units = "in")
# ####not used####        }
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####        ############################################
# ####not used####        ############################################
# ####not used####        # nuisance.png #############################
# ####not used####        ############################################
# ####not used####
# ####not used####        # Illustrate what statisticians call "nuisance parameters"
# ####not used####
# ####not used####        rm(list=ls())
# ####not used####        setwd("~/VsMap/")
# ####not used####
# ####not used####        library(ggplot2)
# ####not used####        # library(geoR) # for inverse chi-squared distribution. not really needed but more "realistic" for sigma distribution.
# ####not used####        library(invgamma)  # only this library has the qinvchisq function!
# ####not used####
# ####not used####        set.seed(1) # for repeatable figure generation
# ####not used####
# ####not used####        x <- seq(-10,10,0.01)  # x values for plotting
# ####not used####        n <- 100                # number of observations
# ####not used####
# ####not used####        big_std_dev <- 1
# ####not used####        measurements  <- rnorm(n,0,big_std_dev) # generate a vector of observations
# ####not used####        measUncer     <- runif(n,0.05,0.5) # generate a vector of arbitrary "measurement uncertainties" (standard deviations), one for each observation
# ####not used####
# ####not used####        probarray <- array(dim = c(length(x),n))
# ####not used####
# ####not used####        for(i in 1:n) {
# ####not used####          probarray[,i] <- dnorm(x = x, mean = measurements[i],sd = measUncer[i])
# ####not used####        }
# ####not used####
# ####not used####
# ####not used####        probDF <- stack(as.data.frame(probarray))
# ####not used####        probDF$x <- x
# ####not used####        probDF$totTheo <- dnorm(x = x, mean = 0, sd = big_std_dev)
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####        # Now, conduct some trials. sample from each observation's distribution repeatedly.
# ####not used####
# ####not used####        numTrials <- 200
# ####not used####        trials <- array(dim = c(numTrials, n))
# ####not used####
# ####not used####        for(t in 1:numTrials) {
# ####not used####          trials[t,] <- rnorm(n = n, mean = measurements, sd = measUncer)
# ####not used####        }
# ####not used####
# ####not used####        # save a vector of means and standard deviations; compute each mu and std.dev. from the corresponding "trial" statistics
# ####not used####        trialMeans <- rowSums(trials) / ncol(trials)
# ####not used####        trialSDs <- vector(length = numTrials)
# ####not used####        for(t in 1:numTrials) {
# ####not used####          trialSDs[t]   <-   sqrt(sum((trials[t,] - trialMeans[t])^2) / (n-1))
# ####not used####        }
# ####not used####
# ####not used####
# ####not used####
# ####not used####        # Maximum likelihood functions for the inverse chi-sq distribution:  used to choose best parameter values for
# ####not used####
# ####not used####        MLE <- function(x, df) {   # maximum likelihood estimator. the SMALLER the output, the better the fit.
# ####not used####          R = dinvchisq(x, df)
# ####not used####          -sum(log(R))
# ####not used####        }
# ####not used####
# ####not used####        MLEv <- function(x, dfV) {   # maximum likelihood estimator - vector for a range of dfV. The SMALLEST output value corresponds to best fit.
# ####not used####          out <- vector(length = length(dfV))
# ####not used####          for(i in 1:length(dfV)) {
# ####not used####            R = dinvchisq(x, dfV[i])
# ####not used####            out[i] <- -sum(log(R))
# ####not used####          }
# ####not used####          out
# ####not used####        }
# ####not used####
# ####not used####        # now do cumulative averaging of the mean and standard deviation for increasing number of trials.
# ####not used####        # The final (mu,std.dev.) couple should be closest to the underlying distribution.
# ####not used####        trialsAve <- array(dim = c(numTrials, 2))
# ####not used####        for(t in 1:numTrials) {
# ####not used####          trialsAve[t,1] <- mean(trialMeans[1:t])
# ####not used####          # df_vec <- seq(1,10,0.1)
# ####not used####          # fit <- MLEv(trialSDs[1:t],df_vec)
# ####not used####          # dfBest <- df_vec[fit==min(fit)]
# ####not used####          # sigmaGuess <- 1/(dfBest+2) # definition of mode of inverse chi-sq dist: 1/(nu+2)
# ####not used####          # trialsAve[t,2] <- sigmaGuess
# ####not used####          trialsAve[t,2] <- mean(trialSDs[1:t])
# ####not used####        }
# ####not used####
# ####not used####
# ####not used####
# ####not used####        # I'm not sure if it makes better sense to show cumulative averaging across many trials
# ####not used####        # (i.e. such that more trials -> closer agreement with the sample distribution), OR to show
# ####not used####        # a number of trials without cumulative averaging (i.e. so that the "cloud" of "trial distributions" is centered around the sample distribution).
# ####not used####
# ####not used####        cumulativeAveraging <- F
# ####not used####        if(cumulativeAveraging) {
# ####not used####          meanVec <- trialsAve[,1]
# ####not used####          stdvVec <- trialsAve[,2]
# ####not used####        } else {
# ####not used####          meanVec <- trialMeans
# ####not used####          stdvVec <- trialSDs
# ####not used####        }
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####
# ####not used####        # finally create a trialDist (trial distributions) array and populate it with (quantile, density) for distributions
# ####not used####        # that are defined by their trial's mean and standard deviation.
# ####not used####        trialDists <- array(dim=c(numTrials,length(x)))
# ####not used####        for(t in 1:numTrials) {
# ####not used####          trialDists[t,] <- dnorm(x=x, mean=meanVec[t], sd = stdvVec[t])
# ####not used####        }
# ####not used####
# ####not used####        # trialDists <- trialDists[c(1,2,3,4,5,10,30,100)*numTrials/100,] # if cumulativeAveraging=T, then the later members of trialDists will be closer to the sample distribution. If F, then the ordering of entries is not important.
# ####not used####        trialDF <- stack(as.data.frame(t(trialDists)))
# ####not used####        trialDF$x <- x
# ####not used####
# ####not used####
# ####not used####
# ####not used####        # generate sample distribution
# ####not used####        sampleMean <- mean(measurements)
# ####not used####        sampleSD   <- sqrt(
# ####not used####                           sum(   (measurements - sampleMean)^2    )
# ####not used####                            /
# ####not used####                           (length(measurements) - 1))
# ####not used####        sampleDist <- dnorm(x,sampleMean,sampleSD)
# ####not used####        sampleDistDF <- data.frame(x = x, y = sampleDist)
# ####not used####
# ####not used####
# ####not used####        png("out/figs/nuisance.png",width = 6,height = 3,units = "in",res=600)
# ####not used####        ggplot() +
# ####not used####          geom_line(data=probDF,          mapping = aes(x = x, y=values,   group=ind), color="gray", alpha=0.3) +  # density functions for each measurement; s.d. indicates uncertainty
# ####not used####          # geom_line(data=probDF[,c('x','totTheo')], aes(x = x, y=totTheo), color = "black", size=3) +  # true underlying distribution
# ####not used####          # geom_line(data=trialDF, mapping = aes(x=x, y=values, color=ind)) +  # density functions representing successive estimates of underlying distribution, with each "trial" representing a new set of realizations of the observations based on underlying measurement uncertainty distributions.
# ####not used####          geom_line(data=trialDF,         mapping = aes(x=x,   y=values,   group=ind), color="red", alpha=0.1) +  # density functions representing successive estimates of underlying distribution, with each "trial" representing a new set of realizations of the observations based on underlying measurement uncertainty distributions.
# ####not used####          geom_line(data=sampleDistDF,              aes(x = x, y = y),     color = "blue",  size=1) +
# ####not used####          theme_void() +
# ####not used####          ylim(0,2) +
# ####not used####          xlim(-2,2)
# ####not used####          # theme(legend.position = "none") #+
# ####not used####          # xlim(c(-2,2))
# ####not used####        dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
# # For comparing the predictions of geology and terrain models
# rm(list=ls())
# library(ggplot2)
# 
# source("R/functions.R")
# load("Rdata/vspr.Rdata")
# vs <- subsetVsPoints(data.frame(vspr))
# vs <- vs$noQ3
# 
# source("R/Vs30palette.R")
# 
# # categories
# GeoCat <- vs$groupID_AhdiAK
# TerCat <- vs$groupID_YongCA
# 
# geoPost  <- vs$res_AhdiAK_noQ3_hyb09c / vs$stDv_AhdiAK_noQ3_hyb09c
# geoPri   <- vs$res_AhdiAK             / vs$stDv_AhdiAK
# terPost  <- vs$res_YongCA_noQ3        / vs$stDv_YongCA_noQ3
# terPri   <- vs$res_YongCA             / vs$stDv_YongCA
# geoMVN   <- vs$res_MVN_AhdiAK_noQ3_hyb09c / vs$stDv_MVN_AhdiAK_noQ3_hyb09c
# terMVN   <- vs$res_MVN_YongCA_noQ3        / vs$stDv_MVN_YongCA_noQ3
# 
# Vs30 <- vs$Vs30
# 
# resF <- data.frame(geoPri, geoPost,
#                    terPri, terPost,
#                    geoMVN, terMVN,
#                    GeoCat,TerCat,Vs30)
# 
# axlims = 5.0 * c(-1,1)
# 
# alph=0.4
# 
# baseline <- ggplot() + 
#   theme_minimal() + 
#   coord_equal() + xlim(axlims) + ylim(axlims) +
#   geom_line( data    = data.frame(x=axlims,y=axlims),
#              mapping = aes(x,y)) 
# 
# xl <- function(s) {sprintf("Normalized %s\ngeology model residual (ζ)",s)}
# yl <- function(s) {sprintf("Normalized %s\nterrain model residual (ζ)",s)}
# 
# h=3
# w=4
# 
# rt <- "prior"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoPri,terPri, color=GeoCat)) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPrior_colByGeo.png",height = h,width=w)
# 
# rt <- "prior"
# baseline + 
#   geom_point(data = resF,
#              mapping = aes(geoPri,terPri, color=TerCat)) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPrior_colByTer.png",height = h,width=w)
# 
# rt <- "prior"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoPri,terPri, color=Vs30), alpha = alph) +
#   scale_colour_gradientn(
#     # colours=rainbow(6)) +
#     colours=modelCol, limits=zlims) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPrior_colByVs30.png",height = h,width=w)
# 
# rt <- "prior"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoPri,terPri)) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPrior.png",height = h,width=w)
# 
# rt <- "posterior"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoPost,terPost, color=GeoCat)) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPost_colByGeo.png",height = h,width=w)
# 
# rt <- "posterior"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoPost,terPost, color=TerCat)) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPost_colByTer.png",height = h,width=w)
# 
# rt <- "posterior"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoPost,terPost, color=Vs30), alpha = alph) +
#   scale_colour_gradientn(
#     # colours=rainbow(6)) +
#     colours=modelCol, limits=zlims) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPost_colByVs30.png",height = h,width=w)
# 
# rt <- "posterior"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoPost,terPost, color=Vs30), alpha = alph) +
#   scale_colour_gradientn(
#     # colours=rainbow(6)) +
#     colours=modelCol, limits=zlims) +
#   xlab(expression("Geology-based residual ζ"[i])) + 
#   ylab(expression("Terrain-based residual ζ"[i])) + 
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPost_colByVs30manual.png",height = h,width=w)
# 
# rt <- "posterior"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoPost,terPost)) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPost.png",height = h,width=w)
# 
# rt <- "posterior"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoPost,terPost, color=Vs30), alpha = alph) +
#   scale_colour_gradientn(
#     # colours=rainbow(6)) +
#     colours=modelCol, limits=zlims) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiPost_colByVs30.png",height = h,width=w)
# 
# 
# # After MVN:
# rt <- "MVN"
# baseline +
#   geom_point(data = resF,
#              mapping = aes(geoMVN,terMVN, color=Vs30), alpha = alph) +
#   scale_colour_gradientn(
#     # colours=rainbow(6)) +
#     colours=modelCol, limits=zlims) +
#   xlab(xl(rt)) + ylab(yl(rt))
# ggsave(filename = "/home/kmf76/VsMap/out/plots/splot_YongAhdiMVN_colByVs30.png",height = h,width=w)
