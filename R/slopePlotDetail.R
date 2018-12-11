# slopePlotDetail.R
#
# repeats some of the slope plots in plots1.R for detailed examination
# and developing individual hybrid model
#

rm(list = ls())
setwd("~/VsMap/")

pubFigs <- "yes" # mark true for publication; false for more info per plot.
# pubFigs <- "no" # mark true for publication; false for more info per plot.






simultaneous_CBs <- function(linear_model, newdata, level = 0.95){
  #
  # From https://stats.stackexchange.com/questions/231632/how-to-plot-simultaneous-and-pointwise-confidence-bands-for-linear-regression-wi
  #
  # Thanks to user: https://stats.stackexchange.com/users/58675/deltaiv
  
  # Working-Hotelling 1 – α confidence bands for the model linear_model
  # at points newdata with α = 1 - level
  
  # estimate of residual standard error
  lm_summary <- summary(linear_model)
  # degrees of freedom 
  p <- lm_summary$df[1]
  # residual degrees of freedom
  nmp <-lm_summary$df[2]
  # F-distribution
  Fvalue <- qf(level,p,nmp)
  # multiplier
  W <- sqrt(p*Fvalue)
  # confidence intervals for the mean response at the new points
  CI <- predict(linear_model, newdata, se.fit = TRUE, interval = "confidence", 
                level = level)
  # mean value at new points
  Y_h <- CI$fit[,1]
  # Working-Hotelling 1 – α confidence bands
  LB <- Y_h - W*CI$se.fit
  UB <- Y_h + W*CI$se.fit
  sim_CB <- data.frame(LowerBound = LB, Mean = Y_h, UpperBound = UB)
}










a <- 1:9*10
b <- seq(from=2, to=8, by=2)*10
aa <- 1:9
bb <- seq(2,8,2)

# research plot sizes
HEIGHT = 6
VsYlim <- c(20, 3000)
VsYbrksMaj <- c(2:10*10, 2:10*100, 2:3*1000)
VsYbrksMin <- c(20+aa, 30+aa, 40+aa, 50+bb, 60+bb, 70+bb, 80+bb, 90+bb,
                100+a, 200+a, 300+a, 400+a, 500+b, 600+b, 700+b, 800+b, 900+b,
                seq(from=1100, to=1900, by=100), seq(from=2100, to=2900, by=100))
VsYlab <- as.character(VsYbrksMaj)
log10slopeLims <- c(-5,0)  # this is consistent across both Vs30 and residual plots, and (so long as vertical axes are chosen to be consistent) yields same slope of lines whether looking at model residuals or raw Vs30 values.

# publication plot sizes
HEIGHT = 2
WIDTH=4
# VsYlim <- c(100, 1000)
VsYlim <- c(100, 600)
# VsYbrksMaj <- c(    100, 2:10*100)
VsYbrksMaj <- c(    100, 2:6*100)
# VsYbrksMin <- c(100+a, 200+a, 300+a, 400+a, 500+b, 600+b, 700+b, 800+b, 900+b)
VsYbrksMin <- c(100+a, 200+a, 300+a, 400+a, 500+b)
VsYlab <- as.character(VsYbrksMaj)
log10slopeLims <- c(-4,0)  # this is consistent across both Vs30 and residual plots, and (so long as vertical axes are chosen to be consistent) yields same slope of lines whether looking at model residuals or raw Vs30 values.
log10slopeSeq  <- log10slopeLims[1]:log10slopeLims[2]
log10slopeLabs <- format(10^log10slopeSeq, scientific=F,drop0trailing=T)

vsprFileName <- "Rdata/vspr.Rdata"
load(vsprFileName)

library(sp)
library(ggplot2)

source("R/functions.R")
source("R/models.R")
source("R/classifyThings.R")


# Set groupScheme to choose how geology groups will be chosen.
groupScheme <- "AhdiAK"


# obtain subsets of data
vsprdf <- data.frame(vspr)
vsprSubsets.df <- subsetVsPoints(vsprdf)




for(showDenStrips in c(F)) { ############################################################################# loop over showDenStrips
  # showDenStrips <- F
  dstr <- switch(showDenStrips, T = "_dsY",
                 F = "_dsN")

  # for(i in 1:length(vsprSubsets.df)) { ################################################################### loop over all data subsets
  for(i in 3) {                 ################################################################### loop over some data subsets
    # i <- 3
    vsprdf <- vsprSubsets.df[[i]]
    subsetName <- names(vsprSubsets.df)[[i]]

    groupID <- paste0("groupID_", groupScheme)
    vsprdf$groupID <- vsprdf[[groupID]]

    
    slopeTable <- data.frame(groupID      = c(),
                             logSlopeMin  = c(), 
                             logSlopeMax  = c(), 
                             Vs30min      = c(), 
                             Vs30max      = c(), 
                             sigma_uncond = c(), 
                             sigma_fSlope = c(), 
                             reduc_coeff  = c()) 
    slopeTableRow <- 0
    
    # for (grp in levels(vsprdf$groupID)) { ######################################################## loop over each geology category
    for (grp in levels(vsprdf$groupID)[c(4,5,6,8)]) { # loop over just the ones with slope dependence
      # grp <- "06_alluvium"
      # grp <- "04_fill"
      
      slopeTableRow <- slopeTableRow + 1
      
      
      vsprdf1 <- subset(vsprdf, groupID==grp)


      # vsprdf1$logslp09c <- log(vsprdf1$slp09c)
      # vsprdf1$logslp30c <- log(vsprdf1$slp30c)
      vsprdf1$log10slp09c <- log10(vsprdf1$slp09c)      # log10 is more intuitive for slope axis.
      # vsprdf1$log10slp30c <- log10(vsprdf1$slp30c)

      slp <- "09c"
      # slp <- "30c"


      # for (MODEL in allMODELs) {
      # for (MODEL in c(#"AhdiAK", "AhdiAK_noQ3", 
      #                 "AhdiAK_noQ3_hyb09c") ) {
      #   ############################################### 
      #   # loop over each model. (most likely will not do more than one.)
      #   ############################################### 
      #   # MODEL <- "AhdiAK_noQ3_hyb09c"
      # 
      #   resName <- paste0("res_",MODEL)
      #   vsprdf1$residual <- vsprdf1[[resName]]
      #   groupIDstr <- paste0("groupID_", MODEL)
      #   vsprdf1$groupID  <- vsprdf1[[groupIDstr]]
      # 
      # 
      # 
      #   # ## Set Ylimits based on max & min residuals
      #   # if(dim(vsprdf1)[1] >0) { # check that the subset of interest has >0 residuals
      #   #   resYlim <- max(abs(vsprdf1$residual))
      #   #   resYlim <- ceiling(resYlim/0.5)*0.5
      #   #   resYlim <- c(-resYlim, resYlim)
      #   # } else {
      #   #   resYlim <- c(-1,1)
      #   # }
      # 
      # 
      #   # INSTEAD, set resYlim as constant so that vertical scales and slopes are comparable with Vs30 version.
      #   # These limits are great enough to include all residuals from current models.
      #   resYlim <- c(-2.5,2.5)
      #   # because log(3000/20) ~= 5 = 2.5-(-2.5), max and min Vs30 of 3000 and 20 m/s are good choices for scaling of the Vs30 plots below.
      # 
      # 
      #   # groupID  -  slope-residual plots
      #   groupScheme <- "AhdiAK"
      #   groupID <- paste0("groupID_", groupScheme)
      # 
      # 
      # 
      # 
      #   plt <- ggplot(vsprdf1, aes(log10slp09c, residual)) +
      #     geom_point(alpha= 0.2) +
      #     ylim(resYlim) +
      #     ylab("residual, log(obs/pred)") +
      #     xlim(log10slopeLims) +
      #     xlab("Slope") +
      #     ggtitle(paste0(subsetName,
      #                    ": groupID=",grp,", residual vs ", slp, " slope")) +
      #     theme_linedraw() +
      #     geom_smooth(method = "lm")
      #   ggsave(filename = paste0("out/plots/slope_res--grp-",groupScheme,"-mod-",MODEL,"-",
      #                            grp,"_dat_",subsetName,"_",slp,dstr,".png"),
      #          plot = plt, width=6, height=6)
      # }


      ########################################################################################## This part generates Vs30 plots not residual plots. No need to loop over models....


      


      plt <- ggplot() +
                geom_point( data = vsprdf1, mapping = aes(log10slp09c, Vs30), alpha= 0.2) +
                ylab(expression("V"[s30]*" (m/s)")) +
                scale_y_log10(breaks = VsYbrksMaj, labels = VsYlab, limits = VsYlim, minor_breaks = VsYbrksMin) +
                xlab(switch(pubFigs,
                       no  = paste0("log10(",slp," slope)"),
                       yes = "slope")) +
                scale_x_continuous(breaks = log10slopeSeq, minor_breaks = c(), labels = log10slopeLabs,limits = log10slopeLims) +
                theme_linedraw()


      if(dim(vsprdf1)[1] > 0) {
        # Cannot incorporate fitting part if no data.
        plt <- plt + geom_smooth(data = vsprdf1, mapping = aes(log10slp09c, Vs30), method = "lm",
                                 alpha = 0, color="black", size=0.3)

        # Determine min and max Vs30 values on fit line
        lmp <- lm(log(Vs30) ~ log10(slp09c), data = vsprdf1)
        xmn <- min(vsprdf1$log10slp09c)
        xmx <- max(vsprdf1$log10slp09c)
        
        # compute confidence bands to plot lines. (Can't figure out how to do this in ggplot2 using geom_smooth.)
        log10slp_plt <- seq(log10slopeLims[1],log10slopeLims[2], 0.02) # entire plot
        log10slp_plt <- seq(xmn              , xmx             , 0.02) # just within limits
        nd <- data.frame(slp09c = 10^log10slp_plt)
        CBs <- simultaneous_CBs(linear_model = lmp, newdata = nd,level = 0.95)
        CBs$UpperBound <- exp(CBs$UpperBound)
        CBs$LowerBound <- exp(CBs$LowerBound)
        CBs$Mean       <- exp(CBs$Mean)
        CBs$log10slp <- log10slp_plt

        plt <- plt + geom_line(data = CBs, aes(x=log10slp, y=LowerBound), linetype="dotted") +
                     geom_line(data = CBs, aes(x=log10slp, y=UpperBound), linetype="dotted")
        
        # compute std devs for both prior and posterior residual vectors:
        meanLogVs30 <- mean(log(vsprdf1$Vs30))
        trendVals <- (vsprdf1$log10slp09c)*lmp$coefficients[2] + lmp$coefficients[1]
        stDvPrior     <- sqrt(  sum(
                              (( log(vsprdf1$Vs30)   -   meanLogVs30 )^2)
                                    )
                                 / (nrow(vsprdf1)-1)
                              )
        stDvPosterior <- sqrt(sum(
                          (( log(vsprdf1$Vs30)   -   trendVals)^2)
                          ) / (nrow(vsprdf1)-1))

        
        Vs30mn <- exp(predict.lm(lmp, newdata = data.frame(slp09c = 10^c(xmn,xmx))))
        newDF <- data.frame(log10slp09c = c(xmn, xmx), Vs30pred = c(Vs30mn[1], Vs30mn[2]))

        plt <- plt + geom_point(data=newDF, aes(log10slp09c, Vs30pred), size=0.5)
        if(pubFigs=="no") {
          plt <- plt + 
                  geom_label(data=newDF, 
                    aes(x=log10slp09c, y=Vs30pred, label=sprintf("(%1.2f,%4.0f)",log10slp09c, Vs30pred)),
                    nudge_y = -0.3) +  # for showing coordinate pairs on plot 
                  ggtitle(label = paste0(subsetName, ": groupID=",grp,", Vs30 vs ", slp, " slope"),
                                         subtitle=sprintf("SIGMA uncond.: %05.3f  f(slope): %05.3f  reduction coeff: %05.4f",
                                                          stDvPrior, stDvPosterior, stDvPosterior/stDvPrior))
          }
      
        newDF <- data.frame(log10slp09c = c(xmn, xmx), Vs30pred = c(Vs30mn[1], Vs30mn[2]))
        
        slopeTableRowDF <- data.frame(groupID      = grp, 
                                      logSlopeMin  = xmn,
                                      logSlopeMax  = xmx,
                                      Vs30min      = Vs30mn[1],
                                      Vs30max      = Vs30mn[2],
                                      sigma_uncond = stDvPrior,
                                      sigma_fSlope = stDvPosterior,
                                      reduc_coeff  = stDvPosterior/stDvPrior)
        
        slopeTable <- rbind(slopeTable, slopeTableRowDF)  
        
      } else {}

      
      
      ggsave(filename = paste0("out/plots/slope_Vs30--grp-",groupScheme,"-",
                               grp,"_dat_",subsetName,"_",slp,dstr,".pdf"),
             plot = plt, width=WIDTH, height=HEIGHT)
    }
  print(slopeTable)
  }
}


save(slopeTable, file = "~/VsMap/Rdata/slopeTable.Rdata")


