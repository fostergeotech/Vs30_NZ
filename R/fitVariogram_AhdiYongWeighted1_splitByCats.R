rm(list=ls())
setwd("~/VsMap")
library(gstat)
library(ggplot2)
library(colorRamps)
source("R/models.R")
source("R/functions.R")
load("Rdata/vspr.Rdata")


# choosing a variogram for the following model:
MODEL <- "AhdiYongWeighted1"

# lagwidth or boundaries
lagwidths = c(1,2,5,10,20,30,50,75,100,120)*1e3 # metres
cutoffDist <- 500e3 # metres
# boundaries <- 2^seq(1,10,by=0.2)
# boundaries <- c(0.5,1,1.5,2,3,5,8,13,21,34,50,75,100,150,200,250,300,400,500)
# boundaries <- c(boundaries[boundaries<cutoffDist],cutoffDist)

# manual variogram fit params
psill=0.1
range=5e3
nugget=0
kappa = 0.9



dat <- subsetVsPoints(vspr)
# quick n dirty loop to remove NA points
for(subsetName1 in names(dat)) {
  subset1 <- dat[[subsetName1]]
  subset2 <- subset1[!is.na(subset1$res_AhdiYongWeighted1),]
  dat[[subsetName1]] <- subset2
}

for(catType in c("AhdiAK","YongCA")) {
# catType <- "YongCA" #testing
for(lagwidth in lagwidths) {
  # lagwidth <- lagwidths[1] # testing
  for(modelSubset in c("Kaiser_all", "allData", "McGann", "noQ3", "noQ3noMcGann")) {
    # modelSubset <- "noQ3" #testing
                       
    
    dat_subset <- dat[[modelSubset]]
    
    vgdf <- data.frame()
    
    if(identical(catType,"AhdiAK")) {
      cats <- levels(vspr$groupID_AhdiAK)
    } else {if(identical(catType,"YongCA")) {
      cats <- levels(vspr$groupID_YongCA)
    } else {stop("error")}}
    
    for(geoCatN in seq_along(cats)) {
      # geoCatN <- 1
      geoCat <- cats[geoCatN]
      
      # subset to just one geology category.
      dat_cat <- dat_subset[dat_subset[[paste0("groupID_",catType)]]==geoCat,]
      
      if(nrow(dat_cat)>=2) { # Need at least a pair of pts for variogram.
        ev   <- variogram(res_AhdiYongWeighted1~1,data = dat_cat,   cutoff=cutoffDist, width=lagwidth)
        if(!is.null(ev)) {  # variogram() returns a null object if no pairs of points are closer than cutoff.
          # ev = empirical variogram
          vgdf <- rbind(vgdf,
                        cbind(ev[,c("dist","gamma","np")],
                              data.frame(groupID=rep(geoCat,nrow(ev)))))
      }}
      # evp = empirical variogram plot
      evp  <- ggplot() +
        geom_point(data = vgdf, aes(x=dist/1000, y=gamma, col=groupID))
    }
    distanceVec <- exp(seq(log(10), log(5e5), length.out = 100))
    fitVarManual <- vgm(psill  = psill,
                        model  = "Mat",
                        range  = range,
                        nugget = nugget,
                        kappa  = kappa)
    
    vgmTheo_manual <- variogramLine(fitVarManual, dist_vector = distanceVec)
    
    
    evp + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
      ggtitle("Empirical and theoretical variogram",
              subtitle = sprintf("Data: %s   Lag width: %03.0fkm", modelSubset, lagwidth/1000))
    ggsave(filename = sprintf("out/plots/vgram_emp+theo_by_cats_%s_catType_%s_dat_%s_lag%03.0fkm.png",MODEL, catType, modelSubset, lagwidth/1000),
           width = 8, height=6)
  }
}
}


variogram <- fitVarManual
save(variogram, file = paste0("Rdata/variogram_by_cats_", MODEL, ".Rdata"))

