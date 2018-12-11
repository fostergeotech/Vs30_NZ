rm(list=ls())
setwd("~/VsMap")
library(gstat)
library(ggplot2)
source("R/models.R")
source("R/functions.R")
load("Rdata/vspr.Rdata")


# choosing a variogram for the following model:

MODEL <- "AhdiYongWeighted1"




modelSubset <- "Kaiser_all" # testing
modelSubset <- "allData" # testing
modelSubset <- "McGann" # testing
modelSubset <- "noQ3" # testing


dat <- subsetVsPoints(vspr)

# quick n dirty loop to remove NA points
for(subsetName1 in names(dat)) {
  subset1 <- dat[[subsetName1]]
  subset2 <- subset1[!is.na(subset1$res_AhdiYongWeighted1),]
  dat[[subsetName1]] <- subset2
}

# make empirical variograms with three separate lag widths
ev1k  = variogram(res_AhdiYongWeighted1~1,data = dat$Kaiser_all, cutoff=500e3, width=1000)
ev2k  = variogram(res_AhdiYongWeighted1~1,data = dat$Kaiser_all, cutoff=500e3, width=2000)
ev3k  = variogram(res_AhdiYongWeighted1~1,data = dat$Kaiser_all, cutoff=500e3, width=5000)
ev1a  = variogram(res_AhdiYongWeighted1~1,data = dat$allData,    cutoff=500e3, width=1000)
ev2a  = variogram(res_AhdiYongWeighted1~1,data = dat$allData,    cutoff=500e3, width=2000)
ev3a  = variogram(res_AhdiYongWeighted1~1,data = dat$allData,    cutoff=500e3, width=5000)
ev1m  = variogram(res_AhdiYongWeighted1~1,data = dat$McGann,     cutoff=500e3, width=1000)
ev2m  = variogram(res_AhdiYongWeighted1~1,data = dat$McGann,     cutoff=500e3, width=2000)
ev3m  = variogram(res_AhdiYongWeighted1~1,data = dat$McGann,     cutoff=500e3, width=5000)
ev1n  = variogram(res_AhdiYongWeighted1~1,data = dat$noQ3,       cutoff=500e3, width=1000)
ev2n  = variogram(res_AhdiYongWeighted1~1,data = dat$noQ3,       cutoff=500e3, width=2000)
ev3n  = variogram(res_AhdiYongWeighted1~1,data = dat$noQ3,       cutoff=500e3, width=5000)
ev1nm = variogram(res_AhdiYongWeighted1~1,data = dat$noQ3noMcGann,       cutoff=500e3, width=1000)
ev2nm = variogram(res_AhdiYongWeighted1~1,data = dat$noQ3noMcGann,       cutoff=500e3, width=2000)
ev3nm = variogram(res_AhdiYongWeighted1~1,data = dat$noQ3noMcGann,       cutoff=500e3, width=5000)


emp1k <- ggplot() + geom_point(data = ev1k, aes(x=dist/1000, y=gamma)) 
emp2k <- ggplot() + geom_point(data = ev2k, aes(x=dist/1000, y=gamma)) 
emp3k <- ggplot() + geom_point(data = ev3k, aes(x=dist/1000, y=gamma)) 
emp1a <- ggplot() + geom_point(data = ev1a, aes(x=dist/1000, y=gamma)) 
emp2a <- ggplot() + geom_point(data = ev2a, aes(x=dist/1000, y=gamma)) 
emp3a <- ggplot() + geom_point(data = ev3a, aes(x=dist/1000, y=gamma)) 
emp1m <- ggplot() + geom_point(data = ev1m, aes(x=dist/1000, y=gamma)) 
emp2m <- ggplot() + geom_point(data = ev2m, aes(x=dist/1000, y=gamma)) 
emp3m <- ggplot() + geom_point(data = ev3m, aes(x=dist/1000, y=gamma)) 
emp1n <- ggplot() + geom_point(data = ev1n, aes(x=dist/1000, y=gamma)) 
emp2n <- ggplot() + geom_point(data = ev2n, aes(x=dist/1000, y=gamma)) 
emp3n <- ggplot() + geom_point(data = ev3n, aes(x=dist/1000, y=gamma)) 
emp1nm <- ggplot() + geom_point(data = ev1nm, aes(x=dist/1000, y=gamma)) 
emp2nm <- ggplot() + geom_point(data = ev2nm, aes(x=dist/1000, y=gamma)) 
emp3nm <- ggplot() + geom_point(data = ev3nm, aes(x=dist/1000, y=gamma)) 

distanceVec <- exp(seq(log(10), log(5e5), length.out = 100))


fitVarManual <- vgm(psill=0.3,
                    model="Mat",
                    range=20e3,
                    nugget=0.01,
                    kappa = 0.9)
vgmTheo_manual <- variogramLine(fitVarManual, dist_vector = distanceVec)


emp1k + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 1km", "Kaiser_all"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag1km_%s_dat_%s.png",MODEL, "Kaiser_all"))

emp2k + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 2km", "Kaiser_all"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag2km_%s_dat_%s.png",MODEL, "Kaiser_all"))

emp3k + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 5km", "Kaiser_all"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag5km_%s_dat_%s.png",MODEL, "Kaiser_all"))


emp1m + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.2)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 1km", "McGann")) +
  xlim(c(0,50))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag1km_%s_dat_%s.png",MODEL, "McGann"))

emp2m + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.2)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 2km", "McGann")) +
  xlim(c(0,50))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag2km_%s_dat_%s.png",MODEL, "McGann"))

emp3m + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.2)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 5km", "McGann")) +
  xlim(c(0,50))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag5km_%s_dat_%s.png",MODEL, "McGann"))




emp1a + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 1km", "allData"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag1km_%s_dat_%s.png",MODEL, "allData"))

emp2a + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 2km", "allData"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag2km_%s_dat_%s.png",MODEL, "allData"))
  
emp3a + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 5km", "allData"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag5km_%s_dat_%s.png",MODEL, "allData"))





emp1n + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 1km", "noQ3"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag1km_%s_dat_%s.png",MODEL, "noQ3"))

emp2n + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 2km", "noQ3"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag2km_%s_dat_%s.png",MODEL, "noQ3"))

emp3n + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 5km", "noQ3"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag5km_%s_dat_%s.png",MODEL, "noQ3"))




emp1nm + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 1km", "noQ3noMcGann"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag1km_%s_dat_%s.png",MODEL, "noQ3noMcGann"))

emp2nm + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 2km", "noQ3noMcGann"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag2km_%s_dat_%s.png",MODEL, "noQ3noMcGann"))

emp3nm + geom_line(data = vgmTheo_manual, aes(x=dist/1000,y=gamma)) + ylim(c(0,0.35)) + xlab("dist, km") +
  ggtitle("Empirical and theoretical variogram",
          subtitle = sprintf("Data: %s   Lag width: 5km", "noQ3noMcGann"))
ggsave(filename = sprintf("out/plots/vgram_emp+theo_lag5km_%s_dat_%s.png",MODEL, "noQ3noMcGann"))










  
variogram <- fitVarManual
save(variogram, file = paste0("Rdata/variogram_", MODEL, ".Rdata"))

