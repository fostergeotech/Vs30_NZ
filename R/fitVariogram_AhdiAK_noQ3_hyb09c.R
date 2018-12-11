rm(list=ls())
setwd("~/VsMap")
library(gstat)
library(ggplot2)
source("R/models.R")
source("R/functions.R")
load("Rdata/vspr.Rdata")


# choosing a variogram for the following model:
# MODEL <- "AhdiAK_KaiAll_hyb09c"
MODEL <- "AhdiAK_noQ3_hyb09c"




modelSubset <- "Kaiser_all" # testing
modelSubset <- "allData" # testing
modelSubset <- "McGann" # testing
modelSubset <- "noQ3" # testing


dat <- subsetVsPoints(vspr)

# make empirical variograms with three separate lag widths
ev1k = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$Kaiser_all, cutoff=500e3, width=1000)
ev2k = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$Kaiser_all, cutoff=500e3, width=2000)
ev3k = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$Kaiser_all, cutoff=500e3, width=5000)
ev1a = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$allData,    cutoff=500e3, width=1000)
ev2a = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$allData,    cutoff=500e3, width=2000)
ev3a = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$allData,    cutoff=500e3, width=5000)
ev1m = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$McGann,     cutoff=500e3, width=1000)
ev2m = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$McGann,     cutoff=500e3, width=2000)
ev3m = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$McGann,     cutoff=500e3, width=5000)
ev1n = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$noQ3,       cutoff=500e3, width=1000)
ev2n = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$noQ3,       cutoff=500e3, width=2000)
ev3n = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$noQ3,       cutoff=500e3, width=5000)
ev1nm = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$noQ3noMcGann,       cutoff=500e3, width=1000)
ev2nm = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$noQ3noMcGann,       cutoff=500e3, width=2000)
ev3nm = variogram(res_AhdiAK_noQ3_hyb09c~1,data = dat$noQ3noMcGann,       cutoff=500e3, width=5000)


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

# The variogram fit will be based on a compromise between KaiserAll and allData.
# (The fit is "good enough" for allData; this decision is intended to be "conservative"
# in the sense that it does not understate uncertainty---which is biased in short range
# in the AllData set because of McGann & Christchurch data.)

# the nugget and kappa parameters will be fine-tuned based on McGann data, which is the only thing
# that can constrain them.

          # vgmModel <- vgm(psill = 0.1, model = "Mat", range = 1e5, nugget = 0.1)
          # 
          # # fitVariogram  <- fit.variogram(empiricalVariogram, vgmModel, fit.sills = F, fit.kappa = T)
          # # even though the fitting algorithm complains about not converging with fit.kappa = T,
          # # the final product looks good. Just to be sure, use the greater lag width of ev2:
          # fitVariogram  <- fit.variogram(ev2,                vgmModel, fit.sills = F, fit.kappa = T)
          # # no convergence problems here. lag width helps here.
          # # I'm not satisfied with the auto-fitted variogram because it doesn't "compromise" well
          # # with the allData set. I'd like the nugget to be a little lower, so I override it.
          # # Here's the autofit version:
          # # > fitVariogram
          # # model      psill    range kappa
          # # 1   Nug 0.11421713     0.00     0
          # # 2   Mat 0.07743204 12447.67     5
          # fitVarManual  <- vgm(psill = 0.12, model = "Mat", range = 12450, nugget = 0.08, kappa = 5)
          # # vgmTheo <- variogramLine(fitVariogram, dist_vector = distanceVec)
          # vgmTheo <- variogramLine(fitVarManual, dist_vector = distanceVec)
  
fitVarManual <- vgm(psill=0.15,
                    model="Mat",
                    range=20e3,
                    nugget=0.05,
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

