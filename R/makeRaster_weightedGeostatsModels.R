#  makeRaster_AhdiYongKRGWeighted1.R
# 
# Creates a model raster by combining AhdiAK_noQ3_hyb09c and YongCA_noQ3.

rm(list=ls())
library(raster)

# model <- "KRG"
model <- "MVN"


#########################################################################################################################
# Making the final mean map:

Vs30Ahdi <- raster(switch(model,
                          KRG="~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
                          MVN="~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif"
                          ))
Vs30Yong <- raster(switch(model,
                          KRG = "~/big_noDB/models/KRG_Vs30_NZGD00_allNZ_YongCA_noQ3_v7.tif",
                          MVN = "~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif"
                          ))
lY <- log(Vs30Yong)
lA <- log(Vs30Ahdi)
lAY <- ((lY + lA)*0.5)
Vs30AhdiYong <- exp(lAY)
writeRaster(Vs30AhdiYong, sprintf(
  "~/big_noDB/models/AhdiYongWeighted%s_Vs30.tif", switch(model,
                                                          KRG = "KRG",
                                                          MVN = "MVN_nTcrp1.5")),
  format="GTiff", overwrite=T)


#########################################################################################################################
# Making the final sigma map:

sigmaAhdi  <- raster(switch(model,
                            KRG = "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_v6.tif",
                            MVN = "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif"
                            ))
sigmaYong  <- raster(switch(model,
                            KRG = "~/big_noDB/models/KRG_stDv_NZGD00_allNZ_YongCA_noQ3_v7.tif",
                            MVN = "~/big_noDB/models/MVN_stDv_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif"
                            ))

sig1sq <- sigmaAhdi^2
sig2sq <- sigmaYong^2
mu1    <- lA
mu2    <- lY
mu     <- lAY

# Reference: https://en.wikipedia.org/wiki/Mixture_distribution#Moments
sigsq <- 0.5 * ( (( mu1 - mu)^2) + sig1sq +
                   (( mu2 - mu)^2) + sig2sq)
sig <- sigsq^0.5

writeRaster(sig, sprintf("~/big_noDB/models/AhdiYongWeighted%s_sigma.tif", switch(model,
                                                                                  KRG = "KRG",
                                                                                  MVN = "MVN_nTcrp1.5"
                                                                                  )),
            format="GTiff", overwrite=T)
  

