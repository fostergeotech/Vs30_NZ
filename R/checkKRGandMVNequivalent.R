# Confirm that Kriging and MVN are precisely the same
# for special case where CRP=0 and noise=F
# 
# (Note: both employ 0.1 metre rejection criterion
# to prevent singular covariance matrix).
# 
rm(list=ls())
library(raster)
Vs30KRG <- raster("tmp/krige_NZGD00_allNZ_x05y04_AhdiAK_noQ3_hyb09c_v6.tif")
Vs30MVN <- raster("tmp/MVkrg_NZGD00_allNZ_x05y04_AhdiAK_noQ3_hyb09c_noisyF_minDist0.0km_v6_crp0.0.tif")
Vs30dif <- Vs30KRG/Vs30MVN
maxValue(Vs30dif) # 1.002
minValue(Vs30dif) # 0.998

# In short: Vs30 values are the same.

sigmaKRGnorm <- raster("tmp/stDev_NZGD00_allNZ_x05y04_AhdiAK_noQ3_hyb09c_v6.tif")
sigmaMVN <- raster("tmp/MVsdv_NZGD00_allNZ_x05y04_AhdiAK_noQ3_hyb09c_noisyF_minDist0.0km_v6_crp0.0.tif")
plot(sigmaKRGnorm)
plot(sigmaMVN) # not same - one is normalised
# To compare, load the input sigma. Also must crop to same size...
sigmaHyb <- raster("~/big_noDB/models/sig_NZGD00_allNZ_AhdiAK_noQ3_hyb09c.tif")
sigmaHyb <- crop(sigmaHyb,sigmaKRGnorm)
sigmaKRG <- sigmaKRGnorm*sigmaHyb
sigmaDif <- sigmaKRG/sigmaMVN
maxValue(sigmaDif) # 1.0117
minValue(sigmaDif) # 0.9998

# in short: sigma values are the same. (after accounting for normalization)

w=6 #inches

residKRG <- raster("tmp/resid_NZGD00_allNZ_x05y04_AhdiAK_noQ3_hyb09c_v6.tif")
residMVN <- raster("tmp/MVres_NZGD00_allNZ_x05y04_AhdiAK_noQ3_hyb09c_noisyF_minDist0.0km_v6_crp0.0.tif")
Kmn<-minValue(residKRG) # -1.14
Kmx<-maxValue(residKRG) # 2.91
KMX<-max(abs(Kmn),abs(Kmx))
png("~/VsMap/tmp/krgRes.png", width=w)
plot(residKRG,zlim=c(-KMX,KMX))
dev.off()

Mmn <- minValue(residMVN) # -0.48
Mmx <- maxValue(residMVN) # 0.79
MMX <- max(abs(Mmn),abs(Mmx))
png("~/VsMap/tmp/mvnRes.png",width = w)
plot(residMVN, zlim=c(-MMX,MMX))
dev.off()

plot(residKRG/residMVN)


# In short: residual surface is NOT the same.
# 
# Problem was that the MVN residual surface is not normalized. Normalizing it by the input stDev solves the problem:
plot(residMVN/sigmaHyb)
plot(residMVN/sigmaHyb - residKRG, zlim=c(-0.5,0.5))
