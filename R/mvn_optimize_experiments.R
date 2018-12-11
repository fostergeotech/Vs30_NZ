library(raster)

d = raster("~/big_noDB/models/RESAMP_0900x0900_SI_AhdiYongWeighted1.tif")
e = terrain(d, opt = 'slope', unit = 'tangent')
f = terrain(d, opt = 'slope', unit = 'degrees')
g = terrain(d, opt = 'TRI')

sigPri = raster("~/big_noDB/models/RESAMP_0900x0900_SI_AhdiYongWeighted1_sigma.tif")
sigMVN = raster("~/big_noDB/models/RESAMP_0900x0900_SI_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif")
sigMVN = raster("~/big_noDB/models/RESAMP_0900x0900_SI_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyF_minDist0.0km_v3_crp0.0.tif")
sigRat = sigMVN/sigPri
plot(sigRat)

sr95 <- sigRat > 0.95
sr96 <- sigRat < 0.96
sr9596 <- (sr95 & sr96)
plot(sr9596)


