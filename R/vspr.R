# vspr.R
#
# This program does a few things to prepare data for analysis, the main thing is loading Vs30 data and finding geology & topography for those locations.
      # Load the various Vs30 data sources (done by calling loadVs.R)
      # Find the slope at each Vs30 location
      # Find geologic unit at each Vs30 location
      # compute geologic average Vs30 values by category and compute residuals
      # Converting Vs30 coordinates to NZGD2000 (instead of NZMG).
      # Apply geology-based Vs30 functions to each point
      # Find the Iwahashi & Pike (IP) geology category for each point. (Since these are used as input to Yong CA model, name of variables = groupID_YongCA, etc.)
      # Save the Yong (2012) Vs30 value corresponding to each point (YongCA_2012)

# OUTPUT:
#    ~/VsMap/Rdata/vspr.Rdata

# INPUT:
     # Rdata/nzsi_9c_slp.Rdata
     # Rdata/nzsi_30c_slp.Rdata
     # Rdata/nzsi_9c_DEM.Rdata
     # Rdata/nzsi_30c_DEM.Rdata



rm(list = ls())

library(rgdal)
library(sp)
library(ggplot2)
library(raster)
library(spatstat)
library(utils)

setwd("~/VsMap/")

source("R/loadVs.R")
source("R/functions.R")
source("R/geodetic.R")
source("R/models.R")

vsprFileName <- "~/VsMap/Rdata/vspr.Rdata"





whichMap <- "NZ"



# =============================================================================
# read in Qmap -----------------------------------------------------------


load(file = "~/big_noDB/geo/QMAP_Seamless_July13K_NZGD00.Rdata")
map_NZGD49 <- convert2NZGD49(map_NZGD00)



# =============================================================================
# Load slope data (handled by loadDEM.R - need only run once) ---------------
load(file="Rdata/nzsi_9c_slp.Rdata")  # spatialGridDataFrame
load(file="Rdata/nzsi_30c_slp.Rdata") # spatialGridDataFrame
load(file="Rdata/nzsi_9c_DEM.Rdata")  # raster
load(file="Rdata/nzsi_30c_DEM.Rdata") # raster
load(file="Rdata/nzni_9c_slp.Rdata")  # spatialGridDataFrame
load(file="Rdata/nzni_30c_slp.Rdata") # spatialGridDataFrame
load(file="Rdata/nzni_9c_DEM.Rdata")  # raster
load(file="Rdata/nzni_30c_DEM.Rdata") # raster










# =============================================================================
# Load shear wave velocity data ------------------------------------------
#
# Only run this once when loading Vs data for first time
# Thereafter it is stored in vspr file along with polygon & topo overlay data

VsPts_NZGD49 <- loadVs(downSampleMcGann = TRUE)








# ----------------------------------------------------------------------
# Sort/categorize Vs data in terms of map polygons & geology metadata --

# Find out which points are in which polygon using over() (point-in-polygon testing)
polys <- VsPts_NZGD49 %over% map_NZGD49  # returns a dataframe with polygon metadata for each Vs point.
vspr  <- SpatialPointsDataFrame(VsPts_NZGD49, data=polys)
rm(polys)



# This shouldn't be necessary, but the %over% calls in vspr are complaining 
# that identicalCRS()=FALSE, despite the crs() strings being identical.
# As a kludge, I force identical crs() strings as follows:
p4s <- crs(vspr)
crs(slp_nzsi_9c.sgdf) <- p4s
crs(slp_nzsi_30c.sgdf) <- p4s
crs(slp_nzni_9c.sgdf) <- p4s
crs(slp_nzni_30c.sgdf) <- p4s
rm(p4s)

# Add slope overlay data (has to be done in two parts, north and south island)
df_si <- data.frame(vspr %over% slp_nzsi_9c.sgdf, vspr %over% slp_nzsi_30c.sgdf)
colnames(df_si) <- c("slp09c","slp30c")
df_ni <- data.frame(vspr %over% slp_nzni_9c.sgdf, vspr %over% slp_nzni_30c.sgdf)
colnames(df_ni) <- c("slp09c","slp30c")

# combine north and south island slope vectors
na_ni_09c <- is.na(df_ni$slp09c)
na_ni_30c <- is.na(df_ni$slp30c)
na_si_09c <- is.na(df_si$slp09c)
na_si_30c <- is.na(df_si$slp30c)

# error check: EVERY datapoint should be NA for EITHER North or South island if not both
if(!all(na_ni_09c | na_si_09c)) {print("ERROR")}
if(!all(na_ni_30c | na_si_30c)) {print("ERROR")}


slp09c <- slp30c <- rep(NA, length(vspr))
slp09c[!na_ni_09c] <- df_ni$slp09c[!na_ni_09c]
slp09c[!na_si_09c] <- df_si$slp09c[!na_si_09c]
slp30c[!na_ni_30c] <- df_ni$slp30c[!na_ni_30c]
slp30c[!na_si_30c] <- df_si$slp30c[!na_si_30c]

df <- data.frame(slp09c=slp09c,slp30c=slp30c)

vspr <- spCbind(vspr,df)
rm(df, df_ni, df_si,
   na_ni_30c,
   na_si_30c,
   na_ni_09c,
   na_si_09c)


########################################################################################
########################################################################################
# This is where I assign the Iwahashi & Pike terrain categories.
# (note, for geology categories this is done by assigning new fields beginning with
# "groupID_" in the script classifyThings.R, but I need to do it here because classifyThings
# uses only polygon data. Here I need a pixel-based overlay similar to the slope and DEM
# overlays done above.)

IP <- raster("~/big_noDB/topo/terrainCats/IwahashiPike_NZ_100m_16.tif")
IPdf <- as(IP, "SpatialGridDataFrame")
source("R/IP_levels.R")
vsprNZGD00 <- convert2NZGD00(vspr)
groupID_YongCA <- vsprNZGD00 %over% IPdf
colnames(groupID_YongCA) <- "ID" # rename column for plyr::join
groupID_YongCA_names <- plyr::join(groupID_YongCA,IPlevels[[1]]) # should get message "joining by: ID"
vspr$groupID_YongCA <- vspr$groupID_YongCA_noQ3 <- groupID_YongCA_names$category



# 2018-11  -  I want to generate post-MVN residuals. This is done here. (Must be run again after MVN rasters have been created.)

MVN_geology <- raster("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif")
MVN_terrain <- raster("~/big_noDB/models/MVN_Vs30_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif")
MVN_geology_sigma <- raster("~/big_noDB/models/MVN_stDv_NZGD00_allNZ_AhdiAK_noQ3_hyb09c_noisyT_minDist0.0km_v6_crp1.5.tif")
MVN_terrain_sigma <- raster("~/big_noDB/models/MVN_stDv_NZGD00_allNZ_YongCA_noQ3_noisyT_minDist0.0km_v7_crp1.5.tif")

MVNdf_geo <- as(MVN_geology, "SpatialGridDataFrame")
MVNdf_ter <- as(MVN_terrain, "SpatialGridDataFrame")
MVNdf_geo_sigma <- as(MVN_geology_sigma, "SpatialGridDataFrame")
MVNdf_ter_sigma <- as(MVN_terrain_sigma, "SpatialGridDataFrame")

Vs30_MVN_AhdiAK_noQ3_hyb09c <- vsprNZGD00 %over% MVNdf_geo
Vs30_MVN_YongCA_noQ3        <- vsprNZGD00 %over% MVNdf_ter
sigma_MVN_AhdiAK_noQ3_hyb09c <- vsprNZGD00 %over% MVNdf_geo_sigma
sigma_MVN_YongCA_noQ3        <- vsprNZGD00 %over% MVNdf_ter_sigma

names(Vs30_MVN_AhdiAK_noQ3_hyb09c) <- "Vs30"
names(Vs30_MVN_YongCA_noQ3)        <- "Vs30"
names(sigma_MVN_AhdiAK_noQ3_hyb09c) <- "sigma"
names(sigma_MVN_YongCA_noQ3)        <- "sigma"

vspr$Vs30_MVN_AhdiAK_noQ3_hyb09c <- Vs30_MVN_AhdiAK_noQ3_hyb09c$Vs30
vspr$Vs30_MVN_YongCA_noQ3        <- Vs30_MVN_YongCA_noQ3$Vs30
vspr$stDv_MVN_AhdiAK_noQ3_hyb09c <- sigma_MVN_AhdiAK_noQ3_hyb09c$sigma
vspr$stDv_MVN_YongCA_noQ3        <- sigma_MVN_YongCA_noQ3$sigma



# Here is where I create groupID_AhdiYongWeighted1 and similar. (This will produce an error
# if a new weighted model is added and not handled below. Update MANUALLY.)
for(weightedModelName in wtdMODELs) {
  # weightedModelName =  wtdMODELs[1] # testing
  if(identical(weightedModelName,  "AhdiYongWeighted1")) {
    vspr$groupID_AhdiYongWeighted1 <- interaction(vspr$groupID_AhdiAK,  vspr$groupID_YongCA)
  } else {stop("Weighted model is not currently handled in vspr.R")}
}



############
####################################
#################################################################
# The following needs to be updated MANUALLY when new models are produced.
#################################################################
####################################
############
# Produce model estimates for points based on slope criteria
# (geology-only models are already handled by processQmap.R)

# Vs30_AhdiAK_KaiAll_hyb09c <- AhdiAK_KaiAll_hyb09c_set_Vs30(as.data.frame(vspr))  # KaiAll model has been removed.
# stDv_AhdiAK_KaiAll_hyb09c <- AhdiAK_KaiAll_hyb09c_set_stDv(as.data.frame(vspr))  # KaiAll model has been removed.

Vs30_AhdiAK_noQ3_hyb09c <- AhdiAK_noQ3_hyb09c_set_Vs30(as.data.frame(vspr))     # 20171205
stDv_AhdiAK_noQ3_hyb09c <- AhdiAK_noQ3_hyb09c_set_stDv(as.data.frame(vspr))     # 20171205

Vs30_YongCA             <- YongCA_set_Vs30(as.data.frame(vspr))                 # 20171218
stDv_YongCA             <- YongCA_set_stDv(as.data.frame(vspr))                 # 20171218

Vs30_YongCA_noQ3        <- YongCA_noQ3_set_Vs30(as.data.frame(vspr))            # 20171218
stDv_YongCA_noQ3        <- YongCA_noQ3_set_stDv(as.data.frame(vspr))            # 20171218

Vs30_AhdiYongWeighted1  <- exp(0.5 * (log(Vs30_AhdiAK_noQ3_hyb09c)   + log(Vs30_YongCA_noQ3)))  # 20180110
mu1 <- log(Vs30_AhdiAK_noQ3_hyb09c)
mu2 <- log(Vs30_YongCA_noQ3)
mu  <- log(Vs30_AhdiYongWeighted1)
sig1sq <- stDv_AhdiAK_noQ3_hyb09c^2
sig2sq <- stDv_YongCA_noQ3^2
sigsq  <- 0.5 * ( (( mu1 - mu)^2) + sig1sq +
                    (( mu2 - mu)^2) + sig2sq)
stDv_AhdiYongWeighted1  <- sqrt(sigsq)              # FIXED on 20180405


vspr <- spCbind(vspr, data.frame(
                                 # Vs30_AhdiAK_KaiAll_hyb09c,   # KaiAll model has been removed.
                                 # stDv_AhdiAK_KaiAll_hyb09c,   # KaiAll model has been removed.
                                 Vs30_AhdiAK_noQ3_hyb09c,                       # 20171205
                                 stDv_AhdiAK_noQ3_hyb09c,                       # 20171205
                                 Vs30_YongCA,                                   # 20171218
                                 stDv_YongCA,                                   # 20171218
                                 Vs30_YongCA_noQ3,                              # 20171218
                                 stDv_YongCA_noQ3,                              # 20171218
                                 Vs30_AhdiYongWeighted1,                        # 20180110
                                 stDv_AhdiYongWeighted1                         # 20180110
                                 ))


# Add Vs metadata
vspr <- spCbind(vspr,VsPts_NZGD49@data)
rm(VsPts_NZGD49)



# Convert to NZGD2000
source("~/VsMap/R/geodetic.R")
vspr <- convert2NZGD00(vspr)


# =============================================================================
# Cull vspr points based on removal criteria -----------------------------------------

# WATER points.....
# at some point may fix this by finding nearest value.
# possible ways to do this discussed here:
#  http://stackoverflow.com/questions/26308426/how-do-i-find-the-polygon-nearest-to-a-point-in-r
vspr$cull_water   <- (vspr$UNIT_CODE == "water")
vspr$cull_na      <- (is.na(vspr$UNIT_CODE))


# SLOPELESS points -------------
vspr$cull_noSlope09 <- is.na(vspr$slp09c)
vspr$cull_noSlope30 <- is.na(vspr$slp30c)
vspr$cull_noSlope   <- vspr$cull_noSlope09 | vspr$cull_noSlope30


# DUPLICATE points ------------
vspr$cull_duplicate     <- logical(dim(vspr)[1])


duplicatePoints <- zerodist(vspr)
dupeVs30        <- numeric(dim(duplicatePoints)[1]) # initialize vector
for (i in seq_along(dupeVs30)) {
  if(vspr[duplicatePoints[i,1],]$Vs30 ==  # if the two points have same Vs30 value... 
     vspr[duplicatePoints[i,2],]$Vs30) {
    dupeVs30[i] <- vspr[duplicatePoints[i,1],]$Vs30
    vspr$cull_duplicate[i] <- T  # mark datapoint for removal
  } else { # if the Vs30 values differ, retain both points.
    dupeVs30[i] <- 0 # mark this by 0
  }
}
removeTheseDupes <- dupeVs30>0
df <- data.frame(point1_idx=duplicatePoints[,1],
                 point2_idx=duplicatePoints[,2],
                 point1_Vs30=vspr[duplicatePoints[,1],]$Vs30,
                 point2_Vs30=vspr[duplicatePoints[,2],]$Vs30,
                 dupeVs30=dupeVs30,
                 remove=removeTheseDupes
                 ) # making this dataframe for reporting cull as txtfile

# write duplicate culling removal info to a text file
# (first, CSV in ./tmp/ , then call util-linux "column" command via system2)
write.csv(df,file = "tmp/duplicatePointsRemoval.csv",row.names = F)
system2("column",args=c("-t",
                        "-s",
                        ","),
                 stdin = "tmp/duplicatePointsRemoval.csv",
                 stdout= "out/duplicatePointsRemoval.csv")


# all rows marked as "cull"
vspr$cull         <- vspr$cull_noSlope | 
                     vspr$cull_water | 
                     vspr$cull_na |
                     vspr$cull_duplicate


vspr_pre_cull <- vspr # Save old version first - with info about which rows are marked to be culled.
vspr <- vspr[vspr$cull==FALSE,]







# =============================================================================
# Extract geology statistics and compute slope residuals -----------------
avgs <- aggregate.data.frame(x = vspr$Vs30, by = list(vspr$dep), FUN = mean)
stdv <- aggregate.data.frame(x = vspr$Vs30, by = list(vspr$dep), FUN = sd)
colnames(avgs) <- c("dep", "meanVsByGroup")
colnames(stdv) <- c("dep", "stDevVsByGroup")

vspr2 <- merge(vspr, avgs, by = "dep")
vspr3 <- merge(vspr2, stdv, by = "dep")
vspr <- vspr3
rm(vspr2, vspr3)


# rGeo is the geology residual - equivalent to that in Thompson et al 2014
vspr$rGeo_byMeans  <- vspr$Vs30 / vspr$meanVsByGroup



save(vspr_pre_cull,vspr,file = vsprFileName) # Save "pre-cull vspr"














#==============================================================================
# Compute residuals
setwd("~/VsMap/")
load(vsprFileName) 
library(sp)
source("R/functions.R")
source("R/models.R")



############
####################################
#################################################################
# The following needs to be updated MANUALLY when new models are produced.
#################################################################
####################################
############


res_testing               <- log(vspr$Vs30) - log(vspr$Vs30_testing)
res_AhdiAK                <- log(vspr$Vs30) - log(vspr$Vs30_AhdiAK)
res_AhdiAK_KaiAll         <- log(vspr$Vs30) - log(vspr$Vs30_AhdiAK_KaiAll)
res_AhdiAK_KaiNoQ3        <- log(vspr$Vs30) - log(vspr$Vs30_AhdiAK_KaiNoQ3)
# res_AhdiAK_KaiAll_hyb09c  <- log(vspr$Vs30) - log(vspr$Vs30_AhdiAK_KaiAll_hyb09c)    # KaiAll model has been removed.
res_AhdiAK_noQ3           <- log(vspr$Vs30) - log(vspr$Vs30_AhdiAK_noQ3)          # added 20171129
res_AhdiAK_noQ3noMcGann   <- log(vspr$Vs30) - log(vspr$Vs30_AhdiAK_noQ3noMcGann)  # added 20171129
res_AhdiAK_noQ3_hyb09c    <- log(vspr$Vs30) - log(vspr$Vs30_AhdiAK_noQ3_hyb09c)   # added 20171205
res_YongCA                <- log(vspr$Vs30) - log(vspr$Vs30_YongCA)               # added 20171218
res_YongCA_noQ3           <- log(vspr$Vs30) - log(vspr$Vs30_YongCA_noQ3)          # added 20171218
res_AhdiYongWeighted1     <- log(vspr$Vs30) - log(vspr$Vs30_AhdiYongWeighted1)    # added 20180110
res_MVN_AhdiAK_noQ3_hyb09c <- log(vspr$Vs30) - log(vspr$Vs30_MVN_AhdiAK_noQ3_hyb09c) # added 20181107
res_MVN_YongCA_noQ3        <- log(vspr$Vs30) - log(vspr$Vs30_MVN_YongCA_noQ3)        # added 20181107


vspr <- spCbind(vspr, data.frame(res_testing,
                                 res_AhdiAK,
                                 res_AhdiAK_KaiAll,
                                 res_AhdiAK_KaiNoQ3,
                                 # res_AhdiAK_KaiAll_hyb09c,    # KaiAll model has been removed.
                                 res_AhdiAK_noQ3,                                 # added 20171129
                                 res_AhdiAK_noQ3noMcGann,                         # added 20171129
                                 res_AhdiAK_noQ3_hyb09c,                          # added 20171205
                                 res_YongCA,                                      # added 20171218
                                 res_YongCA_noQ3,                                 # added 20171218
                                 res_AhdiYongWeighted1,                           # added 20180110
                                 res_MVN_AhdiAK_noQ3_hyb09c,                      # added 20181107
                                 res_MVN_YongCA_noQ3                              # added 20181107
                                 ))

save(vspr_pre_cull, vspr, file = vsprFileName)




#################################################################################################
# output tables for GMT to use.
#
source("R/vspr_write.R")
vspr_write()

