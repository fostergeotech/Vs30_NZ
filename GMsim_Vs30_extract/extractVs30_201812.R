#!/usr/bin/Rscript
#
# For a list of coordinates provided in stats_Vs30_in.ll, perform overlay operation on
# Vs30 map estimate and produce stats_Vs30_out.ll with pointwise Vs30 estimates.
# 
# The old version performed overlay on polygons and therefore could not 
# obtain "hybrid" and/or kriging estimates. This new version works on rasters.
#
# Update 201801:   modified to reference models saved as TIFFs. (Mostly saved in /home/kmf76/big_noDB/models/)
# Update 201812:   refers to latest model version
#
# Kevin Foster  kevin.foster@pg.canterbury.ac.nz
#
# Usage:
#   R --vanilla < extractVs30_201812.R --args /path/to/in_coords.ll  /path/to/out_coords.ll /path/to/in_Vs30.tif  /path/to/in_sigma.tif
#
#    default:
#      in_coords  = ./stats_Vs30_in.ll
#      out_coords = ./stats_Vs30_out.ll
#      in_Vs30     = /home/kmf76/big_noDB/models/VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_Vs30.tif
#      in_sigma    = /home/kmf76/big_noDB/models/VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_sigma.tif
#
#  File details:
#             * /home/kmf76/big_noDB/models/AhdiYongWeighted1.tif
#                  - GeoTiff format NZ map file containing median Vs30 estimates  (m/s)
#                  - Latest version saved permanently as /home/kmf76/big_noDB/models/201804_AhdiYongWeighted1.tif
#             * /home/kmf76/big_noDB/models/AhdiYongWeighted1_sigma.tif
#                  - GeoTiff format NZ map file containing sigma (natural log standard deviation of Vs30)
#                  - Latest version saved permanently as /home/kmf76/big_noDB/models/201804_AhdiYongWeighted1_sigma.tif
#             * /home/kmf76/big_noDB/models/VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_Vs30.tif
#               and
#               /home/kmf76/big_noDB/models/VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_sigma.tif
#                  - Latest model version With "best" geostatistics applied. (As submitted to Earthquake Spectra, 12/2018)
#             * stats_Vs30_in.ll
#                  - space-delimited text file with station and/or grid coordinates.
#                       columns: Site.Longitude  Site.Latitude  Site.Code
#             * stats_Vs30_out.ll
#                  - space-delimited text file with station and/or grid coordinates.
#                      columns: same as input, plus Vs30_median_estimate and sigma_median_estimate
#
#
#
#


library(raster)
library(sp)
library(tools)    # for tools::file_path_sans_ext()
library(uuid)
source("geodetic.R")


inpArgs <- commandArgs(trailingOnly = TRUE) # grab command line args
defInpVs30  <- "/home/kmf76/big_noDB/models/VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_Vs30.tif"
defInpSigma <- "/home/kmf76/big_noDB/models/VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_sigma.tif"

if(length(inpArgs) == 0) {
    inputVs30map     <-    defInpVs30
    inputSigmaMap    <-    defInpSigma
    inputFile        <-    "stats_Vs30_in.ll"
    outputFile       <-    "stats_Vs30_out.ll"
  } else {
  if(length(inpArgs) == 1 ) {
    inputVs30map     <-    defInpVs30
    inputSigmaMap    <-    defInpSigma
    inputFile        <-    inpArgs[1]
    outputFile       <-    "stats_Vs30_out.ll"
  } else {
  if(length(inpArgs) == 2 ) {
    inputVs30map     <-    defInpVs30
    inputSigmaMap    <-    defInpSigma
    inputFile        <-    inpArgs[1]
    outputFile       <-    inpArgs[2]
  } else {
  if(length(inpArgs) == 4 ) {
    inputFile    <-    inpArgs[1]
    outputFile   <-    inpArgs[2]
    inputVs30map     <-    inpArgs[3]
    inputSigmaMap    <-    inpArgs[4]
} else {
  error("ERROR")
}}}}

Vs30map_raster <- raster(inputVs30map)
Vs30Map <- as(Vs30map_raster, "SpatialGridDataFrame")

sigmaMap_raster <- raster(inputSigmaMap)
sigmaMap<- as(sigmaMap_raster, "SpatialGridDataFrame")



# Loading locations where Vs30 value estimates are needed for simulation -------------
points_WGS84 <- read.delim(inputFile, sep="")


coordsWGS84 <- SpatialPoints(coords = points_WGS84[c("Site.Longitude", "Site.Latitude")],
                             proj4string = crsWGS84(), bbox = NULL)
points_WGS84 <- SpatialPointsDataFrame(coords = coordsWGS84,
                                       data   = data.frame(points_WGS84$Site.Code))
points_NZGD00 <- convert2NZGD00(points_WGS84)  # Convert input pts to NZ Grid Datum 2000.

# Find out which points are in which polygon using over() (point-in-polygon testing)
Vs30vec <- points_NZGD00 %over% Vs30Map
sigVec  <- points_NZGD00 %over% sigmaMap

colnames(Vs30vec) <- c("Vs30_median_estimate")
colnames(sigVec)  <- c("sigma_median_estimate")
Vs30vec2 <- cbind(Vs30vec, sigVec, as.data.frame(points_NZGD00))
Vs30vec3  <- SpatialPointsDataFrame(points_NZGD00, data=Vs30vec2)
outPoints <- convert2WGS84(Vs30vec3)
outP.df <- as.data.frame(outPoints)
outP.df2 <- subset(outP.df, select = c("points_WGS84.Site.Code","Site.Longitude.1","Site.Latitude.1","Vs30_median_estimate","sigma_median_estimate"))


# Rename columns
names(outP.df)[names(outP.df) == "points_WGS84.Site.Code"] <- "Site.Code"
names(outP.df)[names(outP.df) == "Site.Longitude.1"]       <- "Site.Longitude"
names(outP.df)[names(outP.df) == "Site.Latitude.1"]        <- "Site.Latitude"


ffff <- paste0("/tmp/Vs30_",UUIDgenerate())
write.table(outP.df2, file = ffff, row.names = FALSE, sep=" ", quote=F)
system2(command = "column",stdin = ffff, stdout = outputFile,args = "-t")



