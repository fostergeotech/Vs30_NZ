#!/bin/bash

# This "last step" converts the final model TIF files from NZ coordinates to UTM South Zone 59 (EPSG:32759) coordinates.
# First model user Brett Maurer noted that ArcMap was unable to correctly georeference the files, even though
# the results of gdalinfo indicated that the georeferencing did indeed exist.

# Confirmation from Brett still pending whether this conversion fixes the problem. (31 July).
# If so, obviously the cubic interpolated versions should be superior to nearest-neighbor, and they're still relatively
# quick to generate.

# See also my GIS StackExchange post here https://gis.stackexchange.com/questions/291152/georeferencing-metadata-in-geotif-files

cd /home/kmf76/big_noDB/models

#gdalwarp -t_srs EPSG:32759 -r near VERSION18.5_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif VERSION18.5_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5_EPSG32759nearestNeighbor.tif
#gdalwarp -t_srs EPSG:32759 -r cubic VERSION18.5_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif VERSION18.5_MVN_Vs30_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5_EPSG32759cubic.tif
#
#gdalwarp -t_srs EPSG:32759 -r near VERSION18.5_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif VERSION18.5_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5_EPSG32759nearestNeighbor.tif
#gdalwarp -t_srs EPSG:32759 -r cubic VERSION18.5_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5.tif VERSION18.5_MVN_stDv_NZGD00_allNZ_AhdiYongWeighted1_noisyT_minDist0.0km_v3_crp1.5_EPSG32759cubic.tif

cp AhdiYongWeightedMVN_nTcrp1.5_Vs30.tif VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_Vs30.tif
cp AhdiYongWeightedMVN_nTcrp1.5_sigma.tif VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_sigma.tif

gdalwarp -t_srs EPSG:32759 -r near VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_Vs30.tif VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_Vs30_EPSG32759nearestNeighbor.tif
gdalwarp -t_srs EPSG:32759 -r near VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_sigma.tif VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_sigma_EPSG32759nearestNeighbor.tif
