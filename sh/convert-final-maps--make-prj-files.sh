#!/bin/bash


#Reference:
#https://gis.stackexchange.com/questions/291152/georeferencing-metadata-in-geotiff-files
#and emails with Brett Maurer around the beginning of August 2018 regarding trouble with georeferencing.

gdalsrsinfo ~/VsMap/sh/proj.wkt -o wkt_esri > ~/big_noDB/models/VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_Vs30.prj
gdalsrsinfo ~/VsMap/sh/proj.wkt -o wkt_esri > ~/big_noDB/models/VERSION18.12_AhdiYongWeightedMVN_nTcrp1.5_sigma.prj

