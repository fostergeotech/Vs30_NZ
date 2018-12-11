#!/bin/bash -vx
# -v echoes commands as they are executed, without expanding variables. http://www.faqs.org/docs/abs/HTML/options.html
# -x echoes commands as they are executed, and expands variables. http://www.faqs.org/docs/abs/HTML/options.html


############################################################################################################################################
### INPUT AND OUTPUT TABLE #############################################################################################################
##################################################################################################################################
####    # 
####    #     INPUT                                   OUTPUT                                       NOTES
####    # 
############################################################################################################################################
####    # 
####    #     ~/big_noDB/topo/nzsi_30c_slp.tif        ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz       WGS84 XYZ SLOPE FILES (30c & 9c)
####    #     ~/big_noDB/topo/nzsi_9c_slp.tif         ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz
####    #     ~/big_noDB/topo/nzni_30c_slp.tif        ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz
####    #     ~/big_noDB/topo/nzni_9c_slp.tif         ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz
####    #                                             VsMap/img/nzsi_30c_slp_nnb.nc                  1 ARC-SECOND RESOLUTION
####    #                                             VsMap/img/nzsi_9c_slp_nnb.nc                   1 ARC-SECOND RESOLUTION
####    #                                             VsMap/img/nzni_30c_slp_nnb.nc                  1 ARC-SECOND RESOLUTION
####    #                                             VsMap/img/nzni_9c_slp_nnb.nc                   1 ARC-SECOND RESOLUTION
####    # 
####    #                                             VsMap/img/nzsi_30c_slp_nnb_1s_WGS84.tif
####    #                                             VsMap/img/nzni_30c_slp_nnb_1s_WGS84.tif
####    #                                             VsMap/img/nzsi_9c_slp_nnb_1s_WGS84.tif
####    #                                             VsMap/img/nzni_9c_slp_nnb_1s_WGS84.tif
####    # 
####    #
####    #                                             VsMap/img/nzni_30c_slp_nnb_1s_NZGD00.tif
####    #                                             VsMap/img/nzsi_30c_slp_nnb_1s_NZGD00.tif
####    #                                             VsMap/img/nzni_9c_slp_nnb_1s_NZGD00.tif
####    #                                             VsMap/img/nzsi_9c_slp_nnb_1s_NZGD00.tif

{
echo START AT $(date)

echo CONVERTING TIF FILE TO XYZ....
time gdal_translate -of XYZ -a_nodata "-1.69999999999999994e+308" ~/big_noDB/topo/nzsi_30c_slp.tif /tmp/si30nzmg.xyz  & 
time gdal_translate -of XYZ -a_nodata "-1.69999999999999994e+308" ~/big_noDB/topo/nzsi_9c_slp.tif  /tmp/si9nzmg.xyz   & 
time gdal_translate -of XYZ -a_nodata "-1.69999999999999994e+308" ~/big_noDB/topo/nzni_30c_slp.tif /tmp/ni30nzmg.xyz  & 
time gdal_translate -of XYZ -a_nodata "-1.69999999999999994e+308" ~/big_noDB/topo/nzni_9c_slp.tif  /tmp/ni9nzmg.xyz   & 
wait

echo PROJECTING XYZ NZMG COORDINATES TO WGS84....
time proj -I -f "%.8f" +proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +datum=nzgd49 +units=m +no_defs +ellps=intl /tmp/si30nzmg.xyz > /tmp/si30wgs84inf.xyz  &
time proj -I -f "%.8f" +proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +datum=nzgd49 +units=m +no_defs +ellps=intl /tmp/si9nzmg.xyz  > /tmp/si9wgs84inf.xyz  &
time proj -I -f "%.8f" +proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +datum=nzgd49 +units=m +no_defs +ellps=intl /tmp/ni30nzmg.xyz > /tmp/ni30wgs84inf.xyz  &
time proj -I -f "%.8f" +proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +datum=nzgd49 +units=m +no_defs +ellps=intl /tmp/ni9nzmg.xyz  > /tmp/ni9wgs84inf.xyz  &
wait

echo REMOVING NaNs FROM XYZ FILE....
grep -v inf /tmp/si30wgs84inf.xyz  > ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz  &
grep -v inf /tmp/si9wgs84inf.xyz   > ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz  &
grep -v inf /tmp/ni30wgs84inf.xyz  > ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz  &
grep -v inf /tmp/ni9wgs84inf.xyz   > ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz  &
echo REMOVING OLD NZ FILES...
rm img/nz?i_*c_slp_*.nc &
wait

echo GENERATING NC FILE USING NEARNEIGHBOR... 1 ARC-SECOND RESOLUTION....
# search radius = 1/2 * 30 sec * sqrt 2. This is 21.3 for 30c XYZ and 6.4 for 9c XYZ.
# 14 degrees * 60 min/deg * 60 sec/min * (1/1)  dots/sec * (1/300) inch/dot = 33.58 inches width @ 300 dpi or 16.79 inches at 600dpi
# 14 degrees * 60 min/deg * 60 sec/min * (1/5)  dots/sec * (1/300) inch/dot = 33.58 inches width @ 300 dpi or 16.79 inches at 600dpi
# 14 degrees * 60 min/deg * 60 sec/min * (1/15) dots/sec * (1/300) inch/dot = 11.6 inches width @ 300 dpi or 5.8 inches at 600dpi
time gmt nearneighbor ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz  -Gimg/nzsi_30c_slp_nnb.nc          -R166/180/-48/-34 -I1s  -S21.3s -N1 &
time gmt nearneighbor ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz   -Gimg/nzsi_9c_slp_nnb.nc           -R166/180/-48/-34 -I1s  -S6.4s  -N1 &
wait
time gmt nearneighbor ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz  -Gimg/nzni_30c_slp_nnb.nc          -R166/180/-48/-34 -I1s  -S21.3s -N1 &
time gmt nearneighbor ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz   -Gimg/nzni_9c_slp_nnb.nc           -R166/180/-48/-34 -I1s  -S6.4s  -N1 &
wait







####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz  -Gimg/nzsi_30c_slp_nnb_coarse5.nc  -R166/180/-48/-34 -I5s  -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz   -Gimg/nzsi_9c_slp_nnb_coarse5.nc   -R166/180/-48/-34 -I5s  -S6.4s  -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz  -Gimg/nzni_30c_slp_nnb_coarse5.nc  -R166/180/-48/-34 -I5s  -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz   -Gimg/nzni_9c_slp_nnb_coarse5.nc   -R166/180/-48/-34 -I5s  -S6.4s  -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz  -Gimg/nzsi_30c_slp_nnb_coarse15.nc -R166/180/-48/-34 -I15s -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz   -Gimg/nzsi_9c_slp_nnb_coarse15.nc  -R166/180/-48/-34 -I15s -S6.4s  -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz  -Gimg/nzni_30c_slp_nnb_coarse15.nc -R166/180/-48/-34 -I15s -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz   -Gimg/nzni_9c_slp_nnb_coarse15.nc  -R166/180/-48/-34 -I15s -S6.4s  -N1 &

####    no longer used        ########        # 20170809  adding 30s, 1m, 2m, 3m coarser grids for testing
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz  -Gimg/nzsi_30c_slp_nnb_coarse30.nc -R166/180/-48/-34 -I30s -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz   -Gimg/nzsi_9c_slp_nnb_coarse30.nc  -R166/180/-48/-34 -I30s -S6.4s  -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz  -Gimg/nzni_30c_slp_nnb_coarse30.nc -R166/180/-48/-34 -I30s -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz   -Gimg/nzni_9c_slp_nnb_coarse30.nc  -R166/180/-48/-34 -I30s -S6.4s  -N1 &
####    no longer used        ########        
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz  -Gimg/nzsi_30c_slp_nnb_coarse1m.nc -R166/180/-48/-34 -I1m -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz   -Gimg/nzsi_9c_slp_nnb_coarse1m.nc  -R166/180/-48/-34 -I1m -S6.4s  -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz  -Gimg/nzni_30c_slp_nnb_coarse1m.nc -R166/180/-48/-34 -I1m -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz   -Gimg/nzni_9c_slp_nnb_coarse1m.nc  -R166/180/-48/-34 -I1m -S6.4s  -N1 &
####    no longer used        ########        
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz  -Gimg/nzsi_30c_slp_nnb_coarse2m.nc -R166/180/-48/-34 -I2m -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz   -Gimg/nzsi_9c_slp_nnb_coarse2m.nc  -R166/180/-48/-34 -I2m -S6.4s  -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz  -Gimg/nzni_30c_slp_nnb_coarse2m.nc -R166/180/-48/-34 -I2m -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz   -Gimg/nzni_9c_slp_nnb_coarse2m.nc  -R166/180/-48/-34 -I2m -S6.4s  -N1 &
####    no longer used        ########        
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz  -Gimg/nzsi_30c_slp_nnb_coarse3m.nc -R166/180/-48/-34 -I3m -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz   -Gimg/nzsi_9c_slp_nnb_coarse3m.nc  -R166/180/-48/-34 -I3m -S6.4s  -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz  -Gimg/nzni_30c_slp_nnb_coarse3m.nc -R166/180/-48/-34 -I3m -S21.3s -N1 &
####    no longer used        ########        time gmt nearneighbor ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz   -Gimg/nzni_9c_slp_nnb_coarse3m.nc  -R166/180/-48/-34 -I3m -S6.4s  -N1 &





echo CONVERT WGS84 NC FILES TO TIF
gdal_translate img/nzsi_30c_slp_nnb.nc img/nzsi_30c_slp_nnb_1s_WGS84.tif   &
gdal_translate img/nzni_30c_slp_nnb.nc img/nzni_30c_slp_nnb_1s_WGS84.tif   &
gdal_translate img/nzsi_9c_slp_nnb.nc  img/nzsi_9c_slp_nnb_1s_WGS84.tif   &
gdal_translate img/nzni_9c_slp_nnb.nc  img/nzni_9c_slp_nnb_1s_WGS84.tif   &
wait

echo WARP 1 ARC-SECOND TIF SLOPE FILES TO NZGD00
gdalwarp -s_srs WGS84  -t_srs "EPSG:2193"  -r near img/nzni_30c_slp_nnb_1s_WGS84.tif img/nzni_30c_slp_nnb_1s_NZGD00.tif &
gdalwarp -s_srs WGS84  -t_srs "EPSG:2193"  -r near img/nzsi_30c_slp_nnb_1s_WGS84.tif img/nzsi_30c_slp_nnb_1s_NZGD00.tif &
gdalwarp -s_srs WGS84  -t_srs "EPSG:2193"  -r near img/nzni_9c_slp_nnb_1s_WGS84.tif img/nzni_9c_slp_nnb_1s_NZGD00.tif   &
gdalwarp -s_srs WGS84  -t_srs "EPSG:2193"  -r near img/nzsi_9c_slp_nnb_1s_WGS84.tif img/nzsi_9c_slp_nnb_1s_NZGD00.tif   &
wait

echo $(date) DONE
} 2>&1 | tee ~/VsMap/sh/DEMprocessing.out




