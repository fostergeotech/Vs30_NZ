#!/bin/bash -vx
# -v echoes commands as they are executed, without expanding variables. http://www.faqs.org/docs/abs/HTML/options.html
# -x echoes commands as they are executed, and expands variables. http://www.faqs.org/docs/abs/HTML/options.html

#################################################################################################################################################
######## INPUT/OUTPUT FILES ######################################################################################################
#################################################################################################################################################
#  
#  INPUT                                                        OUTPUT
# -------------------------------------------------------------------------------------------------------
#  ~/big_noDB/topo/topo_orig_NZMG/ni/*.tif                      ~/big_noDB/topo/DEM_all_ni.tif 
#  ~/big_noDB/topo/topo_orig_NZMG/si/*.tif                      ~/big_noDB/topo/DEM_all_si.tif 
#  
##############################################################  ~/big_noDB/topo/DEM_all_ni_warp100.tif
##############################################################  ~/big_noDB/topo/DEM_all_si_warp100.tif
##############################################################  ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz
####                                                     #####  ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz
####                                                     #####  ~/VsMap/img/nzni_DEM_nnb.nc
####                                                     #####  ~/VsMap/img/nzsi_DEM_nnb.nc
####       All of these outputs are remnants             #####  
####        of GMT plotting workflow                     #####  ~/VsMap/img/nzni_DEM_tri_100m_1s.nc
####          and are no longer needed                   #####  ~/VsMap/img/nzni_DEM_tri_100m_5s.nc
####                                                     #####  ~/VsMap/img/nzni_DEM_tri_100m_15s.nc
####                                                     #####  ~/VsMap/img/nzsi_DEM_tri_100m_1s.nc
####                                                     #####  ~/VsMap/img/nzsi_DEM_tri_100m_5s.nc
####                                                     #####  ~/VsMap/img/nzsi_DEM_tri_100m_15s.nc
####                                                     #####  
####                                                     #####  ~/VsMap/img/nzni_hillshade_100m_1s.nc
####                                                     #####  ~/VsMap/img/nzni_hillshade_100m_5s.nc
####                                                     #####  ~/VsMap/img/nzni_hillshade_100m_15s.nc
####                                                     #####  ~/VsMap/img/nzsi_hillshade_100m_1s.nc
####                                                     #####  ~/VsMap/img/nzsi_hillshade_100m_5s.nc
####                                                     #####  ~/VsMap/img/nzsi_hillshade_100m_15s.nc
####                                                     #####  
##############################################################  ~/VsMap/img/NZ_hillshade_100m_15s.nc
##############################################################  ~/VsMap/img/NZ_hillshade_100m_5s.nc
##############################################################  ~/VsMap/img/NZ_hillshade_100m_1s.nc


REG_NZ=166/180/-48/-34
REG_ni=170/180/-42/-34
REG_si=166/175/-48/-40


time gdal_merge.py -o ~/big_noDB/topo/DEM_all_ni.tif ~/big_noDB/topo/topo_orig_NZMG/ni/*.tif &
time gdal_merge.py -o ~/big_noDB/topo/DEM_all_si.tif ~/big_noDB/topo/topo_orig_NZMG/si/*.tif &
wait
#### GMT remnants ####    
#### GMT remnants ####    gdalwarp  -tr 100 100 -r cubicspline -overwrite ~/big_noDB/topo/DEM_all_ni.tif ~/big_noDB/topo/DEM_all_ni_warp100.tif &
#### GMT remnants ####    gdalwarp  -tr 100 100 -r cubicspline -overwrite ~/big_noDB/topo/DEM_all_si.tif ~/big_noDB/topo/DEM_all_si_warp100.tif &
#### GMT remnants ####    wait
#### GMT remnants ####    
#### GMT remnants ####    time gdal_translate -of XYZ -a_nodata "-3.4028234663852886e+38" ~/big_noDB/topo/DEM_all_ni_warp100.tif /tmp/all_ni_warp100.xyz  &
#### GMT remnants ####    time gdal_translate -of XYZ -a_nodata "-3.4028234663852886e+38" ~/big_noDB/topo/DEM_all_si_warp100.tif /tmp/all_si_warp100.xyz  &
#### GMT remnants ####    wait
#### GMT remnants ####    time proj -I -f "%.8f" +proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +datum=nzgd49 +units=m +no_defs +ellps=intl /tmp/all_ni_warp100.xyz > /tmp/all_ni_warp100_WGS84.xyz  &
#### GMT remnants ####    time proj -I -f "%.8f" +proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +datum=nzgd49 +units=m +no_defs +ellps=intl /tmp/all_si_warp100.xyz > /tmp/all_si_warp100_WGS84.xyz  &
#### GMT remnants ####    wait
#### GMT remnants ####    time grep -v 3.4028234663852886e+38    /tmp/all_ni_warp100_WGS84.xyz > ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz  &
#### GMT remnants ####    time grep -v 3.4028234663852886e+38    /tmp/all_si_warp100_WGS84.xyz > ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz  &
#### GMT remnants ####    wait
#### GMT remnants ####    
#### GMT remnants ####    time gmt nearneighbor ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz  -Gimg/nzni_DEM_nnb.nc -R166/180/-48/-34 -I1s -S1.5s -N1
#### GMT remnants ####    time gmt nearneighbor ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz  -Gimg/nzsi_DEM_nnb.nc -R166/180/-48/-34 -I1s -S1.5s -N1
#### GMT remnants ####    
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz  -Gimg/nzni_DEM_tri_100m_1s.nc   -R${REG_NZ} -I1s  &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz  -Gimg/nzsi_DEM_tri_100m_1s.nc   -R${REG_NZ} -I1s  &
#### GMT remnants ####    wait
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz  -Gimg/nzni_DEM_tri_100m_5s.nc   -R${REG_NZ} -I5s  &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz  -Gimg/nzni_DEM_tri_100m_15s.nc  -R${REG_NZ} -I15s &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz  -Gimg/nzsi_DEM_tri_100m_5s.nc   -R${REG_NZ} -I5s  &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz  -Gimg/nzsi_DEM_tri_100m_15s.nc  -R${REG_NZ} -I15s &
#### GMT remnants ####    wait
#### GMT remnants ####     20170809 adding 30s, 1m, 2m, 3m grids for testing
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz  -Gimg/nzni_DEM_tri_100m_30s.nc   -R${REG_NZ} -I30s  &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz  -Gimg/nzsi_DEM_tri_100m_30s.nc   -R${REG_NZ} -I30s  &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz  -Gimg/nzni_DEM_tri_100m_1m.nc  -R${REG_NZ} -I1m &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz  -Gimg/nzsi_DEM_tri_100m_1m.nc  -R${REG_NZ} -I1m &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz  -Gimg/nzni_DEM_tri_100m_2m.nc  -R${REG_NZ} -I2m &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz  -Gimg/nzsi_DEM_tri_100m_2m.nc  -R${REG_NZ} -I2m &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_ni_warp100_WGS84.xyz  -Gimg/nzni_DEM_tri_100m_3m.nc  -R${REG_NZ} -I3m &
#### GMT remnants ####    time gmt triangulate ~/big_noDB/topo/DEM_all_si_warp100_WGS84.xyz  -Gimg/nzsi_DEM_tri_100m_3m.nc  -R${REG_NZ} -I3m &
#### GMT remnants ####    wait
#### GMT remnants ####    
#### GMT remnants ####    
#### GMT remnants ####    time gmt grdblend  -Cu img/nzni_DEM_tri_100m_15s.nc   img/nzsi_DEM_tri_100m_15s.nc   -Gimg/NZ_DEM_tri_100m_15s.nc -R${REG_NZ} -I15s 
#### GMT remnants ####    time gmt grdblend  -Cu img/nzni_DEM_tri_100m_5s.nc    img/nzsi_DEM_tri_100m_5s.nc    -Gimg/NZ_DEM_tri_100m_5s.nc  -R${REG_NZ} -I5s 
#### GMT remnants ####    time gmt grdblend  -Cu img/nzni_DEM_tri_100m_1s.nc    img/nzsi_DEM_tri_100m_1s.nc    -Gimg/NZ_DEM_tri_100m_1s.nc  -R${REG_NZ} -I1s 
#### GMT remnants ####    
#### GMT remnants ####    # 20170809 new resolutions
#### GMT remnants ####    time gmt grdblend  -Cu img/nzni_DEM_tri_100m_30s.nc   img/nzsi_DEM_tri_100m_30s.nc   -Gimg/NZ_DEM_tri_100m_30s.nc -R${REG_NZ} -I30s 
#### GMT remnants ####    time gmt grdblend  -Cu img/nzni_DEM_tri_100m_1m.nc   img/nzsi_DEM_tri_100m_1m.nc   -Gimg/NZ_DEM_tri_100m_1m.nc -R${REG_NZ} -I1m 
#### GMT remnants ####    time gmt grdblend  -Cu img/nzni_DEM_tri_100m_2m.nc   img/nzsi_DEM_tri_100m_2m.nc   -Gimg/NZ_DEM_tri_100m_2m.nc -R${REG_NZ} -I2m 
#### GMT remnants ####    time gmt grdblend  -Cu img/nzni_DEM_tri_100m_3m.nc   img/nzsi_DEM_tri_100m_3m.nc   -Gimg/NZ_DEM_tri_100m_3m.nc -R${REG_NZ} -I3m 
#### GMT remnants ####    wait
#### GMT remnants ####    
#### GMT remnants ####    
#### GMT remnants ####    
#### GMT remnants ####    time gmt grdgradient img/NZ_DEM_tri_100m_1s.nc   -Gimg/NZ_hillshade_100m_1s.nc   -R${REG_NZ}  -A315/45  -Nt -fg & 
#### GMT remnants ####    time gmt grdgradient img/NZ_DEM_tri_100m_5s.nc   -Gimg/NZ_hillshade_100m_5s.nc   -R${REG_NZ}  -A315/45  -Nt -fg  &
#### GMT remnants ####    time gmt grdgradient img/NZ_DEM_tri_100m_15s.nc  -Gimg/NZ_hillshade_100m_15s.nc  -R${REG_NZ}   -A315/45  -Nt -fg  &
#### GMT remnants ####    
#### GMT remnants ####    
#### GMT remnants ####    # 20170809 new resolutions
#### GMT remnants ####    time gmt grdgradient img/NZ_DEM_tri_100m_30s.nc  -Gimg/NZ_hillshade_100m_30s.nc  -R${REG_NZ}   -A315/45  -Nt -fg  &
#### GMT remnants ####    time gmt grdgradient img/NZ_DEM_tri_100m_1m.nc  -Gimg/NZ_hillshade_100m_1m.nc  -R${REG_NZ}   -A315/45  -Nt -fg  &
#### GMT remnants ####    time gmt grdgradient img/NZ_DEM_tri_100m_2m.nc  -Gimg/NZ_hillshade_100m_2m.nc  -R${REG_NZ}   -A315/45  -Nt -fg  &
#### GMT remnants ####    time gmt grdgradient img/NZ_DEM_tri_100m_3m.nc  -Gimg/NZ_hillshade_100m_3m.nc  -R${REG_NZ}   -A315/45  -Nt -fg  &
#### GMT remnants ####    
#### GMT remnants ####    
#### GMT remnants ####    
#### GMT remnants ####    wait

