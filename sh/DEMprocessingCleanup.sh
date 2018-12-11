#!/bin/bash
#
# These files are used only temporarily during topography wrangling and then are not needed.
#
# this script removes them. It should be run after DEMprocessing.sh has succeeded.

rm ~/big_noDB/topo/nzni_30c_slp.tif
rm ~/big_noDB/topo/nzsi_30c_slp.tif
rm ~/big_noDB/topo/nzni_9c_slp.tif
rm ~/big_noDB/topo/nzsi_9c_slp.tif

rm ~/big_noDB/topo/nzni_30c_slp_WGS84.xyz
rm ~/big_noDB/topo/nzsi_30c_slp_WGS84.xyz
rm ~/big_noDB/topo/nzni_9c_slp_WGS84.xyz
rm ~/big_noDB/topo/nzsi_9c_slp_WGS84.xyz

rm ~/VsMap/img/nzni_30c_slp_nnb_1s_WGS84.tif
rm ~/VsMap/img/nzni_9c_slp_nnb_1s_WGS84.tif
rm ~/VsMap/img/nzsi_30c_slp_nnb_1s_WGS84.tif
rm ~/VsMap/img/nzsi_9c_slp_nnb_1s_WGS84.tif

