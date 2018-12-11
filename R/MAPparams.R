# This is so I can call all MAP scripts at once e.g. through sh/runAllMaps.sh


source("R/functions.R")
source("R/MAP_NZGD00_regions.R") # contains regions

# regions to loop:
# regionVec <- names(extList)
# regionVec <- c("NEL", "NELURB", "NZ")
regionVec <- c("CH","NZ")
# regionVec <- c("CH")





mapSizeVec <- c("three","poster")
mapSizeVec <- c("three")
mapSizeVec <- c("three","six")

grayScaleWhiteWater <- T # in response to BB figure comments


if(grayScaleWhiteWater) {
  waterCol <- rgb(1,1,1)
} else {
  waterCol <- rgb(0.6, 0.9, 1)
}











mapIDvec_Vs30 <- c(  
  ## old ## "AhdiAK_noQ3_hyb09c_KRG",
  ## old ## "AhdiAK_noQ3_hyb09c_MVN",
  ## old ## "AhdiYongWeighted1_MVN",
  ## old ## "AhdiYongWeighted1_KRG",
  ## old ## "AhdiYongWeighted1_KRG_slopeWeighting1",
  ## old ## "AhdiYongWeighted1_MVN_noisyT_minDist0_v2",
  ## old ## "AhdiYongWeighted1_KRG_v4",
  "AhdiAK",
  "AhdiAK_noQ3",
  "AhdiAK_noQ3_hyb09c",
  "YongCA",
  "YongCA_noQ3",
  # "AhdiYongWeighted1",
  # "AhdiYongWeighted1_MVN_slopeWeighting1",
  # "AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5",
  # "AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0",
  # "AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp1.5",
  # "AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0",
  # "AhdiYongWeighted1_KRG_v5",
  # "AhdiYongWeighted1_KRG_slopeWeighting1_v5",
  "AhdiAK_noQ3_hyb09c_KRG_v6" ,
  "YongCA_noQ3_KRG_v7",
  "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5",
  "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5",
  # "AhdiAK_noQ3_hyb09c_KRG_v8" ,
  # "YongCA_noQ3_KRG_v9",
  "AhdiYongWeightedMVN"
)


mapIDvec_sigma <- c(  
  ## old ## "AhdiYongWeighted1_MVN",
  ## old ## "AhdiYongWeighted1_KRG"
  ## old ## "AhdiYongWeighted1_KRG_slopeWeighting1",
  ## old ## "AhdiYongWeighted1_MVN_noisyT_minDist0_v2",
  ## old ## "AhdiYongWeighted1_KRG_v4",
  "AhdiAK"#,
  # "AhdiAK_noQ3",
  # "AhdiAK_noQ3_hyb09c",
  # "YongCA",
  # "YongCA_noQ3"#,
  # "AhdiYongWeighted1",
  # "AhdiYongWeighted1_MVN_slopeWeighting1",
  # "AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5",
  # "AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0",
  # "AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp1.5",
  # "AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0",
  # "AhdiYongWeighted1_KRG_v5",
  # "AhdiAK_noQ3_hyb09c_KRG_v6_normSigma",
  # "YongCA_noQ3_KRG_v7_normSigma",
  # "AhdiAK_noQ3_hyb09c_KRG_v6",
  # "YongCA_noQ3_KRG_v7",
  # "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5"#,
  # "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5",
  # "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma",
  # "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma"
  # "AhdiAK_noQ3_hyb09c_KRG_v8",
  # "YongCA_noQ3_KRG_v9"
  # "AhdiYongWeightedMVN"
)


mapIDvec_rats <- c(
  #"KRGweighted_fullKRG", # now updated for variogram v5 versions
  #"AhdiBayes",
  "AhdiHyb",    # now does ratio rather than log of ratio.
  "AhdiBOTH",   # now does ratio rather than log of ratio.
  "AhdiBOTHsigma",
  "YongBayes",  # now does ratio rather than log of ratio.
  "YongBayesSigma",
  "YongAhdi",   # now does ratio rather than log of ratio.
  #"YongAhdiPrior",
  #"MVNweighted_fullMVN",
  #"fullMVN_MVNcrp1.5",
  #"MVNslpWtd_MVNcrp1.5",
  #"MVN_noiseT_crp0",
  #"MVN_noiseT_crp1.5"
  "AhdiMVN_KRGv6",
  "YongMVN_KRGv7"
  #"AhdiMVN_KRGv8",
  #"YongMVN_KRGv9"
)
ratioNotLog <- c(  # in response to BB comments - these are produced same as other plots, but LABELS are changed to show ratio, rather than log.
  "AhdiBOTH",
  "YongBayes",
  "AhdiHyb",
  "YongAhdi",
  "AhdiMVN_KRGv6",
  "YongMVN_KRGv7")



mapIDvec_elev <- c("elevation")  

mapIDvec_cats <- c("Terrain", "Geology")




publication <- T # right now, just a flag to prevent 6x redundant north arrows and graphic scales on the 6-pane figures.










# output
maxpix          = 1e7  # plot() for rasters is complicated. Using maxpix with massive
# rasters doesn't NECESSARILY result in quicker plotting (1e7
# was not working in my experience) but if raster is resampled
# beforehand, maxpixels works as expected and doesn't cause
# much/any delay.
makeMask        = F

