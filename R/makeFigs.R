# makeFigs.R
#
# Replaces sh/makeFigs.sh
#
# A bunch of calls to imagemagick to combine 3- and 6-inch sized maps with their respective
# legend colorbars.
#
# Some special figures such as the 6-pane updating figures.

mapPath <- "/home/kmf76/VsMap/out/maps/"

regionVec <- c("CH","NZ")
#regionVec <- c("CH")

mapSizes  <- c("three","six")
# mapSizes  <- c("three")


categories <- c("AhdiGeoCats", "IPcats")
mapTypes <- c("Vs30", "sigma")
models <- c("AhdiAK",
            "AhdiAK_noQ3",
            "AhdiAK_noQ3_hyb09c",
            "AhdiAK_noQ3_hyb09c_KRG_v6",
            "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5",
            "YongCA",
            "YongCA_noQ3",
            "YongCA_noQ3_KRG_v7",
            "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5",
            "AhdiYongWeightedMVN")

resMods <- c(
  "AhdiAK_noQ3_hyb09c_KRG_v6",
  "AhdiAK_noQ3_hyb09c_KRG_v6_normSigma",
  "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5",
  "AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_normSigma",
  "YongCA_noQ3_KRG_v7",
  "YongCA_noQ3_KRG_v7_normSigma",
  "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5",
  "YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_normSigma"
)

ratMods <- c(
  "AhdiBayes",
  "AhdiHyb",
  "AhdiBOTH",
  "AhdiBOTHsigma",
  "YongBayes",
  "YongBayesSigma",
  "YongAhdi",
  "YongAhdiPrior",
  "AhdiMVN_KRGv6",
  "YongMVN_KRGv7"
)






for(mapSize in mapSizes) {
  for(region in regionVec) {
    

    # # Category maps:
    # 
    # for(category in categories) {
    #   inp1 <- paste0(mapPath, category, "_", region, "_", mapSize, ".png")
    #   inp2 <- paste0(mapPath, category, "_", mapSize,  "_legend.png")
    #   outp <- paste0(mapPath, category, "_", region, "_", mapSize, "_legendF.png")
    #   cmd  <- "convert"
    #   args <- c("-gravity", "Center", inp1, inp2, "-trim", "+append", outp)
    #   print(do.call(sprintf,as.list(c("%s %s %s %-120s %-120s %15s %15s %-120s",cmd,args))))
    #   system2(command = cmd, args = args)
    # }

    
    # general maps (Vs30, sigma)

    for(mapType in mapTypes) {
      for(model in models) {
        inp1 <- paste0(mapPath, mapType, "_", region, "_", model, "_", mapSize, ".png")
        inp2 <- paste0(mapPath, mapType, "_", mapSize,  "_legend.png")
        outp <- paste0(mapPath, mapType, "_", region, "_", model, "_", mapSize, "_legendF.png")
        cmd  <- "convert"
        args <- c("-gravity", "Center", inp1, inp2, "-trim", "+append", outp)
        print(do.call(sprintf,as.list(c("%s %s %s %-120s %-120s %15s %15s %-120s",cmd,args))))
        system2(command = cmd, args = args)
      }
    }
    # 
    # 
    # # resid models
    # 
    # for(model in resMods) {
    #   inp1 <- paste0(mapPath, "resid_", region, "_", model, "_", mapSize, ".png")
    #   inp2 <- paste0(mapPath, "resid_", model, "_", mapSize,  "_legend.png")
    #   outp <- paste0(mapPath, "resid_", region, "_", model, "_", mapSize, "_legendF.png")
    #   cmd  <- "convert"
    #   args <- c("-gravity", "Center", inp1, inp2, "-trim", "+append", outp)
    #   print(do.call(sprintf,as.list(c("%s %s %s %-120s %-120s %15s %15s %-120s",cmd,args))))
    #   system2(command = cmd, args = args)
    # }
    
    
    # ratio models

    for(model in ratMods) {
      inp1 <- paste0(mapPath, "ratio_", region, "_", model, "_", mapSize, ".png")
      inp2 <- paste0(mapPath, "ratio_", model, "_", mapSize,  "_legend.png")
      outp <- paste0(mapPath, "ratio_", region, "_", model, "_", mapSize, "_legendF.png")
      cmd  <- "convert"
      args <- c("-gravity", "Center", inp1, inp2, "-trim", "+append", outp)
      print(do.call(sprintf,as.list(c("%s %s %s %-120s %-120s %15s %15s %-120s",cmd,args))))
      system2(command = cmd, args = args)
    }

  }
}
