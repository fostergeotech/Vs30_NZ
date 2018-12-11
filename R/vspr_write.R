#vspr_write.R
#
# called at end of vspr.R to write fixed-width format text output file

vspr_write <- function() {
  
  rm(list=ls())
  
  vsprFileName <- "~/VsMap/Rdata/vspr.Rdata"
  source("R/geodetic.R")
  source("R/functions.R")
  source("R/classifyThings.R")
  source("R/models.R")
  
  # Output to a format suitable for GMT plotting
  setwd("~/VsMap/")
  load(file = vsprFileName)
  
  
  # automatically loop over many vs subsets.
  vspr_subsets <- subsetVsPoints(vspr)
  
  # Added 2018-11
  # I needed a lazy way to plot all McGann points (i.e. before the resampling),
  # but these points don't contain model predictions, etc. and are not of the same
  # format as the rest of the model subsets. Consequently I need to remove "McGannALL"
  # from vspr_subsets before looping:
  vspr_subsets <- vspr_subsets[names(vspr_subsets) != "McGannALL"]
  
  # i <- 1
  for(i in 1:length(vspr_subsets)) {
   
    nameStr <- names(vspr_subsets)[[i]] 
    vspr <- vspr_subsets[[i]]
    
    vsprWGS84   <- convert2WGS84(vspr)
    
    modelColList <- c()
    for (MODEL in allMODELs) {
      groupName         <- paste0("groupID_",       MODEL)
      Vs30estimateName  <- paste0("Vs30_",      MODEL)
      sigmaEstimateName <- paste0("stDv_",  MODEL)
      residualsName     <- paste0("res_",      MODEL)
      modelColList <- c(modelColList      ,
                        groupName         ,
                        Vs30estimateName  ,
                        sigmaEstimateName ,
                        residualsName)
    }
    
    
    vsprWGS84df <- as.data.frame(vsprWGS84)
    GMTout <- vsprWGS84df[,c(
      "StationID","DataSource",
      "Easting","Northing",
      "Vs30",           # measured
      "dep","gs",       # depositional group & grain size 
      "slp09c","slp30c",# slopes
      "QualityFlag",
      "QMAP_NAME",
      "NorthSouth",
      "SIMPLE_NAME",
      "MAIN_ROCK",
      modelColList
    )]
    
    
    
    # Because write.csv can't do a nicely formatted fixed-width table,
    # use Linux "column" command.
    write.csv(GMTout,file="/tmp/vspr.ll",na="NA",quote=TRUE,row.names=FALSE)
    system2("column",args=c("-t",
                            "-s",
                            ","),
            stdin = "/tmp/vspr.ll",
            stdout = paste0("Vs30_tables/vspr_data_",nameStr,".ll"))
    
    
    # Need to also write a "labels-only" file if station labels are desired.
    GMTout <- GMTout[c("Easting","Northing","StationID")]
    write.csv(GMTout,file="/tmp/vspr.ll",na="NA",quote=TRUE,row.names=FALSE)
    system2("column",args=c("-t",
                            "-s",
                            ","),
            stdin = "/tmp/vspr.ll",
            stdout = paste0("Vs30_tables/vspr_text_",nameStr,".ll"))
  }
}
