#vspr_write_publish.R
#
# new version for pretty printed publication table.


rm(list=ls())
library(spatstat)

vsprFileName <- "~/VsMap/Rdata/vspr.Rdata"
source("R/geodetic.R")
source("R/functions.R")
source("R/classifyThings.R")
source("R/models.R")

setwd("~/VsMap/")
load(file = vsprFileName)


# automatically loop over many vs subsets.
vspr_subsets <- subsetVsPoints(vspr)
names(vspr_subsets)
vspr_subsets <- vspr_subsets[c(
  "McGann",
  "Kaiser_noQ3",
  "Wotherspoon"
)]







# write.csv(GMTout,file="/tmp/vspr.ll",na="NA",quote=TRUE,row.names=FALSE)
titleLinePrintf <- paste(
  "%12s", # lat
  "%12s", # lon
  "%8s",  # Vs30
  "%12s", # sigma_meas
  "%-32s", # ID
  "%-28s", # source
  "%-8s" , # quality (Q1, Q2, NA)
  "%-20s"  # region (QMAP_NAME)
)
dataLinePrintf <- paste(
  "%12.6f", # lat
  "%12.6f", # lon
  "%8.1f", # Vs30
  "%12.1f", # lnMeasUncer
  "%-32s", # ID
  "%-28s", # source
  "%-8s" , # quality (Q1, Q2, NA)
  "%-20s"  # region (QMAP_NAME)
)

TitleLine <- sprintf(titleLinePrintf,"Lat","Lon","Vs30","sigma_meas.","Location ID (if present)","Data source","Qflag","Region (QMAP quadrant)")


lines <- TitleLine



dataSourceNiceStrings <- data.frame(
  oldNames = c("McGannCPT", "KaiserEtAl",
               "Wotherspoon et al. (2013). Geotechnical characterisation of",
               "Cox et al. (2011) University of Arkansas Preliminary Data Report",
               "Wood et al. (2016). Vs-based Evaluation of Select Liquefaction Case Histories from the",
               "van Houtte et al. (2014). Hard-Site",
               "Wotherspoon et al. (2016). Dynamic Site Characterisation of Canterbury Strong Motion Stations using",
               "Cox et al. (2016). Deep Vs Profiling in Christchurch"),
  newNames = c(
    "McGann et al. (2017)",
    "Kaiser et al. (2017)",
    "Wotherspoon et al. (2013)",
    "Cox et al. (2011)",
    "Wood et al. (2017)",
    "van Houtte et al. (2014)",
    "Wotherspoon et al. (2016)",
    "Cox et al. (2016)"
  )
)




for(i in 1:length(vspr_subsets)) {
  # i <- 1 # testing
 
  nameStr <- names(vspr_subsets)[[i]] 
  vspr <- vspr_subsets[[i]]
  
  vsprWGS84   <- convert2WGS84(vspr)
  
  
  vsprWGS84df <- as.data.frame(vsprWGS84)
  GMTout <- vsprWGS84df[,c(
    "Northing",
    "Easting",
    "Vs30",           # measured
    "lnMeasUncer",
    "StationID",
    "DataSource",
    "DataSourceW",
    "QualityFlag",
    "QMAP_NAME"
  )]
  
  
  for(j in 1:nrow(GMTout)) {
    line <- GMTout[j,]
    source1 <- switch(as.character(line$DataSource), # source
                      Wotherspoon201711 = as.character(line$DataSourceW),
                      as.character(line$DataSource))
    source2 <- as.character(dataSourceNiceStrings$newNames[
      begins(as.character(dataSourceNiceStrings$oldNames),
             substr(source1,0,32)
             )
      ])
    if(length(source2)==0) {stop("Error!")}
    
    l <- sprintf(dataLinePrintf,
            line$Northing,      # lat
            line$Easting,       # lon
            line$Vs30,          # Vs30
            line$lnMeasUncer,   # sigma_meas
            line$StationID,     # ID
            source2, # source
            line$QualityFlag,
            line$QMAP_NAME)  # quality
    if(length(l)==0) {stop("Error!")}
    lines <- c(lines, l)
  }
          
  
}


f <- file("Vs30_tables/Vs30_table.txt","w")
writeLines(lines, con=f)
close(f)
