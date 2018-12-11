# Load geology database, add some metadata, save as new database.
#
# Run time on 20170825
# system.time(source("R/processQmap.R"))
#    ...
# user  system elapsed 
# 129.356   2.611 139.081 



# Also wrangles data into nice form for ggplot2.*
# *(not anymore---this is now part of maps1.R)
rm(list = ls())

# INPUT FILES:
in__QMAP_orig <- "in/QMAP_Seamless_July13/QMAP_Seamless2013.gdb/"

# OUTPUT FILES:
  ## no longer using "testing" model ## out__ageTablePolysPath   <- "out/ageTable.csv"
  ## no longer using "testing" model ## out__ageTableSimplePath  <- "out/ageTableSimple.csv"
  ## no longer using "testing" model ## out__grainSizeTablePath  <- "out/grainSizeTable.csv"
  ## no longer using "testing" model ## out__grainSizeSimplePath <- "out/grainSizeSimple.csv"
  ## no longer using "testing" model ## out__depTablePath        <- "out/depTable.csv"
  ## no longer using "testing" model ## out__depTableSimplePath  <- "out/depTableSimple.csv"
  out__QMAP_Rdata          <-  "~/big_noDB/geo/QMAP_Seamless_July13K_NZGD00.Rdata"
  out__AhdiAKPath          <- "out/MODEL_AhdiAK_classifications.csv"
  out__AhdiAKPath_kable    <- "out/MODEL_AhdiAK_classifications.kable"

reclassify <- TRUE # reclassify data. If FALSE, open previously saved database instead.

library(rgdal)
library(knitr)
setwd("~/VsMap/")
source("R/classifyThings.R")


# testing only - revert when everything works.
if(reclassify) {
  #==============================================================================
  # Load original QMAP ---------------------------------------------------------
  setwd("~/VsMap/")
  # a <- ogrListLayers(in__QMAP_orig)
  
  map_NZGD00 <- readOGR(in__QMAP_orig, layer = "NZL_GNS_250K_geological_units")
  
  # for testing - just first ten polygons
  # map_NZGD00.old <- map_NZGD00
  # map_NZGD00 <- map_NZGD00[1:10,]
  # save(map_NZGD00, map_NZGD00.old, file = "Rdata/tempTestMap.Rdata")
  # load("Rdata/tempTestMap.Rdata")
  
  
  # Set INDEX field
  map_NZGD00$INDEX         <- as.numeric(row.names(map_NZGD00))
  
  whichMap <- "NZ"
  
  
  # Apply classifications - no predictions just categories.
  source("R/models.R") # contains model lists

  # testing only:
  inp = map_NZGD00
  
  # categorize geology polygons for the various models and add columns to data.
  map_NZGD00 <- classifyThings( inp = map_NZGD00 )
  df <- as.data.frame(map_NZGD00)
  
  dfo <- df                         # initialize output. must cbind to dfo and retain its link
  dfo <- dfo[,c(), drop = F]        # to the original df or else spCbind() complains!
  
  
  ############
  ####################################
  #################################################################
  # The following needs to be updated MANUALLY when new models are produced.
  #################################################################
  ####################################
  ############
  
  # Apply geology models via table lookup
  # testing
  Vs30_testing <- testing_set_Vs30(df)
  stDv_testing <- testing_set_stDv(df)
  dfo <- cbind(dfo, Vs30_testing, stDv_testing)
  
  # AhdiAK (i.e. priors)
  Vs30_AhdiAK <- AhdiAK_set_Vs30(df)
  stDv_AhdiAK <- AhdiAK_set_stDv(df)
  dfo <- cbind(dfo, Vs30_AhdiAK, stDv_AhdiAK)
  
  
  # Bayes-updated models
  # These will not be available unless modelsUpdate has been run
  Vs30_AhdiAK_KaiAll <- AhdiAK_KaiAll_set_Vs30(df)
  stDv_AhdiAK_KaiAll <- AhdiAK_KaiAll_set_stDv(df)
  dfo <- cbind(dfo, Vs30_AhdiAK_KaiAll, stDv_AhdiAK_KaiAll)
  
  Vs30_AhdiAK_KaiNoQ3 <- AhdiAK_KaiNoQ3_set_Vs30(df)
  stDv_AhdiAK_KaiNoQ3 <- AhdiAK_KaiNoQ3_set_stDv(df)
  dfo <- cbind(dfo, Vs30_AhdiAK_KaiNoQ3, stDv_AhdiAK_KaiNoQ3)
  
  # added 20171129 - all data except Q3...
  Vs30_AhdiAK_noQ3 <- AhdiAK_noQ3_set_Vs30(df)
  stDv_AhdiAK_noQ3 <- AhdiAK_noQ3_set_stDv(df)
  dfo <- cbind(dfo, Vs30_AhdiAK_noQ3, stDv_AhdiAK_noQ3)
  
  # added 20171129 - all data except Q3 and McGann...
  Vs30_AhdiAK_noQ3noMcGann <- AhdiAK_noQ3noMcGann_set_Vs30(df)
  stDv_AhdiAK_noQ3noMcGann <- AhdiAK_noQ3noMcGann_set_stDv(df)
  dfo <- cbind(dfo, Vs30_AhdiAK_noQ3noMcGann, stDv_AhdiAK_noQ3noMcGann)
 



  map_NZGD00 <- spCbind(map_NZGD00, dfo)
  
  
  NorthSouth <- vector(length = dim(map_NZGD00)[1])
  NorthSouth[map_NZGD00$QMAP_NAME %in% c("Hawkes Bay",
                                        "Raukumara",
                                        "Rotorua",
                                        "Taranaki",
                                        "Wairarapa",
                                        "Auckland",
                                        "Waikato",
                                        "Kaitaia",
                                        "Whangarei"
                                        )] <- "N"
  NorthSouth[map_NZGD00$QMAP_NAME %in% c("Aoraki",
                                        "Christchurch",
                                        "Kaikoura",
                                        "Dunedin",
                                        "Nelson",
                                        "Greymouth",
                                        "Haast",
                                        "Wakatipu",
                                        "Fiordland",
                                        "Murihiku",
                                        "Waitaki"
                                        )] <- "S"
  
  # The Wellington map contains both North and South Island---but conveniently,
  # everything EAST of 174:30 is North island, and WEST of 174:30 is South Island.
  # 174 degrees 30 minutes (WGS84) = 1725441 in NZGD00
  NorthSouth[(map_NZGD00$QMAP_NAME %in% "Wellington") & 
             (coordinates(map_NZGD00)[,1]  >  1725441)] <- "N"
  NorthSouth[(map_NZGD00$QMAP_NAME %in% "Wellington") &
             (coordinates(map_NZGD00)[,1]  <= 1725441)] <- "S"
  
  map_NZGD00 <- spCbind(map_NZGD00, NorthSouth)
  
  
  
  # write a CSV to examine the classifications... this can be commented out between classifyThings updates
  setwd("~/VsMap/")
  source("R/verify.R")
  # Use scripts in Verify to spit out CSV for review
  ##  no longer using "testing" model ##  write.csv(ageTablePolys(map_NZGD00),   file = out__ageTablePolysPath,   row.names = FALSE)
  ##  no longer using "testing" model ##  write.csv(ageTableSimple(map_NZGD00),  file = out__ageTableSimplePath,  row.names = FALSE)
  ##  no longer using "testing" model ##  write.csv(grainSizeTable(map_NZGD00),  file = out__grainSizeTablePath,  row.names = FALSE)
  ##  no longer using "testing" model ##  write.csv(grainSizeSimple(map_NZGD00), file = out__grainSizeSimplePath, row.names = FALSE)
  ##  no longer using "testing" model ##  write.csv(depTable(map_NZGD00),        file = out__depTablePath,        row.names = FALSE)
  ##  no longer using "testing" model ##  write.csv(depTableSimple(map_NZGD00),  file = out__depTableSimplePath,  row.names = FALSE)
  
  # for checking Ahdi units
  write.csv(AhdiAKtable(map_NZGD00),     file = out__AhdiAKPath,          row.names = FALSE)
  # format with kable() for including in Pandoc reports
  write.csv(AhdiAKtable(map_NZGD00),     file = out__AhdiAKPath,          row.names = FALSE)
  fileConn <- file(out__AhdiAKPath_kable)
  writeLines(kable(x = AhdiAKtable(map_NZGD00),format = "pandoc", digits = 3, row.names = T,
        align = NULL, caption = "This is the caption", escape = T), fileConn)
  close(fileConn)
  
  
  # =============================================================================
  # Get subset of Canterbury-only polygons (for speed in plotting) -------------
  pMargin <- 35000  #35km to account for big polygons
  CantXmin <- 1470000 - pMargin
  CantXmax <- 1620000 + pMargin
  CantYmin <- 5100000 - pMargin
  CantYmax <- 5250000 + pMargin
  
  mask <-
    (as.vector(coordinates(map_NZGD00)[,1] > CantXmin)) &
    (as.vector(coordinates(map_NZGD00)[,1] < CantXmax)) &
    (as.vector(coordinates(map_NZGD00)[,2] > CantYmin)) &
    (as.vector(coordinates(map_NZGD00)[,2] < CantYmax))
  
  map_NZGD00.Cant <- map_NZGD00[mask,]
  

  
  
  
  
  
  
  
  
  #==============================================================================
  # Save results to new location
  
  
  # NOTE - must write in CURRENT working directory with writeOGR. Other paths seem to cause problems.
  setwd("~/big_noDB/geo")
  
  # NetCDF format does NOT seem to work at all. (File grows to >1.5 GB and never stops.)
  

  
  
  
  #saving new geology-classified QMAP data ==============================================================================
  # Only needs to be run ONCE each time geology categories are changed!
  
      # Optional: saving to GeoDB format ----------------------------------------------------
      # ONLY WORKS with the ESRI driver. See https://trac.osgeo.org/gdal/wiki/FileGDB
      # The open driver, OpenFileGDB, is available in GDAL 1.11+ and is read-only. Easier to use Rdata format.
      # Note - database must be deleted to be rewritten. I.e. cannot overwrite.
      # setwd("~/big_noDB/geo")
      # d <- "QMAP_Seamless_July13K_NZGD00.gdb" # For future reference: NO trailing slash.
      # unlink(d, recursive = TRUE) # careful - deletes file prior to write
      # x <- writeOGR(obj = map_NZGD00, dsn = d, layer = "NZL_GNS_250K_geological_units",
      #               driver = "FileGDB")
    
  
      # OR: Saving to Rdata format
      save(map_NZGD00,
           map_NZGD00.Cant,
           file=out__QMAP_Rdata)
      
      
      
  
} else {
  
  # # To load modified QMAP -------------------------------------------------------
  
    # # OPTIONAL: use FileGDB format
    # setwd("~/big_noDB/geo")
    # d <- "QMAP_Seamless_July13K_NZGD00.gdb"
    # # a <- ogrListLayers(d)
    # map_NZGD00 <- readOGR(d, layer = "NZL_GNS_250K_geological_units") 
  
    # or: use Rdata
    setwd("~/big_noDB/geo")
    load("QMAP_Seamless_July13K_NZGD00.Rdata")
}

setwd("~/VsMap")

