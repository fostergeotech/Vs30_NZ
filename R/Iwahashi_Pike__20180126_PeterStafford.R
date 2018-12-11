#Iwahashi_Pike.R
#
# Playing with terrain categorization.
# Cleaned this up a bit to send to Peter Stafford re: email chain 20180126
#
# My notes on computation time are below; may help in testing the code on Spain. 
# Defintiely recommend cropping or resampling DEM for easier testing.
#
# Examining time for various raster sizes
#   50x50     23 sec
#  100x100    23 sec
#  150x150    23 sec
#  300x300    30 sec
#  600x600    45 sec
# 1200x1200   142 sec
#
# This means total time for computations is about:
#       8.5*10^(-5) np + 20      seconds, where np = number of pixels.
# One 6000x8000 pixel tile will take more or less one hour to compute?
# (update - Ended up being closer to 30 minutes)
#

# Created all-NZ rasters overnight 20171214-15. EACH raster took about 6 hours!
# (The "building block" rasters---slp, med, txt---took significantly less time. Majority
# of time is spent on the nested-means classification.)


rm(list=ls())

workDir <- "~/big_noDB/topo/terrainCats/" # working directory

library(raster)   # try: focal, terrain
library(matrixStats) # maybe helpful? # NO
library(rgdal)
library(viridis)



regenerate <- T # recompute rasters or just load saved files?
testSize <- F   # if True, compute a subset of input rasters. Quicker for testing. For all NZ, set to False.
testSizeDim <- 600 # number of cells on a side. For grabbing a small square cropped area from larger raster for testing.

DEMresample <- 100 # if 0, no resampling. If >0, new resolution in metres.

# numCats is number of desired categories. must be one of 8, 12, 16.
for (numCats in c(8,12,16)) {

  if (numCats != 8 && numCats != 12 && numCats != 16 ) {
    stop("numCats must be 8, 12, or 16.")
  }


  # All NZ:
  slpFileName <- paste0(workDir, "slp_NZ_", DEMresample, ".tif")
  medFileName <- paste0(workDir, "med_NZ_", DEMresample, ".tif")
  txtFileName <- paste0(workDir, "txt_NZ_", DEMresample, ".tif") # texture (Iwahashi & Pike)
  cvxFileName <- paste0(workDir, "cvx_NZ_", DEMresample, ".tif") # local convexity (Iwahashi & Pike)
  outFileName <- sprintf("%sIwahashiPike_NZ_%03dm_%02d.tif",
                         DEMresample, numCats)
  
  
  

  DEM00    <- raster(paste0(workDir,"DEM_all_NZGD00_resampled.tif")) # resampled: 100m (water is NA!)
  
  
  DEM <- DEM00
  
  
  if(DEMresample>0) {
    if(regenerate) {
      DEMr <- raster(DEM)
      res(DEMr) <- DEMresample
      DEMr <- resample(x=DEM, y=DEMr, method="bilinear")
      DEM <- DEMr
      writeRaster(DEM, filename = sprintf("~/big_noDB/topo/DEM_all_NZGD00_%03dm.tif",DEMresample), format = "GTiff", overwrite=T)
    } else {
      DEM <- raster(sprintf("~/big_noDB/topo/DEM_all_NZGD00_%03dm.tif",DEMresample))
    }
  }

  if (regenerate) {
    ##############################################################################################
    # SLOPE GRADIENT
    slp <- terrain(x=DEM,
                   opt = "slope",
                   unit = "tangent",
                   neighbors = 8,
                   filename = slpFileName,
                   overwrite=T)



    ####################################################################################################
    # median filter

    if(testSize) {
      # testing: subset
      ts <- testSizeDim
      DEM <- raster(x = "~/big_noDB/topo/topo_orig_NZMG/si/0007-0003.tif")
      DEM <- DEM[1:ts,1:ts,drop=F]
      slp <- slp[1:ts,1:ts,drop=F]
    }


    # Median raster
    med <- focal(x = DEM,
                 w = matrix(1,3,3),
                 fun = median,
                 na.rm = T,
                 filename = medFileName, overwrite=T)





    ################################################################################################
    # Compute texture as in Iwahashi and Pike.

    pits  <- (med-DEM) # not needed, just use abs(peaks) as below
    peaks <- (DEM-med)
    peaksAndPits       <- abs(peaks)
    peaksAndPitsBinary <- peaksAndPits>0

    window <- focalWeight(x=DEM[1:21,1:21,drop=F], #21x21 raster from DEM - for the 10-cell radius
                          d = 250, # 10 pixels x 25 metre resolution
                          type="circle")

    texture  <- focal(x = peaksAndPitsBinary, w=window, fun=sum, na.rm=T, filename=txtFileName, overwrite=T)


    #################################################################################################
    # Compute local convexity as in Iwahashi and Pike

    LaplacianFilter <- matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3) # from ?focal documentation
    laplacian       <- focal(x = DEM, w=LaplacianFilter, fun=sum, na.rm=F) # NOTE: na.rm=T causes problems near edges! It creates false values over the "edge" NA pixels of DEM.
    convexity       <- focal(x = laplacian, w=window, fun=sum, na.rm=T, filename=cvxFileName, overwrite=T) # by contrast, the choice of na.rm doesn't seem to matter here.

    # NOTE ON CONVEXITY CONVENTION:
    #   convexity > 0  indicates "bowl" terrain,
    #   convexity < 0 indicates "mound."
    #   https://mjo.osborne.economics.utoronto.ca/index.php/tutorial/index/1/cv1/t

  } else {
    slp         <- raster(x=slpFileName)
    med         <- raster(x=medFileName)
    texture     <- raster(x=txtFileName)
    convexity   <- raster(x=cvxFileName)
  }



  # find mean value for slope, convexity, texture
  slpMean <- extract(x=slp,       y=extent(slp),       fun=mean, na.rm=T)
  cvxMean <- extract(x=convexity, y=extent(convexity), fun=mean, na.rm=T)
  txtMean <- extract(x=texture,   y=extent(texture),   fun=mean, na.rm=T)

  # mask for high slope regions
  steeper <- slp >  slpMean  # mountainous (steep) areas (categories 1-4)
  flatter <- slp <= slpMean  # flatter areas (categories 5 and up)

  # mask for high and low convexity regions
  cvxHi <- convexity >  cvxMean
  cvxLo <- convexity <= cvxMean

  # mask for high and low convexity regions
  txtHi <- texture >  txtMean
  txtLo <- texture <= txtMean

  #######################################################################
  # begin assigning categories according to Iwahashi & Pike figs. 2 and 3

  # make empty raster to hold categorical values
  # IwahashiPikeCats <- raster(x=slp)
  IwahashiPikeCats <- raster(nrows=nrow(slp), ncols=ncol(slp),
                             xmn=xmin(slp), xmx=xmax(slp),
                             ymn=ymin(slp), ymx=ymax(slp),
                             crs=crs(slp), ext=extent(slp),
                             resolution=res(slp), vals=NULL)

  IwahashiPikeCats[steeper & cvxHi & txtHi] <- 1
  IwahashiPikeCats[steeper & cvxHi & txtLo] <- 2
  IwahashiPikeCats[steeper & cvxLo & txtHi] <- 3
  IwahashiPikeCats[steeper & cvxLo & txtLo] <- 4


  # conditional branching based on numCats
  if (numCats == 8) {
    IwahashiPikeCats[flatter & cvxHi & txtHi] <- 5
    IwahashiPikeCats[flatter & cvxHi & txtLo] <- 6
    IwahashiPikeCats[flatter & cvxLo & txtHi] <- 7
    IwahashiPikeCats[flatter & cvxLo & txtLo] <- 8
  } else {

    # Apply second threshold ...........

    slpF <- slp[flatter, drop=F]
    cvxF <- convexity[flatter, drop=F]
    txtF <- texture[flatter, drop=F]
    slpMean2 <- extract(x=slpF,   y=extent(slp), fun=mean, na.rm=T)
    cvxMean2 <- extract(x=cvxF,   y=extent(slp), fun=mean, na.rm=T)
    txtMean2 <- extract(x=txtF,   y=extent(slp), fun=mean, na.rm=T)

    steeper2 <- slpF >  slpMean2
    flatter2 <- slpF <= slpMean2
    cvxHi2   <- cvxF >  cvxMean2
    cvxLo2   <- cvxF <= cvxMean2
    txtHi2   <- txtF >  txtMean2
    txtLo2   <- txtF <= txtMean2

    IwahashiPikeCats[steeper2 & cvxHi2 & txtHi2] <- 5
    IwahashiPikeCats[steeper2 & cvxHi2 & txtLo2] <- 6
    IwahashiPikeCats[steeper2 & cvxLo2 & txtHi2] <- 7
    IwahashiPikeCats[steeper2 & cvxLo2 & txtLo2] <- 8

    if (numCats == 12) {
      IwahashiPikeCats[flatter2 & cvxHi2 & txtHi2] <- 9
      IwahashiPikeCats[flatter2 & cvxHi2 & txtLo2] <- 10
      IwahashiPikeCats[flatter2 & cvxLo2 & txtHi2] <- 11
      IwahashiPikeCats[flatter2 & cvxLo2 & txtLo2] <- 12
    } else {

      # Apply third threshold ...........

      slpFF <- slpF[flatter2, drop=F]
      cvxFF <- cvxF[flatter2, drop=F]
      txtFF <- txtF[flatter2, drop=F]
      slpMean3 <- extract(x=slpFF,   y=extent(slp), fun=mean, na.rm=T)
      cvxMean3 <- extract(x=cvxFF,   y=extent(slp), fun=mean, na.rm=T)
      txtMean3 <- extract(x=txtFF,   y=extent(slp), fun=mean, na.rm=T)

      steeper3 <- slpFF >  slpMean3
      flatter3 <- slpFF <= slpMean3
      cvxHi3   <- cvxFF >  cvxMean3
      cvxLo3   <- cvxFF <= cvxMean3
      txtHi3   <- txtFF >  txtMean3
      txtLo3   <- txtFF <= txtMean3

      IwahashiPikeCats[steeper3 & cvxHi3 & txtHi3] <- 9
      IwahashiPikeCats[steeper3 & cvxHi3 & txtLo3] <- 10
      IwahashiPikeCats[steeper3 & cvxLo3 & txtHi3] <- 11
      IwahashiPikeCats[steeper3 & cvxLo3 & txtLo3] <- 12

      IwahashiPikeCats[flatter3 & cvxHi3 & txtHi3] <- 13
      IwahashiPikeCats[flatter3 & cvxHi3 & txtLo3] <- 14
      IwahashiPikeCats[flatter3 & cvxLo3 & txtHi3] <- 15
      IwahashiPikeCats[flatter3 & cvxLo3 & txtLo3] <- 16

    }
  }




  writeRaster(x=IwahashiPikeCats, filename = outFileName,
    format="GTiff", datatype="INT1U",
    overwrite=T
    )
}

pp <- raster(x=paste0(workDir, "IwahashiPike_NZ_100m_08.tif"))
qq <- raster(x=paste0(workDir, "IwahashiPike_NZ_100m_12.tif"))
rr <- raster(x=paste0(workDir, "IwahashiPike_NZ_100m_16.tif"))

# The following palette was copied from
# Iwahashi & Pike for direct comparison!
uniPal <- c("#7D4633", #1
            "#FE00FE", #2
            "#C36E2C", #3
            "#FC97CF", #4
            "#FB9600", #5
            "#FC43A6", #6
            "#FBCF66", #7
            "#FAC3D5", #8
            "#009F71", #9
            "#C4B71D", #10
            "#0072B0", #11
            "#D6D600", #12
            "#A1FD8E", #13
            "#E9E915", #14
            "#003B5B", #15
            "#F2FEBF") #16

up8  <- uniPal[1:8]
up12 <- uniPal[1:12]
up16 <- uniPal[1:16]

png(filename = "out/maps/IwahashiPike08.png")
  plot(pp, col=up8)
dev.off()
png(filename = "out/maps/IwahashiPike12.png")
  plot(qq, col=up12)
dev.off()
png(filename = "out/maps/IwahashiPike16.png")
  plot(rr, col=up16)
dev.off()




# Assign Yong et al. Vs30 values from California to the 16-category version.

Vs30_YongEtAl <- raster(nrows=nrow(slp),      ncols=ncol(slp),
                        xmn=xmin(slp),       xmx=xmax(slp),
                        ymn=ymin(slp),       ymx=ymax(slp),
                        crs=crs(slp),        ext=extent(slp),
                        resolution=res(slp), vals=NULL)

Vs30_YongEtAl[rr== 1] <- 519
Vs30_YongEtAl[rr== 2] <- 393
Vs30_YongEtAl[rr== 3] <- 547
Vs30_YongEtAl[rr== 4] <- 459
Vs30_YongEtAl[rr== 5] <- 402
Vs30_YongEtAl[rr== 6] <- 345
Vs30_YongEtAl[rr== 7] <- 388
Vs30_YongEtAl[rr== 8] <- 374
Vs30_YongEtAl[rr== 9] <- 497
Vs30_YongEtAl[rr==10] <- 349
Vs30_YongEtAl[rr==11] <- 328
Vs30_YongEtAl[rr==12] <- 297
Vs30_YongEtAl[rr==13] <- 500  # No data in Yong et al. This is my guess for incised terraces.
Vs30_YongEtAl[rr==14] <- 209
Vs30_YongEtAl[rr==15] <- 363
Vs30_YongEtAl[rr==16] <- 246


writeRaster(x   = Vs30_YongEtAl,
      filename  = paste0(workDir,"Yong2012_Cali_Vs30.tif"),
      format    = "GTiff",
      overwrite = T
      )


modelCol <- viridis(n = 100,  begin = 0.05, end = 1, direction= -1, option = "D")
png("out/maps/YongEtAlVs30.png")
  plot(Vs30_YongEtAl, col=modelCol, zlim=c(0,1550))
dev.off()

