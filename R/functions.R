################################################
# this function plots an empirical variogram.
# it can be given subsets by geologic grouping.
# intended to loop over e.g. 6-pane figure.
variogram_plot <-  function(dataset,
                            whichResid,
                            lagwidth,  # meters
                            cutoff,    # meters
                            xmax,      # meters - just for plotting
                            ymax,      # just for plotting
                            label,
                            MODEL) {
  if(length(dataset)<=1) {
    plot.new()
    mtext(paste0(label, " (NOT ENOUGH DATA)"), line=0.4)
  } else {
    empiricalVariogram <- switch(
      whichResid,
      geo = variogram(as.formula(paste0("res_",MODEL,"~1")),
                      dataset, cutoff=cutoff, width=lagwidth),
      hyb = variogram(sgeohybVs30res~1, dataset, cutoff=cutoff, width=lagwidth)
    )
    aplot(empiricalVariogram$dist/1000,
          empiricalVariogram$gamma,
          #xlim=c(0,xmax/1000),
          xlim=c(0,ceiling(max(empiricalVariogram$dist/1000)/100)*100),
          ylim=c(0,ymax),
          ylab="Semivariance", xlab="Distance, km",
          xann=TRUE, yann=TRUE)
    mtext(label, line=0.2)
  }
}

################################################
# same as above but also plots theoretical variogram.
variogram_fit_plot <-  function(dataset,
                                whichResid,
                                lagwidth,  # meters
                                cutoff,    # meters
                                xmax,      # meters - just for plotting
                                ymax,      # just for plotting
                                vgm,
                                label,
                                filename) {
  if(length(dataset)==0) {
    empiricalVariogram <- 0
  } else {
    empiricalVariogram <- switch(
      whichResid,
      geo = variogram(sgeoVs30res~1,     dataset, cutoff=cutoff, width=lagwidth),
      hyb = variogram(sgeohybVs30res~1, dataset, cutoff=cutoff, width=lagwidth)
    )}
  if (length(empiricalVariogram) > 1) {
    aplot(empiricalVariogram$dist/1000,
          empiricalVariogram$gamma,
          xlim=c(0,xmax/1000),
          ylim=c(0,ymax),
          ylab="Semivariance", xlab="Distance, km",
          xann=TRUE, yann=TRUE)
    VL <- variogramLine(vgm, cutoff)
    lines(VL$dist/1000, VL$gamma)
    mtext(label, line=0.2)
  } else {
    plot.new()
    mtext(paste0(label, " (NOT ENOUGH DATA)"), line=0.4)
  }
}








################################################
# PLOT EMPIRICAL VARIOGRAMS ONLY
plotEmpPage <- function(inputVspr, cutoff, lagwidth, filename, whichResid, MODEL) {
  png(filename, height = 8.5, width = 6, res = 400, units = "in")
  par(mfrow = c(3, 2), # rows and cols
      mar = c(1, 1, 1, 1), # margins
      oma = c(3, 3, 1, 0), # outer margins
      ps = 10, # point size of text
      tcl = -0.2 # tick mark length
  )
  xmax <- cutoff
  ymax <- 0.8
  for (group in c("ALL", "H", "HP", "P", "PQ", "R")) {
    switch(group,
           ALL = variogram_plot(dataset = inputVspr,                     
                                whichResid, lagwidth, cutoff, xmax, ymax, 
                                "All data"            , MODEL),
           H   = variogram_plot(dataset = subset(inputVspr, age1=="H"),  
                                whichResid, lagwidth, cutoff, xmax, ymax, 
                                "Holocene"            , MODEL),
           HP  = variogram_plot(dataset = subset(inputVspr, age1=="HP"), 
                                whichResid, lagwidth, cutoff, xmax, ymax, 
                                "Holocene-Pleistocene", MODEL),
           P   = variogram_plot(dataset = subset(inputVspr, age1=="P"),  
                                whichResid, lagwidth, cutoff, xmax, ymax, 
                                "Pleistocene"         , MODEL),
           PQ  = variogram_plot(dataset = subset(inputVspr, age1=="PQ"), 
                                whichResid, lagwidth, cutoff, xmax, ymax, 
                                "Pre-quaternary"      , MODEL),
           R   = variogram_plot(dataset = subset(inputVspr, age1=="R"),  
                                whichResid, lagwidth, cutoff, xmax, ymax, 
                                "Rock only"           , MODEL)
    )}
  dev.off()
}




################################################
subsetVsPoints <- function(vspr) {
  
  # Various subsets of the available data
  
  
  
  # vspr_no_Kaiser <- vspr[vspr$DataSource  != "KaiserEtAl",]
  vspr_Kaiser <- vspr[vspr$DataSource  == "KaiserEtAl",]
  
  vspr_no_McGann <- vspr[vspr$DataSource  != "McGannCPT",]
  
  qualityVec <- vspr$QualityFlag
  qualityVec[is.na(qualityVec)] <- "Q1"
  qualityVec <- qualityVec != "Q3"
  vspr_no_Q3     <- vspr[qualityVec,]
  rm(qualityVec)
  
  KaiserQ1 <- vspr[vspr$QualityFlag=="Q1"  &  vspr$DataSource=="KaiserEtAl",]
  KaiserQ2 <- vspr[vspr$QualityFlag=="Q2"  &  vspr$DataSource=="KaiserEtAl",]
  KaiserQ3 <- vspr[vspr$QualityFlag=="Q3"  &  vspr$DataSource=="KaiserEtAl",]
  
  
  vspr_no_Q3_no_McGann <-
    vspr_no_Q3[vspr_no_Q3$DataSource  != "McGannCPT",]
  
  vspr_Kaiser_noQ3 <- vspr_no_Q3[vspr_no_Q3$DataSource  == "KaiserEtAl",]
  
  vspr_no_Q3_no_McGann_chch <-
    vspr_no_Q3_no_McGann[vspr_no_Q3_no_McGann$QMAP_NAME  == "Christchurch",]
  
  vspr_McGannOnly <- vspr[vspr$DataSource  == "McGannCPT",]
  
  vspr_noQ3noCanterbury <- vspr_no_Q3[vspr_no_Q3$QMAP_NAME != "Christchurch",]
  
  allData <- vspr

  vspr_North <- vspr[vspr$NorthSouth=="N",]
  vspr_South <- vspr[vspr$NorthSouth=="S",]
  
  
  vspr_nQnM_North <- vspr_no_Q3_no_McGann[vspr_no_Q3_no_McGann$NorthSouth=="N",]
  vspr_nQnM_South <- vspr_no_Q3_no_McGann[vspr_no_Q3_no_McGann$NorthSouth=="S",]
  
  # added 20181002: Try removing all flat data (terrain cats 15 and 16) to reduce some problems
  # with the terrain model variogram. (This is a crude post-hoc attempt to support a theory
  # about subtle differences between the geology & terrain model predictions.)
  rm1516 <- function(inpDF) {
    idx <- inpDF$groupID_YongCA != "16 - Fluvial plain, alluvial fan, low-lying flat plains, etc." &
           inpDF$groupID_YongCA != "15 - Dune, incised terrace, etc."
    idx[is.na(idx)] <- TRUE
    return(inpDF[idx,])
  }
  vspr_McGannOnlyB       <- rm1516(vspr_McGannOnly)
  vspr_no_Q3_no_McGannB  <- rm1516(vspr_no_Q3_no_McGann)
  vspr_noQ3noCanterburyB <- rm1516(vspr_noQ3noCanterbury)

  vspr_Wotherspoon <- vspr[vspr$DataSource  == "Wotherspoon201711",]
  
  # adding this to use in publication plot showing data locations.
  # (not for anything else!)
  load("Rdata/McGannDownsample.Rdata")
  
  vsprSubsets <- list(
    allData           = allData,
    # noKaiser          = vspr_no_Kaiser,
    noMcGann          = vspr_no_McGann,
    noQ3              = vspr_no_Q3,
    noQ3noMcGann      = vspr_no_Q3_no_McGann,
    noQ3noMcGannB     = vspr_no_Q3_no_McGannB,
    Kaiser_all  = vspr_Kaiser,
    Kaiser_noQ3 = vspr_Kaiser_noQ3,
    KaiserQ1 = KaiserQ1,
    KaiserQ2 = KaiserQ2,
    KaiserQ3 = KaiserQ3,
    
    
    # noQ3noMcGann_chch = vspr_no_Q3_no_McGann_chch,
    # allSouth          = vspr_South,
    # allNorth          = vspr_North,
    McGann            = vspr_McGannOnly,
    McGannB           = vspr_McGannOnlyB,
    # nQnM_North        = vspr_nQnM_North,
    # nQnM_South        = vspr_nQnM_South,
    noQ3noCanterbury  = vspr_noQ3noCanterbury,
    noQ3noCanterburyB = vspr_noQ3noCanterburyB,
    McGannALL = McGannAll,
    Wotherspoon = vspr_Wotherspoon
  )
  return(vsprSubsets)
}





################################################
variogram_plot2 <-  function(dataset,
                            whichResid,
                            lagwidth,  # meters
                            cutoff,    # meters
                            xmax,      # meters - just for plotting
                            ymax,      # just for plotting
                            label,
                            MODEL) {
# variogram_plot switched to ggplot2
# for generating and returning a single ggplot object
  if(length(dataset)<=1) {
    return(ggplot())
  } else {
    empiricalVariogram <- switch(
      whichResid,
      geo = variogram(as.formula(paste0("res_",MODEL,"~1")),
                      dataset, cutoff=cutoff, width=lagwidth),
      hyb = variogram(sgeohybVs30res~1, dataset, cutoff=cutoff, width=lagwidth)
    )
    eV <- empiricalVariogram
    eV$dist <- eV$dist/1000 # make it metres
    p <- ggplot(eV, aes(dist, gamma)) + geom_point() +
            xlab("distance, km") + ylab("semivariance, gamma") +
            # ylim(c(0,ymax)) + xlim(c(0, ceiling(max(eV$dist)/100)*100)) +
            ylim(c(0,ymax)) + xlim(c(0, xmax/1000)) +
            ggtitle("x")
    return(p)
  }
}













darkenRGB <- function(rgbHexVec) {
  # BB requested to make neutral (zero) 
  # color bar values gray
  # so that background can be white. This darkens
  # middle values and leaves extreme (+ and -)
  # values unchanged.
  
  darkenBy <- 0.05
  n <- length(rgbHexVec)
  mx <- 0.5*(n+1)
  darkByIndex <- function(i) {darkenBy * (1 - abs((i-mx)/(mx-1)))}
  darkenVector <- darkByIndex(1:n)
  rgbHexOut <- rgbHexVec
  for(i in 1:n) {
    rgbHex <- rgbHexVec[i]
    rgbHexOut[i] <- rgb(
      col2rgb(modelCol[i])[1] * (1-darkenVector[i]),
      col2rgb(modelCol[i])[2] * (1-darkenVector[i]),
      col2rgb(modelCol[i])[3] * (1-darkenVector[i]),
      maxColorValue = 255)
  }
  rgbHexOut
}











################################################
rasterizeQmap <- function(Qmap, xLims, yLims, nrow, ncol) {
  library(raster)
  library(sp) # dunno if I need this. just in case.
  
  # xLims, yLims: coordinate extents in NZGD00
  
  
  # Define extent
  ext   <-  extent(c(xLims  , yLims  ))
  
  
  # # Set up a raster "template" for rasterize
  r    <- raster(ext,    ncol=ncol, nrow=nrow)
  
  # Assign projection string to new raster object
  pr   <- proj4string(Qmap)
  proj4string(r)    <- pr
  
  out <- rasterize(Qmap, r, "INDEX")
  
  return(out)
}


################################################
makeTIFandXYZ_WGS84 <- function(NZGD00raster, TIFname, XYZname) {
  source("~/VsMap/R/geodetic.R")
  WGS84raster <- projectExtent(object=NZGD00raster,crs=crsWGS84())
  WGS84raster <- projectRaster(from=NZGD00raster, to=WGS84raster, method='ngb')
  
  
  # The TIF file that is used as an intermediary to generate XYZ files is NOT used hereafter.
  # Quick solution is to provide input TIFname in /tmp/. This change has been done.
  
  
  writeRaster(WGS84raster, filename=TIFname, format = "GTiff", overwrite=TRUE)
  
  
  # # Notes on the following command - coercing WGS84raster to type SpatialPixelsDataFrame using "as"....
  # #  1. Either SpatialPixelsDataFrame OR SpatialGridDataFrame works fine. writeGDAL can use either one.
  # #  2. However, if ALL pixels in WGS84Raster are NaNs, then the coersion doesn't know what to do.
  # #      - devising a workaround for this...
  # q <- as(WGS84raster,"SpatialPixelsDataFrame")
  # rgdal::writeGDAL(q,XYZname,drivername = "XYZ")
  #
  # ALTERNATE solution!
  # Perform GDAL_TRANSLATE on each TIF file directly using system2()
  system2("gdal_translate", args=c(
    "-of", "XYZ",
    TIFname,
    paste0("/tmp/",basename(XYZname))
  ))
  # The following GREP operation removes NaN lines (which are encoded as -inf)
  system2("grep", args=c(
    "-v",
    "inf",
    paste0("/tmp/",basename(XYZname))),
    stdout=XYZname)
  
  
  return(WGS84raster)
}


resampleRaster <- function(inpRaster, TheMethod, newRes_m) {
  library(raster)
  # outputs new low-res rasters given newRes input
  resampleRaster <- inpRaster
  res(resampleRaster) <- newRes_m 
  resampledRaster <- raster::resample(inpRaster, resampleRaster, method=TheMethod) #MUST be NGB for index!
  return(resampledRaster)
}




################################################
scalebar <- function(x, y, width = 5){
  tck <- pretty(c(0, width))
  nt <- length(tck)
  lines(c(x, x+width*1000), c(y, y))
  segments(x+tck*1000, rep(y, nt), x+tck*1000, rep(y+par("cxy")[2]/4, nt))
  text(x+width*1000/2, y+par("cxy")[2]/4, paste(width, "km"), pos = 3 )
}

################################################
diffCol <- function(x){
  grd <- c(-50, 0, 50)
  R <- c(255, 255,  0)
  G <- c( 90, 255, 90)
  B <- c(  0, 255,255)
  Rint <- approx(grd, R, x, rule = 2)$y
  Gint <- approx(grd, G, x, rule = 2)$y
  Bint <- approx(grd, B, x, rule = 2)$y
  Rint[is.na(Rint)] <- 126
  Gint[is.na(Gint)] <- 126
  Bint[is.na(Bint)] <- 126
  return(rgb(Rint, Gint, Bint, max = 255))
}

################################################
diffLeg <- function(x, y, hx = 4){
  cxy <- par("cxy")
  h <- hx * cxy[2]
  w <- cxy[1]
  n <- 60
  x1 <- -50
  x2 <- 50
  xg <- seq(x1, x2, len = n + 1)
  xMid <- 0.5*xg[-1]+0.5*xg[-(n+1)]
  cols <- diffCol(xMid)
  l <- x - w/2
  r <- x + w/2
  tmp <- seq(y - h, y, len = n + 1)
  u <- tmp[-1]
  b <- tmp[-(n+1)]
  for(i in 1:n)
    rect(l, b[i], r, u[i], col = cols[i], border = NA)
  lines(c(l, l, r, r, l), c(b[1], u[n], u[n], b[1], b[1]))
  tck <- c(-100, -50, 0, 50, 100)
  ytk <- approx(c(x1, x2), c(y-h, y), tck )$y
  segments(r, ytk, r+w/3, ytk)
  text(r+w/3, ytk, lab = tck, pos = 4)
  text(0.5*l+0.5*r, u[n], lab = "Difference", pos = 3)
}

################################################
sdCol <- function(x){
  grd <- c(0.07, 0.7, 1.2, 0.22)
  R <- c(255,255,254, 69)
  G <- c(150,170,255,117)
  B <- c( 75, 50, 50,209)
  Rint <- approx(grd, R, x, rule = 2)$y
  Gint <- approx(grd, G, x, rule = 2)$y
  Bint <- approx(grd, B, x, rule = 2)$y
  Rint[is.na(Rint)] <- 126
  Gint[is.na(Gint)] <- 126
  Bint[is.na(Bint)] <- 126
  return(rgb(Rint, Gint, Bint, max = 255))
}

################################################
sdLeg <- function(x, y, hx = 4){
  cxy <- par("cxy")
  h <- hx * cxy[2]
  w <- cxy[1]
  n <- 60
  x1 <- 0.07
  x2 <- 0.22
  xg <- seq(x1, x2, len = n + 1)
  xMid <- 0.5*xg[-1]+0.5*xg[-(n+1)]
  cols <- sdCol(xMid)
  l <- x - w/2
  r <- x + w/2
  tmp <- seq(y - h, y, len = n + 1)
  u <- tmp[-1]
  b <- tmp[-(n+1)]
  for(i in 1:n)
    rect(l, b[i], r, u[i], col = cols[i], border = NA)
  lines(c(l, l, r, r, l), c(b[1], u[n], u[n], b[1], b[1]))
  tck <- c(0.1, 0.15, 0.2)
  ytk <- approx(c(x1, x2), c(y-h, y), tck )$y
  segments(r, ytk, r+w/3, ytk)
  text(r+w/3, ytk, lab = tck, pos = 4)
  text(0.5*l+0.5*r, u[n], lab = "s (ln)", pos = 3)
}

################################################
resCol <- function(x){
  grd <- c(-0.4, 0, 0.4)
  R <- c(255, 255,  0)
  G <- c( 90, 255, 90)
  B <- c(  0, 255,255)
  Rint <- approx(grd, R, x, rule = 2)$y
  Gint <- approx(grd, G, x, rule = 2)$y
  Bint <- approx(grd, B, x, rule = 2)$y
  Rint[is.na(Rint)] <- 126
  Gint[is.na(Gint)] <- 126
  Bint[is.na(Bint)] <- 126
  return(rgb(Rint, Gint, Bint, max = 255))
}

################################################
resLeg <- function(x, y, hx = 4){
  cxy <- par("cxy")
  h <- hx * cxy[2]
  w <- cxy[1]
  n <- 60
  x1 <- -0.4
  x2 <- 0.4
  xg <- seq(x1, x2, len = n + 1)
  xMid <- 0.5*xg[-1]+0.5*xg[-(n+1)]
  cols <- resCol(xMid)
  l <- x - w/2
  r <- x + w/2
  tmp <- seq(y - h, y, len = n + 1)
  u <- tmp[-1]
  b <- tmp[-(n+1)]
  for(i in 1:n)
    rect(l, b[i], r, u[i], col = cols[i], border = NA)
  lines(c(l, l, r, r, l), c(b[1], u[n], u[n], b[1], b[1]))
  tck <- c(-0.4, 0, 0.4)
  ytk <- approx(c(x1, x2), c(y-h, y), tck )$y
  segments(r, ytk, r+w/3, ytk)
  text(r+w/3, ytk, lab = tck, pos = 4)
  text(0.5*l+0.5*r, u[n], lab = "res (ln)", pos = 3)
}

################################################
VsCol <- function(Vs){
  Vgrd <- c(100, 160, 200, 230, 270, 310, 490, 620, 760, 900, 1200)
  R <- c(200, 200, 226, 255, 255, 255, 102, 51, 51, 10, 220)
  G <- c(0, 20, 70, 145, 200, 255, 204, 153, 102, 50, 50)
  B <- c(0, 0, 0, 0, 0, 74, 102, 51, 102, 220, 220)
  Rint <- approx(Vgrd, R, Vs, rule = 2)$y
  Gint <- approx(Vgrd, G, Vs, rule = 2)$y
  Bint <- approx(Vgrd, B, Vs, rule = 2)$y
  Rint[is.na(Rint)] <- 126
  Gint[is.na(Gint)] <- 126
  Bint[is.na(Bint)] <- 126
  return(rgb(Rint, Gint, Bint, max = 255))
}

################################################
VsLeg <- function(x, y, hx = 4){
  cxy <- par("cxy")
  h <- hx * cxy[2]
  w <- cxy[1]
  n <- 60
  V1 <- 100
  V2 <- 1400
  Vs <- seq(V1, V2, len = n + 1)
  VsMid <- 0.5*Vs[-1]+0.5*Vs[-(n+1)]
  cols <- VsCol(VsMid)
  l <- x - w/2
  r <- x + w/2
  tmp <- seq(y, y - h, len = n + 1)
  u <- tmp[-(n+1)]
  b <- tmp[-1]
  for(i in 1:n)
    rect(l, b[i], r, u[i], col = cols[i], border = NA)
  lines(c(l, l, r, r, l), c(b[n], u[1], u[1], b[n], b[n]))
  tck <- c(180, 360, 760, 1200)
  ytk <- approx(c(V1, V2), c(y, y-h), tck )$y
  segments(r, ytk, r+w/3, ytk)
  text(r+w/3, ytk, lab = tck, pos = 4)
  text(0.5*l+0.5*r, u[1], lab = "Vs30 (m/s)", pos = 3)
}


################################################
# These are hard-coded fits for plotting. Determine coefficients by examining r_geo plots and using linear fits in analysisKF.R.
Hslope <- function(slp){
  # 2017-02-12
        # > Vs_slopeFits30c_H_bounded$Alluvial.H$coefficients
        # (Intercept) log10(slp30c) 
        # 2.3229682     0.0103971 
  intercept <- 2.3229682
  slope <- 0.0103971 
  alluvHbounds   <- c(0.0003,  0.1 )
  Vs30 <- 10^((log10(alluvHbounds) * slope) + intercept)   # Vs30
  vs <- log10(Vs30)
  ls <- log10(slp)                                       # inputs in log space
  s  <- log10(alluvHbounds)                              # bounds in log space
  return(10^(approx(s, vs, ls, rule = 2)$y))
}

Qslope <- function(slp){
  # Canterbury data is not sufficient to constrain this...
  # return to it when more data added
        # > Vs_slopeFits30c_HP_bounded$Alluvial.HP$coefficients
            # (Intercept) log10(slp30c) 
            # 2.477121            NA 
  intercept <- 2.477121
  slope <- 0
  alluvHPbounds   <- c(0.0001,  1)
  Vs30 <- 10^((log10(alluvHPbounds) * slope) + intercept)   # Vs30
  vs <- log10(Vs30)
  ls <- log10(slp)                                       # inputs in log space
  s  <- log10(alluvHPbounds)                              # bounds in log space
  return(10^(approx(s, vs, ls, rule = 2)$y))
}

Pslope <- function(slp){
  # 2017-02-12
          # Vs_slopeFits30c_P_bounded$Alluvial.P$coefficients
              # (Intercept) log10(slp30c) 
              # 3.2051229      0.2948727 
  intercept <- 3.2051229
  slope <- 0.2948727
  alluvPbounds   <- c(0.004 ,  0.1 )
  Vs30 <- 10^((log10(alluvPbounds) * slope) + intercept)   # Vs30
  vs <- log10(Vs30)
  ls <- log10(slp)                                       # inputs in log space
  s  <- log10(alluvPbounds)                              # bounds in log space
  return(10^(approx(s, vs, ls, rule = 2)$y))
}

WAactive30 <- function(m){
  # Modified for Allen and Wald (2009) update
  Vs <- numeric(length(m))
  slp <- log(c(3e-4, 3.5e-3, 0.01, 0.018, 0.05, 0.1, 0.14))
  wa.vs30 <- log(c(180, 240, 300, 360, 490, 620, 760))
  Vs <- exp(approx(slp, wa.vs30, log(m), rule = 2)$y)
  return(Vs)
}




################################################
vsum <- function(x, f){
  cat("n:", length(which(f)), "\n")
  cat("Mean:", round(exp(mean(log(x$Vs30[f]))), 0), "\n")
  cat("sd:", round(sd(log(x$Vs30[f])), 3), "\n")
}




########################################
getDesc <- function(x){
  cat(unique(map$DESCRIPTION[which(map$UNITNAME == x)]), "\n")
  cat(unique(geo2$desc[which(geo2$name == x)]), "\n")
  cat(unique(geo3$desc[which(geo3$name == x)]), "\n")
  cat(unique(geo4$desc[which(geo4$name == x)]), "\n")
  cat(unique(geo5$desc[which(geo5$name == x)]), "\n")
}

#####################################################
bins <- function(x, y, nb = NA, xlog = TRUE, ylog = TRUE,
                 seom = FALSE,
                 bks = NULL, col = rgb(0, 0.5, 1),
                 cex = 1, pch = 16, lwd = 1){
  # Function to add median and +/- 1 sd horizontal line at midpoint
  # of bins.
  #
  # Arguments
  # ---------
  # nb:    number of breaks.
  # xlog:  TRUE/FALSE; should breaks be spaced logarithmically.
  #        Only used if 'nb' is not NA. Default is TRUE.
  # ylog:  Are the y values lognormal? Default is TRUE.
  # seom:  TRUE/FALSE: bars are standard error of mean if TRUE, or
  #        standard deviation if FALSE (the default)
  # bks:   the break points for the bins. If you use this,
  #        then the bins automatically spand the data range;
  #        i.e., the min and max values are automatically added.
  #        *Only used if 'nb' is NA*
  # col:   color of point and lines.
  # cex:   see 'par'; default is 1.
  # pch:   see 'par'; default is 16, a closed circle.
  # lwd:   see 'par'; default is 1.

  # Changes
  # -------
  # 11/15/2013 - Add horizontal segments at end of bars.
  # 11/16/2013 - Allow SD or SEOM

  # Define the lower, upper, and mid value of each bin
  
  
  if(is.na(nb)){
    l <- c(min(x), bks)
    u <- c(bks,max(x))
    if(xlog){
      mid <- sqrt(l*u)
    }else{
      mid <- (l+u)/2
    }
  }else{
    if(xlog){
      bks <- exp(seq(log(min(x)), log(max(x)), len = nb+1))
      l <- bks[-length(bks)]
      u <- bks[-1]
      mid <- sqrt(l*u)
    }else{
      bks <- seq(min(x), max(x), len = nb+1)
      l <- bks[-length(bks)]
      u <- bks[-1]
      mid <- (l+u)/2
    }
  }

  # Compute median/mean and sd or se, and plot
  m <- sdev <- bar <- rep(NA, length(bks)+1)
  w <- strwidth("x")
  lw <- 10^(w/2)
  for(i in 1:length(m)){
    if(i == 1)
      keep <- x >= l[i] & x <= u[i]
    else
      keep <- x > l[i] & x <= u[i]
    if(ylog){
      m[i] <- exp(mean(log(y[keep])))
      sdev[i] <- sd(log(y[keep]))
      if(seom)
        bar[i] <- sdev[i]/sqrt(length(y[keep]))
      else
        bar[i] <- sdev[i]
      points(mid[i], m[i], pch = pch, cex = cex, col = col)
      segments(mid[i], exp(log(m[i])+bar[i]),
               mid[i], exp(log(m[i])-bar[i]),
               lwd = lwd, col = col )
      if(xlog){
        segments(mid[i]*lw, exp(log(m[i])+bar[i]),
                 mid[i]/lw, exp(log(m[i])+bar[i]),
                 lwd = lwd, col = col)
        segments(mid[i]*lw, exp(log(m[i])-bar[i]),
                 mid[i]/lw, exp(log(m[i])-bar[i]),
                 lwd = lwd, col = col)
      }else{
        segments(mid[i]+w/2, exp(log(m[i])+bar[i]),
                 mid[i]-w/2, exp(log(m[i])+bar[i]),
                 lwd = lwd, col = col)
        segments(mid[i]+w/2, exp(log(m[i])-bar[i]),
                 mid[i]-w/2, exp(log(m[i])-bar[i]),
                 lwd = lwd, col = col)
      }
    }else{
      m[i] <- mean(y[keep])
      sdev[i] <- sd(y[keep])
      if(seom)
        bar[i] <- sdev[i]/sqrt(length(y[keep]))
      else
        bar[i] <- sdev[i]
      points(mid[i], m[i], pch = pch, cex = cex, col = col)
      segments(mid[i], m[i]+bar[i], mid[i], m[i]-bar[i],
               lwd = lwd, col = col )
      if(xlog){
        segments(mid[i]*lw, m[i]+bar[i], mid[i]/lw, m[i]+bar[i],
                 lwd = lwd, col = col)
        segments(mid[i]/lw, m[i]-bar[i], mid[i]*lw, m[i]-bar[i],
                 lwd = lwd, col = col)
      }else{
        segments(mid[i]+w/2, m[i]+bar[i], mid[i]-w/2, m[i]+bar[i],
                 lwd = lwd, col = col)
        segments(mid[i]+w/2, m[i]-bar[i], mid[i]-w/2, m[i]-bar[i],
                 lwd = lwd, col = col)
      }
    }
  }
  invisible(list(m = m, bar = bar))
}


#####################################################
slope.srtm30 <- function(lon, lat ){
  # This relies on an input grid file containing the slope data.
  # KF used a shell script and the gdal_translate tool to convert geoTiffs to GMT (netCDF) format.
  tmp <- paste("echo", lon, lat, 1,"| grdtrack -Gutah_slope30.grd -Q ",
               sep = " ")
  out <- system(tmp, intern = TRUE)
  slp <- as.numeric(strsplit(out, "\t")[[1]][4])
  return(slp)
}

#####################################################
slope.srtm30vec <- function(lon, lat, slopeRes){
  # This relies on an input grid file containing the slope data.
  # No longer uses tiled NC files generated from shell scripts looping on original topo files.
  # Instead uses two NC files generated within loadDEM.R using system2() calls on GMT and GDAL modules.
  # The old way was (a) not using 9c and 30 resampled slopes, and (b) had issues at tile edges.
  #
  #
  #
  setwd("~/VsMap/")
  
  
  fname <- "tmp.xyz"
  fname2 <- "tmp2.xyz"
  
  xyz <- cbind(lon, lat, 1:length(lon))
  write.table(xyz, file = fname, quote = FALSE, row.names = FALSE,
              col.names = FALSE)
  if(Sys.info()["nodename"][[1]] == "hypocentre") {
    grdtrackPath <- "/opt/gmt-5.1.2/bin/grdtrack"
  } else {
  if(Sys.info()["nodename"][[1]] == "neurofunk") {
    grdtrackPath <- "/usr/local/src/gmt/bin/grdtrack"
  }}
  if(slopeRes == "30c") {
    slopeFiles <- c("-G/home/kmf76/VsMap/img/nzsi_30c_slp_nnb.nc",
                    "-G/home/kmf76/VsMap/img/nzni_30c_slp_nnb.nc")
  } else {
  if(slopeRes == "9c") {
    slopeFiles <- c("-G/home/kmf76/VsMap/img/nzsi_9c_slp_nnb.nc",
                    "-G/home/kmf76/VsMap/img/nzni_9c_slp_nnb.nc")
  }}
  system2(grdtrackPath, args=c(fname,
           slopeFiles,
           "-nb",
           "-N"),
            stdout = fname2)
  out <- scan(fname2, what = list(x = 0, y = 0, id = 0,
                                      slp_si=0, slp_ni=0
                                      ))
  allSlopesOut <- cbind(out$slp_si, out$slp_ni)  # combine all to form a matrix.

  slopeVals <- c()
  slopeVals <- rowSums(allSlopesOut, na.rm = FALSE)  # if at least one entry per row is NaN, then this will be a vector of NaNs!
  if (length(which(!is.na(slopeVals))) > 0) {
    print("Warning! Some grid locations yielded a match on both north and south island---i.e. something is wrong.")
  } 
  slopeVals <- rowSums(allSlopesOut, na.rm = TRUE) # having verified above that all is well, na.rm=TRUE yields an output with one slope per row.
  for (i in 1:length(lat)) {   
    if (all(is.na(allSlopesOut[i,]))) {
      slopeVals[i] <- NaN
    }
  }
  system2("rm", args = c(fname, fname2))
  return(slopeVals)  # note that the R function terrain() does NOT need to be divided by 100, unlike the old method that used GDALDEM.
}

######################################################
aplot <- function(x, y = NULL,
                  xlab = NULL, ylab = NULL,
                  xann = TRUE, yann = TRUE,
                  xtck = TRUE, ytck = TRUE,
                  sci = FALSE,
                  xside = 1, yside = 2,
                  ...){
  #######################################
  # Description
  # -----------
  # This is a replacement plot function, primarily for
  # improved handling of axes/tick/labels.
  #
  # Arguments
  # ---------
  # x,y        = see 'plot.default'
  # xlab, ylab = see 'plot.default'
  # sci        = use 10^x format to label tick marks
  # xann       = label x-axis tick marks
  # yann       = lable y-axis tick marks
  # xside      = side for x-axis labels (default is 1)
  # yside      = side for y-axis labels (default is 2)
  # ...        = additional parameters for 'plot'
  #
  # Notes
  # -----
  #   o Should only be expected to work for typical
  #     scatterplots; x, y, or a list with x/y should
  #     work. But nonstanard classes, like data.frames,
  #     should not be expected to work.
  #   o 'tcl' must be set. Not compatible with use of
  #     'tck' for tick size. I do not know how to
  #      convert a value of 'tck' to 'tcl'.
  #
  # Changes
  # -------
  # 11/22/2013 Created (EMT)
  # 11/26/2013 Edited horizontal axis placement to be
  #            consistent with vertial.
  # 12/04/2013 Added xtck, ytck.
  #
  #######################################

  plot(x, y, axes = FALSE, xlab = "", ylab = "", ...)
  if(is.null(xlab))
    xlab <- "x"
  if(is.null(ylab))
    ylab <- "y"

  # Take care of ticks, get yoffset for ylab:
  lt <- ltics(xann = xann, yann = yann,
              xtck = xtck, ytck = ytck,
              xside = xside, yside = yside,
              sci = sci)
  xoff <- lt$xoffset
  yoff <- lt$yoffset

  # Take care of xlab and ylab
  # a little fudge factor:
  xpad <- 0.2
  ypad <- 0.4

  if(xann){
    if(xside == 1){
      title(xlab = xlab, line = max(c(xoff-par("tcl")+xpad, xoff)+xpad), xpd = NA)
    }else{
      mtext(xlab, side = 3, line = max(c(xoff-par("tcl")+xpad, xoff+xpad)), xpd = NA)
    }
  }
  if(yann){
    if(yside == 2){
      title(ylab = ylab, line = max(yoff-par("tcl")+ypad, yoff+ypad), xpd = NA)
    }else{
      # this is a bit more complicated...
      line <- max(yoff-par("tcl")+ypad, yoff+ypad)
      if(all(par("mar")[1] > 0))
        in2lines <- (par("mar")/par("mai"))[1]
      else
        in2lines <- (par("oma")/par("omi"))[1]
      lin <- line/in2lines
      in2user <- diff(par("usr")[1:2])/par("pin")[1] # /in
      luser <- lin*in2user
      if(par("ylog"))
        yy <- 10^mean(par("usr")[3:4])
      else
        yy <- mean(par("usr")[3:4])
      if(par("xlog"))
        xx <- 10^(par("usr")[2] + luser)
      else
        xx <- par("usr")[2] + luser
      text(xx, yy, ylab, srt = 270, xpd = NA)
    }
  }
}


#####################################################
ltics <- function(xann = TRUE, yann = TRUE, xtck = TRUE, ytck = TRUE,
                  xside = 1, yside = 2, sci = FALSE){

  #######################################
  # Description
  # -----------
  # Function for handling log tick marks.
  #
  # Arguments
  # ---------
  # xann  =   label x-axis tick marks
  # yann  =   lable y-axis tick marks
  # xside =   side for x-axis labels (default is 1, can be 3)
  # yside =   side for y-axis labels (default is 2, can be 4)
  # sci   =   use 10^x format to label tick marks
  #
  # Changes
  # -------
  # ??/??/2010 Created (EMT)
  # 11/21/2013 Modified to draw the non-loged ticks as well
  #            and place labels closer to plot, accounting for
  #            the value of 'tcl' (see 'par').
  # 11/22/2013 o Removed the 'what' argument since that information
  #              can be detected from par(xlog, ylog)
  #            o Returns yoffset to aid in placing y labels for
  #              when las = 1.
  # 11/26/2013 o Added calculation of xoffset
  #            o Fixed tick labels to align properly with all
  #              values of 'las'
  # 12/04/2013 Added xtck and ytck.
  #######################################

  # check that xside/yside make sense:
  if(xside != 1 & xside != 3)
    stop("xside must be 1 or 3. \n")
  if(yside != 2 & yside != 4)
    stop("yside must be 2 or 4. \n")

  if(all(par("mar")[1] > 0))
    in2lines <- (par("mar")/par("mai"))[1]
  else{
    if(all(par("oma")[1] > 0))
      in2lines <- (par("oma")/par("omi"))[1]
    else
      stop("first element of 'mar' or 'oma' must be nonzero.\n")
  }

  dec <- 1:10
  box()
  xpd <- FALSE
  if(par("las") == 0 |  par("las") == 1){
    # currently, doesn't work for arbitrary cex.axis values
    # works best with cex.axis = 1
    # this is an attempt to find a fix:
    axfac <- (par("cex")-par("cex.axis"))*
      strheight("0", units = "inches", cex = par("cex.axis"))*in2lines
    axfac <- 0 # but doesn't work
    xline <- max(c(-1 - axfac - par("tcl"), -1 - axfac) )
  }else
    xline <- max(c(-0.8 - par("tcl"), -0.8))
  if(par("las") == 1 |  par("las") == 2)
    yline <- max(c(-0.8 - par("tcl"), -0.8))
  else{
    yline <- max(c(-1 - par("tcl"), -1 ))
  }
  if(par("xlog")){
    xlim <- 10^par("usr")[1:2]
    lxmax <- ceiling(log10(max(xlim)))
    lxmin <- floor(log10(min(xlim)))
    g <- as.integer(gl(lxmax-lxmin+1 , 10))
    lxtic <- (10^(lxmin:lxmax)[g]) * dec
    Lxtic <- 10^(lxmin:lxmax)
    if(xtck){
      axis(side = 1, at = lxtic, lab = FALSE, tcl = par("tcl")*0.5)
      axis(side = 1, at = Lxtic, lab = FALSE)
    }
    if(xann){
      if(sci){
        # simplification: assume str width of 101 is close enough for sci
        # but maybe add a fudge factor later for superscript.
        xa <- rep(101, length(Lxtic))
        for(i in 1:length(Lxtic)){        ###      Fudge factor: vvv
          axis(side = xside, at = Lxtic[i], tick = FALSE, line = 0.8*xline,
               lab = substitute(10^a, list(a=log10(Lxtic[i]) )),
               xpd = xpd )
        }
      }else{
        xa <- axis(side = xside, at = Lxtic, lab = Lxtic, tick = FALSE,
                   line = xline, xpd = xpd)
      }
    }
    if(xtck){
      axis(side = 3, at = lxtic, lab = FALSE, tcl = par("tcl")*0.5)
      axis(side = 3, at = Lxtic, lab = FALSE)
    }
  }else{
    if(xtck){
      axis(side = 1, lab = FALSE)
      axis(side = 3, lab = FALSE)
    }
    if(xann){
      xa <- axis(side = xside, tick = FALSE, line = xline, xpd = xpd)
    }
  }
  if(par("ylog")){
    ylim <- 10^par("usr")[3:4]
    lymax <- ceiling(log10(max(ylim)))
    lymin <- floor(log10(min(ylim)))
    g <- as.integer(gl(lymax-lymin+1 , 10))
    lytic <- (10^(lymin:lymax)[g]) * dec
    Lytic <- 10^(lymin:lymax)
    Lytic <- Lytic[Lytic >= 10^par("usr")[3] &
                   Lytic <= 10^par("usr")[4] ]
    if(ytck){
      axis(side = 2, at = lytic, lab = FALSE, tcl = par("tcl")*0.5)
      axis(side = 2, at = Lytic, lab = FALSE)
    }
    if(yann){
      if(sci){
        # simplification: assume str width of 101 is close enough for sci
        ya <- rep(101, length(Lytic))
        for(i in 1:length(Lytic)){
          axis(side = yside, at = Lytic[i], tick = FALSE,
               line = yline,
               lab = substitute(10^a, list(a=log10(Lytic[i]) )),
               xpd = xpd)
        }
      }else{
        ya <- axis(side = yside, at = Lytic, lab = Lytic,
                   tick = FALSE, line = yline, xpd = xpd)
      }
    }
    if(ytck){
      axis(side = 4, at = lytic, lab = FALSE, tcl = par("tcl")*0.5)
      axis(side = 4, at = Lytic, lab = FALSE)
    }
  }else{
    if(ytck){
      axis(side = 2, lab = FALSE)
      axis(side = 4, lab = FALSE)
    }
    if(yann){
      ya <- axis(side = yside, tick = FALSE, line = yline, xpd = xpd)
    }
  }
  if(xann){
    if(par("las") == 0 | par("las") == 1){
      # fugdge again
      xoffset <- 1.2*max(strheight(xa, units = "inches", cex = par("cex.axis")))
    }else
      xoffset <- max(strwidth(xa, units = "inches", cex = par("cex.axis")))
  }else
    xoffset <- 0
  if(yann){
    if(par("las") == 1 | par("las") == 2)
      yoffset <- max(strwidth(ya, units = "inches", cex = par("cex.axis")))
    else
      yoffset <- max(strheight(ya, units = "inches", cex = par("cex.axis")))
  }else
    yoffset <- 0
  xoffset <- xoffset*in2lines
  yoffset <- yoffset*in2lines
  invisible(list(xoffset = xoffset, yoffset = yoffset))
}






splitDepByAlluvAge <- function(vspr.df) {
  depAndAge <- as.character(interaction(vspr.df$dep, vspr.df$age1))
  vspr.df$depAgeSplit <- as.character(vspr.df$dep)
  alluvs <- (vspr.df$dep=="Alluvial") & !(is.nan(vspr.df$dep))
  vspr.df$depAgeSplit[alluvs] <- depAndAge[alluvs]
  vspr.df$depAgeSplit <- as.factor(vspr.df$depAgeSplit)
  return(vspr.df)
}







#####################################################
slope.srtm30vec.old <- function(lon, lat ){
  #### OLD FUNCTION #################################
  #### NO LONGER USED
  ###################################################
  ###################################################
  ###################################################
  ###################################################
  ###################################################
  
  # This relies on an input grid file containing the slope data.
  # KF used a shell script and the gdal_translate tool to convert geoTiffs to GMT (netCDF) format.
  
  # This function can probably be implemented using over() without resorting to passing shell commands.
  # Leaving that alone for now.
  
  setwd("~/VsMap/")
  
  xyz <- cbind(lon, lat, 1:length(lon))
  write.table(xyz, file = "tmp.xyz", quote = FALSE, row.names = FALSE,
              col.names = FALSE)
  if(Sys.info()["nodename"][[1]] == "hypocentre") {
    grdtrackPath <- "/opt/gmt-5.1.2/bin/grdtrack"
  } else {
    if(Sys.info()["nodename"][[1]] == "neurofunk") {
      grdtrackPath <- "/usr/local/src/gmt/bin/grdtrack"
    }}
  system2(grdtrackPath, args=c("tmp.xyz",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/ni_0001-0001.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/ni_0002-0002.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/ni_0003-0005.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/ni_0004-0003.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/ni_0005-0004.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/ni_0006-0006.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/ni_0007-0005.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/ni_0008-0004.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/si_0001-0005.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/si_0002-0004.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/si_0003-0003.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/si_0004-0005.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/si_0005-0004.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/si_0006-0002.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/si_0007-0003.nc", #
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/si_0008-0001.nc",
                               "-G/home/kmf76/big_noDB/topo/slp_WGS84/si_0009-0002.nc",
                               "-nb",
                               "-N"),
          stdout = "tmp2.xyz")
  out <- scan("tmp2.xyz", what = list(x = 0, y = 0, id = 0,
                                      slp_ni_0001_0001=0,
                                      slp_ni_0002_0002=0,
                                      slp_ni_0003_0005=0,
                                      slp_ni_0004_0003=0,
                                      slp_ni_0005_0004=0,
                                      slp_ni_0006_0006=0,
                                      slp_ni_0007_0005=0,
                                      slp_ni_0008_0004=0,
                                      slp_si_0001_0005=0,
                                      slp_si_0002_0004=0,
                                      slp_si_0003_0003=0,
                                      slp_si_0004_0005=0,
                                      slp_si_0005_0004=0,
                                      slp_si_0006_0002=0,
                                      slp_si_0007_0003=0,
                                      slp_si_0008_0001=0,
                                      slp_si_0009_0002=0))
  allSlopesOut <- cbind(out$slp_ni_0001_0001,  # combine all to form a matrix.
                        out$slp_ni_0002_0002,
                        out$slp_ni_0003_0005,
                        out$slp_ni_0004_0003,
                        out$slp_ni_0005_0004,
                        out$slp_ni_0006_0006,
                        out$slp_ni_0007_0005,
                        out$slp_ni_0008_0004,
                        out$slp_si_0001_0005,
                        out$slp_si_0002_0004,
                        out$slp_si_0003_0003,
                        out$slp_si_0004_0005,
                        out$slp_si_0005_0004,
                        out$slp_si_0006_0002,
                        out$slp_si_0007_0003,
                        out$slp_si_0008_0001,
                        out$slp_si_0009_0002)
  allSlopesOut[allSlopesOut==-9999] <- NaN   # Some values end up being -9999, presumably black... image boundaries etc.
  slopeVals <- c()
  for (i in 1:length(lat)) {   
    if (length(which(!is.na(allSlopesOut[i,]))) > 1) {
      #print("WARNING!")  # Just to be really sure... don't want to find out there is "tile overlap" (i.e. more than one slope value returned per coordinate.)
      print(i)
    }
  }
  slopeVals <- rowSums(allSlopesOut, na.rm = TRUE)
  for (i in 1:length(lat)) {   
    if (all(is.na(allSlopesOut[i,]))) {
      slopeVals[i] <- NaN
    }
  }
  system2("rm", args = c("tmp.xyz", "tmp2.xyz"))
  # GDALDEM, the utility used to generate slope files, gives values in PERCENT
  # need to divide by 100 for use within codes.
  return(slopeVals/100)
}


# Thanks to https://gist.github.com/sheymann/2399659
ExportPlot <- function(plot, filename, width=2, height=1.5) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), plot, width = width, height = height)
  # postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  # print(plot)
  # dev.off()
  # png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
  # print(plot)
  # dev.off()
}
