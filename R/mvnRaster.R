#mvnRaster.R

# Where appropriate, equations are referenced by number in the draft PDF of Worden et al.
# PDF title: worden-al-2017--20170719.pdf
# saved in this directory for future reference.
#
# shorthand: "Wea" = Worden et al.

source("R/mvn.R")




mvnRaster <- function(inpIndexRaster, inpModelRaster, inpStdDevRaster,
                      vspr, variogram, MODEL, covReducPar) {
  
  # interpolates residuals and variance geographically for a raster input.
  # Mostly the same as krigeRaster().
  #
  # In addition, use flag krige_or_MVN to compare results of original (kriging-based)
  # and new (multivariate normal, i.e. Wea paper) methods.
  
  # KRIGING OR MVN APPROACH
  krige_or_MVN <- "krige"
  krige_or_MVN <- "MVN"
  
  lnObsPred <- stdDev <- inpModelRaster
  
  if (!(all(as.vector(is.na(inpModelRaster))))) { # catches the case where entire input raster is NAs.
    iALL_na_idx <- Which(!is.na(inpIndexRaster), cells=TRUE)
    # Because inpIndexRaster actually does not contain all NA cells identical to the model
    # rasters, some NA values can get into the MVN inputs and cause annoying problems.
    # The following lines, although extra cautious, ensure that ONLY cells with defined values
    # in ALL THREE input rasters will be used for prediction.....
    iALL_na_mod <- Which(!is.na(inpModelRaster), cells=TRUE)
    iALL_na_sdv <- Which(!is.na(inpStdDevRaster), cells=TRUE)
    iALL_na <- intersect(iALL_na_idx, iALL_na_mod)
    iALL_na <- intersect(iALL_na, iALL_na_sdv)
    
    xy <- xyFromCell(inpModelRaster, iALL_na, spatial = TRUE)
    
    index_allNA <- inpIndexRaster[iALL_na]
    modelVar    <- inpStdDevRaster[iALL_na]^2
    
    if(MAKE_ALL_STDEV_ONEHALF) {modelVar    <- rep(0.5^2, length(modelVar))} # diagnostic: replace all input StdDev with 0.5.
                                

    locations <- vspr
    newdata   <- xy
    newdata_modelVar <- modelVar
    model <- variogram
    beta <- 0
    modeledValues <- log(inpModelRaster[iALL_na])  # data is transformed to log space here
    modelVarObs   <- (locations[[sprintf("stDv_%s",MODEL)]])^2   # vector of model variances for locations of observations.
    if(MAKE_ALL_STDEV_ONEHALF) {modelVarObs <- rep(0.5^2, nrow(locations))} # diagnostic: replace all input StdDev with 0.5.
    residuals     <- locations[[sprintf("res_%s",MODEL)]]
    logModVs30obs  <- log(locations[[sprintf("Vs30_%s",MODEL)]])
    
    mvnOutput <- mvn(locations, newdata, newdata_modelVar, model, beta, 
                     krige_or_MVN, modeledValues, MODEL, modelVarObs, residuals,
                     covReducPar, logModVs30obs)
    lnObsPred[iALL_na] <- mvnOutput$pred - modeledValues # still in log space here
    stdDev[iALL_na] <- sqrt(mvnOutput$var)
    print("#### mvnRaster ")
    print(sprintf("max(modeledValues): %s", max(modeledValues, na.rm = T)))
    print(sprintf("min(modeledValues): %s", min(modeledValues, na.rm = T)))
    print(sprintf("max(mvnOutputPred): %s", max(mvnOutput$pred, na.rm = T)))
    print(sprintf("min(mvnOutputPred): %s", min(mvnOutput$pred, na.rm = T)))
    
  }
  if(DIVIDE_RESID_BY_TWO){lnObsPred <- 0.5*lnObsPred} # DIAGNOSTIC - seems like this may fix problem!
  krigedHybOutput <- (c(lnObsPred = lnObsPred, stdDev = stdDev))
  return(krigedHybOutput)
}











