# fitVariogram_weighting.R
# 
# A specialised function for "weighting" multiple variograms.
# This presumes that
#  (1) input models are exponential and normalised,
#  (2) ranges of input models do not differ substantially.
# 
# Otherwise results (might) not be desireable.
# 
# Samples input functions linearly from 0 to the longest theoretical range,
# and uses sample points to constrain a new fit with default weightings
# (i.e. "method 7" from fit.variogram() documentation, with weightings
# inversely proportional to square of distance.)
# 
# 
# 
# 


# Testing stuff....
# inpVarList <- list()
# varFileList <- c("Rdata/variogram_AhdiAK_noQ3_hyb09c_v6.Rdata",
#                  "Rdata/variogram_YongCA_noQ3_v7.Rdata")
# for(v in varFileList) {
#   load(v)
#   inpVarList[[length(inpVarList)+1]] <- variogram
# }

fitVariogram_weighting <- function(inpVarList,weightingList) {
  
  if(sum(weightingList) != 1) {stop("Weights must sum to 1.")}
  
  rangeList <- sapply(X = inpVarList, FUN = function(z){z[2,]$range})
  rm <- 10000*ceiling(max(rangeList/10000))
  r <- seq(from=1000,to=rm,by = 1000)
  av  <- matrix(nrow=length(r), ncol = length(inpVarList))
  # save resampled variogram values and distances
  for(v in 1:length(inpVarList)) {
    vg <- inpVarList[[v]]
    vl <- variogramLine(vg,dist_vector = r)
    av[,v] <- vl$gamma
  }
  
  w <- 1/(r^2) # weighting factors = 1 / dist^2
  wss <- c() # weighted sum of squared differences
  # tr = trial range
  for(tr in 1:rm) {
    # tv = trial variogram
    tv <- vgm(psill  = 1,
              model  = "Mat",
              range  = tr,
              nugget = 0,
              kappa  = 0.5)
    tvl <- variogramLine(tv,dist_vector = r)$gamma
    squares <- av # allocating. to be overwritten.
    for(i in 1:length(inpVarList)) {
      squares[,i] <- ((tvl-av[,i])^2)*w*weightingList[i]
    }
    wss[tr] <- sum(squares)
  }
  bestIdx <- which(wss==min(wss))
  bestRange <- bestIdx
  
  outpVar <- vgm(psill  = 1,
                 model  = "Mat",
                 range  = bestRange,
                 nugget = 0,
                 kappa  = 0.5)
  
}