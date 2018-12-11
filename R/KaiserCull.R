# KaiserCull.R
#
# One-time script for finding duplicate Vs30 values in Kaiser et al.
#
# The pairs of duplicates identified in the dataframe "tooCloseDFsorted"
# are MANUALLY removed by commenting out in the input file
# ~/VsMap/in/Vs/201703_allNZ/20170817_vs_allNZ_duplicatesCulled.ll
#
# NB the rejection criterion for "too close" is 2 (TWO) metres.
# This was done solely to facilitate well-behaved kriging behaviour
# in the first model run. (i.e. too close data --> bad covariance matrix).
#
# Next model version could arguably incorporate a larger rejection distance
# which would potentially speed up MVN application. (At moment, Christchurch
# takes far longer to compute in parallel than other map regions, presumably
# because of data density).
# 
# 

setwd("~/VsMap/")
source("R/loadVs.R")

vsprCheck <- loadKaiserEtAlVs("in/Vs/201703_allNZ/20170329_vs_allNZ.ll")
  


# create distance matrix with no duplicate symmetric entries OR diagonal entries.
nPoints <- length(vsprCheck)
distMat <- as.matrix(dist(x = coordinates(vsprCheck), 
                          method = "euclidean",
                          diag = T, upper = F)) # for FINDING which points are members of the offending pairs.
distMat[upper.tri(distMat,diag = T)] <- NA



# see how number of nearby points increases for a given threshold distance...
checkDists <- exp(seq(log(0.1), log(100), 0.1))
Ndists <- rep(0,length(checkDists))
for (i in 1:length(checkDists)) {
  Ndists[i] <- length(which(distMat<checkDists[i]))
}
png("out/plots/vspr_points_too_close.png")
plot(checkDists, Ndists, main="Finding likely duplicate Vs30 point pairs", 
     xlab="Distance, m", ylab = c("Unique point pairs","at/below this distance"),
     type='l') 
dev.off()


cutoffDistance <- 2 # m


# two ways of identifying pairs of "too-close" points...
whichPtsInd <- which(distMat <= cutoffDistance)
whichPtsCoord <- which(distMat <=cutoffDistance,arr.ind = T)

if (!identical(distMat[whichPtsCoord], distMat[whichPtsInd])) {
  stop("Something's wrong")
} else {
  whichPtsDist <- as.vector(distMat[whichPtsCoord])
}

whichPtsrow  <- as.vector(whichPtsCoord[,"row"])
whichPtscol  <- as.vector(whichPtsCoord[,"col"])

whichPtsVs30_row <- vsprCheck$Vs30[whichPtsrow]
whichPtsVs30_col <- vsprCheck$Vs30[whichPtscol]
whichPtsQ_row <- vsprCheck$QualityFlag[whichPtsrow]
whichPtsQ_col <- vsprCheck$QualityFlag[whichPtscol]
whichPtsLabel_row <- as.character(vsprCheck$StationID[whichPtsrow])
whichPtsLabel_col <- as.character(vsprCheck$StationID[whichPtscol])



tooCloseInd        <- c(whichPtsrow,      whichPtscol)
tooCloseOtherInd   <- c(whichPtscol,      whichPtsrow)
tooCloseID         <- c(whichPtsLabel_row, whichPtsLabel_col)
tooCloseOtherID    <- c(whichPtsLabel_col, whichPtsLabel_row)
tooCloseDist       <- c(whichPtsDist,     whichPtsDist)
tooCloseThisVs30   <- c(whichPtsVs30_row, whichPtsVs30_col)
tooCloseOtherVs30  <- c(whichPtsVs30_col, whichPtsVs30_row)
tooCloseThisQ      <- c(whichPtsQ_row,    whichPtsQ_col)
tooCloseOtherQ     <- c(whichPtsQ_col,    whichPtsQ_row)


tooCloseDF <- data.frame(Ind  = tooCloseInd,
                         otherInd = tooCloseOtherInd,
                         ID = tooCloseID,
                         otherID = tooCloseOtherID,
                         dist = tooCloseDist,
                         Vs30 = tooCloseThisVs30,
                         otherVs30 = tooCloseOtherVs30,
                         logVsDif  = log(tooCloseOtherVs30/tooCloseThisVs30),
                         Q = tooCloseThisQ,
                         otherQ = tooCloseOtherQ)

tooCloseInd_sorted_df <- data.frame(count=sort(table(tooCloseInd), decreasing=TRUE)) # sort indices by number of occurrences


# just in case
if(!(dim(tooCloseInd_sorted_df)[1] == numPoints)) {
  stop("Something wrong with too-close Vs30 point rejection procedure...")
}
tooCloseDFsorted <- na.omit(tooCloseDF[order(
  tooCloseDF[,c("Ind","dist","logVsDif")]
),])


# FIRST, examine the variable created above, tooCloseInd_sorted_df,
# and identify points that are members of more than one "too-close" pair.
# BEGIN the culling with these ones.
tooCloseInd_sorted_df

# Use the info in the dataframe below, tooCloseDFsorted, to decide whether points
# should be removed or not.

tooCloseDFsorted

