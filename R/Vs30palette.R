# Vs30palette.R

library(viridis)

# color scale stuff
zlims <- c(100,800)
constLims <- c(200,700) # per BB comments, make upper and lower margins constant color to increase contrast in middle range.
zscale <- seq(from=zlims[1],to=zlims[2],by = 100)
nModCol     <- 100
modelCol        = viridis(n = nModCol,  begin = 0, end = 1, direction= -1, option = "D")
# modelCol        = rainbow(nModCol, start=0, end=1)
# Make 100:200 and 700:800 same color.
nConstLow  <- constLims[1]-zlims[1]
nConstHigh <- zlims[2]-constLims[2]
nScale <- constLims[2]-constLims[1]
nColLow  <- round(nModCol * (nConstLow/nScale))
nColHigh <- round(nModCol * (nConstHigh/nScale))

modelCol <- c(rep(modelCol[1],nColLow), modelCol, rep(modelCol[length(modelCol)], nColHigh))
nModCol <- length(modelCol)
