# MAP_NZGD00_regions.R
#
# extents defined for various map subsets.
#

# library(GISTools)
library(sp)

extList <- list(
  CH     = extent(1512000, 1614400, 5109600, 5212000),
  NI     = extent(1560000, 2132000, 5376000, 6224000),
  SI     = extent(1060000, 1720000, 4716000, 5572000),
  AKL    = extent(1644000, 1918000, 5820000, 6040000),
  WEL    = extent(1660000, 1834000, 5376000, 5542000),
  NEL    = extent(1496000, 1664000, 5376000, 5546000),
  NELURB = extent(1600000, 1644000, 5404000, 5462000),
  NZ     = extent(1020000, 2260000, 4730000, 6220000),
  NAP    = extent(1905000, 1955000, 5585000, 5635000), # Napier
  WHA    = extent(1920000, 2000000, 5750000, 5830000), # Whakatane
  BLN    = extent(1610000, 1710000, 5325000, 5425000), # Blenheim incl. Sedon, Ward & valleys
  WPT    = extent(1465000, 1520000, 5350000, 5395000), # Westport incl. Inangahua
  GM     = extent(1450000, 1500000, 5275000, 5325000)  # Greymouth
)
graphicScaleLocLengths <- list(  # x       # y      # km
  CH     =          c(            1600000,  5210000, 20   ),
  NI     =          c(            2000000,  6180000, 200  ),
  SI     =          c(            1200000,  5510000, 200  ),
  AKL    =          c(            1890000,  6035000, 40   ),
  WEL    =          c(            1683000,  5540000, 40   ),
  NEL    =          c(            1525000,  5530000, 40   ),
  NELURB =          c(            1637000,  5460000, 10   ),
  NZ     =          c(            2000000,  4800000, 200  ),
  # 1905000, 1955000, 5585000, 5635000
  NAP    =          c(1947500, 5632500, 10), # Napier
  # 1920000, 2000000, 5750000, 5830000
  WHA    =          c(1980000, 5820000, 20), # Whakatane
  # 1610000, 1710000, 5325000, 5425000
  BLN    =          c(1695000, 5335000, 20), # Blenheim incl. Sedon, Ward & valleys
  # 1465000, 1520000, 5350000, 5395000
  WPT    =          c(1480000, 5390000, 10), # Westport incl. Inangahua
  # 1450000, 1500000, 5275000, 5325000
  GM     =          c(1457500, 5322500, 10)  # Greymouth
)
NorthArrowLocs <- list(
  CH     =          c(            1600000,  5200000),
  NI     =          c(            2000000,  6050000),
  SI     =          c(            1030000,  5480000),
  AKL    =          c(            1890000,  6020000),
  WEL    =          c(            1655000,  5530000),
  NEL    =          c(            1495000,  5520000),
  NELURB =          c(            1637000,  5457000),
  NZ     =          c(            2000000,  5000000),
  NAP    =          c(1945000, 5627500, 10), # Napier
  WHA    =          c(1920000, 5825000, 20), # Whakatane
  BLN    =          c(1695000, 5420000, 20), # Blenheim incl. Sedon, Ward & valleys
  WPT    =          c(1465000, 5390000, 10), # Westport incl. Inangahua
  GM     =          c(1450000, 5320000, 10)  # Greymouth
)


drawScale <- function(region) {
  scaleXY     <- graphicScaleLocLengths[[region]][1:2]
  scaleLength <- graphicScaleLocLengths[[region]][3]
  scaleX <- scaleY <- c(0,0)
  scaleX[1] <- scaleXY[1] - (scaleLength*500) # half of scale length, in km
  scaleX[2] <- scaleXY[1] + (scaleLength*500) # ditto
  scaleY <- rep(scaleXY[2],2)
  
  # ticks at end of scale
  tickLX <- rep(scaleX[1],2)
  tickRX <- rep(scaleX[2],2)
  c <- 50
  tickY <- c(scaleY[1]+scaleLength*c, scaleY[1]-scaleLength*c)
  
  lines(scaleX,scaleY, lwd=4)
  lines(tickLX,tickY,lwd=2)
  lines(tickRX,tickY,lwd=2)
  text(x=scaleXY[1], y=scaleXY[2], 
       labels=trimws(sprintf("% 3.0fkm",scaleLength)),
       pos=1)
}

northarrow <- function(region) {
  northXY     <- NorthArrowLocs[[region]]
  scaleLength <- graphicScaleLocLengths[[region]][3]
  northX <- northXY[1]
  northY <- northXY[2]
  
  cy <- 400*scaleLength
  cx <- 0.7*cy
  d <- 400*scaleLength
  e <- -500*scaleLength
  arrowX <- c( 0, 0, -0.5, 0, 0.5)*cx + northX + d
  arrowY <- c(-1, 1,  0.5, 1, 0.5)*cy + northY + e
  NX     <- c(-0.25, -0.25,  0.25, 0.25)*cx + northX + d
  NY     <- c(-0.25,  0.25, -0.25, 0.25)*cy + northY + e
  lines(arrowX, arrowY, lwd=3)
  lines(NX, NY, lwd=2)
}

