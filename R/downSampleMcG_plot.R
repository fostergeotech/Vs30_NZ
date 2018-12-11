# downSampleMcG_plot.R

# Plot the results of downSampleMcG.R for publications
# Note: committed before shutdown 20171206


library(ggplot2)
library(ggthemes)
library(rasterVis) # use to plot raster of land/water underneath data

source("R/loadVs.R") # need this to load McGann points
source("R/downSampleMcG.R") 
source("R/geodetic.R")

McGannVsPts_NZGD49       <- loadMcGannVs()
inputSPDF = McGannVsPts_NZGD49
outp <- downSampleMcG(inputSPDF)


# Sanity check - plot
checkDF_in  <- data.frame(Easting    = coordinates(convert2NZGD00(inputSPDF))[,1],
                          Northing   = coordinates(convert2NZGD00(inputSPDF))[,2])
checkDF_out <- data.frame(Easting    = coordinates(convert2NZGD00(outp))[,1],
                          Northing   = coordinates(convert2NZGD00(outp))[,2])

# save these to use in MAP_allElev_justForVs30subsets.R
# (for publication plots)
McGannAll <- checkDF_in
McGannDownsampled <- checkDF_out
save(McGannAll, McGannDownsampled, file = "/home/kmf76/VsMap/Rdata/McGannDownsample.Rdata")


facx <- 0.4
facy <- 0.1
xmn <- min(checkDF_in$Easting)
xmx <- max(checkDF_in$Easting)
xw <- xmx-xmn
xmx <- xmx + facx*xw
xmn <- xmn - facx*xw
ymn <- min(checkDF_in$Northing)
ymx <- max(checkDF_in$Northing)
yw <- ymx-ymn
ymx <- ymx + facy*yw
ymn <- ymn - facy*yw

# Because I can't figure out how to trim Hlines and Vlines to fit the raster,
# it's easier to just do this manually:
ymn <- 5160000
ymx <- 5210000
xmn <- 1550000
xmx <- 1590000
crp <- extent(x = c(xmn,xmx,ymn,ymx))


# get the land/water info
isWater <- raster("~/big_noDB/models/isWater_NZGD00.tif")
isOcean <- raster("~/big_noDB/models/isOcean_NZGD00.tif")
isH2O1   <- isWater | isOcean
isH2O2   <- crop(isH2O1, crp)
isLand <- is.na(isH2O2)


# NOTE - use following to put in background tiff
# https://stackoverflow.com/questions/33359284/r-plot-background-map-from-geotiff-with-ggplot2
gplot(isLand, maxpixels = 5e5) +
  geom_raster(aes(fill = value))  +
# ggplot() + 
  # geom_line(data = )
  # scale_color_discrete() +
  # scale_fill_gradient(low="#012345", high = "#ffcc99") +
  # scale_fill_manual(values = c(1 = "red", 0 = "green")) +
  scale_fill_solarized(accent = ) +
  # scale_fill_manual(values = c("1" = "blue", "0" = "purple")) +
  geom_point(aes(Easting,Northing), data=checkDF_in,  color="gray",  size=1) +
  geom_point(aes(Easting,Northing), data=checkDF_out, color="black", size=1) +
  #### PRETTY ####    geom_vline(xintercept=seq(1550000, 1590000, by=10000), size=0.1) +
  #### PRETTY ####    geom_hline(yintercept=seq(5160000, 5210000, by=10000), size=0.1) +
  coord_equal() +
  #### PRETTY ####    xlab("NZGD00 Easting (metres)") +
  #### PRETTY ####    ylab("NZGD00 Northing (metres)") +
  theme(#axis.title.x=element_blank() ,
        # axis.text.x =element_blank() ,
        axis.ticks.x=element_blank() ,
        #axis.title.y=element_blank() ,
        # axis.text.y =element_blank() ,
        axis.ticks.y=element_blank() ,
        panel.grid.major = element_line(), #element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none"
  ) #### PRETTY ####    +
  #### PRETTY ####    ggtitle(sprintf(
  #### PRETTY ####      paste0("McGann points within %3.0f m of 1kmx1km grid points \n",
  #### PRETTY ####             "(n_old=%d, n_new=%d)"), 
  #### PRETTY ####      tooFar, dim(checkDF_in)[1], dim(checkDF_out)[1]))
ggsave(filename = paste0("out/maps/McGannGridPlot", round(tooFar), ".png"),
       width = 8, height=8)

