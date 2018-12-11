# makeCatTables.R
#
# Output LaTeX for geology and terrain category tables.
# 

rm(list=ls())
setwd("~/VsMap")

library(plyr)

# GEOLOGY

source("R/MODEL_AhdiAK.R")
source("R/MODEL_AhdiAK_noQ3.R")
source("R/functions.R")
load("Rdata/vspr.Rdata")

nq <- subsetVsPoints(vspr)$noQ3

IDpri <- AhdiAK_lookup()
IDpos <- AhdiAK_noQ3_lookup()
IDgeo <- plyr::join(IDpri,IDpos,by="groupID")
IDgeo$IDsimple <- sapply(IDgeo$groupID, FUN = function(i){paste0("G",substr(i,start = 0,stop = 2))})
IDgeo$IDstr <- sapply(IDgeo$groupID, FUN = function(i){substr(i,start=4,stop=999)})
IDgeo$IDstrLong <- c( # taken directly from Ahdi's PBDIII paper
  "peat", #01
  "artificial fill", # 04
  "fluvial \\& estuarine deposits", #05
  "alluvium \\& valley sediments", #06
  "lacustrine (incl. glaciolacustrine)", #08
  "beach, bar, dune deposits", #09
  "fan deposits", #10
  "loess", #11
  "glacigenic sediments (drift \\& outwash)", #12
  "flood deposits", # 13
  "glacial moraines \\& till", #14
  "undifferentiated sediments \\& sedimentary rocks", #15
  "terrace deposits \\& old alluvium", #16
  "volcanic rocks \\& deposits", #17
  "crystalline rocks (igneous \\& metamorphic)" #18
)

geoTable <- c(
  "<!--- This table generated automatically by R/makeCatTables.R --->",
  "\\begin{table}[htbp]",
  "\\caption{Geology categories with $n$=number of observations per category, and prior and posterior (slope-adjusted) $V_{s30}$ and $\\sigma$ values for each.} \\label{tab:geology}",
  "{\\centering",
  "\\begin{tabular}{clccccc}",
  "\\toprule",
  "       &               &     &   \\multicolumn{2}{c}{$V_{s30}$ (m/s)} & \\multicolumn{2}{c}{$\\sigma$} \\\\",
  "\\cmidrule(lr){4-7}",
  "   ID  &  Description  & $n$ &         (pri.)     &       (post.)     &    (pri.)     &        (post.) \\\\",
  "\\midrule"
)

for(i in 1:nrow(IDgeo)) {
  newStr <- sprintf(" %s & %-50s & % 4.0f & % 4.0f & % 4.0f & %03.2f & %03.2f \\\\",
                    IDgeo$IDsimple[i],
                    IDgeo$IDstrLong[i],
                    length(which(as.character(nq$groupID_AhdiAK)==as.character(IDgeo$groupID[i]))), # n
                    IDgeo$Vs30_AhdiAK[i], # prior
                    IDgeo$Vs30_AhdiAK_noQ3[i], # posterior
                    IDgeo$stDv_AhdiAK[i], # prior
                    IDgeo$stDv_AhdiAK_noQ3[i] # posterior
  )
  geoTable <- c(geoTable,newStr)
}

geoTable <- c(geoTable,
       "\\bottomrule",
       "\\end{tabular}",
       "\\par }",
       "\\end{table}"
       )

outFile <- file("ch/geologyTable.markdown")
writeLines(geoTable,outFile)
close(outFile)












# TERRAIN

source("R/MODEL_YongCA.R")
source("R/MODEL_YongCA_noQ3.R")

IDpri <- YongCA_lookup()
IDpos <- YongCA_noQ3_lookup()
IDgeo <- plyr::join(IDpri,IDpos,by="groupID")
IDgeo$IDsimple <- sapply(IDgeo$groupID, FUN = function(i){paste0("T",substr(i,start = 0,stop = 2))})
IDgeo$IDstr <- sapply(IDgeo$groupID, FUN = function(i){substr(i,start=6,stop=999)})
IDgeo$IDstrLong <- IDgeo$IDstr

terTable <- c(
  "<!--- This table generated automatically by R/makeCatTables.R --->",
  "\\begin{table}[htbp]",
  "\\caption{Terrain categories with $n$=number of observations per category, and prior and posterior (slope-adjusted) $V_{s30}$ and $\\sigma$ values for each.} \\label{tab:terrain}",
  "{\\centering",
  "\\begin{tabular}{clccccc}",
  "\\toprule",
  "       &               &     &   \\multicolumn{2}{c}{$V_{s30}$ (m/s)} & \\multicolumn{2}{c}{$\\sigma$} \\\\",
  "\\cmidrule(lr){4-7}",
  "   ID  &  Description  & $n$ &         (pri.)     &       (post.)     &    (pri.)     &        (post.) \\\\",
  "\\midrule"
)

for(i in 1:nrow(IDgeo)) {
  newStr <- sprintf(" %s & %-50s & % 4.0f & % 4.0f & % 4.0f & %03.2f & %03.2f \\\\",
                    IDgeo$IDsimple[i],
                    IDgeo$IDstrLong[i],
                    length(which(as.character(nq$groupID_YongCA)==as.character(IDgeo$groupID[i]))), # n
                    IDgeo$Vs30_YongCA[i], # prior
                    IDgeo$Vs30_YongCA_noQ3[i], # posterior
                    IDgeo$stDv_YongCA[i], # prior
                    IDgeo$stDv_YongCA_noQ3[i] # posterior
  )
  terTable <- c(terTable,newStr)
}

terTable <- c(terTable,
       "\\bottomrule",
       "\\end{tabular}",
       "\\par }",
       "\\end{table}"
)

outFile <- file("ch/terrainTable.markdown")
writeLines(terTable,outFile)
close(outFile)




# Combine above tables into a single float environment:
outFile <- file("ch/geoAndTerTables.markdown")
geoAndTerTables <- c(
  geoTable[1:(length(geoTable)-1)],
  terTable[3:length(terTable)]
  )
writeLines(geoAndTerTables,outFile)
close(outFile)


