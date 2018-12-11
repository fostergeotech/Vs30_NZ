# makeSlopeTable.R
#
# Output LaTeX for slope adjustment table.
# 

rm(list=ls())
setwd("~/VsMap")

library(plyr)

load("Rdata/slopeTable.Rdata")

slopeTable$IDsimple <- sapply(slopeTable$groupID, FUN = function(i){paste0("G",substr(i,start = 0,stop = 2))})

x <- c(
  "<!--- This table generated automatically by R/makeSlopeTable.R --->",
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Slope adjustment details for categories G05, G06, G07, G09.} \\label{tab:slopeAdj}",
  "\\begin{tabular}{cccccc}",
  "\\toprule",
  "   ID  & $\\nabla_0$ & $\\nabla_1$ & $V_{s30,0}$ & $V_{s30,1}$ & $\\sigma_{f(\\nabla)}$ \\\\",
  "\\midrule"
)

for(i in 1:nrow(slopeTable)) {

  newStr <- sprintf(" %s & % 5.4f & % 5.4f  & % 4.0f  & % 4.0f & %03.2f \\\\",
                    slopeTable$IDsimple[i],
                    10^slopeTable$logSlopeMin[i],
                    10^slopeTable$logSlopeMax[i],
                    slopeTable$Vs30min[i],
                    slopeTable$Vs30max[i],
                    slopeTable$sigma_fSlope[i]
  )
  x <- c(x,newStr)   
}

x <- c(x,
       "\\bottomrule",
       "\\end{tabular}",
       "\\end{table}"
       )

outFile <- file("ch/slopeTable.markdown")
writeLines(x,outFile)
close(outFile)
