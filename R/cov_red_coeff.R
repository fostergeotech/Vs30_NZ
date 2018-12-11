# covariance reduction factor
# 
# Choosing a coefficient a, 0 <= a <= 1, to use for reducing entries
# in the covariance matrix for dissimilar geologic points.
# 
# 

rm(list=ls())
setwd("~/VsMap/")

library(ggplot2)

rat <- 0.01*(100:1000)
avals <- c(0,1,1.25,1.5,1.75,2,2.5,3)
avals <- c(1,1.2,1.5,2,3)
dat <- data.frame(Vs30_ratio = rep(rat,length(avals)))
dat$a <- rep(avals,each = length(rat))
dat$covRedFac <- exp(-dat$a*abs(log(dat$Vs30_ratio)))
dat$a <- as.factor(dat$a)

p <- ggplot(data = dat, aes(x = Vs30_ratio,y=covRedFac, linetype=a)) + 
      geom_line() + 
      scale_y_continuous(breaks=c(0,0.5,1),minor_breaks = c(seq(0.1,0.4,0.1),seq(0.6,0.9,0.1)),limits = c(0,1),
                       name= "CRF") +
      scale_x_continuous(breaks = 1:10,minor_breaks = c(),limits = c(1,10),name=expression("V"[s30[1]] / "V"[s30[2]])) +
      theme_minimal() + theme(legend.title = element_text(),
                              legend.position = c(0.57,0.73),
                              legend.direction = "horizontal") +
      scale_linetype_manual(name="a = ",
                            values=c("twodash",
                                     "longdash",
                                     "solid",
                                     "dotted",
                                     "dotdash"#,
                                     #"longdash"
                        ))
  

ggsave("out/plots/CRP.pdf",plot = p, width = 4.0,height = 1.7)
