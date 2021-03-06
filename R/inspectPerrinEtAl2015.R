# MODEL_PerrinEtAl2015.R
# 
# Assigns Vs30 ranges based on KEY_NAME content. This is based on the spreadsheet
# "Vs30_project.xlsx" from Perrin (20160515)

library(plyr)
library(ggplot2)
source("R/functions.R")
perrinEtAl_KEY_NAME_and_siteClass <- data.frame(
KEY_NAME = c("OIS1 (Holocene) Taupo Group rhyolite of Taupo Volcanic Centre",
                         "OIS1 (Holocene) White Island Formation andesite of Central Cone",
                         "OIS11 (Middle Pleistocene) Okataina Group rhyolite of Okataina Volcanic Centre",
                         "OIS12 (Middle Pleistocene) Okataina Group rhyolite of Okataina Volcanic Centre",
                         "OIS13 (Middle Pleistocene) rhyolite of Ohakuri-Kapenga-Rotorua Volcanic Centres",
                         "OIS14 (Early Pleistocene) Okataina Group rhyolite of Okataina Volcanic Centre",
                         "OIS2 (Late Pleistocene) Mangawhero Formation (Ruapehu Group) andesite of Tongariro Volcanic Centre",
                         "OIS2 (Late Pleistocene) Te Mari Andesite of Tongariro Volcanic Centre",
                         "OIS3 (Late Pleistocene) Maroa Group rhyolite of Maroa Volcanic Centre",
                         "OIS3 (Late Pleistocene) North Crater Andesite lava of Tongariro Volcanic Centre",
                         "OIS3 (Late Pleistocene) Taupo Group crystal rich rhyolite of Taupo Volcanic Centre",
                         "OIS3 (Late Pleistocene) Taupo Group rhyolite of Taupo Volcanic Centre",
                         "OIS3 (Late Pleistocene) Te Mari Andesite of Tongariro Volcanic Centre",
                         "OIS3-OIS2 (Late Pleistocene) Mangawhero Formation (Ruapehu Group) andesite of Tongariro Volcanic Centre",
                         "OIS5 (Late Paleocene) Taupo Group rhyolite of Ohakuri-Kapenga-Rotorua Volcanic Centres",
                         "OIS5 (Late Paleocene) Taupo Group rhyolite of Taupo Volcanic Centre",
                         "OIS5 (Late Pleistocene) Maroa Group rhyolite of Maroa Volcanic Centre",
                         "OIS5 (Late Pleistocene) Okataina Group rhyolite of Okataina Volcanic Centre",
                         "OIS6 (Middle Pleistocene) Maroa Group rhyolite of Maroa Volcanic Centre",
                         "OIS6 (Middle Pleistocene) rhyolite of Ohakuri-Kapenga-Rotorua Volcanic Centres",
                         "OIS7 (Middle Pleistocene) ignimbrite of Ohakuri-Kapenga-Rotorua Volcanic Centres",
                         "OIS7 (Middle Pleistocene) Maroa Group rhyolite of Maroa Volcanic Centre",
                         "OIS7 (Middle Pleistocene) Okataina Group rhyolite of Okataina Volcanic Centre",
                         "OIS9 (Middle Pleistocene) Okataina Group rhyolite of Okataina Volcanic Centre",
                         "Early Pleistocene Kaiwaka Formation (Mangaheia Group)",
                         "Early Pleistocene lahar deposits (Maungatautari Formation) of Taupo Volcanic Zone",
                         "Q14+-Q12 (Early Pleistocene - Middle Pleistocene) river deposits and ignimbrite",
                         "OIS11 (Middle Pleistocene) ocean beach deposits",
                         "OIS7 (Middle Pleistocene) ocean beach deposits",
                         "Early Pleistocene debris avalanche deposits (Old Formation) of Egmont Volcanic Centre",
                         "Early Pleistocene river and lake deposits",
                         "Early Pleistocene fan deposits",
                         "Early Pleistocene glacier deposits",
                         "Early Pleistocene Hillersden Gravel",
                         "Early Pleistocene ocean beach deposits",
                         "Middle Pleistocene lahar deposits (Kakaramea Formation) of Taupo Volcanic Zone",
                         "OIS16 (Early Pleistocene) glacier deposits",
                         "OIS5+-OIS2 (Middle Pleistocene - Late Pleistocene) river and hill slope deposits",
                         "OIS6 (Middle Pleistocene) loess deposits",
                         "OIS8 (Middle Pleistocene) loess deposits",
                         "OIS8+ (Middle Pleistocene) loess deposits",
                         "Middle Pleistocene loess deposits",
                         "Awhitu Group alluvium",
                         "Early Pleistocene river deposits",
                         "Middle Pleistocene - Late Pleistocene glacier deposits",
                         "Middle Pleistocene lake deposits and breccia",
                         "OIS10 (Middle Pleistocene) glacier deposits",
                         "OIS10-OIS8 (Middle Pleistocene) glacier deposits",
                         "OIS12 (Middle Pleistocene) glacier deposits",
                         "OIS6-OIS1 (Middle Pleistocene - Holocene) fan deposits",
                         "OIS6-OIS2 (Middle Pleistocene - Late Pleistocene) fan deposits",
                         "OIS6-OIS2 (Middle Pleistocene - Late Pleistocene) lahar flow deposits of Tongariro Volcanic Centre",
                         "OIS6-OIS4 (Middle Pleistocene - Late Pleistocene) glacier deposits",
                         "OIS8 (Middle Pleistocene) outwash deposits",
                         "OIS8+ (Middle Pleistocene) fan deposits",
                         "Undifferentiated Late Pleistocene - Holocene landslide deposits",
                         "Undifferentiated Late Pleistocene lahar deposits of Tongariro Volcanic Centre",
                         "Awhitu Group dunes",
                         "Early Pleistocene - Middle Pleistocene river deposits",
                         "Middle Pleistocene glacer deposits",
                         "OIS12 (Middle Pleistocene) fan deposits",
                         "OIS16 (Early Pleistocene) outwash deposits",
                         "OIS2 (Late Pleistocene) North Crater Andesite scoria of Tongariro Volcanic Centre",
                         "OIS5-OIS2 (Late Pleistocene) landslide deposits",
                         "OIS6 (Middle Pleistocene) glacier deposits",
                         "OIS6 (Middle Pleistocene) ocean beach deposits",
                         "OIS7 (Middle Pleistocene) Maroa Group rhyolite pyroclastics of Maroa Volcanic Centre",
                         "OIS8 (Middle Pleistocene) debris avalanche deposits (Maitahi Formation) of Egmont Volcanic Centre",
                         "OIS8 (Middle Pleistocene) glacier deposits",
                         "Q5+ (Early Pleistocene - Middle Pleistocene) river depoists",
                         "Early Pleistocene parabolic dunes",
                         "Late Pleistocene - Holocene fan deposits",
                         "Middle Pleistocene estuary and shallow marine deposits",
                         "Middle Pleistocene lake deposits",
                         "Middle Pleistocene river deposits",
                         "OIS1 (Holocene) glacier deposits",
                         "OIS1 (Holocene) Okataina Group rhyolite pyroclastics of Okataina Volcanic Centre",
                         "OIS1 (Holocene) rock avalanche deposits",
                         "OIS1 (Holocene) scree deposits",
                         "OIS10 (Middle Pleistocene) debris avalanche deposits (Eltham Formation - Egmont Volcanic Centre",
                          "OIS10 (Middle Pleistocene) fan deposits",
                          "OIS10 (Middle Pleistocene) Okataina Group rhyolite of Okataina Volcanic Centre",
                          "OIS10-OIS3 (Middle Pleistocene - Late Pleistocene) glacier deposits",
                          "OIS10-OIS8 (Middle Pleistocene) fan deposits",
                          "OIS10-OIS8 (Middle Pleistocene) outwash deposits",
                          "OIS11-OIS5 (Middle Pleistocene - Late Pleistocene) lahar flow deposits of Tongariro Volcanice Centre",
                          "OIS12 (Middle Pleistocene) outwash deposits",
                          "OIS14 (Early Pleistocene) fan deposits",
                          "OIS2 (Late Pleistocene) lahar deposits (Te Heu heu Formation) of Tongariro Volcanic Centre",
                          "OIS2 (Late Pleistocene) lahar deposits (Te Heuheu Formation) of Tongariro Volcanic Centre",
                          "OIS2 (Late Pleistocene) lahar flow deposits (Warea Formation) of Egmont Volcanic Centre",
                          "OIS2-OIS1 (Late Pleistocene - Holocene) glacier deposits",
                          "OIS3 (Late Pleistocene) debris avalanche deposit (Ngaere Formation) of Egmont Volcanic Centre",
                          "OIS3 (Late Pleistocene) lahar deposits (Horopito Formation) of Tongariro Volcanic Centre",
                          "OIS3 (Late Pleistocene) lahar deposits (Opunake Formation) of Egmont Volcanic Centre",
                          "OIS3-OIS1 (Late Pleistocene - Holocene) glacier deposits",
                          "OIS4 (Late Pleistocene) glacier deposits",
                          "OIS4-OIS2 (Late Pleistocene) glacier deposits",
                          "OIS5-OIS1 (Late Pleistocene - Holocene) glacier deposits",
                          "OIS6-OIS1 (Late Pleistocene - Holocene) lahar flow deposits of Tongariro Volcanic Centre",
                          "OIS6-OIS2 (Middle Pleistocene - Late Pleistocene) river deposits",
                          "OIS6-OIS4 (Middle Pleistocene - Late Pleistocene) fan deposits",
                          "OIS6-OIS4 (Middle Pleistocene - Late Pleistocene) Maroa Group rhyolite pyroclastics of Maroa Volcanic Centre",
                          "OIS7-OIS5 (Middle Pleistocene - Late Pleistocene) Okataina Group rhyolite pyroclastics of Okataina Volcanic Centre",
                          "OIS8 (Middle Pleistocene) river deposits",
                          "OIS8+ (Middle Pleistocene) alluvial deposits",
                          "Old Man Group gravel",
                          "Undifferentiated Pleistocene - Holocene fan deposits",
                          "Undifferentiated Pleistocene - Holocene glacier deposits",
                          "Middle Pleistocene ocean beach deposits",
                          "OIS15 (Early Pleistocene) ocean beach deposits",
                          "OIS17 (Early Pleistocene) ocean beach deposits",
                          "OIS3 (Late Pleistocene) Okataina Group rhyolite pyroclastics of Okataina Volcanic Centre",
                          "OIS3-OIS2 (Late Pleistocene) fan deposits",
                          "OIS3-OIS2 (Late Pleistocene) scree deposits",
                          "OIS4 (Late Pleistocene) lake deposits",
                          "OIS4 (Late Pleistocene) loess deposits",
                          "OIS4-OIS1 (Late Pleistocene - Holocene) scree deposits",
                          "OIS5+ (Early Pleistocene - Middle Pleistocene) dune deposits",
                          "OIS8+ (Middle Pleistocene) ocean beach deposits",
                          "Late Pleistocene - Holocene landslide deposits",
                          "Middle Pleistocene dune deposits",
                          "Middle Pleistocene fan deposits",
                          "Middle Pleistocene outwash deposits",
                          "OIS1 (Holocene) lahar flow deposits (Hangatahua Formation) of Egmont Volcanic Centre",
                          "OIS1 (Holocene) lahar flow deposits (Kahui Formation) of Egmont Volcanic Centre",
                          "OIS1 (Holocene) lahar flow deposits (Ngatoro Formation) of Egmont Volcanic Centre",
                          "OIS1 (Holocene) lahar flow deposits (Te Popo Formation) of Egmont Volcanic Centre",
                          "OIS1 (Holocene) lahar flow deposits (Warea Formation) of Egmont Volcanic Centre",
                          "OIS1 (Holocene) lahar flow deposits of Tongariro Volcanic Centre",
                          "OIS10 (Middle Pleistocene) landslide deposits",
                          "OIS10 (Middle Pleistocene) outwash deposits",
                          "OIS10 (Middle Pleistocene) river deposits",
                          "OIS10-OIS8 (Middle Pleistocene) river deposits",
                          "OIS12 (Middle Pleistocene) river deposits",
                          "OIS13 (Middle Pleistocene) ocean beach deposits",
                          "OIS16+ (Early Pleistocene) river deposits",
                          "OIS2 (Late Pleistocene) debris avalanche deposits (Pungarehu Formation) of Egmont Volcanic Centre",
                          "OIS2 (Late Pleistocene) glacier deposits",
                          "OIS2 (Late Pleistocene) lake gravel",
                          "OIS2 (Late Pleistocene) landslide deposits",
                          "OIS2-OIS1 (Late Pleistocene - Holocene) lake beach deposits",
                          "OIS2-OIS1 (Late Pleistocene - Holocene) scree deposits",
                          "OIS4-OIS1 (Late Pleistocene - Holocene) landslide deposits",
                          "OIS4-OIS2 (Late Pleistocene) fan deposits",
                          "OIS4-OIS3 (Late Pelistocene) lahar deposits (Stratford Formation) of Egmont Volcanic Centre",
                          "OIS5 (Late Pleistocene) landslide deposits",
                          "OIS5 (Late Pleistocene) ocean beach deposits",
                          "OIS5+ (Middle Pleistocene - Late Pleistocene) fan and river deposits",
                          "OIS5-OIS4 (Late Pelistocene) lahar deposits (Waimarino Formation) of Tongariro Volcanic Centre",
                          "OIS6 (Middle Pleistocene) debris avalanche deposits (Motunui Formation) of Egmont Volcanic Centre",
                          "OIS6 (Middle Pleistocene) outwash deposits",
                          "OIS6 (Middle Pleistocene) river deposits",
                          "OIS6-OIS4 (Middle Pleistocene - Late Pleistocene) lake deposits",
                          "OIS6-OIS4 (Middle Pleistocene - Late Pleistocene) river deposits",
                          "OIS7 (Middle Pleistocene) river deposits",
                          "OIS8 (Middle Pleistocene) fan deposits",
                          "OIS8+ (Middle Pleistocene) river deposits",
                          "OIS8-OIS2 (Middle Pleistocene - Late Pleistocene) river deposits",
                          "OIS8-OIS4 (Middle Pleistocene - Late Pleistocene) river deposits",
                          "OIS8-OIS6 (Middle Pleistocene) river deposits",
                          "OIS9 (Middle Pleistocene) ocean beach deposits",
                          "OIS9 (Middle Pleistocene) river deposits",
                          "Undifferentiated Pleistocene - Holocene landslide deposits",
                          "Undifferentiated Pleistocene - Holocene ocean beach deposits",
                          "Undifferentiated Pleistocene - Holocene scree deposits",
                          "OIS1 (Holocene) lake breakout deposits from Okataina Volcanic Centre",
                          "OIS1 (Holocene) stable dune deposits",
                          "OIS2-OIS1 (Late Pleistocene - Holocene) fan deposits",
                          "OIS3 (Late Pleistocene) fan deposits",
                          "Early Pleistocene (Mangatuna Formation) river and estuary deposits",
                          "Late Pleistocene - Holocene river deposits",
                          "Late Pleistocene river and lake deposits",
                          "Middle Pleistocene fan, river and lake deposits",
                          "Middle Pleistocene lake deposits mantled by thick Mangaone Subgroup fall deposits of Okataina Volcanic Centre",
                          "OIS1 (Holocene) active dune deposits",
                          "OIS1 (Holocene) active ocean beach deposits",
                          "OIS1 (Holocene) active riverbed deposits",
                          "OIS1 (Holocene) debris avalanche deposits (Murimotu Formation) of Tongariro Volcanic Centre",
                          "OIS1 (Holocene) debris avalanche deposits (Opua Formation) of Egmont Volcanic Centre",
                          "OIS1 (Holocene) debris avalanche deposits at Ketetahi",
                          "OIS1 (Holocene) dune deposits",
                          "OIS1 (Holocene) fan deposits",
                          "OIS1 (Holocene) lake beach deposits",
                          "OIS1 (Holocene) lake gravel",
                          "OIS1 (Holocene) landslide deposits",
                          "OIS1 (Holocene) parabolic dunes",
                          "OIS1 (Holocene) river deposits derived from Tarawera Formation",
                          "OIS1 (Holocene) rock glacier deposits",
                          "OIS1 (Holocene) Taupo Group tephra of Taupo Volcanic Centre",
                          "OIS12 (Middle Pleistocene) lake deposits",
                          "OIS12 (Middle Pleistocene) Okataina Group rhyolite mantled by thick Mangaone Subgroup fall deposits of Okataina Volcanic Centre",
                          "OIS2 (Late Pleistocene) dune deposits",
                          "OIS2 (Late Pleistocene) fan and river deposits",
                          "OIS2 (Late Pleistocene) fan deposits",
                          "OIS2 (Late Pleistocene) river deposits",
                          "OIS2-OIS1 (Late Pleistocene - Holocene) fan and river deposits",
                          "OIS2-OIS1 (Late Pleistocene - Holocene) river deposits",
                          "OIS3 (Late Pleistocene) ocean beach deposits",
                          "OIS3 (Late Pleistocene) river deposits",
                          "OIS3-OIS1 (Late Pleistocene - Holocene) river deposits",
                          "OIS3-OIS2 (Late Pleistocene) river deposits",
                          "OIS3-OIS2 (Late Pleistocene) stable dune deposits",
                          "OIS4 (Late Pleistocene) fan deposits",
                          "OIS4 (Late Pleistocene) ocean beach deposits",
                          "OIS4 (Late Pleistocene) outwash deposits",
                          "OIS4-OIS1 (Late Pleistocene - Holocene) fan deposits",
                          "OIS4-OIS1 (Late Pleistocene - Holocene) river deposits",
                          "OIS4-OIS2 (Late Pleistocene) river deposits",
                          "OIS4-OIS3 (Late Pleistocene) river deposits",
                          "OIS5 (Late Pleistocene) debris avalanche deposits (Okawa Formation) of Egmont Volcanic Centre",
                          "OIS5 (Late Pleistocene) fan deposits",
                          "OIS5 (Late Pleistocene) parabolic dunes",
                          "OIS5 (Late Pleistocene) river deposits",
                          "OIS5 (Late Pleistocene) stable dune deposits",
                          "OIS5+-OIS2 (Middle Pleistocene - Late Pleistocene) fan deposits",
                          "OIS5-OIS1 (Late Pleistocene - Holocene) river deposits",
                          "OIS5-OIS2 (Late Pleistocene) river deposits",
                          "OIS5-OIS4 (Late Pleistocene) river deposits",
                          "OIS5-OIS4 (Late Pleistocene) stable dune deposits",
                          "OIS6 (Middle Pleistocene) fan deposits",
                          "OIS6-OIS4 (Middle Pleistocene - Late Pleistocene) lake deposits mantled by thick Mangaone Subgroup fall deposits of Okataina Volcanic Centre",
                          "OIS7 (Middle Pleistocene) lake deposits",
                          "OIS8-OIS4 (Middle Pleistocene - Late Pleistocene) fan deposits",
                          "OIS8-OIS6 (Middle Pleistocene) fan deposits",
                          "OIS8-OIS6 (Middle Pleistocene) outwash deposits",
                          "Q5+ (Early Pleistocene - Middle Pleistocene) pumicious river deposits",
                          "Undifferentiated Pleistocene - Holocene lake deposits",
                          "Undifferentiated Pleistocene - Holocene river deposits",
                          "Late Pleistocene - Holocene ocean beach and swamp deposits",
                          "Late Pleistocene - Holocene river and ocean beach deposits",
                          "Late Pleistocene - Holocene scree deposits",
                          "Late Pleistocene river deposits mantled by thick Mangaone Subgroup fall deposits of Okataina Volcanic Centre",
                          "OIS1 (Holocene) anthropic deposits",
                          "OIS1 (Holocene) engineered fill",
                          "OIS1 (Holocene) lake deposits",
                          "OIS1 (Holocene) landfill",
                          "OIS1 (Holocene) landfill and reclaimed land",
                          "OIS1 (Holocene) mining waste",
                          "OIS1 (Holocene) ocean beach deposits",
                          "OIS1 (Holocene) reclaimed land",
                          "OIS1 (Holocene) river delta deposits",
                          "OIS1 (Holocene) river deposits",
                          "OIS1 (Holocene) river deposits (Taupo Pumice Alluvium)",
                          "OIS2 (Late Pleistocene) outwash deposits",
                          "OIS3 (Late Pleistocene) lake deposits",
                          "OIS3 (Late Pleistocene) river sand deposits",
                          "OIS3-OIS2 (Late Pleistocene) river deposits (Hinuera Formation)",
                          "OIS4-OIS2 (Late Pleistocene) fan and river deposits",
                          "OIS5+ (Early Pleistocene - Middle Pleistocene) estuary, river and swamp deposits",
                          "OIS5-OIS2 (Late Pleistocene) stable dune deposits",
                          "OIS6+ (Early Pleistocene - Middle Pleistocene) estuary, river and swamp deposits",
                          "Late Pleistocene - Holocene swamp deposits",
                          "Late Pleistocene river and estuary deposits",
                          "OIS1 (Holocene) estuary deposits",
                          "OIS1 (Holocene) swamp deposits",
                          "OIS2 (Late Pleistocene) swamp deposits",
                          "OIS4 (Late Pleistocene) river deposits",
                          "OIS4-OIS1 (Late Pleistocene - Holocene) estuary, river and swamp deposits",
                          "OIS4-OIS1 (Late Pleistocene - Holocene) swamp deposits",
                          "OIS5 (Late Pleistocene) river, lake and swamp deposits"
                         ),
siteClass = c(
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "B",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C1",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C2",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "C3",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D1",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D2",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "D3",
  "E",
  "E",
  "E",
  "E",
  "E",
  "E",
  "E",
  "E",
  "E"
))

PerrinEtAl_SiteClassByVs30 <- data.frame(
  siteClass = c("E", "D3", "D2", "D1", "C3", "C2", "C1", "B"),
  minVs30   = c(0, 180, 240, 300, 360, 490, 620, 760),
  maxVs30   = c(   180, 240, 300, 360, 490, 620, 760, Inf))


load("~/big_noDB/geo/QMAP_Seamless_July13K_NZGD00.Rdata")
qmapKN <- as.character(map_NZGD00$KEY_NAME)
qmapMatches <- rep(0,length(qmapKN))
qmapPerrinInd <- rep(NA,length(qmapKN))

for(i in 1:length(qmapKN)) {
  #i <- 2
  whichMatch <- which(grepl(qmapKN[i], perrinEtAl_KEY_NAME_and_siteClass$KEY_NAME, 
                            ignore.case=T, fixed=T))
  qmapMatches[i] <- qmapMatches[i] + length(whichMatch)
  ind <- tail(whichMatch, n=1)
  if (length(whichMatch)>0) { qmapPerrinInd[i] <- ind }
}
  

matchingDF <- data.frame(INDEX           = map_NZGD00$INDEX, 
                         KEY_NAME        = qmapKN,
                         PerrinMatches   = qmapMatches,
                         PerrinIndex     = qmapPerrinInd,
                         PerrinKEYNAME   = perrinEtAl_KEY_NAME_and_siteClass$KEY_NAME[qmapPerrinInd],
                         siteClass       = perrinEtAl_KEY_NAME_and_siteClass$siteClass[qmapPerrinInd]
                         )

mapVsVals <- data.frame(map_NZGD00[,c("Vs30_AhdiAK",
                          "Vs30_AhdiAK_KaiAll",
                          "Vs30_testing",
                          "groupID_testing",
                          "groupID_AhdiAK")])

matchingDFwithQmapVals <- cbind(matchingDF, mapVsVals)


matchingDF <- matchingDFwithQmapVals[matchingDF$PerrinMatches>0,]


matchDFwithPerrin <- 
  join(x = matchingDF, y=PerrinEtAl_SiteClassByVs30, by="siteClass")

matchDFwithPerrin_num <- matchDFwithPerrin[,c("minVs30", "maxVs30", "Vs30_testing", "Vs30_AhdiAK",
                                              "Vs30_AhdiAK_KaiAll", "KEY_NAME")]
mDF <- unique(matchDFwithPerrin_num)
mDF$medVs30 <- exp(log(mDF$maxVs30+log(mDF$minVs30)/2))

# p <- ggplot() + xlim(c(0,1200)) + ylim(c(0,1100))  + theme(plot.title = element_text(hjust = 0.5,
#                                                                                      size=10)) +
#      ggtitle(c("Polygon-wise (geology-only) model", "comparison with Perrin classifications")) +
#      coord_equal() + xlab("Perrin et al. (max and min values by site class)") 
# p + geom_jitter(data=mDF, aes(x=minVs30, y=Vs30_testing), height=0, width=10, alpha=0.3) +
#     geom_jitter(data=mDF, aes(x=maxVs30, y=Vs30_testing), height=0, width=10, alpha=0.3) 
# ggsave(filename = "out/plots/PerrinCompare01_polygons_Vs30_testing.png")
# p + geom_jitter(data=mDF, aes(x=minVs30, y=Vs30_AhdiAK), height=0, width=10, alpha=0.3)  +
#     geom_jitter(data=mDF, aes(x=maxVs30, y=Vs30_AhdiAK), height=0, width=10, alpha=0.3)
# ggsave(filename = "out/plots/PerrinCompare02_polygons_Vs30_AhdiAK.png")
# p + geom_jitter(data=mDF, aes(x=minVs30, y=Vs30_AhdiAK_KaiAll), height=0, width=10, alpha=0.3) +
#     geom_jitter(data=mDF, aes(x=maxVs30, y=Vs30_AhdiAK_KaiAll), height=0, width=10, alpha=0.3) 
# ggsave(filename = "out/plots/PerrinCompare03_polygons_Vs30_KaiAll.png")
# 
# # Now comparing points in Vspr
load("Rdata/vspr.Rdata")
vs  <- subsetVsPoints(vspr)$Kaiser_all %>% data.frame
vs2 <- join(vs, matchDFwithPerrin, by="INDEX")
# 
# p <- ggplot(data=vs2) + xlim(c(0,1100)) + ylim(c(0,1100))  + theme(plot.title = element_text(hjust = 0.5,
#                                                                                              size=10)) +
#      ggtitle(c("Point-wise (geology and hybrid)\nmodel comparison with Perrin classifications")) +
#      coord_equal() + xlab("Perrin et al. (max and min values by site class)") 
# p + geom_jitter(aes(x=minVs30, y=Vs30_testing), height=0, width=10, alpha=0.3) +
#   geom_jitter(aes(x=maxVs30, y=Vs30_testing), height=0, width=10, alpha=0.3) 
# ggsave(filename = "out/plots/PerrinCompare04_points_Vs30_testing.png")
# 
# p + geom_jitter(aes(x=minVs30, y=Vs30_AhdiAK), height=0, width=10, alpha=0.3)  +
#   geom_jitter(aes(x=maxVs30, y=Vs30_AhdiAK), height=0, width=10, alpha=0.3)
# ggsave(filename = "out/plots/PerrinCompare05_points_Vs30_AhdiAK.png")
# p + geom_jitter(aes(x=minVs30, y=Vs30_AhdiAK_KaiAll), height=0, width=10, alpha=0.3) +
#   geom_jitter(aes(x=maxVs30, y=Vs30_AhdiAK_KaiAll), height=0, width=10, alpha=0.3) 
# ggsave(filename = "out/plots/PerrinCompare06_points_Vs30_AhdiAK_KaiAll.png")
# p + geom_jitter(aes(x=minVs30, y=Vs30_AhdiAK_KaiAll_hyb09c), height=0, width=10, alpha=0.3) +
#   geom_jitter(aes(x=maxVs30, y=Vs30_AhdiAK_KaiAll_hyb09c), height=0, width=10, alpha=0.3) 
# ggsave(filename = "out/plots/PerrinCompare07_points_Vs30_AhdiAK_KaiAll_hyb09c.png")

p <- ggplot(data=vs2) + xlim(c(0,1100)) + ylim(c(0,1100))  + 
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size=10)) +
  ggtitle(c("Measured Vs30 (Kaiser et al.)\ncomparison with Perrin classifications")) +
  coord_equal() + xlab("Perrin et al. (max and min values by site class)") 
p + geom_jitter(aes(x=minVs30, y=Vs30, color=QualityFlag), height=0, width=10, alpha=0.3) +
  geom_jitter(aes(x=maxVs30, y=Vs30, color=QualityFlag), height=0, width=10, alpha=0.3) 
ggsave(filename = "out/plots/PerrinCompare08_points_Vs30_measured.png",width = 4)

