#!/bin/bash -xv

d=/home/kmf76/VsMap/out/maps/

###################################################################################################
# Commented lines below are (a) no longer used or (b) have been moved to the R script, R/makeFigs.R
###################################################################################################

####    
####    # category maps
####    
####    convert -gravity Center ${d}AhdiGeoCats_CH_three.png                                              ${d}AhdiGeoCats_three_legend.png                                         -trim +append ${d}AhdiGeoCats_CH_three_legendF.png
####    convert -gravity Center ${d}AhdiGeoCats_NZ_three.png                                              ${d}AhdiGeoCats_three_legend.png                                         -trim +append ${d}AhdiGeoCats_NZ_three_legendF.png
####    convert -gravity Center ${d}IPcats_CH_three.png                                                   ${d}IPcats_three_legend.png                                              -trim +append ${d}IPcats_CH_three_legendF.png
####    convert -gravity Center ${d}IPcats_NZ_three.png                                                   ${d}IPcats_three_legend.png                                              -trim +append ${d}IPcats_NZ_three_legendF.png
####    
####    
####    # Vs30 and Sigma maps
####    
####    convert -gravity Center ${d}Vs30_CH_AhdiAK_three.png                                              ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_CH_AhdiAK_three_legendF.png
####    convert -gravity Center ${d}sigma_CH_AhdiAK_three.png                                             ${d}sigma_three_legend.png                                               -trim +append ${d}sigma_CH_AhdiAK_three_legendF.png
####    convert -gravity Center ${d}Vs30_CH_AhdiAK_noQ3_three.png                                         ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_CH_AhdiAK_noQ3_three_legendF.png
####    convert -gravity Center ${d}Vs30_NZ_AhdiAK_noQ3_hyb09c_three.png                                  ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_NZ_AhdiAK_noQ3_hyb09c_three_legendF.png
####    convert -gravity Center ${d}Vs30_CH_AhdiYongWeighted1_three.png                                   ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_CH_AhdiYongWeighted1_three_legendF.png
####    convert -gravity Center ${d}sigma_CH_AhdiYongWeighted1_three.png                                  ${d}sigma_three_legend.png                                               -trim +append ${d}sigma_CH_AhdiYongWeighted1_three_legendF.png
####    convert -gravity Center ${d}Vs30_CH_AhdiYongWeighted1_KRG_v5_three.png                            ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_CH_AhdiYongWeighted1_KRG_v5_three_legendF.png
####    convert -gravity Center ${d}sigma_CH_AhdiYongWeighted1_KRG_v5_three.png                           ${d}sigma_three_legend.png                                               -trim +append ${d}sigma_CH_AhdiYongWeighted1_KRG_v5_three_legendF.png
####    convert -gravity Center ${d}Vs30_CH_AhdiYongWeighted1_KRG_slopeWeighting1_v5_three.png            ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_CH_AhdiYongWeighted1_KRG_slopeWeighting1_v5_three_legendF.png
####    convert -gravity Center ${d}Vs30_CH_AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0_three.png     ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_CH_AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0_three_legendF.png
####    convert -gravity Center ${d}sigma_CH_AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0_three.png    ${d}sigma_three_legend.png                                               -trim +append ${d}sigma_CH_AhdiYongWeighted1_MVN_noisyF_minDist0_v3_crp0.0_three_legendF.png
####    convert -gravity Center ${d}Vs30_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0_three.png     ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0_three_legendF.png
####    convert -gravity Center ${d}sigma_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0_three.png    ${d}sigma_three_legend.png                                               -trim +append ${d}sigma_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp0.0_three_legendF.png
####    convert -gravity Center ${d}Vs30_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_three.png     ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_three_legendF.png
####    convert -gravity Center ${d}sigma_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_three.png    ${d}sigma_three_legend.png                                               -trim +append ${d}sigma_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_three_legendF.png
####    
####    convert -gravity Center ${d}Vs30_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_six.png       ${d}Vs30_three_legendsix.png                                             -trim +append ${d}Vs30_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_six_legendF.png
####    convert -gravity Center ${d}sigma_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_six.png      ${d}sigma_three_legendsix.png                                            -trim +append ${d}sigma_CH_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_six_legendF.png
####    convert -gravity Center ${d}Vs30_NZ_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_six.png       ${d}Vs30_three_legendsix.png                                             -trim +append ${d}Vs30_NZ_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_six_legendF.png
####    convert -gravity Center ${d}sigma_NZ_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_six.png      ${d}sigma_three_legendsix.png                                            -trim +append ${d}sigma_NZ_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_six_legendF.png
####    
####    convert -gravity Center ${d}Vs30_NZ_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_three.png     ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_NZ_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_three_legendF.png
####    convert -gravity Center ${d}sigma_NZ_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_three.png    ${d}sigma_three_legend.png                                               -trim +append ${d}sigma_NZ_AhdiYongWeighted1_MVN_noisyT_minDist0_v3_crp1.5_three_legendF.png
####    
####    convert -gravity Center ${d}Vs30_NZ_AhdiYongWeighted1_three.png                                   ${d}Vs30_three_legend.png                                                -trim +append ${d}Vs30_NZ_AhdiYongWeighted1_three_legendF.png
####    convert -gravity Center ${d}sigma_NZ_AhdiYongWeighted1_three.png                                  ${d}sigma_three_legend.png                                               -trim +append ${d}sigma_NZ_AhdiYongWeighted1_three_legendF.png
####    
####    # resid maps
####    
####    convert -gravity Center ${d}resid_CH_AY1_KRG_v5_three.png                                         ${d}resid_AY1_KRG_v5_three_legend.png                                    -trim +append ${d}resid_CH_AY1_KRG_v5_three_legendF.png
####    convert -gravity Center ${d}resid_CH_AY1_MVN_noisyF_minDist0_v3_crp0.0_three.png                  ${d}resid_AY1_MVN_noisyF_minDist0_v3_crp0.0_three_legend.png             -trim +append ${d}resid_CH_AY1_MVN_noisyF_minDist0_v3_crp0.0_three_legendF.png
####    convert -gravity Center ${d}resid_CH_AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1_three.png          ${d}resid_AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1_three_legend.png     -trim +append ${d}resid_CH_AY1_MVN_noisyF_minDist0_v3_crp0.0_normAY1_three_legendF.png
####    
####    
####    # ratio maps
####    
####    convert -gravity Center ${d}ratio_CH_AhdiBayes_three.png                                          ${d}ratio_AhdiBayes_three_legend.png                                     -trim +append ${d}ratio_CH_AhdiBayes_three_legendF.png
####    convert -gravity Center ${d}ratio_NZ_AhdiHyb_three.png                                            ${d}ratio_AhdiHyb_three_legend.png                                       -trim +append ${d}ratio_NZ_AhdiHyb_three_legendF.png
####    convert -gravity Center ${d}ratio_CH_AhdiHyb_three.png                                            ${d}ratio_AhdiHyb_three_legend.png                                       -trim +append ${d}ratio_CH_AhdiHyb_three_legendF.png
####    convert -gravity Center ${d}ratio_CH_KRGweighted_fullKRG_three.png                                ${d}ratio_KRGweighted_fullKRG_three_legend.png                           -trim +append ${d}ratio_CH_KRGweighted_fullKRG_three_legendF.png
####    convert -gravity Center ${d}ratio_CH_MVN_noiseTF_crp0_three.png                                   ${d}ratio_MVN_noiseTF_crp0_three_legend.png                              -trim +append ${d}ratio_CH_MVN_noiseTF_crp0_three_legendF.png
####    convert -gravity Center ${d}ratio_CH_MVNweighted_fullMVN_three.png                                ${d}ratio_MVNweighted_fullMVN_three_legend.png                           -trim +append ${d}ratio_CH_MVNweighted_fullMVN_three_legendF.png
####    
####    
####    convert -gravity Center ${d}ratio_NZ_YongAhdi_three.png                                           ${d}ratio_YongAhdi_three_legend.png                                      -trim +append ${d}ratio_NZ_YongAhdi_three_legendF.png
####    convert -gravity Center ${d}ratio_NZ_YongAhdiPrior_three.png                                      ${d}ratio_YongAhdiPrior_three_legend.png                                 -trim +append ${d}ratio_NZ_YongAhdiPrior_three_legendF.png
####    convert -gravity Center ${d}ratio_CH_YongAhdi_three.png                                           ${d}ratio_YongAhdi_three_legend.png                                      -trim +append ${d}ratio_CH_YongAhdi_three_legendF.png
####    convert -gravity Center ${d}ratio_CH_YongAhdiPrior_three.png                                      ${d}ratio_YongAhdiPrior_three_legend.png                                 -trim +append ${d}ratio_CH_YongAhdiPrior_three_legendF.png
####    
####    
####    
####    
####    # "three-pane" figure showing terrain model updating
####    convert -gravity Center ${d}Vs30_CH_YongCA_three.png            ${d}Vs30_CH_YongCA_noQ3_three.png     \
####                            ${d}Vs30_three_legend.png                                                 \
####                            ${d}ratio_CH_YongBayes_three.png        ${d}ratio_YongBayes_three_legend.png  \
####                                      -trim +append ${d}YongBayes_prior_posterior_ratio.png
####    
####    
####    
####    # "one-pane" fig showing the left-hand side of above
####    convert -gravity Center ${d}Vs30_CH_YongCA_three.png            ${d}Vs30_three_legend.png                                                                                  -trim +append ${d}Vs30_CH_YongCA_three_legendF.png
####    # "one-pane" fig showing the right-hand side of above
####    convert -gravity Center ${d}ratio_CH_YongBayes_three.png        ${d}ratio_YongBayes_three_legend.png                                                                       -trim +append ${d}ratio_CH_YongBayes_three_legendF.png
####    
####    
####    convert -gravity Center ${d}Vs30_CH_YongCA_noQ3_three.png            ${d}Vs30_three_legend.png                                                                                  -trim +append ${d}Vs30_CH_YongCA_noQ3_three_legendF.png
####    
####    













































###########################################################################################################
# six-pane figure showing both geology and terrain updating................................................

# first annotate each pane and trim whitespace
convert ${d}Vs30_CH_AhdiAK_three.png                -gravity SouthWest -pointsize 48 -annotate +100+100 '(a) Geology prior'       -shave 86  /tmp/tl.png
convert ${d}Vs30_CH_AhdiAK_noQ3_hyb09c_three.png    -gravity SouthWest -pointsize 48 -annotate +100+100 '(c) Geology posterior'   -shave 86  /tmp/ml.png
convert ${d}ratio_CH_AhdiBOTH_three.png             -gravity SouthWest -pointsize 48 -annotate +100+100 '(e) Geology ratio'       -shave 86  /tmp/bl.png
convert ${d}Vs30_CH_YongCA_three.png                -gravity SouthWest -pointsize 48 -annotate +100+100 '(b) Terrain prior'       -shave 86  /tmp/tr.png
convert ${d}Vs30_CH_YongCA_noQ3_three.png           -gravity SouthWest -pointsize 48 -annotate +100+100 '(d) Terrain posterior'   -shave 86  /tmp/mr.png
convert ${d}ratio_CH_YongBayes_three.png            -gravity SouthWest -pointsize 48 -annotate +100+100 '(f) Terrain ratio'       -shave 86  /tmp/br.png

# next, trim whitespace from legend bars
convert ${d}Vs30_three_legendtall.png           -gravity west -trim -extent 250x2030 /tmp/Vs30leg.png
convert ${d}ratio_YongBayes_three_legend.png     -gravity west -trim -extent 250x1000 /tmp/ratleg.png

# next, add some whitespace to each pane uniformly
convert /tmp/tl.png  -gravity SouthWest -splice 30x30 /tmp/tl1.png ; convert /tmp/tl1.png -gravity NorthEast -splice 30x30 /tmp/tl2.png
convert /tmp/ml.png  -gravity SouthWest -splice 30x30 /tmp/ml1.png ; convert /tmp/ml1.png -gravity NorthEast -splice 30x30 /tmp/ml2.png
convert /tmp/bl.png  -gravity SouthWest -splice 30x30 /tmp/bl1.png ; convert /tmp/bl1.png -gravity NorthEast -splice 30x30 /tmp/bl2.png
convert /tmp/tr.png  -gravity SouthWest -splice 30x30 /tmp/tr1.png ; convert /tmp/tr1.png -gravity NorthEast -splice 30x30 /tmp/tr2.png
convert /tmp/mr.png  -gravity SouthWest -splice 30x30 /tmp/mr1.png ; convert /tmp/mr1.png -gravity NorthEast -splice 30x30 /tmp/mr2.png
convert /tmp/br.png  -gravity SouthWest -splice 30x30 /tmp/br1.png ; convert /tmp/br1.png -gravity NorthEast -splice 30x30 /tmp/br2.png

# next, put it all together
convert -gravity Center /tmp/tl2.png  /tmp/tr2.png     +append /tmp/six-pane-top.png
convert -gravity Center /tmp/ml2.png  /tmp/mr2.png     +append /tmp/six-pane-mid.png
convert -gravity Center /tmp/six-pane-top.png /tmp/six-pane-mid.png -append /tmp/six-pane-topmid.png
convert -gravity Center /tmp/six-pane-topmid.png /tmp/Vs30leg.png +append /tmp/six-pane-topmidleg.png
convert -gravity Center /tmp/bl2.png  /tmp/br2.png   /tmp/ratleg.png   +append /tmp/six-pane-bot.png
convert -gravity Center /tmp/six-pane-topmidleg.png  /tmp/six-pane-bot.png  -append ${d}Vs30_6pane_legendF.png




###########################################################################################################
# six-pane figure showing both geology and terrain updating (SIGMA).......................................

# first annotate each pane and trim whitespace
convert ${d}sigma_CH_AhdiAK_three.png                -gravity SouthWest -pointsize 48 -annotate +100+100 '(a) Geology prior'       -shave 86  /tmp/tl.png
convert ${d}sigma_CH_AhdiAK_noQ3_hyb09c_three.png    -gravity SouthWest -pointsize 48 -annotate +100+100 '(c) Geology posterior'   -shave 86  /tmp/ml.png
convert ${d}ratio_CH_AhdiBOTHsigma_three.png         -gravity SouthWest -pointsize 48 -annotate +100+100 '(e) Geology ratio'       -shave 86  /tmp/bl.png
convert ${d}sigma_CH_YongCA_three.png                -gravity SouthWest -pointsize 48 -annotate +100+100 '(b) Terrain prior'       -shave 86  /tmp/tr.png
convert ${d}sigma_CH_YongCA_noQ3_three.png           -gravity SouthWest -pointsize 48 -annotate +100+100 '(d) Terrain posterior'   -shave 86  /tmp/mr.png
convert ${d}ratio_CH_YongBayesSigma_three.png        -gravity SouthWest -pointsize 48 -annotate +100+100 '(f) Terrain ratio'       -shave 86  /tmp/br.png

# next, trim whitespace from legend bars
convert ${d}sigma_three_legendtall.png            -gravity west -trim -extent 250x2030 /tmp/sigmaleg.png
convert ${d}ratio_YongBayesSigma_three_legend.png -gravity west -trim -extent 250x1000 /tmp/sigmaratioleg.png

# next, add some whitespace to each pane uniformly
convert /tmp/tl.png  -gravity SouthWest -splice 30x30 /tmp/tl1.png ; convert /tmp/tl1.png -gravity NorthEast -splice 30x30 /tmp/tl2.png
convert /tmp/ml.png  -gravity SouthWest -splice 30x30 /tmp/ml1.png ; convert /tmp/ml1.png -gravity NorthEast -splice 30x30 /tmp/ml2.png
convert /tmp/bl.png  -gravity SouthWest -splice 30x30 /tmp/bl1.png ; convert /tmp/bl1.png -gravity NorthEast -splice 30x30 /tmp/bl2.png
convert /tmp/tr.png  -gravity SouthWest -splice 30x30 /tmp/tr1.png ; convert /tmp/tr1.png -gravity NorthEast -splice 30x30 /tmp/tr2.png
convert /tmp/mr.png  -gravity SouthWest -splice 30x30 /tmp/mr1.png ; convert /tmp/mr1.png -gravity NorthEast -splice 30x30 /tmp/mr2.png
convert /tmp/br.png  -gravity SouthWest -splice 30x30 /tmp/br1.png ; convert /tmp/br1.png -gravity NorthEast -splice 30x30 /tmp/br2.png

# next, put it all together
convert -gravity Center /tmp/tl2.png  /tmp/tr2.png   +append /tmp/four-pane-top.png
convert -gravity Center /tmp/ml2.png  /tmp/mr2.png   +append /tmp/four-pane-mid.png
convert -gravity Center /tmp/four-pane-top.png /tmp/four-pane-mid.png -append /tmp/four-pane-topmid.png
convert -gravity Center /tmp/four-pane-topmid.png /tmp/sigmaleg.png +append /tmp/four-pane-topmidleg.png
convert -gravity Center /tmp/bl2.png  /tmp/br2.png   /tmp/sigmaratioleg.png  +append /tmp/four-pane-bot.png
convert -gravity Center /tmp/four-pane-topmidleg.png  /tmp/four-pane-bot.png  -append ${d}sigma_6pane_legendF.png







# two-pane figure showing geology- and terrain-based KRG Vs30 maps
convert /home/kmf76/VsMap/out/maps/Vs30_CH_AhdiAK_noQ3_hyb09c_KRG_v6_three.png \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(a)' \
                  -shave 86         /tmp/a.png

convert /home/kmf76/VsMap/out/maps/Vs30_CH_YongCA_noQ3_KRG_v7_three.png \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(b)' \
                  -shave 86         /tmp/b.png

convert -gravity Center /tmp/a.png /tmp/b.png +append /tmp/2.png

convert /home/kmf76/VsMap/out/maps/Vs30_three_legend.png -trim -geometry x950 /tmp/l.png

convert /tmp/2.png /tmp/l.png -trim +append ${d}Vs30_CH_both_KRG_three_legendF.png


# two-pane figure showing geology- and terrain-based KRG sigma maps
convert  /home/kmf76/VsMap/out/maps/sigma_CH_AhdiAK_noQ3_hyb09c_KRG_v6_three.png \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(a)' \
                  -shave 86         /tmp/a.png

convert /home/kmf76/VsMap/out/maps/sigma_CH_YongCA_noQ3_KRG_v7_three.png \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(b)' \
                  -shave 86         /tmp/b.png

convert -gravity Center /tmp/a.png /tmp/b.png +append /tmp/2.png

convert /home/kmf76/VsMap/out/maps/sigma_three_legend.png -trim -geometry x950 /tmp/l.png

convert /tmp/2.png /tmp/l.png -trim +append ${d}sigma_CH_both_KRG_three_legendF.png





# two-pane figure showing geology- and terrain-based MVN Vs30 maps
convert  /home/kmf76/VsMap/out/maps/Vs30_CH_AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_three.png \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(a)' \
                  -shave 86         /tmp/a.png

convert /home/kmf76/VsMap/out/maps/Vs30_CH_YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_three.png   \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(b)' \
                  -shave 86         /tmp/b.png

convert -gravity Center /tmp/a.png /tmp/b.png +append /tmp/2.png

convert /home/kmf76/VsMap/out/maps/Vs30_three_legend.png -trim -geometry x950 /tmp/l.png

convert /tmp/2.png /tmp/l.png -trim +append ${d}Vs30_CH_both_MVN_three_legendF.png


# two-pane figure showing geology- and terrain-based MVN sigma maps
convert  /home/kmf76/VsMap/out/maps/sigma_CH_AhdiAK_noQ3_hyb09c_MVN_noisyT_minDist0_v6_crp1.5_three.png \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(a)' \
                  -shave 86         /tmp/a.png

convert /home/kmf76/VsMap/out/maps/sigma_CH_YongCA_noQ3_MVN_noisyT_minDist0_v7_crp1.5_three.png \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(b)' \
                  -shave 86         /tmp/b.png

convert -gravity Center /tmp/a.png /tmp/b.png +append /tmp/2.png

convert /home/kmf76/VsMap/out/maps/sigma_three_legend.png -trim -geometry x950 /tmp/l.png

convert /tmp/2.png /tmp/l.png -trim +append ${d}sigma_CH_both_MVN_three_legendF.png







# two-pane figure showing ratio of MVN to KRG Vs30 maps
convert  /home/kmf76/VsMap/out/maps/ratio_CH_AhdiMVN_KRGv6_three.png   \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(a)' \
                  -shave 86         /tmp/a.png

convert /home/kmf76/VsMap/out/maps/ratio_CH_YongMVN_KRGv7_three.png   \
        -gravity SouthWest -pointsize 48 -annotate +100+100 '(b)' \
                  -shave 86         /tmp/b.png

convert -gravity Center /tmp/a.png /tmp/b.png +append /tmp/2.png

convert /home/kmf76/VsMap/out/maps/ratio_AhdiMVN_KRGv6_three_legend.png -trim -geometry x950 /tmp/l.png

convert /tmp/2.png /tmp/l.png -trim +append ${d}ratio_CH_bothMVN_KRG_three_legendF.png

