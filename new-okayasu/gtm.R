#GTM試し
library(myimg)
library(myfs)
library(TDA)
library(rgl)
library(phacm)
library(interpo3d)
library(seephacm)
require(phacm)
require(pracma)
require(deldir)
require(ggplot2)
require(plyr)
require(reshape2)
require(ggmap)
require(tidyverse)


##GTMによる次元削減
torus18_dist<-dist(torus.collect18[[1]][["noizyX"]])
trs18_vics1<-interpo3d:::get_vicinity(torus18_dist, 1, 30)
figurePlot3d(torus.collect18[[1]][["noizyX"]][-trs18_vics1, ])
points3d(torus.collect18[[1]][["noizyX"]][trs18_vics1, ], col=2)

trs18<-torus.collect18[[1]][["noizyX"]]

trs18_pca1<-prcomp(trs18[trs18_vics1, ])
plot(trs18_pca1[["x"]][, 1:2], col=4, pch=16)


library(maptools)
pointLabel(trs18_pca1[["x"]][, 1:2], as.character(trs18_vics1))

#GTM補間後、ベッチ数推定してみる
trs300_incolle_set1<-gtm_interpolate(torus300_colle_set[[1]][1:5], 30)

{
trs300_incolle_set1_test_aggr<-proposedMethodOnly(trs300_incolle_set1, 2, 3, 10)
save2Rdata(trs300_incolle_set1_test_aggr)
}
