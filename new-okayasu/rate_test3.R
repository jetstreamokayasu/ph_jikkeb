require(phacm)
require(tidyverse)
require(TDA)
require(myfs)
require(rgl)

#補間前後のPD比較
#300点トーラス

trs15_300_1_1_pd<-ripsDiag(torus15.300subs[[1]][[1]][["noizyX"]], 2, 3, printProgress = T)
plot(trs15_300_1_1_pd[[1]], main="data1")

trs15_in300_1_1_pd<-ripsDiag(torus15.300insubs[[1]][[1]][["noizyX"]], 2, 3, printProgress = T)
plot(trs15_in300_1_1_pd[[1]])

trs15_300_1_2_10_pd<-lapply(2:10, function(k)ripsDiag(torus15.300subs[[1]][[k]][["noizyX"]], 2, 3, printProgress = T))
plot(trs15_300_1_2_10_pd[[1]][[1]])
save2Rdata(trs15_300_1_2_10_pd)

for (k in 1:9) {
  plot(trs15_300_1_2_10_pd[[k]][[1]], main=paste0("data", k+1))
}

trs15_in300_1_1_10_pd<-lapply(1:10, function(k)ripsDiag(torus15.300insubs[[1]][[k]][["noizyX"]], 2, 3, printProgress = T))
save2Rdata(trs15_in300_1_1_10_pd)
plot(trs15_in300_1_1_10_pd[[2]][[1]])

for (k in 1:10) {
  plot(trs15_in300_1_1_10_pd[[k]][[1]], main=paste0("in_data", k))
}

plotPDs(trs15_300_1_2_10_pd)
plotPDs(trs15_in300_1_1_10_pd)

trs15_300_1_2_pl<-calcLandscape(trs15_300_1_2_10_pd[[1]])
trs15_in300_1_2_pl<-calcLandscape(trs15_in300_1_1_10_pd[[2]])

