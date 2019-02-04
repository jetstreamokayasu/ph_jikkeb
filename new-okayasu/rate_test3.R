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


#補間前に不正解なのと補間後に正解したデータセット比較
wrong1_10<-which(torus15.300subs.aggrs[[1]][[2]] < 0.5)[1:10]
trs15_300_1_w1_10_pd<-lapply(wrong1_10, function(k)ripsDiag(torus15.300subs[[1]][[k]][["noizyX"]], 2, 3, printProgress = T))
save2Rdata(trs15_300_1_w1_10_pd)
plotPDs(trs15_300_1_w1_10_pd)

trs15_in300_1_w1_10_pd<-lapply(wrong1_10, function(k)ripsDiag(torus15.300insubs[[1]][[k]][["noizyX"]], 2, 3, printProgress = T))
save2Rdata(trs15_in300_1_w1_10_pd)
plotPDs(trs15_in300_1_w1_10_pd)

#補間点の誤差を調べる
trs15_in300_1_er<-calc_error(torus15.300insubs1_3[[1]][[1]][["noizyX"]], maxr = 2.5, minr = 1, nps = 300)

trs15_in300_1ers<-lapply(1:100, function(i)calc_error(torus15.300insubs1_3[[1]][[i]][["noizyX"]], maxr = 2.5, minr = 1, nps = 300))
par(mgp=c(2.5,1,0))
boxplot(trs15_in300_1ers[1:50], xlab="Data Set", ylab="Error", cex.lab=1.6, cex.axis=1.6)

oldpar <- par(no.readonly=T)

trs15_in300_1_1_der<-torus_disterror(torus15.300insubs1_3[[1]][[1]][["noizyX"]], maxr = 2.5, minr = 1, nps = 300)
hist(trs15_in300_1_1_der, col="#993435")
