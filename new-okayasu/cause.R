require(phacm)
require(tidyverse)
require(TDA)
require(myfs)
require(rgl)
library(seephacm)
library(tictoc)
library(devtools)
library(interpo3d)

#なぜ1次ベッチ数の精度が下がるのか調べる
#350点トーラスで比較
#torus.collect18とtorus_colle_set1が350点の補間前トーラス

#補間前に正解なのと補間後に正解しなかったデータセット比較
crrct1_10<-which(torus_colset1_aggrs[[1]][[1]] >= 1.5 & torus_colset1_aggrs[[1]][[1]] < 2.5)

inwrng1_10<-which(torus350_incolle_aggrs[[2]][[1]] < 1.5 | torus_colset1_aggrs[[1]][[1]] >= 2.5)

c_inw_1_10<-intersect(crrct1_10, inwrng1_10)[1:10]

trs350_2_cw1_10_pd<-lapply(c_inw_1_10, function(k)ripsDiag(torus350_colle_set[[2]][[k]][["noizyX"]], 2, 3, printProgress = T))
plotPDs(trs350_2_cw1_10_pd)

trs350_2_cw1_10_pls<-lapply(1:10, function(k)calcLandscape(trs350_2_cw1_10_pd[[k]]))
plot_lands(trs350_2_cw1_10_pls, 1)

trs_in350_2_cw1_10_pd<-lapply(c_inw_1_10, function(k)ripsDiag(torus350_incolle_set[[2]][[k]][["noizyX"]], 2, 3, printProgress = T))
plotPDs(trs_in350_2_cw1_10_pd)

trs_in350_2_cw1_10_pls<-lapply(1:10, function(k)calcLandscape(trs_in350_2_cw1_10_pd[[k]]))
plot_lands(trs_in350_2_cw1_10_pls, 1)


#新手法の補間の誤差を調べる
trs_in350_2_26_der<-torus_disterror(torus350_incolle_set[[2]][[26]][["noizyX"]], maxr = 2.5, minr = 1, nps = 350)
hist(trs_in350_2_26_der, col="#993435")

oldpar <- par(no.readonly = TRUE)  

trs_in350_1_w1_10_ders<-lapply(inwrng1_10, function(i)torus_disterror(torus350_incolle_set[[2]][[i]][["noizyX"]], maxr = 2.5, minr = 1, nps = 350))
par(mgp=c(2.5,1,0))
boxplot(trs_in350_1_w1_10_ders, xlab="Data Set", ylab="Error", cex.lab=1.6, cex.axis=1.6)


trs_in300_1_1_10_ders<-lapply(1:10, function(i)torus_disterror(torus300_1_1[[i]][["noizyX"]], maxr = 2.5, minr = 1, nps = 300))
par(mgp=c(2.5,1,0))
boxplot(trs_in300_1_1_10_ders, xlab="Data Set", ylab="Error", cex.lab=1.6, cex.axis=1.6)


#サブサンプルのPDを確かめる
##補間前
trs_350subs2_26_subs<-lapply(1:10, function(k){
  data<-torus_colle_set1[[1]][[26]][["noizyX"]][sample(350, 350*0.8),]
  return(data)
})

trs_350subs2_26_subs_pds<-lapply(1:10, function(k)
  ripsDiag(trs_350subs2_26_subs[[k]], maxdimension = 2, maxscale = 3, printProgress = T))

plotPDs(trs_350subs2_26_subs_pds)

trs_350subs2_26_subs_pls<-lapply(1:10, function(k)calcLandscape(trs_350subs2_26_subs_pds[[k]]))
plot_lands(trs_350subs2_26_subs_pls, dim = 1)


##補間後
trs_in350subs2_26_subs<-lapply(1:10, function(k){
  npoints<-nrow(torus350_incolle_set[[2]][[26]][["noizyX"]])
  data<-torus350_incolle_set[[2]][[26]][["noizyX"]][sample(npoints, npoints*0.8),]
  return(data)
})

trs_in350subs2_26_subs_pds<-lapply(1:10, function(k)
  ripsDiag(trs_in350subs2_26_subs[[k]], maxdimension = 2, maxscale = 3, printProgress = T))

plotPDs(trs_in350subs2_26_subs_pds)

trs_in350subs2_26_subs_pls<-lapply(1:10, function(k)calcLandscape(trs_in350subs2_26_subs_pds[[k]]))
plot_lands(trs_in350subs2_26_subs_pls, dim = 1)


#補間点数の差による計算時間差を調べる
#旧手法で
##300点トーラスで試す
t300_a1_intime<-system.time(torus300_incolle_1_a1<-variable_interpo(torus300_colle_set[[1]], 15, 1))

t300_a2_intime<-system.time(torus300_incolle_1_a2<-variable_interpo(torus300_colle_set[[1]], 15, 2))

t300_a3_intime<-system.time(torus300_incolle_1_a3<-variable_interpo(torus300_colle_set[[1]], 15, 3))

t300_a4_intime<-system.time(torus300_incolle_1_a4<-variable_interpo(torus300_colle_set[[1]], 15, 4))

t300_a5_intime<-system.time(torus300_incolle_1_a5<-variable_interpo(torus300_colle_set[[1]], 15, 5))

#6点選んで補う
#6点から開始
t300_a6_intime<-system.time(torus300_incolle_1_a6<-variable_interpo(torus300_colle_set[[1]], 15, 6))

t300_a7_intime<-system.time(torus300_incolle_1_a7<-variable_interpo(torus300_colle_set[[1]], 15, 7))

t300_a8_intime<-system.time(torus300_incolle_1_a8<-variable_interpo(torus300_colle_set[[1]], 15, 8))

torus300_1_test<-all_interpolate(torus300_colle_set[[1]], nvic = 15)

torus300_incolle_1_a6_2<-variable_interpo(torus300_colle_set[[1]], 15, 6)

torus300_incolle_1_a6_2_77<-variable_interpo(list(torus300_colle_set[[1]][[77]]), 15, 6)

intt300_1_a3_time<-system.time(trs300_incolle1_a3_aggr<-proposedMethodOnly(torus300_incolle_1_a3, 2, 3, 10))

intt300_1_a6_time<-system.time(trs300_incolle1_a6_aggr<-proposedMethodOnly(torus300_incolle_1_a6, 2, 3, 10))



## plotly試し

torus350<-torus350_incolle_set[[2]][[26]][["noizyX"]] %>%
  dplyr::as_data_frame() %>% 
  cbind(., c(rep(1, 300), rep(2, 279)))
colnames(torus350)[4]<-"inter"
plotly::plot_ly(torus350, x = ~x, y = ~y, z = ~z, size = 1, color= ~inter, colors = c('#BF382A', '#0C4B8E')) %>% 
plotly::add_markers()
