#データ密度と推定制度の関係を調べる
#とりあえず500点トーラスから
#ごちゃごちゃになってしまったので整理

require(phacm)
require(tidyverse)
require(TDA)
require(myfs)
require(rgl)
require(boot)
require(doParallel)
require(foreach)
require(plotly)
require(viridis)
require(pterrace)
require(phacm)

torus.collect15<- lapply(1:100, function(i){
  nsample <- 500
  #var <- runif(1, var.min, var.max)
  #noize.torus <- matrix(rnorm(nsample * 3, 0, var), nrow = nsample)
  torus <- torusUnif(nsample, 1, 2.5)
  return(list(nsample = nsample, noizyX = torus, diag = 0))
})
save(torus.collect15, file = "./data/torus.collect15")

torus15.aggr<-proposedMethodOnly(torus.collect15, 2, 3, 10)
save(torus.collect15, file = "./data/torus15.aggr")

#サブサンプルリストを作る3############
torus15.310subs<-lapply(1:5, function(k)subsampleExclude(torus.collect15, nsub = 310))
save(torus15.310subs, file="./data/torus15_310subs.RData")

torus15.300subs<-lapply(1:5, function(k)subsampleExclude(torus.collect15, nsub = 300))
save(torus15.300subs, file="./data/torus15_300subs.RData")

######################################

#サブサンプルを分析##############
torus15.300subs.aggrs<-lapply(1:length(torus15.300subs), function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.300subs[[k]], 2, 3, 10))})
save(torus15.300subs.aggrs, file="./data/torus15_300subs_aggrs.RData")

torus15.300subs.rate<-aggrSuccessRates(torus15.300subs.aggrs, correct = c(2,1))

###############################

#サブサンプルに補間
torus15.300insubs<-lapply(torus15.300subs, function(sub)intering(sub))
save(torus15.300insubs, file="./data/torus15_300insubs.RData")

torus15.310insubs<-lapply(torus15.310subs, function(sub)intering(sub))
save2Rdata(torus15.310insubs)
##############################

#補間したサブサンプルを分析
torus15.300subs4_5.aggrs<-lapply(4:5, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.300insubs[[k]], 2, 3, 10))})

save(torus15.300subs4_5.aggrs, file="./data/torus15_300subs4_5_aggrs.RData")

torus15.300insubs4_5rate<-aggrSuccessRates(torus15.300subs4_5.aggrs, correct = c(2,1))

#凸包外にある補間点を除く補間
torus15.300insubs1_3<-lapply(torus15.300subs[1:3], function(sub)intering(sub))
save2Rdata(torus15.300insubs1_3)

torus15.300sub1_27<-voronoiInterpo(torus15.300subs[[1]][[27]][["noizyX"]], 15)
torus15.300sub2_56<-voronoiInterpo(torus15.300subs[[2]][[56]][["noizyX"]], 15)

torus15.300insubs1_3.aggrs<-lapply(1:3, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.300insubs1_3[[k]], 2, 3, 10))})

torus15.300sub1_36<-proposedMethodOnly(torus15.300insubs1_3[[1]][36], maxdim = 2, maxscale = 3, samples = 10)

torus15.300insubs2.aggr<-proposedMethodOnly(torus15.300insubs1_3[[2]], maxdim = 2, maxscale = 3, samples = 10)
save2Rdata(torus15.300insubs2.aggr)
torus15.300insubs2.rate<-aggrSuccessRates(list(torus15.300insubs2.aggr), correct=c(2,1))

torus15.300insubs1.aggr<-proposedMethodOnly(torus15.300insubs1_3[[1]], maxdim = 2, maxscale = 3, samples = 10)
save2Rdata(torus15.300insubs1.aggr)
torus15.300insubs1.rate<-aggrSuccessRates(list(torus15.300insubs1.aggr), c(2,1))

torus15.300insubs3.aggr<-proposedMethodOnly(torus15.300insubs1_3[[3]], maxdim = 2, maxscale = 3, samples = 10)
save2Rdata(torus15.300insubs3.aggr)
torus15.300insubs3.rate<-aggrSuccessRates(list(torus15.300insubs3.aggr), c(2,1))

#310点
torus15.310insubs1_3.aggrs<-lapply(1:3, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.310insubs[[k]], 2, 3, 10))})
save2Rdata(torus15.310insubs1_3.aggrs)

torus15.310insubs1_3.rates<-aggrSuccessRates(torus15.310insubs1_3.aggrs, c(2,1))

torus15.310insubs4_5.aggrs<-lapply(4:5, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.310insubs[[k]], 2, 3, 10))})
save2Rdata(torus15.310insubs4_5.aggrs)

torus15.310insubs4_5.rates<-aggrSuccessRates(torus15.310insubs4_5.aggrs, c(2,1))

#320点
torus15.320subs<-lapply(1:5, function(k)subsampleExclude(torus.collect15, nsub = 320))
save(torus15.320subs, file="./data/torus15_320subs.RData")

torus15.320insubs<-lapply(torus15.320subs, function(sub)intering(sub))
save2Rdata(torus15.320insubs)

torus15.320insubs1_3.aggrs<-lapply(1:3, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.320insubs[[k]], 2, 3, 10))})
save2Rdata(torus15.320insubs1_3.aggrs)

torus15.320insubs1_3.rates<-aggrSuccessRates(torus15.320insubs1_3.aggrs, c(2,1))

torus15.320insubs4_5.aggrs<-lapply(4:5, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.320insubs[[k]], 2, 3, 10))})
save2Rdata(torus15.320insubs4_5.aggrs)

torus15.320insubs4_5.rates<-aggrSuccessRates(torus15.320insubs4_5.aggrs, c(2,1))

#330点
torus15.330subs<-lapply(1:5, function(k)subsampleExclude(torus.collect15, nsub = 330))
save2Rdata(torus15.330subs)

torus15.330insubs<-lapply(torus15.330subs, function(sub)intering(sub))
save2Rdata(torus15.330insubs)

torus15.330insubs1_3.aggrs<-lapply(1:3, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.330insubs[[k]], 2, 3, 10))})
save2Rdata(torus15.330insubs1_3.aggrs)

torus15.330insubs1_3.rates<-aggrSuccessRates(torus15.330insubs1_3.aggrs, c(2,1))

torus15.330insubs4_5.aggrs<-lapply(4:5, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.330insubs[[k]], 2, 3, 10))})
save2Rdata(torus15.330insubs4_5.aggrs)

torus15.330insubs4_5.rates<-aggrSuccessRates(torus15.330insubs4_5.aggrs, c(2,1))

#340点
torus15.340subs<-lapply(1:5, function(k)subsampleExclude(torus.collect15, nsub = 340))
save2Rdata(torus15.340subs)

torus15.340insubs<-lapply(torus15.340subs, function(sub)intering(sub))
save2Rdata(torus15.340insubs)

torus15.340insubs1_3.aggrs<-lapply(1:3, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.340insubs[[k]], 2, 3, 10))})
save2Rdata(torus15.340insubs1_3.aggrs)

torus15.340insubs1_3.rates<-aggrSuccessRates(torus15.340insubs1_3.aggrs, c(2,1))

#350点
torus15.350subs<-lapply(1:5, function(k)subsampleExclude(torus.collect15, nsub = 350))
save2Rdata(torus15.350subs)

torus15.350insubs<-lapply(torus15.350subs, function(sub)intering(sub))
save2Rdata(torus15.350insubs)

torus15.350insubs1_3.aggrs<-lapply(1:3, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.350insubs[[k]], 2, 3, 10))})
save2Rdata(torus15.350insubs1_3.aggrs)

torus15.350insubs1_3.rates<-aggrSuccessRates(torus15.350insubs1_3.aggrs, c(2,1))

