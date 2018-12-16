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

#サブサンプルに補間
torus15.300insubs<-lapply(torus15.300subs, function(sub)intering(sub))
save(torus15.300insubs, file="./data/torus15_300insubs.RData")
##############################

#補間したサブサンプルを分析
torus15.300subs4_5.aggrs<-lapply(4:5, function(k){
  cat("list", k, "calc\n")
  return(proposedMethodOnly(torus15.300insubs[[k]], 2, 3, 10))})

save(torus15.300subs2_3.aggrs, file="./data/torus15_300subs2_3_aggrs.RData")

torus15.300insubs2_3.rate<-aggrSuccessRates(torus15.300subs2_3.aggrs, correct = c(2,1))