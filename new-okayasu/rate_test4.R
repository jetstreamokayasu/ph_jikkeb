require(phacm)
require(tidyverse)
require(TDA)
require(myfs)
require(rgl)
library(seephacm)
library(tictoc)
library(devtools)
library(interpo3d)

#実行時間を計測する
#seephacmも試す

#350点トーラス
torus.collect18<- lapply(1:100, function(i){
  nsample <- 350
  #var <- runif(1, var.min, var.max)
  #noize.torus <- matrix(rnorm(nsample * 3, 0, var), nrow = nsample)
  torus <- torusUnif(nsample, 1, 2.5)
  return(list(nsample = nsample, noizyX = torus, diag = 0))
})

save2Rdata(torus.collect18)

figurePlot(torus.collect18[[1]][["noizyX"]])

#実行時間計測
{
tic()

torus18_aggr<-proposedMethodOnly(torus.collect18, maxdim = 2, maxscale = 3, samples = 10)

toc()
}

time<-system.time(torus18_test<-proposedMethodOnly(list(torus.collect18[[1]]), maxdim = 2, maxscale = 3, samples = 10))

#350点トーラス4セット
torus_colle_set1<-lapply(1:4, function(j){
  
  torus_collect<- lapply(1:100, function(i){
    nsample <- 350
    #var <- runif(1, var.min, var.max)
    #noize.torus <- matrix(rnorm(nsample * 3, 0, var), nrow = nsample)
    torus <- torusUnif(nsample, 1, 2.5)
    return(list(nsample = nsample, noizyX = torus, diag = 0))
  })
  
  return(torus_collect)
  
})
save2Rdata(torus_colle_set1)


times_list<-lapply(1:2, function(k){
  
  time<-system.time(torus18_test<-proposedMethodOnly(list(torus.collect18[[k]]), maxdim = 2, maxscale = 3, samples = 10))
  
})



torus_colset1_aggrs<-lapply(1:4, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus_colle_set1[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))

})
save2Rdata(torus_colset1_aggrs)

torus350_colset_rate<-aggrSuccessRates(torus_colset1_aggrs, correct = c(2, 1))

#350点トーラス補間
t350_intime<-system.time(torus350_incolle_set<-lapply(torus350_colle_set, function(k)all_interpolate(k, 15)))
save2Rdata(t350_intime)

torus350_incolle_set_5<-all_interpolate(collect = torus.collect18, nvic = 15)
save2Rdata(torus350_incolle_set)

torus350_colle_set<-append(torus_colle_set1, list(torus.collect18))
save2Rdata(torus350_colle_set)

torus350_incolle_aggrs<-lapply(1:5, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus350_incolle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus350_incolle_aggrs)

torus350_incolle_set3<-all_interpolate(torus_colle_set1[[3]])
torus350_incolle_set4<-all_interpolate(torus_colle_set1[[4]])
torus350_incolle_set4_1<-voronoiInterpo(torus_colle_set1[[4]][[1]][[2]], 15)

#340点トーラス5セット
torus340_colle_set<-lapply(1:5, function(j){
  
  torus_collect<- lapply(1:100, function(i){
    nsample <- 340
    #var <- runif(1, var.min, var.max)
    #noize.torus <- matrix(rnorm(nsample * 3, 0, var), nrow = nsample)
    torus <- torusUnif(nsample, 1, 2.5)
    return(list(nsample = nsample, noizyX = torus, diag = 0))
  })
  
  return(torus_collect)
  
})
save2Rdata(torus340_colle_set)

torus340_colset_aggrs<-lapply(1:5, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus340_colle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus340_colset_aggrs)

torus340_colset_rate<-aggrSuccessRates(torus340_colset_aggrs, correct = c(2, 1))

#340点トーラス補間
t340_intime<-system.time(torus340_incolle_set<-lapply(torus340_colle_set, function(k)all_interpolate(k, 15)))
save2Rdata(t340_intime)

save2Rdata(torus340_incolle_set)

##340点トーラス補間後1~3セット目を推定
torus340_incolle13_aggrs<-lapply(1:3, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus340_incolle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus340_incolle13_aggrs)

#330点トーラス5セット
torus330_colle_set<-lapply(1:5, function(j){
  
  torus_collect<- lapply(1:100, function(i){
    nsample <- 330
    #var <- runif(1, var.min, var.max)
    #noize.torus <- matrix(rnorm(nsample * 3, 0, var), nrow = nsample)
    torus <- torusUnif(nsample, 1, 2.5)
    return(list(nsample = nsample, noizyX = torus, diag = 0))
  })
  
  return(torus_collect)
  
})
save2Rdata(torus330_colle_set)

torus330_colset_aggrs<-lapply(1:5, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus330_colle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus330_colset_aggrs)

#330点トーラス補間後
t330_intime<-system.time(torus330_incolle_set<-lapply(torus330_colle_set, function(k)all_interpolate(k, 15)))
save2Rdata(t330_intime)

save2Rdata(torus330_incolle_set)

##330点トーラス補間後1~3セット目を推定
torus330_incolle13_aggrs<-lapply(1:3, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus330_incolle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus330_incolle13_aggrs)



#320点トーラス5セット
torus320_colle_set<-lapply(1:5, function(j){
  
  torus_collect<- lapply(1:100, function(i){
    nsample <- 320
    #var <- runif(1, var.min, var.max)
    #noize.torus <- matrix(rnorm(nsample * 3, 0, var), nrow = nsample)
    torus <- torusUnif(nsample, 1, 2.5)
    return(list(nsample = nsample, noizyX = torus, diag = 0))
  })
  
  return(torus_collect)
  
})
save2Rdata(torus320_colle_set)

torus320_colset_aggrs<-lapply(1:5, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus320_colle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus320_colset_aggrs)

#320点トーラス補間後
t320_intime<-system.time(torus320_incolle_set<-lapply(torus320_colle_set, function(k)all_interpolate(k, 15)))
save2Rdata(t320_intime)

save2Rdata(torus320_incolle_set)

##330点トーラス補間後1~3セット目を推定
torus320_incolle13_aggrs<-lapply(1:3, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus320_incolle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus320_incolle13_aggrs)



#300点トーラス5セット
torus300_colle_set<-lapply(1:5, function(j){
  
  torus_collect<- lapply(1:100, function(i){
    nsample <- 300
    #var <- runif(1, var.min, var.max)
    #noize.torus <- matrix(rnorm(nsample * 3, 0, var), nrow = nsample)
    torus <- torusUnif(nsample, 1, 2.5)
    return(list(nsample = nsample, noizyX = torus, diag = 0))
  })
  
  return(torus_collect)
  
})
save2Rdata(torus300_colle_set)

torus300_colset_aggrs<-lapply(1:5, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus300_colle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus300_colset_aggrs)

trs300_1_time<-system.time(trs300_1_aggr<-proposedMethodOnly(torus300_colle_set[[1]], 2, 3, 10))
save2Rdata(trs300_1_aggr)

#300点トーラス補間後
torus300_1_1<-all_interpolate(torus300_colle_set[[1]])

intt300_1_time<-system.time(intrs300_1_aggr<-proposedMethodOnly(torus300_1_1, 2, 3, 10))
save2Rdata(intrs300_1_aggr)

inter_time_2<-system.time(intrs300_2<-all_interpolate(torus300_colle_set[[2]]))
figurePlot(torus300_colle_set[[2]][[1]][["noizyX"]])
points3d(intrs300_2[[1]][[2]][301:487, ], col=2)

nsample_intrs300_2<-lapply(intrs300_2, function(intrs){intrs[["nsample"]]})


#