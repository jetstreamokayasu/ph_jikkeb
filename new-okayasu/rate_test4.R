require(phacm)
require(tidyverse)
require(TDA)
require(myfs)
require(rgl)
library(seephacm)
library(tictoc)

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
save2Rdata(torus340_colle_set)

torus340_colset_aggrs<-lapply(1:5, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus340_colle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus340_colset_aggrs)


