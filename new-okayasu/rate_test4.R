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
{
tic()

torus18_aggr<-proposedMethodOnly(list(torus.collect18[[1]]), maxdim = 2, maxscale = 3, samples = 10)

toc()
}
