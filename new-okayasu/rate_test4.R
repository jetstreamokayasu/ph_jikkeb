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

figurePlot(torus350_incolle_set_5[[1]][["noizyX"]][1:350,])
rgl.snapshot("./data/trs350_colle_set_5_1.png")
points3d(torus350_incolle_set_5[[1]][["noizyX"]][351:556,], col=2)
rgl.snapshot("./data/trs350_incolle_set_5_1.png")

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

##320点トーラス補間後1~3セット目を推定
torus320_incolle13_aggrs<-lapply(1:3, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus320_incolle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus320_incolle13_aggrs)


#310点トーラス5セット
torus310_colle_set<-lapply(1:5, function(j){
  
  torus_collect<- lapply(1:100, function(i){
    nsample <- 310
    #var <- runif(1, var.min, var.max)
    #noize.torus <- matrix(rnorm(nsample * 3, 0, var), nrow = nsample)
    torus <- torusUnif(nsample, 1, 2.5)
    return(list(nsample = nsample, noizyX = torus, diag = 0))
  })
  
  return(torus_collect)
  
})
save2Rdata(torus310_colle_set)

torus310_colset_aggrs<-lapply(1:5, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus320_colle_set[[k]], 2, 3, 10))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus310_colset_aggrs)

#310点トーラス補間後
t310_intime<-system.time(torus310_incolle_set<-lapply(torus310_colle_set, function(k)all_interpolate(k, 15)))
save2Rdata(t310_intime)

save2Rdata(torus310_incolle_set)

##310点トーラス補間後1~3セット目を推定
torus310_incolle13_aggrs<-lapply(1:3, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-proposedMethodOnly(torus310_incolle_set[[k]], 2, 3, 10))
  save(list = aggr, file = paste0("./data/in310_aggr_", k, ".RData"))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus310_incolle13_aggrs)



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


#推定成功率まとめ
#2次ベッチ数
#補間後
torus350_incolle_rate<-aggr_success_rates(torus350_incolle_aggrs, c(2,1))
torus340_incolle13_rate<-aggr_success_rates(torus340_incolle13_aggrs, c(2,1))
torus330_incolle13_rate<-aggr_success_rates(torus330_incolle13_aggrs, c(2,1))
torus320_incolle13_rate<-aggr_success_rates(torus320_incolle13_aggrs, c(2,1))
torus300_1_1_rate<-aggr_success_rates(list(intrs300_1_aggr), c(2,1))

in350_rates<-do.call(rbind, torus350_incolle_rate)
in340_rates<-do.call(rbind, torus340_incolle13_rate)
in330_rates<-do.call(rbind, torus330_incolle13_rate)
in320_rates<-do.call(rbind, torus320_incolle13_rate)

in300_1_rate<-do.call(rbind, torus300_1_1_rate)

insub_rates<-list("300"=in300_1_rates,
                  "320"=in320_rates,
                  "330"=in330_rates,
                  "340"=in340_rates,
                  "350"=in350_rates)

points(rep(300, 1), in300_1_rates[,2], col=2, pch=16)
points(rep(310, 5), insub310.rate, col=2, pch=16)
points(rep(320, 3), in320_rates[,2], col=2, pch=16)
points(rep(330, 3), in330_rates[,2], col=2, pch=16)
points(rep(340, 3), in340_rates[,2], col=2, pch=16)
points(rep(350, 5), in350_rates[,2], col=2, pch=16)

in_dim2_mean<-sapply(insub_rates, function(rate)mean(unlist(rate[,2])))
lines(c(300, seq(320, 350, by=10)), in_dim2_mean, col=2)

in_dim2_sd<-sapply(insub_rates, function(rate)sd(unlist(rate[,2])))
lines(c(300, seq(320, 350, by=10)), in_dim2_mean-in_dim2_sd, lty="dashed", col=2)
lines(c(300, seq(320, 350, by=10)), in_dim2_mean+in_dim2_sd, lty="dashed", col=2)


#1次ベッチ数
#補間前
suc300_rates<-aggr_success_rates(torus15.300subs.aggrs, c(2,1)) %>% do.call(rbind, .)
suc310_rates<-aggr_success_rates(torus15.310subs.aggrs, c(2,1)) %>% do.call(rbind, .)
suc320_rates<-do.call(cbind, torus15.320rates[["320sub"]])
suc330_rates<-do.call(cbind, torus15.330rates[["330sub"]])
suc340_rates<-do.call(cbind, torus15.340rates[["340sub"]])
#suc350_rates<-do.call(cbind, torus15.350rates[["350sub"]])
        
suc350_rates_dim1<-sapply(torus15.350rates[["350sub"]][["dim1"]], function(rate){return(rate[1])})
suc350_rates_dim2<-sapply(torus15.350rates[["350sub"]][["dim2"]], function(rate){return(rate[1])})

suc350_rates<-data.frame(dim1rate=suc350_rates_dim1, dim2rate=suc350_rates_dim2)

sub_rates<-list("300"=suc300_rates,
                "310"=suc310_rates,
                "320"=suc320_rates,
                "330"=suc330_rates,
                "340"=suc340_rates,
                "350"=suc350_rates)


plot(sucrate.dim2.tidy_2, pch=16, cex.axis=1.6, xlab="Data Density", ylab="Success Rates", cex.lab=1.6, ylim=c(0.2, 1.0), xaxt="n", type="n")
axis(side=1, at=seq(300, 350, by=10), labels=c(paste0(seq(30, 35), "/(pi^2)")), cex.axis=1.1)

points(rep(300, 5), suc300_rates[,1], pch=16)
points(rep(310, 5), suc310_rates[,1], pch=16)
points(rep(320, 5), suc320_rates[,1], pch=16)
points(rep(330, 5), suc330_rates[,1], pch=16)
points(rep(340, 5), suc340_rates[,1], pch=16)
points(rep(350, 5), suc350_rates[,1], pch=16)

dim1_mean<-sapply(sub_rates, function(rate)mean(unlist(rate[,1])))
lines(seq(300, 350, by=10), dim1_mean)

dim1_sd<-sapply(sub_rates, function(rate)sd(unlist(rate[,1])))
lines(seq(300, 350, by=10), dim1_mean-dim1_sd, lty="dashed")
lines(seq(300, 350, by=10), dim1_mean+dim1_sd, lty="dashed")

#補間後
points(rep(300, 1), in300_1_rates[,1], col=2, pch=16)
points(rep(310, 5), insub310.rate, col=2, pch=16)
points(rep(320, 3), in320_rates[,1], col=2, pch=16)
points(rep(330, 3), in330_rates[,1], col=2, pch=16)
points(rep(340, 3), in340_rates[,1], col=2, pch=16)
points(rep(350, 5), in350_rates[,1], col=2, pch=16)

in_dim1_mean<-sapply(insub_rates, function(rate)mean(unlist(rate[,1])))
lines(c(300, seq(320, 350, by=10)), in_dim1_mean, col=2)

in_dim1_sd<-sapply(insub_rates, function(rate)sd(unlist(rate[,1])))
lines(c(300, seq(320, 350, by=10)), in_dim1_mean-in_dim1_sd, lty="dashed", col=2)
lines(c(300, seq(320, 350, by=10)), in_dim1_mean+in_dim1_sd, lty="dashed", col=2)

#旧手法
#1次ベッチ数補間後

insub300_rate13<-aggr_success_rates(list(torus15.300insubs1.aggr, torus15.300insubs2.aggr, torus15.300insubs3.aggr), c(2,1))

insub300.rate<-append(insub300_rate13, torus15.300insubs4_5rate) %>% do.call(rbind, .)

insub310.rate<-aggr_success_rates(c(torus15.310insubs1_3.aggrs, torus15.310insubs4_5.aggrs), c(2,1)) %>% do.call(rbind, .)

insub320.rate<-aggr_success_rates(c(torus15.320insubs1_3.aggrs, torus15.320insubs4_5.aggrs), c(2,1)) %>% do.call(rbind, .)

insub330.rate<-aggr_success_rates(c(torus15.330insubs1_3.aggrs, torus15.330insubs4_5.aggrs), c(2,1)) %>% do.call(rbind, .)

insub340.rate<-aggr_success_rates(c(torus15.340insubs1_3.aggrs, torus15.340insubs4_5.aggrs), c(2,1)) %>% do.call(rbind, .)

insub350.rate<-aggr_success_rates(c(torus15.350insubs1_3.aggrs, torus15.350insubs4_5.aggrs), c(2,1)) %>% do.call(rbind, .)


insub.rates<-list("300"=insub300.rate,
                  "310"=insub310.rate,
                  "320"=insub320.rate,
                  "330"=insub330.rate,
                  "340"=insub340.rate,
                  "350"=insub350.rate)

save2Rdata(insub.rates)

points(rep(300, 5), insub300.rate[,1], col=4, pch=16)
points(rep(310, 5), insub310.rate[,1], col=4, pch=16)
points(rep(320, 5), insub320.rate[,1], col=4, pch=16)
points(rep(330, 5), insub330.rate[,1], col=4, pch=16)
points(rep(340, 5), insub340.rate[,1], col=4, pch=16)
points(rep(350, 5), insub350.rate[,1], col=4, pch=16)

insubdim1.mean<-sapply(insub.rates, function(rate)mean(unlist(rate[,1])))
lines(seq(300, 350, by=10), insubdim1.mean, col=4)

insubdim1.sd<-sapply(insub.rates, function(rate)sd(unlist(rate[,1])))
lines(seq(300, 350, by=10), insubdim1.mean-insubdim1.sd, lty="dashed", col=4)
lines(seq(300, 350, by=10), insubdim1.mean+insubdim1.sd, lty="dashed", col=4)

#旧手法
#2次ベッチ数補間後
points(rep(300, 5), insub300.rate[,2], col=4, pch=16)
points(rep(310, 5), insub310.rate[,2], col=4, pch=16)
points(rep(320, 5), insub320.rate[,2], col=4, pch=16)
points(rep(330, 5), insub330.rate[,2], col=4, pch=16)
points(rep(340, 5), insub340.rate[,2], col=4, pch=16)
points(rep(350, 5), insub350.rate[,2], col=4, pch=16)

insubdim2.mean<-sapply(insub.rates, function(rate)mean(unlist(rate[,2])))
lines(seq(300, 350, by=10), insubdim2.mean, col=4)

insubdim2.sd<-sapply(insub.rates, function(rate)sd(unlist(rate[,2])))
lines(seq(300, 350, by=10), insubdim2.mean-insubdim2.sd, lty="dashed", col=4)
lines(seq(300, 350, by=10), insubdim2.mean+insubdim2.sd, lty="dashed", col=4)
