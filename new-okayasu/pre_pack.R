#パーッケジを作るためにいろいろテスト
trs15_300_1_1_phapd<-compute_pd(torus15.300subs[[1]][[1]][["noizyX"]], 2, 3)

autoplot(trs15_300_1_1_phapd)

trs15_300_1_1_mean<-zero_hat_double_threshold(trs15_300_1_1_phapd)

trs15_300_1_1_phapl<-compute_pl(trs15_300_1_1_phapd)
autoplot(trs15_300_1_1_phapl)

trs15_300_1_1_phaspl<-compute_smooth_pl(trs15_300_1_1_phapl)
autoplot(trs15_300_1_1_phaspl)

trs15_300_1_1_cycles<-count_smooth_maximal(trs15_300_1_1_phapl)
trs15_300_1_1_cycles2<-count_smooth_maximal(trs15_300_1_1_phapl, exist.method = per_mean, cutoff.method = per_mean)

trs15_300_1_1_cycles3<-count_local_maximal(trs15_300_1_1_phaspl, thresh = zero_hat_double_threshold(trs15_300_1_1_phapd)/2)

torus.collect16<- lapply(1:5, function(i){
  torus <- torusUnif(300, 1, 2.5)
})

trus16_aggr<-calc_bettis(torus.collect16, maxdim = 2, maxscale = 3, samples = 10)
