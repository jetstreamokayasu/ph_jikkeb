#信頼区間、PLとの比較

##350点トーラス1~3セットのベッチ数推定
{
  torus350_colle_conv_aggrs<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-conventional_betti_estimate(torus350_colle_set[[k]], 2, 3, 10))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus350_colle_conv_aggrs)
}