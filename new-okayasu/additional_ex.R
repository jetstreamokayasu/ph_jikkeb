library(parallel)


sphere.collect12<-lapply(1:100, function(i){
  sphere <- sphereUnif(500, 2, 1)
  #cat("nsample=", nsample, "data", i, "\n")
  return(list(nsample = 500, noizyX = sphere, diag = 0))
})
save(sphere.collect12, file = "./data/sphere.collect12")
plot3d(sphere.collect12[[1]][["noizyX"]])

sphere.aggr12<-proposedMethodOnly(X=sphere.collect12, maxdim = 2, maxscale = 3, samples = 10)

#parallelテスト
circles <- lapply(1:12,function(x)circleUnif(n = 200))
cl <- makeCluster(2)
parl<-parLapply(cl,circles,ripsDiag,1,1)
stopCluster(cl)
noparl<-lapply(circles,ripsDiag,1,1)


sphere.aggr12.test<-proposedMethodOnly.parallel(X=sphere.collect12[1], maxdim = 2, maxscale = 3, samples = 2)
sphere.aggr12<-proposedMethodOnly.parallel(X=sphere.collect12, maxdim = 2, maxscale = 3, samples = 10)


ahiru.collect<-list(list(nsample = nrow(ahiru.mat), noizyX = ahiru.mat, diag = 0))
ahiru.aggr<-homMethodsComp2compari3(ahiru.collect, 1, 10, 10)

ahiru.dim1<-cyclenumber(ahiru.aggr[[1]])


#ノイズトーラスKDEによるサイクル推定
collect.aggr<-homMethodsKDE(X = data.collect, maxdim = 2)
collect.dim1<-cyclenumber(collect.aggr[[1]], compare=F)
collect.dim2<-cyclenumber(collect.aggr[[2]], compare=F)

collect.arrange<-plotAggr(collect.aggr, data.collect, 2, 1, compare=F, capture="KDE")

collect.dom1<-plotAggr(aggr = hole.aggr2, collect = data.collect, correct = 2, dim = 1)
collect.dim2<-plotAggr(aggr = hole.aggr2, collect = data.collect, correct = 1, dim = 2)


########アヒルが環状データとはどういうことか
ahiru.pca<-prcomp(ahiru.mat)
plot3d(ahiru.pca[["x"]][,1:3])
text3d(ahiru.pca[["x"]][,1:3], texts = 1:72)
plot(ahiru.pca[["x"]][,1:2])
rgl.snapshot("./data/ahiru3d_2.png")  
