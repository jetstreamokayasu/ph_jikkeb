#もともとの関数
homMethodsComp2compari3 <- function(X,maxdim,maxscale,samples){
  aggr1 <- matrix(0,length(X),5)
  aggr2 <- matrix(0,length(X),5)
  dimnames(aggr1) <- list(paste0("data-set", 1:length(X)), c("confidence","bestconf","landscape","meanland","proposed"))
  dimnames(aggr2) <- dimnames(aggr1)
  
  for(t in 1:length(X)){
    
    cat("data set", t, "calculating\n")
    
    size<-round(X[[t]]$nsample*(4/5))
    B <- bootstrapper(X[[t]][["noizyX"]],size,samples)
    X[[t]]$diag<-calcPhom(X[[t]]$noizyX,maxdim,maxscale, plot = T,ret = T)
    m1 <- calcDiagHomology(attr(B,"hausd"))[2:3]
    m2 <- calcDiagHomology(hausdInterval(X[[t]]$noizyX,nrow(X[[t]]$noizyX)-1))[2:3]
    xland <- lapply(1:maxdim,function(d)landscape(diagram[[1]],d))
    m3 <- sapply(xland,count.local.maximal,T,max(sapply(xland,max))/4)
    mland <- meanLandscape(B,maxdim,maxscale,ret = T)
    m4 <- sapply(1:maxdim,function(d)count.local.maximal(mland[[d]],thresh = max(sapply(xland,max))/4))
    speak <- bootstrap.homology.mk2(B,maxdim,maxscale)
    m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"dhole")]])
    
    aggr1[t,1] <- m1[1]
    aggr1[t,2] <- m2[1]
    aggr1[t,3] <- m3[1]
    aggr1[t,4] <- m4[1]
    aggr1[t,5] <- m5[1]
    
    aggr2[t,1] <- m1[2]
    aggr2[t,2] <- m2[2]
    aggr2[t,3] <- m3[2]
    aggr2[t,4] <- m4[2]
    aggr2[t,5] <- m5[2]
  }
  Xdiag<-lapply(1:length(X), function(k){return(X[[k]]$diag)})
  aggrs <- list(aggr1,aggr2)
  aggrs <- append(aggrs,list(Xsize=nrow(X[[1]]),Xsamples=length(X),
                             Bsize=size,Bsamples=samples,
                             maxdim=maxdim,maxscale=maxscale, Xdiag=Xdiag))
  class(aggrs) <- "bettiComp"
  
  return(aggrs)
}

#信頼区間、PL、MLによるベッチ数推定関数

conventional_betti_estimate <- function(X,maxdim,maxscale,samples){
  aggr1 <- matrix(0,length(X),4)
  aggr2 <- matrix(0,length(X),4)
  dimnames(aggr1) <- list(paste0("data-set", 1:length(X)), c("confidence","bestconf","landscape","meanland"))
  dimnames(aggr2) <- dimnames(aggr1)
  
  for(t in 1:length(X)){
    
    cat("data set", t, "calculating\n")
    
    size<-round(X[[t]]$nsample*(4/5))
    B <- bootstrapper(X[[t]][["noizyX"]],size,samples)
    X[[t]]$diag<-calcPhom(X[[t]]$noizyX,maxdim,maxscale, plot = T,ret = T)
    m1 <- calcDiagHomology(attr(B,"hausd"))[2:3]
    m2 <- calcDiagHomology(hausdInterval(X[[t]]$noizyX,nrow(X[[t]]$noizyX)-1))[2:3]
    xland <- lapply(1:maxdim,function(d)landscape(diagram[[1]],d))
    m3 <- sapply(xland,count.local.maximal,T,max(sapply(xland,max))/4)
    mland <- meanLandscape(B,maxdim,maxscale,ret = T)
    m4 <- sapply(1:maxdim,function(d)count.local.maximal(mland[[d]],thresh = max(sapply(xland,max))/4))
    #speak <- bootstrap.homology.mk2(B,maxdim,maxscale)
    #m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"dhole")]])
    
    aggr1[t,1] <- m1[1]
    aggr1[t,2] <- m2[1]
    aggr1[t,3] <- m3[1]
    aggr1[t,4] <- m4[1]
    #aggr1[t,5] <- m5[1]
    
    aggr2[t,1] <- m1[2]
    aggr2[t,2] <- m2[2]
    aggr2[t,3] <- m3[2]
    aggr2[t,4] <- m4[2]
    #aggr2[t,5] <- m5[2]
  }
  #Xdiag<-lapply(1:length(X), function(k){return(X[[k]]$diag)})
  aggrs <- list(aggr1,aggr2)
  aggrs <- append(aggrs,list(Xsize=nrow(X[[1]]),Xsamples=length(X),
                             Bsize=size,Bsamples=samples,
                             maxdim=maxdim,maxscale=maxscale))
  class(aggrs) <- "bettiComp"
  
  return(aggrs)
}




