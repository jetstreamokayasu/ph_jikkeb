require(phacm)
require(tidyverse)
require(TDA)
require(myfs)
require(rgl)
library(seephacm)
library(tictoc)
library(devtools)
library(interpo3d)

#ボロノイ領域の中心点から最も遠い頂点に補うように変更
#成功率を調べる

intrs300_1<-all_interpolate2(collect = torus300_colle_set[[1]], nvic = 15)

intt300_1_time2<-system.time(intrs300_1_aggr2<-proposedMethodOnly(intrs300_1, 2, 3, 10))


intrs300_1_2nd<-all_interpolate2(collect = torus300_colle_set[[1]], nvic = 15)

intt300_1_time3<-system.time(intrs300_1_aggr3<-proposedMethodOnly(intrs300_1, 2, 3, 10))
