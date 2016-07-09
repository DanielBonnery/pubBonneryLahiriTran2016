#0.Libraries
library(pubBonneryLahiriTran2016)
library(sqldf)
library(R2jags)
library(dataASPEP)
library(ggplot2)
data(aspep2007,aspep2011,aspep2012,aspep2007_gov)
xy<-xyf(aspep2007, aspep2012, aspep2007_gov)
xys<-xysf(aspep2011,xy)
xr<-xrf(xy,xys)

toto=function(d){c(lftemp07=sum(d$lftemp07),
                   ftemp07=sum(d$ftemp07),
                   lftemp12=sum(d$lftemp12),
                   ftemp12=sum(d$ftemp12))}
#Aggregate.
xys_aggr<-plyr::daply(.data=xys,.variables=~state+itemcode+type_of_gov,.fun = toto)
xys_aggr[is.na(xys_aggr)]<-0
xy_aggr<-plyr::daply(.data=xy,.variables=~state+itemcode+type_of_gov,.fun = toto)
xy_aggr[is.na(xy_aggr)]<-0
xr_aggr<-xy_aggr-xys_aggr

xys_aggrd<-sqldf("select count(*) as n, state,itemcode, type_of_gov, sum(lftemp12) as lftemp12, sum(lftemp07) as lftemp07 
                 from xys group by type_of_gov, itemcode, state")

save(xy_aggr,xys_aggr,xy_aggrd,xr_aggr,file="data/summarytab.rda")
load("data/summarytab.rda")