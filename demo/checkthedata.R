library(dataASPEP)

listoftables<-list(aspep2007_census,aspep2009_sample,aspep2010_sample,aspep2011_sample,aspep2012_census,aspep2013_sample)
names(listoftables)<-paste0("y",c(2007,2009:2013))
sapply(listoftables,nrow)
ids=unique(do.call(rbind,lapply(listoftables,function(l){l[c("id","state","type","itemcode")]})))

ids2<-as.data.frame(do.call(cbind,
              lapply(listoftables,
                     function(l){
                       l$un<-1;
                       a=merge(l[c("id","state","type","itemcode","un")],ids,all=TRUE)})))

names(ids2)<-names(listoftables)
ids2<-ids2[do.call(order,ids2),]
ids3<-ids2[paste0("y",c(2007,2011,2012))]
ids3<-ids3[do.call(order,ids2),]

nrow(ids2)
library("VIM")

table(aspep2007_census$itemcode)


VIM::aggr(ids2)
VIM::aggr(ids3)
