library(pubBonneryLahiriTran2016)
library(sqldf)
library(R2jags)
library(dataASPEP)
library(ggplot2)
data(aspep2007,aspep2011,aspep2012,aspep2007_gov)
xy<-xyf(aspep2007, aspep2012, aspep2007_gov)
xys<-xysf(aspep2011,xy)
xr<-xrf(xy,xys)
xys_aggrd<-sqldf("select count(*) as n, state,itemcode, type_of_gov, sum(lftemp12) as lftemp12, sum(lftemp07) as lftemp07 
                 from xys group by type_of_gov, itemcode, state")
#plot(lm(lftemp12~lftemp07,data=xy))
plot(lm(ftemp12~ftemp07,data=xy))
ggplot(data=xy,aes(x=ftemp07,y=ftemp12))+geom_point()+geom_abline(color="red",intercept=0,slope=1)
ggplot(data=xy,aes(x=ftemp07,y=ftemp12))+geom_point()+geom_abline(color="red",intercept=0,slope=1)
ggplot(data=xy,aes(x=lftemp07,y=lftemp12))+geom_point()+geom_abline(color="red",intercept=0,slope=1)

plot1<-ggplot(data = xy, aes(x = ftemp07, y = ftemp12)) +
#  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(method="lm",se=FALSE) +
  geom_point() + facet_wrap(~type_of_gov)

save(plot1,"figure/plot1.rda")

plot(lm(log(1+lftemp12)~log(1+lftemp07),data=xy))
