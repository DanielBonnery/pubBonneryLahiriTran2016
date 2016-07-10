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
  geom_point(size=1) +   geom_smooth(method="lm",se=FALSE) +geom_abline(slope=1,intercept=0,color="red")+
facet_wrap(~type_of_gov)
save(plot1,file="figure/plot1.rda")

plot2<-ggplot(data = xy, aes(x = lftemp07, y = lftemp12)) +
  geom_point() +   geom_smooth(method="lm",se=FALSE) +geom_abline(slope=1,intercept=0,color="red")+
facet_wrap(~type_of_gov)
save(plot2,file="figure/plot2.rda")

plot2bis<-ggplot(data = xy, aes(x = lftemp07, y = lftemp12)) + 
  stat_density2d(geom="tile", aes(fill=..density..^0.1), contour=FALSE) +
  scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256)) +
  geom_smooth(method="lm",se=FALSE) +geom_abline(slope=1,intercept=0,color="red")+
  facet_wrap(~type_of_gov)
save(plot2bis,file="figure/plot2bis.rda")



plot2bisbis<-ggplot(data = xy, aes(x = lftemp07, y = lftemp12)) + 
  stat_density2d(aes(alpha=..level..^.1, fill=..level..^.1), 
                 size=2, 
                 bins=4, 
                 geom="polygon") + 
  scale_fill_gradient(low = "yellow", high = "red") +
  #scale_alpha(range = c(0.00, 1), guide = FALSE) +
  geom_density2d(colour="grey", bins=4) +
  guides(alpha=FALSE) +
facet_wrap(~type_of_gov)
  


plot3<-ggplot(data = xy, aes(x = lftemp07, y = lftemp12)) +
  geom_point(size=1) +   geom_smooth(method="lm",se=FALSE) +geom_abline(slope=1,intercept=0,color="red")+
  facet_wrap(~state)
save(plot3,file="figure/plot3.rda")


plot4<-ggplot(data = xy, aes(x = lftemp07, y = lftemp12)) +
  geom_point(size=1) +   geom_smooth(method="lm",se=FALSE) +geom_abline(slope=1,intercept=0,color="red")+
  facet_wrap(~itemcode)
save(plot4,file="figure/plot4.rda")


plot(lm(log(1+lftemp12)~log(1+lftemp07),data=xy))
