#0.Libraries
#library(pubBonneryLahiriTran2016)
library(dataASPEP)
library(sqldf)
library(R2jags)
library(lme4)
library(ggplot2)
#1. Data
levels(aspep2007_census$type)
levels(aspep2012_census$type)
levels(aspep2011_sample$type)
aspep2011_sample$type=substr(as.character(aspep2011_sample$id),3,1)

asp<-sqldf("select b.type,b.itemcode,b.state,log(1+b.ftemp) as lftemp, log(1+a.ftemp) as lftemp07 from 
           aspep2007_census a, 
           aspep2012_census b,
           aspep2011_sample c
           where a.id=b.id and a.id=c.id")
asp$type=as.factor(asp$type)
asp$itemcode=as.factor(asp$itemcode)
save(asp,file="data/asp.rda")
load("data/asp.rda")


asp2<-sqldf("select b.id,b.type,b.itemcode,b.state, log(1+b.ftemp) as lftemp, log(1+a.ftemp) as lftemp07 from 
            aspep2007_census a, 
            aspep2012_census b
            where a.id=b.id")
asp2<-asp2[!is.element(asp2$id,aspep2011_sample$id),]

save(asp2,file="data/asp2.rda")
load("data/asp2.rda")


summarytab<-sqldf("select count(*) as n, type, itemcode, state, sum(lftemp) as lftemp, sum(lftemp07) as lftemp07 from asp group by type, itemcode, state")
save(summarytab,file="data/summarytab.rda")
load("data/summarytab.rda")
#2. model selection

#2.1. plot of lftemp vs lftemp07.
plot1<-
  ggplot(asp2, aes(x = lftemp07, y = lftemp)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
save(plot1,file="data/plot1.rda")


#3. Computations
usefullvariables<-c("state","itemcode","type","lftemp","lftemp07")
#3.1. individual level, 

N=nrow(asp);
dime=unlist(lapply(asp2[c("state","itemcode","type")],nlevels))

fit <-jags(
  data=c(list(N=N,dime=dime),asp2[usefullvariables]),
  inits=list(list("beta0"=array(0,dime),"beta1"=array(0,dime),"tau"=1)),
  n.chains =1,
  parameters.to.save=c("sigma", "beta"),
  n.iter =10 ,
  n.burnin =3 ,
  model.file= textConnection (
    "model {
    for (i in 1:N) {
    lftemp[i]~dnorm(beta0[state[i],itemcode[i],type[i]]+beta1[state[i],itemcode[i],type[i]]*lftemp07[i],tau)}
    for (i1 in 1:dime[1]) {
    for (i2 in 1:dime[2]) {
    for (i3 in 1:dime[3]) {
    beta0[i1,i2,i3]~ dnorm (0 ,1.0E-4);
    beta1[i1,i2,i3]~ dnorm (0 ,1.0E-4);}}}
    tau~ dgamma (1.0E-4 ,1.0E-4);
    sigma <- 1/tau}"))
print(fit)


fit <-jags(
  data=c(list(N=nrow(summarytab),dime=dime),summarytab[usefullvariables]),
  inits=list(list("beta0"=array(0,dime),"beta1"=array(0,dime),"tau"=1)),
  n.chains =1,
  parameters.to.save=c("sigma", "beta0", "beta1"),
  n.iter =10 ,
  n.burnin =3 ,
  model.file= textConnection (
    "model {
    for (i in 1:N) {
    lftemp[i]~dnorm(beta0[state[i],itemcode[i],type[i]]+beta1[state[i],itemcode[i],type[i]]*lftemp07[i],tau)}
    for (i1 in 1:dime[1]) {
    for (i2 in 1:dime[2]) {
    for (i3 in 1:dime[3]) {
    beta0[i1,i2,i3]~ dnorm (0 ,1.0E-4);
    beta1[i1,i2,i3]~ dnorm (0 ,1.0E-4);}}}
    tau~ dgamma (1.0E-4 ,1.0E-4);
    sigma <- 1/tau}"))
print(fit)


#see 2006 gelman paper on inverted gamma that does not work see own suggestion of gelman
#see 2006 tyry half Cauchy and other proposals.
# see 2005 annals of statistics analysis of variance how to do it.
# see half normal , but make sure that difference of 1 means big thing.

library(R2jags)
modelstring<-"
model{
for (i in 1:N) {
lftemp[i]~dnorm(beta0[itemcode[i],type[i]]+beta1[itemcode[i],type[i]]*lftemp07[i],tau)}
  for (i2 in 1:dime[1]) {
    for (i3 in 1:dime[2]) {
      beta0[i2,i3]~dnorm (0 ,1.0E-4);
      beta1[i2,i3]~dnorm (0 ,1.0E-4);}}
tau~dgamma(1.0E-4,1.0E-4);
sigma<-1/tau
}
"
fit <-jags(
  n.chains =1,
  parameters.to.save=c("sigma", "beta0", "beta1"),
  n.iter =10 ,
  n.burnin =3,
  data=c(list(N=sum(summarytab$state==30),dime=dime[-1]),summarytab[summarytab$state==30,]),
  inits=list(list("beta0"=array(0,dime[-1]),"beta1"=array(0,dime[-1]),"tau"=1)),
  model.file=textConnection (modelstring))
print(fit)