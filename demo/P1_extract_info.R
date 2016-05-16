library(dataASPEP)
#1. Data
aspep2011_sample$type=substr(as.character(aspep2011_sample$id),3,1)
asp<-sqldf("select a.id,b.type,b.itemcode,b.state,log10(1+b.ftpay) as lftpay, log10(1+a.ftpay) as lftpay07 from 
           aspep2007_census a, 
           aspep2012_census b,
           aspep2011_sample c
           where a.id=b.id and a.id=c.id 
            and a.itemcode=b.itemcode  
            and a.itemcode=c.itemcode")
asp$type=as.factor(asp$type)
asp$itemcode=as.factor(asp$itemcode)
save(asp,file="data/asp.rda")

