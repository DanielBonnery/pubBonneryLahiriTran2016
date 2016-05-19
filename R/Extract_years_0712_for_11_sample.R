Extract_years_0712_for_11_sample<-function(){
data(aspep2007,aspep2011,aspep2012,aspep2007_gov)
sqldf::sqldf("
select a.id,
  d.type_of_gov,
  b.itemcode,
  d.state,
  log10(1+c.ftpay) as lftpay, 
  log10(1+a.ftpay) as lftpay07 
from 
   aspep2007 a, 
   aspep2011 b,
   aspep2012 c,
   aspep2007_gov d
   where a.id=b.id 
      and a.id=c.id
      and a.id=d.id 
      and a.itemcode=b.itemcode  
      and a.itemcode=c.itemcode")}