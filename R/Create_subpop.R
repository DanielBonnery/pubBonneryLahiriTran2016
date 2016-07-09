#Create a table for the population
xyf<-function(aspep2007,aspep2012,aspep2007_gov){sqldf::sqldf("
                             select a.id,
                             d.type_of_gov,
                             a.itemcode,
                             d.state,
                             c.ftemp,
                             log10(a.ftemp) as lftemp07, 
                             log10(c.ftemp) as lftemp12 
                             from 
                             aspep2007 a,
                             aspep2012 c,
                             aspep2007_gov d
                             where  a.id=c.id
                             and a.id=d.id 
                             and a.itemcode=c.itemcode
                             and c.ftemp>0
                             and a.ftemp>0")}


#Create a table for the sample
xysf<-function(s,xy){
sqldf::sqldf("
             select a.*
             from 
             xy a,
             s b
             where a.id=b.id  
             and a.itemcode=b.itemcode
             ")}


xrf<-function(xy,xys){sqldf::sqldf("select * from xy except select * from xys")}
