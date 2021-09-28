#CZnominate Poslanci_test.r
#Milan Formanek 2021

#requires
require(tcltk)
require(gtools)
require(tidyverse)

library(wnominate)
library(plotly)
#sources
source("poslanec_lib.r")
source("glue_lib.r")

#Prompt user to select directory with vote pair files
getwd()
# [1] "C:/Users/Root/Documents"
dir <- tclvalue(tkchooseDirectory())  # opens a dialog window in 'My Documents'

pairs<-pair_votes(dir) #pair vote from file names

hlasovani_test<-download_hlasovani(as.integer(pairs[[3]])) #download relevant votes
#poslanci_test<-download_poslanci()

### Begin Nominate section 

##test code##
poslanci_test <- merge(osoby,osoba_extra, by="id_osoba") #merge osoby table with osoba_extra to get party info
poslanci_test <- merge(poslanci_test,poslanec,by="id_osoba")
poslanci_test<-poslanci_test[c("id_osoba","jmeno","prijmeni","strana")] #delete unused info (columns)
poslanci_test$jmeno<-paste(poslanci_test$jmeno, poslanci_test$prijmeni) # concat jmeno and prijimeni columns
poslanci_test<-poslanci_test[!duplicated(poslanci_test),] #de-duplicate table
rownames(poslanci_test) <- NULL #renumber rows
poslanci_test <- merge(poslanci_test,poslanec,by="id_osoba") #merge osoby table with osoba_extra to get party info

poslanci_test <- merge(osoby,poslanec,by="id_osoba")
#organy_test<-organy[organy$id_typ_organu == "6" | organy$id_typ_organu == "1",]

zarazeni_test<-zarazeni[zarazeni$cl_funkce == "0",] #vybrat jenom clenstvi
merging_test<-merge(zarazeni_test, organy, by.x=c("id_of"),by.y=c("id_organ"))
merging_test2<-merge(poslanci_test, merging_test, by="id_osoba")
##end test code##