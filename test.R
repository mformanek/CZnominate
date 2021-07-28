install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
#library(tidyverse)

xml_name(xml_children(data))

xml_path(xml_find_all( data, ".//datumHlasovani" ))

len <- length(xml_path(xml_find_all( data, "//hlasovaniSenatora[*]")))
for(x in 1:len)
{
  print(xml_path(xml_find_all( data, paste("//hlasovaniSenatora[", len, "]"))))
}
i_s <- 1
i_h <- 1
for(i_s in 1:1)
{
  print(paste("row:",i_s))
  for(i_h in 1:10)
  {
    col_name <- paste(xml_text(xml_find_all(raw_xml,paste0("//hlasovaniSenatora[1]/hlasSenatora[",i_h,"]/volebniObdobi"))),xml_text(xml_find_all(raw_xml,paste0("//hlasovaniSenatora[1]/hlasSenatora[",i_h,"]/cisloSchuze"))),xml_text(xml_find_all(raw_xml,paste0("//hlasovaniSenatora[1]/hlasSenatora[",i_h,"]/cisloHlasovani"))))
    print(paste("col:",col_name, " ID:", which(colnames(tabulka_senatoru) == col_name)))
    tabulka_senatoru[i_s:which(colnames(tabulka_senatoru) == col_name)] <- xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora[1]/hlasSenatora[1]/textHlasu"))
  }
}

i_s <- 1
i_h <- 1
while(i_s < 2)
{
  print(paste("row:",i_s))
  while(i_h < 11)
  {
    col_name <- paste(xml_text(xml_find_all(raw_xml,paste0("//hlasovaniSenatora[1]/hlasSenatora[",i_h,"]/volebniObdobi"))),xml_text(xml_find_all(raw_xml,paste0("//hlasovaniSenatora[1]/hlasSenatora[",i_h,"]/cisloSchuze"))),xml_text(xml_find_all(raw_xml,paste0("//hlasovaniSenatora[1]/hlasSenatora[",i_h,"]/cisloHlasovani"))))
    print(paste("col:",col_name, " ID:", which(colnames(tabulka_senatoru) == col_name)))
    tabulka_senatoru[i_s:i_h] <- ""
    i_h = i_h + 1
  }
  i_h = 1
  i_s = i_s + 1
}

for(i_s in 1:1)
{
  print(paste("row:",i_s))
  for(i_h in 1:10)
  {
    col_name <- paste(xml_text(xml_find_all(raw_xml,paste0("//hlasovaniSenatora[1]/hlasSenatora[",i_h,"]/volebniObdobi"))),xml_text(xml_find_all(raw_xml,paste0("//hlasovaniSenatora[1]/hlasSenatora[",i_h,"]/cisloSchuze"))),xml_text(xml_find_all(raw_xml,paste0("//hlasovaniSenatora[1]/hlasSenatora[",i_h,"]/cisloHlasovani"))))
    print(paste("col:",col_name))
    tabulka_senatoru[i_s,which(colnames(tabulka_senatoru) == col_name)] <- xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora[1]/hlasSenatora[1]/textHlasu"))
  }
}

source("xml_to_df.R")

test2 <- xml_to_df("senat_04.06.2021.xml", xpath = "/hlasovaniSenatu/hlasovaniSenatora/hlasSenatora")


#remove all senators from enviroment
for(i_s in 1:pocet_senatoru)
{
  x<-paste0("senator_",i_s)
  if(exists(x, envir = globalenv())) 
  {
    rm(list = x, envir = globalenv())
  }

}

#get all vote types
moznosti_hlasu<-unique(xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora/hlasSenatora/textHlasu")))

#change vote encoding
senator_1$textHlasu<-ifelse(senator_1$textHlasu == "pro",
                            1,
                            senator_1$textHlasu)

senator_1$textHlasu<-ifelse(senator_1$textHlasu == "proti",
                            2,
                            senator_1$textHlasu)

senator_1$textHlasu<-ifelse(senator_1$textHlasu %in% c(1,2),
                            senator_1$textHlasu,
                            0)

unique_vote_types(raw_xml)
senator_1$textHlasu<-format_textHlasu(senator_1$textHlasu)

source("lib.R")
table_test<-format_senator_table("senat_redux.xml")

senator_x<-format_senator_table(senator_1)

#individual Senator table formatting
names(senator_1)<-senator_1[1,]
senator_2<-senator_2[c(FALSE,TRUE),]

senator_1<-senator_1[-1,]
senator_x<-senator_1[!(row.names(senator_1) %in% "cisloHlasovani"),]

senator_x<-data.frame(senator_1[2,],
                      row.names = "textHlasu"
                      )

xml_attr(raw_xml2)

test <- xmlInternalTreeParse(raw_xml2)

nodeset<-getNode(raw_xml2, "/hlasovaniSenatu/hlasovaniSenatora[1]")

xmlGetAttr(raw_xml2)

xpathSApply(raw_xml2, "//hlasovaniSenatu/hlasovaniSenatora[1]", xmlGetAttr, "jmenoSenatora") 

Encoding(temp)<-"UTF-8"

xpathSApply(raw_xml2, 
            paste("/hlasovaniSenatu/hlasovaniSenatora[",i_s,"]"), 
            xmlGetAttr, 
            "jmenoSenatora")

rownames(senator_1)[rownames(senator_1) == "textHlasu"] <- name

row.names(str_as_var(temp))<-c(1,2)

test<-merge(senator_master_table,senator_1, all.x = TRUE)

senator_x<-rbind(senator_1,senator_2)

build_up_table_test<-subset(build_up_table, nchar(as.character(f_name)) <= 1)

build_up_table_test<-merge(senator_master_table,senator_1,all.x=TRUE)

build_up_table_test<-build_up_table[!(rownames(build_up_table) %in% "cisloHlasovani"),]

xpathApply(xmlRoot(raw_xml2),path="count(//hlasovaniSenatu/hlasovaniSenatora)",xmlValue)

senator_x<-Merge(senator_1,senator_10, by="cisloHlasovani")

senator_x<-bind_rows(senator_1,senator_10)

cisloSchuze<-xpathApply(xmlRoot(raw_xml2),path="//hlasovaniSenatu/hlasovaniSenatora/hlasSenatora/cisloSchuze",xmlValue)
cisloHlasovani<-xpathApply(xmlRoot(raw_xml2),path="//hlasovaniSenatu/hlasovaniSenatora/hlasSenatora/cisloHlasovani",xmlValue)
volebniObdobi<-xpathApply(xmlRoot(raw_xml2),path="//hlasovaniSenatu/hlasovaniSenatora/hlasSenatora/volebniObdobi",xmlValue)

vsechna_hlasovani <- mixedsort(unique(paste(volebniObdobi, 
                                  cisloSchuze, 
                                  cisloHlasovani, 
                                  sep = '_')))

#Write out csv table in correct format:
write.table(matrix(all_votes, nrow=1),"all_votes.csv",sep=",", row.names = FALSE, col.names=FALSE)

for(i_v in vote_id){    
  if(i_v %in% temp[1,]){
  }
  else {print("^ no matching votes ^")}
}

a_test<-merge(x = senator_228, y = senator_283, by = NULL)

#add missing columns add order correctly
missing <- setdiff(vote_id, names(temp))  # Find names of missing columns
temp[missing] <- 0                    # Add them, filled with '0's
temp <- temp[vote_id]                       # Put columns in desired order

id<-310
temp<-senator_310
#temp<-temp[-1,]

#m1 <- matrix(NA, ncol=length(vote_id), nrow=length(senator_id), dimnames = list(senator_id,vote_id))
#m1[row.names(temp), colnames(temp)] <- unlist(temp)
temp_miss<-setdiff(vote_id,colnames(temp))
temp_m <- matrix(as.character(0), ncol=length(temp_miss), nrow=2, dimnames = list(rownames(temp),temp_miss))
temp<-cbind(temp,temp_m)
temp<- temp[,vote_id] #re-order columns
temp<-temp[-1,]

## exclude columns not in cols
temp <- temp[ ,colnames(temp) %in% vote_id]
## add missing columns filled with NA
temp[, vote_id[!(vote_id %in% colnames(temp))]] <- NA
## reorder
temp<- temp[,vote_id] #re-order columns

require(plyr)
?rbind.fill

a_test<-rbind(m1,as.temp)

# 1.  funkční období 18.12.1996 - 15.12.1998
# 2.  funkční období 16.12.1998 - 18.12.2000
# 3.  funkční období 19.12.2000 - 03.12.2002
# 4.  funkční období 04.12.2002 - 14.12.2004
# 5.  funkční období 15.12.2004 - 28.11.2006
# 6.  funkční období 29.11.2006 - 25.11.2008
# 7.  funkční období 26.11.2008 - 23.11.2010
# 8.  funkční období 24.11.2010 - 20.11.2012
# 9.  funkční období 21.11.2012 - 18.11.2014
# 10. funkční období 19.11.2014 - 15.11.2016
# 11. funkční období 16.11.2016 - 13.11.2018
# 12. funkční období 14.11.2018 - 10.11.2020
# 13. funkční období 11.11.2020 - do dnes   

require(XML)


download.file("https://senat.cz/senatori/index.php?par_3=243", destfile="index.php")
download.file("https://senat.cz/senatori/index.php?lng=cz&ke_dni=27.7.2021&O=13&par_2=1", destfile="index.php")
temp_x <- htmlParse("https://senat.cz/senatori/index.php?lng=cz&ke_dni=27.7.2021&O=13&par_2=1")

require(xml2)
source("lib.R")

#get amount of funcni obdobi
pocet_obdobi<-(as.integer(format(Sys.Date(), "%Y"))-1996)/2
pocet_obdobi<-ceiling(pocet_obdobi)
funkcni_obdobi<-c(1:pocet_obdobi)
# get rolover dates for funkcni obdobi, anticipate this will break in 2023
dates<-c("15.12.1998",
         "18.12.2000",
         "03.12.2002",
         "14.12.2004", 
         "28.11.2006", 
         "25.11.2008", 
         "23.11.2010", 
         "20.11.2012", 
         "18.11.2014",
         "15.11.2016",
         "13.11.2018",
         "10.11.2020",
         format(Sys.Date(), "%d.%m.%Y"))
url1<-"https://senat.cz/senatori/index.php?lng=cz&ke_dni="
url2<-"&O="
url3<-"&par_2=1"

usable_urls<-paste0(url1,dates,url2,funkcni_obdobi,url3)
for(val in 1:pocet_obdobi) {
  if(!exists("single_table")) {
    single_table<-senators_from_url(usable_urls[val]) #get Senator info page
  } else {
    single_table2<-senators_from_url(usable_urls[val]) #get Senator info page
    single_table<-rbind(single_table,single_table2)
  }
}
single_table<-single_table[!duplicated(single_table[,1]),] #remove duplicates
rownames(single_table)<-single_table[,1]
single_table<-single_table[order(single_table[,1]),]
single_table<-sort(single_table)

party<-xml_text(xml_find_all(test, ".//tr/td[4]")) #parse out senator party names.
names<-xml_text(xml_find_all(test, ".//tr/td[2]")) #parse out senator names.
data<-xml_text(xml_find_all(test, ".//tr/td[2]/a/@href")) #get link <a> text field containing Senator ID
data2<-sapply(strsplit(data, "&", fixed = FALSE, perl = FALSE, useBytes = FALSE),"[",4) #get par_3=XXX text field
data3<-sapply(strsplit(data2, "=", fixed = FALSE, perl = FALSE, useBytes = FALSE),"[",2) #get Senator ID text field
metx<-matrix(c(data3,party,names), ncol=3)