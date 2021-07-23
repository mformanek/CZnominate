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
