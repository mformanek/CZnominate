require(xml2)
require(XML)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(gtools)

source("xml_to_df.R")
source("lib.R")

#string as variable macros
read_str_as_var <- defmacro(a,expr={eval(parse(text = a))})
#write_str_as_var <- defmacro(b,c,expr={assign(b,c)})

#define input .xml file:
file_path<-"senat_04.06.2021.xml"
#file_path<-""senat_redux.xml"

#senator_master_table<-allocate_senator_master_table(file_path)

raw_xml2 <- xmlParse(file_path)

ps<-pocet_senatoru(raw_xml2)
print(paste("Pocet Senatoru:",ps))

#for(i_s in 1:pocet_senatoru)
for(i_s in 1:ps)
{
  name<-xpathSApply(raw_xml2, 
                    paste("/hlasovaniSenatu/hlasovaniSenatora[",i_s,"]"), 
                    xmlGetAttr, 
                    "jmenoSenatora")
  Encoding(name)<-"UTF-8"

  print(paste("Senator:",i_s,name))
  
  
  temp<-xml_to_df(raw_xml2,
                  xpath = paste("/hlasovaniSenatu/hlasovaniSenatora[",i_s,"]/hlasSenatora"),
                  is_xml = TRUE)
  temp<-format_textHlasu(temp) 
  temp<-format_senator_table(temp)
  row.names(temp)[2]<-name
  #temp<-temp[-1,] #remove unwanted row
  if(i_s>1){
    build_up_table<-merge(build_up_table,temp)
    build_up_table<-build_up_table[!(rownames(build_up_table) %in% "cisloHlasovani"),] # remove extra rows
    }else{build_up_table<-temp}
  
  assign(paste0("senator_",i_s),temp)
}




