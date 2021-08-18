#Helper Function
#Milan Formanek 2021

#change vote encoding from string to int
# pro<>1
# proti<>2
# everything else <> 0

#input is column with votes in text format i.e. senator_1$textHlasu
#usage: senator_1$textHlasu<-format_textHlasu(senator_1$textHlasu)
format_textHlasu <- function(input) {
  input$textHlasu<-ifelse(input$textHlasu == "pro",
                          1,
                          input$textHlasu)
  
  input$textHlasu<-ifelse(input$textHlasu == "proti",
                          2,
                          input$textHlasu)
  
  input$textHlasu<-ifelse(input$textHlasu %in% c(1,2),
                          input$textHlasu,
                          0)
  return(input)
}

#get all vote unique types: unique_vote_types(raw_xml)
unique_vote_types <- function(raw_xml) {
  return(unique(xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora/hlasSenatora/textHlasu"))))
}

#filter votes
format_senator_votes <- function(table2){
  return(table2)
}

#format the individual Senator table leaving only relevant info (vote id and vote text in standard format)
format_senator_table <- function(table) {
  table$cisloHlasovani<-paste(table$volebniObdobi, #merge first 3 columns into standard vote ID
                              table$cisloSchuze,
                              table$cisloHlasovani,
                              sep = '_')
  table<-subset(table, select = c(cisloHlasovani, textHlasu)) #delete all columns except for the 2 important ones
  table<-t(table)  #rotate df
  colnames(table)<-table[1,] #name colls from 1st row
  return(table)
}

#Create master Senator data frame of correct size from .xml input file in the Czech Parlament formatting
allocate_senator_master_table <- function(xmlpath) {
  raw_xml <- read_xml(xmlpath)
  xml_ns_strip(raw_xml)
  
  cisloSchuze <- xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora[*]/hlasSenatora[*]/cisloSchuze"))
  cisloHlasovani <- xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora[*]/hlasSenatora[*]/cisloHlasovani"))
  volebniObdobi <- xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora[*]/hlasSenatora[*]/volebniObdobi"))
  jmenoSenatora <- xml_attr(xml_children(raw_xml),"jmenoSenatora")
  
  
  vsechna_hlasovani <- unique(paste(volebniObdobi, 
                                    cisloSchuze, 
                                    cisloHlasovani, 
                                    sep = '_'))
  
  pocet_hlasovani <- length(vsechna_hlasovani)
  
  pocet_senatoru <- length(jmenoSenatora)
  tabulka_senatoru  <- data.frame(array(0,
                                        dim = c(pocet_senatoru,pocet_hlasovani),
                                        dimnames = list(jmenoSenatora, vsechna_hlasovani)),
                                        check.names = FALSE)
  return(tabulka_senatoru)
}

#get count of senators
pocet_senatoru <- function(raw_xml) {
  return(xpathApply(xmlRoot(raw_xml),path="count(//hlasovaniSenatu/hlasovaniSenatora)",xmlValue)
)
}
#get list of all votes sorted
vsechny_hlasy <- function(raw_xml) {
  
  cisloSchuze<-xpathApply(xmlRoot(raw_xml2),path="//hlasovaniSenatu/hlasovaniSenatora/hlasSenatora/cisloSchuze",xmlValue)
  cisloHlasovani<-xpathApply(xmlRoot(raw_xml2),path="//hlasovaniSenatu/hlasovaniSenatora/hlasSenatora/cisloHlasovani",xmlValue)
  volebniObdobi<-xpathApply(xmlRoot(raw_xml2),path="//hlasovaniSenatu/hlasovaniSenatora/hlasSenatora/volebniObdobi",xmlValue)
  
  vsechna_hlasovani <- mixedsort(unique(paste(volebniObdobi, 
                                              cisloSchuze, 
                                              cisloHlasovani, 
                                              sep = '_')))
  return(vsechna_hlasovani)
}

#get list of senators for one funkcni obdobi from a valid url
#XML2 implementation
senators_from_url <- function(url) {
  
  #require(xml2)
  test<-read_html(url) #get Senator info page
  
  party<-xml_text(xml_find_all(test, ".//tr/td[4]")) #parse out senator party names.
  names<-xml_text(xml_find_all(test, ".//tr/td[2]")) #parse out senator names.
  data<-xml_text(xml_find_all(test, ".//tr/td[2]/a/@href")) #get link <a> text field containing Senator ID
  data2<-sapply(strsplit(data, "&", fixed = FALSE, perl = FALSE, useBytes = FALSE),"[",4) #get par_3=XXX text field
  data3<-sapply(strsplit(data2, "=", fixed = FALSE, perl = FALSE, useBytes = FALSE),"[",2) #get Senator ID text field
  metx<-matrix(c(data3,party,names), ncol=3)
  
  return(metx)
}

#get list of senators for all funkcni obdobi from a valid url
#XML2 implementation
get_Senator_table <- function() {
  
  require(xml2)
  #source("lib.R")
  
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
  single_table<-single_table[order(as.numeric(rownames(single_table))),,drop=FALSE]#oreder table by index
  
  return(as.data.frame(single_table))
}

#get list of senators for one funkcni obdobi from a valid url
#XML2 implementation
get_Senator_table_single <- function(funkcni_obdobi) {
  
  require(xml2)
  #source("lib.R")

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
  
  usable_url<-paste0(url1,dates[funkcni_obdobi],url2,funkcni_obdobi,url3)
  single_table<-senators_from_url(usable_url) #get Senator info page

  single_table<-single_table[!duplicated(single_table[,1]),] #remove duplicates
  rownames(single_table)<-single_table[,1]
  single_table<-single_table[order(as.numeric(rownames(single_table))),,drop=FALSE] #order table by index
  
  return(as.data.frame(single_table))
}

#input Senators from .csv file
load_senators_csv<-function(dir) {
  
  library(gtools)
  #load Senatori.csv list of senator IDs:
  senator_id<-read.csv(dir, header=FALSE, sep=',', stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
  senator_id<-mixedsort(unlist(senator_id[1,]))

  return(senator_id)
}  

#input Votes from .csv file
load_votes_csv<-function(dir) {
  
  library(gtools)
  #load Hlasy.csv list of vote IDs:
  vote_id<-read.csv(dir, header=FALSE, sep=',', stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
  vote_id<-mixedsort(unlist(vote_id[1,]))
  
  return(vote_id)
}  