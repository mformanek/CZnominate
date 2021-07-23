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