require(xml2)
require(XML)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

source("xml_to_df.R")
source("lib.R")

raw_xml <- read_xml("senat_redux.xml")
xml_ns_strip(raw_xml)

cisloSchuze <- xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora[*]/hlasSenatora[*]/cisloSchuze"))
cisloHlasovani <- xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora[*]/hlasSenatora[*]/cisloHlasovani"))
volebniObdobi <- xml_text(xml_find_all(raw_xml, "//hlasovaniSenatora[*]/hlasSenatora[*]/volebniObdobi"))
jmenoSenatora <- xml_attr(xml_children(raw_xml),"jmenoSenatora")


vsechna_hlasovani <- unique(paste(volebniObdobi, cisloSchuze, cisloHlasovani, sep = '_'))
pocet_hlasovani <- length(vsechna_hlasovani)

pocet_senatoru <- length(jmenoSenatora)

#tabulka_senatoru <- array(0, dim = c(pocet_senatoru,pocet_hlasovani), dimnames = list(jmenoSenatora, vsechna_hlasovani))
tabulka_senatoru  <- data.frame(array(0, dim = c(pocet_senatoru,pocet_hlasovani), dimnames = list(jmenoSenatora, vsechna_hlasovani)),check.names = FALSE)

raw_xml2 <- xmlParse("senat_redux.xml")

#for(i_s in 1:pocet_senatoru)
for(i_s in 1:2)
{
  print(paste("Senator:",i_s))
  assign(paste0("senator_",i_s),xml_to_df(raw_xml2,xpath = paste("/hlasovaniSenatu/hlasovaniSenatora[",i_s,"]/hlasSenatora"),is_xml = TRUE))
  
}









