install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

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
