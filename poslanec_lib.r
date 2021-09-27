#Helper Functions for Poslanci
#Milan Formanek 2021

require(reshape2)

#Download and decode vote ID's for chamber of deputies
download_hlasovani<-function(volebni_obdobi) {
  
  download_links<-c("https://www.psp.cz/eknih/cdrom/opendata/hl-1993ps.zip",
                    "https://www.psp.cz/eknih/cdrom/opendata/hl-1996ps.zip",
                    "https://www.psp.cz/eknih/cdrom/opendata/hl-1998ps.zip",
                    "https://www.psp.cz/eknih/cdrom/opendata/hl-2002ps.zip",
                    "https://www.psp.cz/eknih/cdrom/opendata/hl-2006ps.zip",
                    "https://www.psp.cz/eknih/cdrom/opendata/hl-2010ps.zip",
                    "https://www.psp.cz/eknih/cdrom/opendata/hl-2013ps.zip",
                    "https://www.psp.cz/eknih/cdrom/opendata/hl-2017ps.zip")
  
  download.file(download_links[volebni_obdobi], destfile="hlasovani_2017.zip") #download file with votes
  unzip(zipfile="hlasovani_2017.zip", exdir = "./hlasovani_2017",) #unzip file
  
  #decode hl_hlasovani table
  hl_hlasovani<-read.table("./hlasovani_2017/hl2017s.unl",sep = "|", fileEncoding = "Windows-1250") 
  hl_hlasovani <- hl_hlasovani[c(1:17)] #remove extra cols
  
  #rename cols to match https://www.psp.cz/sqw/hp.sqw?k=1302
  colnames(hl_hlasovani)<-c("id_hlasovani",
                            "id_organ",
                            "schuze",
                            "cislo",
                            "bod",
                            "datum",
                            "čas",
                            "pro",
                            "proti",
                            "zdrzel",
                            "nehlasoval",
                            "prihlaseno",
                            "kvorum",
                            "druh_hlasovani",
                            "druh_hlasovani",
                            "nazev_dlouhy",
                            "nazev_kratky")
  
  #decode hl_poslanec table
  hl_poslanec<-read.table("./hlasovani_2017/hl2017h1.unl",sep = "|", fileEncoding = "Windows-1250") 
  hl_poslanec <- hl_poslanec[c(1:3)] #remove extra cols
  
  #rename cols to match https://www.psp.cz/sqw/hp.sqw?k=1302
  colnames(hl_poslanec)<-c("id_poslanec",
                           "id_hlasovani",
                           "vysledek")
  
  #change format to wide
  hl_poslanec_wide <- dcast(hl_poslanec, id_poslanec ~ id_hlasovani, value.var="vysledek",drop = FALSE)

  return(hl_poslanec_wide)
}

#download voter info
download_poslanci<-function() {
  
  download.file("https://www.psp.cz/eknih/cdrom/opendata/poslanci.zip", destfile="poslanci.zip") #download file with all voters
  unzip(zipfile="poslanci.zip", exdir = "./poslanci",) #unzip file
  
  #decode osoby table
  osoby<-read.table("./poslanci/osoby.unl",sep = "|", fileEncoding = "Windows-1250") #decode osoby table
  osoby <- osoby[c(1:9)] #remove extra cols
  
  #rename cols to match https://www.psp.cz/sqw/hp.sqw?k=1301
  colnames(osoby)<-c("id_osoba",
                     "pred",
                     "prijmeni",
                     "jmeno",
                     "za",
                     "narozeni",
                     "pohlavi",
                     "zmena",
                     "umrti")
  #decode osoba_extra table
  osoba_extra<-read.table("./poslanci/osoba_extra.unl",sep = "|", fileEncoding = "Windows-1250") #decode osoby table
  osoba_extra <- osoba_extra[c(1:6)] #remove extra cols
  #rename cols to match https://www.psp.cz/sqw/hp.sqw?k=1301
  colnames(osoba_extra)<-c("id_osoba",
                     "id_org",
                     "typ",
                     "obvod",
                     "strana",
                     "id_external")
  
  #decode poslanec table
  poslanec<-read.table("./poslanci/poslanec.unl",sep = "|", fileEncoding = "Windows-1250") #decode poslanec table
  poslanec <- poslanec[c(1:15)] #remove extra cols
  
  #rename cols to match https://www.psp.cz/sqw/hp.sqw?k=1301
  colnames(poslanec)<-c("id_poslanec",
                        "id_osoba",
                        "id_kraj",
                        "id_kandidatka",
                        "id_obdobi",
                        "web",
                        "ulice",
                        "obec",
                        "psc",
                        "email",
                        "telefon",
                        "fax",
                        "psp_telefon",
                        "facebook",
                        "foto")
  
  #decode organy table
  organy<-read.table("./poslanci/organy.unl",sep = "|", fileEncoding = "Windows-1250") 
  organy <- organy[c(1:10)] #remove extra cols
  
  #rename cols to match https://www.psp.cz/sqw/hp.sqw?k=1301
  colnames(organy)<-c("id_organ",
                      "organ_id_organ",
                      "id_typ_organu",
                      "zkratka",
                      "nazev_organu_cz",
                      "nazev_organu_en",
                      "od_organ",
                      "do_organ",
                      "priorita",
                      "cl_organ_base")
  
  #decode  zarazeni table
  zarazeni<-read.table("./poslanci/zarazeni.unl",sep = "|", fileEncoding = "Windows-1250") 
  zarazeni <- zarazeni[c(1:7)] #remove extra cols
  
  #rename cols to match https://www.psp.cz/sqw/hp.sqw?k=1301
  colnames(zarazeni)<-c("id_osoba",
                        "id_of",
                        "cl_funkce",
                        "od_o",
                        "do_o",
                        "od_f",
                        "do_f")
  
  #decode  typ_organu table
  typ_organu<-read.table("./poslanci/typ_organu.unl",sep = "|", fileEncoding = "Windows-1250") 
  typ_organu <- typ_organu[c(1:6)] #remove extra cols
  
  #rename cols to match https://www.psp.cz/sqw/hp.sqw?k=1301
  colnames(typ_organu)<-c("id_typ_org",
                          "typ_id_typ_org",
                          "nazev_typ_org_cz",
                          "nazev_typ_org_en",
                          "typ_org_obecny",
                          "priorita")
  
  return()
}
