#Download and decode vote ID's for chamber of deputies
download.file("https://www.psp.cz/eknih/cdrom/opendata/hl-2017ps.zip", destfile="hlasovani_2017.zip") #download file with votes
unzip(zipfile="hlasovani_2017.zip", exdir = "./hlasovani_2017",) #unzip file
hl_hlasovani<-read.table("./hlasovani_2017/hl2017s.unl",sep = "|", fileEncoding = "Windows-1250") #decode hl_hlasovani table
hl_hlasovani <- hl_hlasovani[c(1:1)] #remove extra cols

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

#download voter info
download.file("https://www.psp.cz/eknih/cdrom/opendata/poslanci.zip", destfile="poslanci.zip") #download file with all voters
unzip(zipfile="poslanci.zip", exdir = "./poslanci",) #unzip file
osoby<-read.table("./poslanci/osoby.unl",sep = "|", fileEncoding = "Windows-1250") #decode osoby table
osoby <- osoby[c(1:9)] #remove extra cols

#rename cols to match https://www.psp.cz/sqw/hp.sqw?k=1301
colnames(osoby)<-c("id_osoba",
                   "pred",
                   "jmeno",
                   "prijmeni",
                   "za",
                   "narozeni",
                   "pohlavi",
                   "zmena",
                   "umrti")

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