require(xml2)
require(XML)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(gtools)
library(wrapr)
library(wnominate)

source("xml_to_df.R")
source("lib.R")

#string as variable macros
read_str_as_var <- defmacro(a,expr={eval(parse(text = a))})
#write_str_as_var <- defmacro(b,c,expr={assign(b,c)})

#define input .xml file:
file_path<-"senat_04.06.2021.xml"
#file_path<-""senat_redux.xml"

#define .csv list of senator IDs:
senator_id<-read.csv("Senatori.csv", header=FALSE, sep=',', stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
senator_id<-mixedsort(unlist(senator_id[1,]))
#define .csv list of vote IDs:
vote_id<-read.csv("Hlasy.csv", header=FALSE, sep=',', stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
vote_id<-mixedsort(unlist(vote_id[1,]))
#senator_master_table<-allocate_senator_master_table(file_path)

#get table containing all the Senators with Party info
senator_party_table<-get_Senator_table()

raw_xml2 <- xmlParse(file_path)

all_votes<-vsechny_hlasy(raw_xml2)
ps<-pocet_senatoru(raw_xml2)
print(paste("Pocet Senatoru:",ps))

for(i_s in 1:ps)
{
  name<-xpathSApply(raw_xml2, 
                    paste("/hlasovaniSenatu/hlasovaniSenatora[",i_s,"]"), 
                    xmlGetAttr, 
                    "jmenoSenatora")
  id<-xpathSApply(raw_xml2, 
                    paste("/hlasovaniSenatu/hlasovaniSenatora[",i_s,"]"), 
                    xmlGetAttr, 
                    "uniqPid")
  Encoding(name)<-"UTF-8"

  print(paste(i_s,"Senator:",name,"ID:",id))
  
  #is senator is in list of selected senators
  if(id %in% senator_id)
  {
    print("    ^ added Senator to master_table ^")
    temp<-xml_to_df(raw_xml2,
                  xpath = paste("/hlasovaniSenatu/hlasovaniSenatora[",i_s,"]/hlasSenatora"),
                  is_xml = TRUE)
    temp<-format_textHlasu(temp) 
    temp<-format_senator_table(temp)
    #row.names(temp)[2]<-name
    row.names(temp)[2]<-id
    
    #if votes of index is in list of selected votes create subset tables
    col.num<-which(colnames(temp) %in% vote_id)
    #temp<- temp[,sort(c(col.num, col.num - 1))]
    temp<- temp[,col.num]
    
    #add missing columns and order correctly
    temp_miss<-setdiff(vote_id,colnames(temp))
    temp_m <- matrix(as.character(0), ncol=length(temp_miss), nrow=2, dimnames = list(rownames(temp),temp_miss))
    temp<-cbind(temp,temp_m)
    temp<- temp[,vote_id] #re-order columns
    temp<-temp[-1,] #remove extra row
    
    #assign(paste0("senator_",id),temp) # create test table with senator id as label
    
    #create master table if it doesn't exist
    if(!exists("master_table")){
      master_table<-temp #Create first row of master table
      first_row_id<-id #save senator id for later to rename
      senator_names<-name #save senator name to list
      
      #create senator_legData metadata table
      senator_party<-senator_party_table$V2[as.numeric(id)] #Create first row of senator_legData table
    }
    else{ # if master_table already exists
      master_table <- rbind(master_table, temp) #add new row  
      rownames(master_table)[rownames(master_table) == "temp"] = id #rename row to match senator id
      senator_names<-c(senator_names,name) #save senator name to list
      senator_party<-c(senator_party,senator_party_table$V2[as.numeric(id)]) #save senator party to list
    }
  }
}
#rename first row to match senator id
rownames(master_table)[rownames(master_table) == "master_table"] = first_row_id 

#format Senator names and parties from list to matrix
senatori_legData<-matrix(senator_party)
rownames(senatori_legData)<-senator_id

master_table<-as.data.frame(master_table)
senatori_legData<-as.data.frame(senatori_legData) #reformat to data frame
row.names(master_table) <- 1:nrow(master_table)
row.names(senatori_legData) <- 1:nrow(senatori_legData) #renumber rows
colnames(senatori_legData) <- "party"  #rename column to party
colnames(master_table) <- 1:ncol(master_table) #renumber columns 
senatori_legData<-data.matrix(senatori_legData)

#cleanup Enviroment
rm(col.num,first_row_id,i_s,id,name,ps,senator_id,senator_party,temp,temp_miss,vote_id,temp_m,all_votes, file_path, raw_xml2,senator_party_table)

###END MASTER TABLE GENERATION###

#WNominate section

rc2 <- rollcall(master_table, yea = "1", nay = "2", missing = c("0"), legis.names = senator_names,  legis.data = senatori_legData, desc = "TEST")

#result <- wnominate(rc2, minvotes = 1, polarity = c(1, 1),verbose = TRUE)
result <- wnominate(rc2, polarity = c(1, 1))

#Plotting section

#plot.coords(result) 
options(encoding = "Windows-1250")

pal <- c("brown", "orange", "#106F2B", "red", "blue", "purple", "#FF00DA" )
p <- plot_ly( hoverinfo ="text" , text = ~paste(UNnames), symbol = I("square"), y = result$legislators$coord1D, x = -(result$legislators$coord2D), type = 'scatter', colors = pal, color = result$legislators$party, mode = 'markers')
p
