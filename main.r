#CZnominate main.r
#Milan Formanek 2021

#requires
require(tcltk)
require(gtools)
require(XML)

library(wnominate)
library(plotly)
#sources
source("glue_lib.r")
source("lib.r")
source("xml_to_df.r")

#define input .xml file:
file_path<-"senat_23.08.2021.xml"

#Prompt user to select directory with vote pair files
getwd()
# [1] "C:/Users/Root/Documents"
dir <- tclvalue(tkchooseDirectory())  # opens a dialog window in 'My Documents'

pairs<-pair_votes(dir) #pair vote from file names

senator_id<-get_Senator_table_single(pairs[[4]])[1] #get Senator ID's for the correct period
senator_id<-as.data.frame(t(senator_id))
senator_id<-mixedsort(unlist(senator_id[1,]))

vote_id<-pairs[[2]]
vote_id<-as.data.frame(t(vote_id))
vote_id<-mixedsort(unlist(vote_id[1,]))

### Begin Nominate section 

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
    if(as.integer(id)>333)    {  
      print("flag")
      }
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
    #temp<- temp[,col.num] #is this the issue?
    
    #add missing columns and order correctly
    temp_miss<-setdiff(vote_id,colnames(temp))
    temp_m <- matrix(as.character(0), ncol=length(temp_miss), nrow=2, dimnames = list(rownames(temp),temp_miss))
    temp<-cbind(temp,temp_m)
    #temp2<- temp[,vote_id] #re-order columns #current issue # wiping all data 
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
      senator_party<-c(senator_party,senator_party_table$V2[senator_party_table$V1==as.numeric(id)]) #save senator party to list
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
#row.names(master_table) <- 1:nrow(master_table)
#row.names(senatori_legData) <- 1:nrow(senatori_legData) #renumber rows
#colnames(senatori_legData) <- "party"  #rename column to party
#colnames(master_table) <- 1:ncol(master_table) #renumber columns 

#senatori_legData<-data.matrix(senatori_legData)

#cleanup Enviroment
#rm(col.num,first_row_id,i_s,id,name,ps,senator_id,senator_party,temp,temp_miss,vote_id,temp_m,all_votes, file_path, raw_xml2,senator_party_table)

###END MASTER TABLE GENERATION###

#WNominate section

rc2 <- rollcall(master_table, yea = '1', nay = '2', missing = '0', legis.names = senator_names,  legis.data = as.matrix(senatori_legData), desc = "TEST")

#result <- wnominate(rc2, minvotes = 1, polarity = c(1, 1),verbose = TRUE)
result <- wnominate(rc2, dims=2, minvotes= 10, polarity = c(8, 7))
#polarity Cunek, Vicha (8,7)
#Plotting section

#plot.coords(result) 
options(encoding = "Windows-1250")

pal <- c("brown", "orange", "#106F2B", "red", "blue", "purple", "#FF00DA", "black" )
p <- plot_ly( hoverinfo ="text" , text = ~paste(senator_names), symbol = I("square"), y = result$legislators$coord1D, x = -(result$legislators$coord2D), type = 'scatter', colors = pal, color = result$legislators$V1, mode = 'markers')
p
