#CZnominate main.r
#Milan Formanek 2021

#requires
require(tcltk)
#sources
source("glue_lib.r")
source("lib.r")

#Prompt user to select directory with vote pair files
getwd()
# [1] "C:/Users/Root/Documents"
dir <- tclvalue(tkchooseDirectory())  # opens a dialog window in 'My Documents'

pairs<-pair_votes(dir) #pair vote from 
senator_id<-get_Senator_table_single(pairs[[3]]) #get Senator ID's for the correct period
vote_id<-pairs[[1]]
