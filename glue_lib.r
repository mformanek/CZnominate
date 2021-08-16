

pair_votes<-function() {
  library(tcltk)
  setwd('~')
  getwd()
  # [1] "C:/Users/Root/Documents"
  dir <- tclvalue(tkchooseDirectory())  # opens a dialog window in 'My Documents'
  files <- list.files(path = dir, pattern = NULL, all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  files2 <- strsplit(files,"-") #split file names into poslanec/senator data
  
  files_senator<-lapply(files2, '[', 1) #take Senator data
  files_senator<-gsub("\\.","_",files_senator) #reformat "." to "_"
  
  files_poslanec<-lapply(files2, '[', 2) #take Poslanec data
  files_poslanec<-gsub('.{5}$', '',files_poslanec) #remove last 4 chars ".docx"
  files_poslanec<-gsub("\\.","_",files_poslanec) #reformat "." to "_"
  
  poslanec_volebni_obdobi<- strsplit(files_poslanec[1],"_")[[1]][1] # save volebni obdobi
  senator_volebni_obdobi<-strsplit(files_senator[1],"_")[[1]][1] # save volebni obdobi
  
  return(list(files_poslanec, files_senator, poslanec_volebni_obdobi, senator_volebni_obdobi))
}