#' Making several text files from a single CSV file
#' 
#' Convert a single CSV file (one text per row) into
#' separate text files. A function in R.
#' 
#' To use this function for the first time run:
#' install.packages("devtools")
#' then thereafter you just need to load the function 
#' fom github like so:
#' library(devtools) # windows users need Rtools installed, mac users need XCode installed
#' source_url("https://gist.github.com/benmarwick/9266072/raw/csv2txts.R")
#' 
#' Here's how to set the argument to the function
#' 
#' mydir is the full path of the folder that contains your csv file
#' for example "C:/Downloads/mycsvfile" Note that it must have 
#' quote marks around it and forward slashes, which are not default
#' in windows. This should be a folder with a *single* CSV file in it.
#' 
#' labels refers to the column number with the document labels. The 
#' default is the first column, but in can your want to use a different
#' column you can set it like so (for example if col 2 has the labels):
#' labels = 2
#' 
#' 
#' A full example, assuming you've sourced the 
#' function from github already:
#' csv2txt("C:/Downloads/mycsvfile", labels = 2)
#' and after a moment you'll get a message in the R console
#' saying 'Your texts files can be found in C:/Downloads/mycsvfile'

library(readxl)
Mydata<-read_xlsx(here("Book1.xlsx"))
library(janitor)
Mydata<-janitor::clean_names(Mydata)
Mydata$histo_result_text<-gsub("^.*(Microscopic Description)","\\1",Mydata$histo_result_text)
head(Mydata$histo_result_text,20)
Mydata$histo_result_text<-gsub("(Harriet|Michael|Baljit|Green|Reported by|Mark).*$","",Mydata$histo_result_text)

library(dplyr)

Mydata<-Mydata%>%filter(endo_result_name=="Gastroscopy (OGD)")

write.csv(Mydata$histo_result_text,here("MypathText.csv"))

mydir<-here::here("Tbb1","MypathText.csv")

csv2txt<- function(mydir, labels = 1){
  
  # Get the names of all the CSV file
  mycsvfile <- mydir
  
  # Read the actual contexts of the text files into R and rearrange a little.
  
  # create a list of dataframes containing the text
  mycsvdata <- read.csv(mycsvfile)
  
  #Remove NA's 
  mycsvdata<-mycsvdata[complete.cases(mycsvdata),]
  
  # combine all except the first column together into
  # one long character string for each row
  mytxtsconcat <- apply(mycsvdata[-(1:labels)], 1, paste, collapse=" ")
  
  # make a dataframe with the file names and texts
  mytxtsdf <- data.frame(filename = mycsvdata[,labels], # get the first col for the text file names
                         fulltext = mytxtsconcat)
  
  # Now write one text file for each row of the csv
  # use 'invisible' so we don't see anything in the console
  
  setwd(mydir)
  invisible(lapply(1:nrow(mytxtsdf), function(i) write.table(mytxtsdf[i,2], 
                                                             file = paste0(mytxtsdf[i,1], ".txt"),
                                                             row.names = FALSE, col.names = FALSE,
                                                             quote = FALSE)))
  
  # now check your folder to see the txt files
  message(paste0("Your text files can be found in ", getwd()))
}
