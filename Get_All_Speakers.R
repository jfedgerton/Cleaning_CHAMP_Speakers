#####################################
###   Complete list of people    ####
#####################################
setwd("C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data")
import_list <- list.files(path = "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data", 
                               pattern = ".Rda", 
                               all.files = FALSE,
                               full.names = FALSE, 
                               recursive = FALSE,
                               ignore.case = FALSE, 
                               include.dirs = FALSE, 
                               no.. = FALSE)
library('tidyverse')
name_list <- list()
names <- list()
for(i in 1:length(import_list)){
  load(import_list[i])
  names[[i]] <- (., New_Speaker, SpeakerNum)
}

all_names <- do.call(rbind, names)
