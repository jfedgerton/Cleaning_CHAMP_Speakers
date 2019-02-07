
## Import the files names as a vector
import.list <- list.files(path = "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/csv_3456", 
                          pattern = ".csv", 
                          all.files = FALSE,
                          full.names = FALSE, 
                          recursive = FALSE,
                          ignore.case = FALSE, 
                          include.dirs = FALSE, 
                          no.. = FALSE)

## Where is the data from
import <- "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/csv_3456/"

## Where is the data exported to
output <- "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/format_csv_3456/"

## functions to fix names 
source("C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Programs/network_fix_function.R")
source("C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Programs/show_fix_function.R")

## Function top reformat files 
reformat_csv_name <- function(data){
  for (i in 1:length(data)){
    
    ## Import the csv file in a loop
    temp <- read.csv(paste(import, import.list[i], sep = ""))
    
    name_data_frame <- as.data.frame(matrix(NA, nrow = 1, ncol = 3))
    ## What is the network name?
    name_data_frame[1,1] <- toupper(temp[1,]$Channel)
    name_data_frame[1,2] <- toupper(temp[1,]$Show)
    name_data_frame[1,3] <- toupper(temp[1,]$year)
    ## What is the show name? 
    
    colnames(name_data_frame) <- c("network", "show", "year")
    
    ## Reformat Network Names
    name_data_frame <- network_fix_function(name_data_frame)
    name_data_frame <- show_fix_function(name_data_frame)
 
    output_name = paste(name_data_frame$network, name_data_frame$show, name_data_frame$year, sep = "_")
    
    save(temp, file = paste(output, output_name, ".Rda", sep = ""))
  }
}
## Fix: Network -- CNBC/DOW JONES BUSINESS
system.time(reformat_csv_name(import.list))
