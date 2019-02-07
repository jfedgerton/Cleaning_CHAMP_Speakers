#################################
####                         ####
####     Merging speakers    ####
####       Newid & Name      ####
####                         ####
#################################
library('readr')
library('tidyverse')

## List of files to import
import.list <- list.files(path = "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/format_csv_3456", pattern = NULL, all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

## Output pathway
output <- "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/format_csv_3456/"

## Data frame with new speakers and IDs
load('C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/data frames to check/Convert_Speaker.Rda')
New_ID_Speaker <- mutate(New_ID_Speaker, Speaker = toupper(Speaker))

## Loop in and save files with newid and Speaker names
for (i in 2:length(import.list)){
  import_file <- paste0(output, import.list[i])
  temp <- suppressMessages(read_delim(import_file,
                     delim = "\t", escape_double = FALSE, trim_ws = TRUE))
  
  temp <- merge(temp, New_ID_Speaker, 
                     by = c("Speaker", "SpeakerNum"),
                     all.x = T)
  
  write.table(temp, file = paste(output, import.list[i], sep = ""), sep = "\t", row.names = F)             
}
