source("C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Programs/Name_Classifier_1.R")
source("C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Programs/Name_Classifier_2.R")
source("C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Programs/Name_Classifier_3.R")
source("C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Programs/Name_Classifier_4.R")
source("C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Programs/Name_Classifier_5.R")

library('readr')
library('tidyverse')
file_pathway <- "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/format_csv_1278/"
import.list <- list.files(path = file_pathway, pattern = ".Rda", all.files = FALSE,
                          full.names = FALSE, recursive = FALSE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

for (i in 1:length(import.list)){
 load(paste0(file_pathway, import.list[i]))
 temp <- name_fix_function1(temp)
 temp <- name_fix_function2(temp)
 temp <- name_fix_function3(temp)
 temp <- clean_names(temp)
 temp <- clean_names_again(temp)
 save(temp, file = paste0(file_pathway, import.list[i]))
 print(i)
}
