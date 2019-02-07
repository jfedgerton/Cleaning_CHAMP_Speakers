#################################
####    Count the speakers   ####
#################################

rm(list = ls())
library('tidyverse')
load("C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data/all_names.Rda")

counts_names <- as.data.frame(table(temp$New_Speaker))
colnames(counts_names) <- c("Speaker", "Appearance")
system.time(error_names <- all_names %>%
                 mutate(., 
                        New_Speaker = trimws(New_Speaker),
                        Check1 = substr(New_Speaker, 1, 1),
                        Check2 = substr(New_Speaker, (nchar(New_Speaker)), nchar(New_Speaker)),
                        Check3 = substr(New_Speaker, 2, 2),
                        Check4 = substr(New_Speaker, (nchar(New_Speaker)-1), nchar(New_Speaker)))) 
flag_items <- c("'", "(", ".", "-", ")", ",", " ", '"')
error_names$flag <- 0
error_names$flag[error_names$Check1 %in% flag_items | 
                 error_names$Check1 %in% flag_items | 
                 error_names$Check1 %in% flag_items | 
                 error_names$Check1 %in% flag_items] <- 1

error_names <- error_names %>% 
                 filter(., flag == 1) %>% 
                 dplyr::select(., -flag, -Check1, -Check2, -Check3, -Check4)

head(counts_names)
head(error_names)

error_names <- as.data.frame(unique(error_names[,1]))
colnames(error_names) <- "Wrong_Names"
save(error_names, file = "C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data/error_names.Rda")
save(counts_names, file = "C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data/count_names.Rda")
