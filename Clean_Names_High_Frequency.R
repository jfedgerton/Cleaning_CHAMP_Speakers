load("CHAMP-Net/Data/Show-Year CSVs/formatted_data/frequency_by_show.Rdata")

for (i in 1:length(show_frequency)){
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "NA"] <- NA
  show_frequency[[i]]$Last_Name[show_frequency[[i]]$Last_Name == "NA"] <- NA
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == ","] <- NA
  show_frequency[[i]]$Last_Name[show_frequency[[i]]$Last_Name == ","] <- NA
  show_frequency[[i]]$New_Speaker2 <- show_frequency[[i]]$Name
  show_frequency[[i]]$New_Speaker3 <- NA
  for (j in 1:nrow(show_frequency[[i]])){
    if (!is.na(show_frequency[[i]][j,]$First_Name) & !is.na(show_frequency[[i]][j,]$Last_Name)){
      show_frequency[[i]][j,]$New_Speaker3 <- paste0(show_frequency[[i]][j,]$Last_Name, ", ", show_frequency[[i]][j,]$First_Name)
  }
 }
}


all_shows <- list.files(path = "CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010", 
                        pattern = ".Rda", 
                        all.files = FALSE,
                        full.names = FALSE, 
                        recursive = FALSE,
                        ignore.case = FALSE, 
                        include.dirs = FALSE, 
                        no.. = FALSE)


show_subset <- substr(all_shows, 1, nchar(all_shows)-9)

data_to_import <- as.data.frame(cbind(as.character(all_shows), as.character(show_subset)))
colnames(data_to_import) <- c("Show_Year", "Show")

for (i in 1:length(show_frequency)){
  show_temp <- unique(show_frequency[[i]]$Show)
  import_data <- subset(data_to_import, Show %in% show_temp) 
  show_frequency[[i]]$Show_Name <- show_frequency[[i]]$Show
  show_frequency_temp <- dplyr::select(show_frequency[[i]], -Show, -Name, -Frequency, -Name_Temp)
  show_frequency_temp <- show_frequency_temp[!duplicated(show_frequency[[i]]$New_Speaker2),]
  show_frequency_temp <- subset(show_frequency_temp, !is.na(Show_Name))
    if (nrow(show_frequency_temp) != 0){
      for (j in 1:nrow(import_data)){
        load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/", import_data[j,]$Show_Year))  
        check1 <- nrow(temp)
        temp <- merge(temp, 
                      show_frequency_temp, 
                      by = "New_Speaker2", 
                      all.x = T)
        temp$New_Speaker3[temp$New_Speaker3 == ""] <- NA
        check2 <- nrow(temp)
        if (check1 != check2){
          print("Error in merge")
        } else {
          save(temp, file = paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/", import_data[j,]$Show_Year))  
        }
    }
  }
}
