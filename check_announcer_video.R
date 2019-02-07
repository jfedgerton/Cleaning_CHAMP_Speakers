setwd("CHAMP-Net/Data/Show-Year CSVs/formatted_data")
## Import the files names as a vector
import.list <- list.files(path = "C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data", 
                          pattern = ".Rda", 
                          all.files = FALSE,
                          full.names = FALSE, 
                          recursive = FALSE,
                          ignore.case = FALSE, 
                          include.dirs = FALSE, 
                          no.. = FALSE)

for (i in 1:length(import.list)){
 load(import.list[i]) 
  if (temp$year[1] >= 2000 & 
      temp$year[1] <= 2010){
  #  temp$Text <- as.character(temp$Text)
  #  non_miss <- subset(temp, !is.na(New_Speaker2))
  #  missing  <- subset(temp, is.na(New_Speaker2))
  #  non_miss$New_Speaker2[non_miss$New_Speaker2 == ""] <- substr(non_miss$Text, 1, 20)
    
   # temp <- rbind(non_miss, missing)
    
 temp$voice      <-  grepl ("voice",      temp$New_Speaker2, ignore.case = TRUE)
 temp$television <-  grepl ("television", temp$New_Speaker2, ignore.case = TRUE)
 temp$audio      <-  grepl ("audio",      temp$New_Speaker2, ignore.case = TRUE)
 temp$announcer  <-  grepl ("announcer",  temp$New_Speaker2, ignore.case = TRUE)
 temp$actor      <-  grepl ("actor",      temp$New_Speaker2, ignore.case = TRUE)
 temp$actress    <-  grepl ("actress",    temp$New_Speaker2, ignore.case = TRUE)
 temp$comm       <-  grepl ("commercial", temp$New_Speaker2, ignore.case = TRUE)
 temp$police     <-  grepl ("police", temp$New_Speaker2, ignore.case = TRUE)
 temp$evening    <-  grepl ("evening",    temp$New_Speaker2, ignore.case = TRUE)
 temp$morning    <-  grepl ("morning",    temp$New_Speaker2, ignore.case = TRUE)
 temp$afternoon  <-  grepl ("afternoon",  temp$New_Speaker2, ignore.case = TRUE)
 temp$hello      <-  grepl ("hello",      temp$New_Speaker2, ignore.case = TRUE)
 temp$day        <-  grepl ("day",        temp$New_Speaker2, ignore.case = TRUE)
 temp$night      <-  grepl ("night",      temp$New_Speaker2, ignore.case = TRUE)
 temp$good       <-  grepl ("good",       temp$New_Speaker2, ignore.case = TRUE)
 
 temp$greeting <- rowSums(temp[,(ncol(temp)-6):ncol(temp)])
 
 temp$New_Speaker2[temp$announcer == T] <- NA
 temp$New_Speaker2[temp$actor     == T] <- NA
 temp$New_Speaker2[temp$actress   == T] <- NA
 temp$New_Speaker2[temp$audio     == T] <- NA
 temp$New_Speaker2[temp$comm      == T] <- NA
 temp$New_Speaker2[temp$police    == T] <- NA
 temp$New_Speaker2[temp$greeting > 1] <- NA

 temp <- temp %>%
           dplyr::select(.,
                         -greeting, -hello, -day, -afternoon,
                         -night, -good, -evening, -morning, -police)
   save(temp, file = paste0("2000 to 2010/", import.list[i]))  
 }
 
}

import.list <- list.files(path = "C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/", 
                          pattern = ".Rda", 
                          all.files = FALSE,
                          full.names = FALSE, 
                          recursive = FALSE,
                          ignore.case = FALSE, 
                          include.dirs = FALSE, 
                          no.. = FALSE)

for (i in 1:length(import.list)){
  load(paste0("2000 to 2010/", import.list[i]))
  missing_data <- sum(is.na(temp$New_Speaker2)) + sum(temp$New_Speaker2 == "", na.rm = T)
  if (missing_data != nrow(temp)){
    table_speakers <- temp %>%
      filter(.,
             !is.na(New_Speaker2),
             New_Speaker2 != "") %>%
      plyr::ddply(.,
                  ~New_Speaker2,
                  summarize, 
                  Name_Count = n()) %>%
      arrange(., 
              New_Speaker2, 
              Name_Count) 
    print(i)
    xlsx::write.xlsx(table_speakers, file = paste0("Name Frequency Spreadsheet/", substr(import.list[i], 1, nchar(import.list[i])-4), ".xlsx"))                      
  }
  
}

missing_speaker_file <- list()

for (i in 1:length(import.list)){
  load(paste0("2000 to 2010/", import.list[i]))
  missing_data <- sum(is.na(temp$New_Speaker2)) + sum(temp$New_Speaker2 == "", na.rm = T)
  corrupted <- missing_data == nrow(temp)
  check <- as.data.frame(cbind(import.list[i], corrupted))
  colnames(check) <- c("Data", "Corrupted")
  missing_speaker_file[[i]] <- check
}

corrupted_data <- do.call(rbind, missing_speaker_file)
corrupted_data <- subset(corrupted_data, Corrupted == 1)