library('tidyverse')

all_shows <- list.files(path = "CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010", 
                        pattern = ".Rda", 
                        all.files = FALSE,
                        full.names = FALSE, 
                        recursive = FALSE,
                        ignore.case = FALSE, 
                        include.dirs = FALSE, 
                        no.. = FALSE)


show_subset <- toupper(unique(substr(all_shows, 1, nchar(all_shows)-9)))

## Delete wrong files
delete <- grepl("ERROR", show_subset) 
show_subset <- show_subset[delete != T]

## Delete wrong files
delete <- grepl("APM_APMMM", show_subset) 
show_subset <- show_subset[delete != T]

## Delete wrong files
delete <- grepl("CTV_CANADA_AM", show_subset) 
show_subset <- show_subset[delete != T]

## Delete wrong files
delete <- grepl("UNC_NAMES_WE", show_subset) 
show_subset <- show_subset[delete != T]

## Delete wrong files
delete <- grepl("NPR_ALL_THINGS_CONSIDERED", show_subset) 
show_subset <- show_subset[delete != T]

## Delete wrong files
delete <- grepl("PBS_WASHINGTON", show_subset) 
show_subset <- show_subset[delete != T]

## Delete wrong files
delete <- grepl("AUSTRALIA", show_subset) 
show_subset <- show_subset[delete != T]

## Find the frequency by show
show_frequency <- list()
for (i in 1:length(show_subset)){
  delete <- grepl(show_subset[i], all_shows) 
  show_freq <- all_shows[delete == T]
  sub_list <- list()
  for (j in 1:length(show_freq)){
    load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/", show_freq[j])) 
    temp$New_Speaker2[is.na(temp$New_Speaker2)] <- "NA"
    sub_list[[j]] <- as.data.frame(table(temp$New_Speaker2))
    
    colnames(sub_list[[j]]) <- c("Name", "Frequency")
    sub_list[[j]]$Frequency <- as.numeric(as.character(sub_list[[j]]$Frequency))
    sub_list[[j]]$Name      <- trimws(as.character(sub_list[[j]]$Name))
    if (j == length(show_freq)){
      Show_Freq <- do.call(rbind, sub_list)
      #Show_Freq$Name[is.na(Show_Freq$Name)] <- "NA"
      Show_Freq <- Show_Freq %>%
                     plyr::ddply(.,
                                 ~Name,
                                 summarize, 
                                 Frequency = sum(Frequency)) %>%
                     arrange(.,
                             Frequency)
    }
  }
  Show_Freq$Show <- show_freq[i]
  show_frequency[[i]] <- Show_Freq
}

## Remove titles
for (i in 1:length(show_frequency)){
  show_frequency[[i]]$Name <- gsub("\\.", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("PRESIDENT", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("PRES", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("REPRESENTATIVE", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("SEC'Y", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("SECRETARY", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("SENATOR", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub(", SEN ", ", ", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub(", REP ", ", ", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("GENERAL", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("SHERIFF", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("OFFICER", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("MAJOR", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("MAYOR", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("CAPTAIN", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("SPEAKER", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("CONGRESSMAN", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("CONGRESSWOMAN", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("HONORABLE", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("BISHOP", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("Speaker", "", show_frequency[[i]]$Name)
  show_frequency[[i]]$Name <- gsub("  ", " ", show_frequency[[i]]$Name)
  
  show_frequency[[i]] <- show_frequency[[i]] %>%
                            arrange(.,
                                    -Frequency) %>%
                            mutate(.,
                                   Name = trimws(Name),
                                   Name_Temp = Name)
  show_frequency[[i]]$Name_Temp[show_frequency[[i]]$Name_Temp == "NA"] <- "NA,"
  show_frequency[[i]]$Name_Temp[is.na(show_frequency[[i]]$Name_Temp)]  <- "NA,"
  show_frequency[[i]]$Name_Temp[show_frequency[[i]]$Name_Temp == ""]   <- "NA,"                          
  show_frequency[[i]]$Name_Temp[show_frequency[[i]]$Name_Temp == ","]  <- "NA,"                         
  

  show_frequency[[i]]$Name_Temp[grepl(",", show_frequency[[i]]$Name_Temp) != T] <- paste0(show_frequency[[i]]$Name_Temp, ",")
  
  name_split <- unlist(str_split(show_frequency[[i]]$Name_Temp, ",", n = 2))
  last_name  <- name_split[seq(from = 1, to = length(name_split)-1, by = 2)]
  first_name <- name_split[seq(from = 2, to = length(name_split), by = 2)]  
  show_frequency[[i]]$Last_Name  <- last_name
  show_frequency[[i]]$First_Name <- first_name
  show_frequency[[i]]$Last_Name[grepl("FILM", show_frequency[[i]]$Last_Name) == T]    <- NA
  show_frequency[[i]]$First_Name[grepl("FILM", show_frequency[[i]]$First_Name) == T]  <- NA
  show_frequency[[i]]$Last_Name[grepl("GAMES", show_frequency[[i]]$Last_Name) == T]   <- NA
  show_frequency[[i]]$First_Name[grepl("GAMES", show_frequency[[i]]$First_Name) == T] <- NA
  show_frequency[[i]]$Last_Name[grepl("WORLD", show_frequency[[i]]$Last_Name) == T]   <- NA
  show_frequency[[i]]$First_Name[grepl("WORLD", show_frequency[[i]]$First_Name) == T] <- NA
  show_frequency[[i]]$Last_Name[grepl("FELLA", show_frequency[[i]]$Last_Name) == T]   <- NA
  show_frequency[[i]]$First_Name[grepl("FELLA", show_frequency[[i]]$First_Name) == T] <- NA
  show_frequency[[i]]$Last_Name[grepl("NATIONNIECESCORED", show_frequency[[i]]$Last_Name) == T]   <- NA
  show_frequency[[i]]$First_Name[grepl("NATIONNIECESCORED", show_frequency[[i]]$First_Name) == T] <- NA
  show_frequency[[i]]$First_Name[nchar(show_frequency[[i]]$First_Name) > 15] <- NA
  show_frequency[[i]]$Last_Name[nchar(show_frequency[[i]]$Last_Name) > 15] <- NA
  
}

for (i in 1:length(show_frequency)){
  show_frequency[[i]]$First_Name <- trimws(show_frequency[[i]]$First_Name)
  show_frequency[[i]]$Last_Name  <- trimws(show_frequency[[i]]$Last_Name)
  show_frequency[[i]]$First_Name[is.na(show_frequency[[i]]$First_Name)] <- ""
  show_frequency[[i]]$Last_Name[is.na(show_frequency[[i]]$Last_Name)]   <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "MR"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "MS"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "REPRESENTATIVE"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "CONGRESSMAN"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "CONGRESSWOMAN"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "SENATOR"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "SEN"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "REP"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "SECRETARY"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "SEC'Y"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "MAYOR"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "GENERAL"] <- ""
  show_frequency[[i]]$First_Name[show_frequency[[i]]$First_Name == "MAJOR"] <- ""
  for (j in 1:nrow(show_frequency[[i]])){
    if (!is.na(show_frequency[[i]][j,]$First_Name)){
      if (show_frequency[[i]][j,]$First_Name == ""){
        last_name_replace <- subset(show_frequency[[i]],
                                    Last_Name == show_frequency[[i]][j,]$Last_Name &
                                      First_Name != "")
        last_name_replace <- last_name_replace[1,] 
        show_frequency[[i]][j,]$First_Name = trimws(last_name_replace$First_Name)
      } 
    }
  }
}

for (i in 1:length(show_subset)){
  show_frequency[[i]]$Show <- show_subset[i]
}

show_frequency[[163]]$First_Name[show_frequency[[163]]$Name == "CASEY,"] <- "BOB"
show_frequency[[163]]$First_Name[show_frequency[[163]]$Name == "CASEY, MR"] <- "BOB"

save(show_frequency, file = "CHAMP-Net/Data/Show-Year CSVs/formatted_data/frequency_by_show.Rdata")
