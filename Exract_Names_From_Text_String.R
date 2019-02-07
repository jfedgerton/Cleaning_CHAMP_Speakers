## Clear working directory
rm(list = ls())

## Load R libraries 
library('tidyverse')
library('foreach')
library('doParallel')

## Load R data
load("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text_Search.Rda")

str(names)
names <- names %>%
           mutate(., 
                  Name = as.character(Name)) %>%
           arrange(., 
                   -Count)

## Extract all the show files 
all_shows <- list.files(path = "CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated", 
                        pattern = ".Rda", 
                        all.files = FALSE,
                        full.names = FALSE, 
                        recursive = FALSE,
                        ignore.case = FALSE, 
                        include.dirs = FALSE, 
                        no.. = FALSE)

## Create a frequency of names for text extraction
frequency_list <- list()
for (i in  1:length(all_shows)){
  load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/", all_shows[i]))
  frequency_list[[i]] <- as.data.frame(table(temp$New_Speaker3))
  if (nrow(frequency_list[[i]]) > 0){
    colnames(frequency_list[[i]]) <- c("Name", "Frequency") 
  }
}
names <- do.call(rbind, frequency_list)
names <- names %>% 
           plyr::ddply(., 
                        ~Name,
                        summarize, 
                        Count = sum(Frequency, na.rm = T)) %>%
           arrange(.,
                   -Count) %>%
           filter(., 
                  Count >= 150)
str(names)
names <- mutate(names, 
                Name = as.character(Name))

names$Name[grepl(",", names$Name) != T] <- paste0(names$Name, ",")

split_data  <- unlist(strsplit(names$Name, ","))
names$lastnames   <- trimws(split_data[seq(from = 1, to = length(split_data)-1, by = 2)])
names$firstnames  <- trimws(split_data[seq(from = 2, to = length(split_data), by = 2)])

## Take out all first names that are just one letter
for (i in 1:length(LETTERS)){
  names$firstnames[names$firstnames == LETTERS[i]] <- NA
}

## Fix incorrect names
missing_names <- subset(names, is.na(firstnames))
not_missing_names <- subset(names, !is.na(firstnames))
missing_names$Name[missing_names$Name == "MCCAIN, J"] <- "MCCAIN, JOHN"
missing_names$Name[missing_names$Name == "LIEBERMAN, J"] <- "LIEBERMAN, JOHN"
missing_names$Name[missing_names$Name == "BUSH, JEB"] <- "BUSH, JEB"
missing_names$Name[missing_names$Name == "HOLMES, TJ,"] <- "HOLMES, TJ"
missing_names$Name[missing_names$Name == "HOLMES, TJ,"] <- "HOLMES, TJ"
missing_names$Name[missing_names$Name == "CLINTON, R"] <- "CLINTON, ROGER"
missing_names$Name[missing_names$Name == "SMITH, GARY B"] <- "SMITH, GARY"
missing_names$Name[missing_names$Name == "KENNEDY, T"] <- "SMITH, GARY"
missing_names$Name[missing_names$Name == "KERRY, J"] <- "KERRY, JOHN"
missing_names$Name[missing_names$Name == "CROSLIN, TIMMY"] <- "CROSLIN, MISTY"
missing_names$Name[missing_names$Name == "CROSLIN, TIMMY MISTY"] <- "CROSLIN, MISTY"
missing_names$Name[missing_names$Name == "EDWARDS, E"] <- "EDWARDS, ELIZABETH"
missing_names$Name[missing_names$Name == "EDWARDS, J"] <- "EDWARDS, JOHN"
missing_names$Name[missing_names$Name == "CYRUS, M"] <- "CYRUS, MILEY"
missing_names$Name[missing_names$Name == "RAMSEY, P"] <- "RAMSEY, PATSY"
missing_names$Name[missing_names$Name == "KENNEDY, ED"] <- "KENNEDY, TED"
missing_names$Name[missing_names$Name == "KENNEDY, ED M"] <- "KENNEDY, TED"
missing_names$Name[missing_names$Name == "KENNEDY, E"] <- "KENNEDY, TED"
missing_names$Name[missing_names$Name == "MEIER, T"] <- "MEIER, TINA"
missing_names$Name[missing_names$Name == "MARTIN, T"] <- "MARTIN, TIM"
missing_names$Name[missing_names$Name == "MARTIN, TI"] <- "MARTIN, TIM"
missing_names$Name[missing_names$Name == "AGOSTINO, D"] <- "DAGOSTINO, MARK"
missing_names$Name[missing_names$Name == "DARBY, J"] <- "DARBY, JOE"
missing_names$Name[missing_names$Name == "HEENE, R"] <- "HEENE, RICHARD"
missing_names$Name[missing_names$Name == "WILLIAMS, E"] <- "WILLIAMS, ERIC"
missing_names$Name[missing_names$Name == "RODHAM, H"] <- "CLINTON, HILLARY"
missing_names$Name[missing_names$Name == "SULEMAN, N"] <- "SULEMAN, NADYA"

## Create a new frequency with corrected names 
names <- rbind(missing_names, 
               not_missing_names)

names <- names %>%
           plyr::ddply(.,
                       ~Name,
                       summarize, 
                       Count = sum(Count, na.rm = T)) %>%
           arrange(.,
                   -Count)

names$Name[grepl(",", names$Name) != T] <- paste0(names$Name, ",")

split_data  <- unlist(strsplit(names$Name, ","))
names$lastnames   <- trimws(split_data[seq(from = 1, to = length(split_data)-1, by = 2)])
names$firstnames  <- trimws(split_data[seq(from = 2, to = length(split_data), by = 2)])

for (i in 1:length(LETTERS)){
  names$firstnames[names$firstnames == LETTERS[i]] <- NA
}

names <- names %>%
           filter(.,
                  !is.na(firstnames))

## Separate the first and last names for text extraction purposes 
split_data  <- unlist(strsplit(names$Name, ","))
lastnames   <- trimws(split_data[seq(from = 1, to = length(split_data)-1, by = 2)])
firstnames  <- trimws(split_data[seq(from = 2, to = length(split_data), by = 2)])



for (i in 10:length(all_shows)){
  ## Loop through all files 
  load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/", all_shows[i]))
  
  ## Change the text variable from factor to string
  temp$Text <- as.character(temp$Text)
  missing_speaker     <- filter(temp, is.na(New_Speaker3))
  missing_speaker$New_Speaker4 <- NA
  not_missing_speaker <- filter(temp, !is.na(New_Speaker3))
  not_missing_speaker$New_Speaker4 <- not_missing_speaker$New_Speaker3
  
  ## create an empty list for us to check if a last name and first name appear in the text string
  first_name_flag <- c()
  last_name_flag  <- c()
  

  ## Loop through the missing speaker rows 
  if (nrow(missing_speaker) > 0){
    for (z in 1:nrow(missing_speaker)){
      
      ## Loop through the frequency of last names for each row
      for (j in 1:length(lastnames)){
        last_name_flag[j]  <- grepl(lastnames[j], substr(missing_speaker$Text[z], 1,  25))
        first_name_flag[j] <- grepl(firstnames[j], substr(missing_speaker$Text[z], 1, 25))
        if (j == length(lastnames)){
          names$first_names_check <- first_name_flag
          names$last_names_check <- last_name_flag
          temp_names <- names
          temp_names$Name_Flag <- F
          temp_names$Name_Flag[temp_names$first_names_check == T & temp_names$last_names_check] <- T
          temp_names <- filter(temp_names, Name_Flag == T)
          
          ## Replace the missing name with the highest frequency name in the list
          if (nrow(temp_names) > 0){
            temp_names <- temp_names[1,]
            missing_speaker[z,]$New_Speaker4 <- temp_names$Name[1]
          }
        }
      } 
      
      if (z == round(0.25*nrow(missing_speaker))){
        print(paste0("Quater done with ", all_shows[i]))  
      }
      
      if (z == round(0.50*nrow(missing_speaker))){
        print(paste0("Halfway done with ", all_shows[i]))  
      }
      
      if (z == round(0.75*nrow(missing_speaker))){
        print(paste0("75% done with ", all_shows[i]))  
      }
       
      if (z == round(0.90*nrow(missing_speaker))){
        print(paste0("90% done with ", all_shows[i]))  
      } 
    }  
  }
  temp <- rbind(missing_speaker, not_missing_speaker)
  save(temp, file = paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search/", all_shows[i]))
  print(paste0("Pct Files Processed ", round(i/length(all_shows), 3)))
}

for (i in 1:length(all_shows)){
  load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search/", all_shows[i]))
  temp$New_Speaker4[temp$New_Speaker4 == "MCCAIN, J"] <- "MCCAIN, JOHN"
  temp$New_Speaker4[temp$New_Speaker4 == "LIEBERMAN, J"] <- "LIEBERMAN, JOHN"
  temp$New_Speaker4[temp$New_Speaker4 == "BUSH, JEB"] <- "BUSH, JEB"
  temp$New_Speaker4[temp$New_Speaker4 == "HOLMES, TJ,"] <- "HOLMES, TJ"
  temp$New_Speaker4[temp$New_Speaker4 == "HOLMES, TJ,"] <- "HOLMES, TJ"
  temp$New_Speaker4[temp$New_Speaker4 == "CLINTON, R"] <- "CLINTON, ROGER"
  temp$New_Speaker4[temp$New_Speaker4 == "SMITH, GARY B"] <- "SMITH, GARY"
  temp$New_Speaker4[temp$New_Speaker4 == "KENNEDY, T"] <- "SMITH, GARY"
  temp$New_Speaker4[temp$New_Speaker4 == "KERRY, J"] <- "KERRY, JOHN"
  temp$New_Speaker4[temp$New_Speaker4 == "CROSLIN, TIMMY"] <- "CROSLIN, MISTY"
  temp$New_Speaker4[temp$New_Speaker4 == "CROSLIN, TIMMY MISTY"] <- "CROSLIN, MISTY"
  temp$New_Speaker4[temp$New_Speaker4 == "EDWARDS, E"] <- "EDWARDS, ELIZABETH"
  temp$New_Speaker4[temp$New_Speaker4 == "EDWARDS, J"] <- "EDWARDS, JOHN"
  temp$New_Speaker4[temp$New_Speaker4 == "CYRUS, M"] <- "CYRUS, MILEY"
  temp$New_Speaker4[temp$New_Speaker4 == "RAMSEY, P"] <- "RAMSEY, PATSY"
  temp$New_Speaker4[temp$New_Speaker4 == "KENNEDY, ED"] <- "KENNEDY, TED"
  temp$New_Speaker4[temp$New_Speaker4 == "KENNEDY, ED M"] <- "KENNEDY, TED"
  temp$New_Speaker4[temp$New_Speaker4 == "KENNEDY, E"] <- "KENNEDY, TED"
  temp$New_Speaker4[temp$New_Speaker4 == "MEIER, T"] <- "MEIER, TINA"
  temp$New_Speaker4[temp$New_Speaker4 == "MARTIN, T"] <- "MARTIN, TIM"
  temp$New_Speaker4[temp$New_Speaker4 == "MARTIN, TI"] <- "MARTIN, TIM"
  temp$New_Speaker4[temp$New_Speaker4 == "AGOSTINO, D"] <- "DAGOSTINO, MARK"
  temp$New_Speaker4[temp$New_Speaker4 == "DARBY, J"] <- "DARBY, JOE"
  temp$New_Speaker4[temp$New_Speaker4 == "HEENE, R"] <- "HEENE, RICHARD"
  temp$New_Speaker4[temp$New_Speaker4 == "WILLIAMS, E"] <- "WILLIAMS, ERIC"
  temp$New_Speaker4[temp$New_Speaker4 == "RODHAM, H"] <- "CLINTON, HILLARY"
  temp$New_Speaker4[temp$New_Speaker4 == "SULEMAN, N"] <- "SULEMAN, NADYA"  
  save(temp, file = paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search/", all_shows[i]))
}


