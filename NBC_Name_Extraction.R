rm(list = ls())
library('tidyverse')
load("C:/Users/jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Edgelist/all_data.Rda")
all_shows <- list.files(path = "CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search", 
                        pattern = ".Rda", 
                        all.files = FALSE,
                        full.names = FALSE, 
                        recursive = FALSE,
                        ignore.case = FALSE, 
                        include.dirs = FALSE, 
                        no.. = FALSE)
all_shows <- all_shows[!(all_shows %in% "Text_Search.Rda")]
nbc_shows <- all_shows[grepl("NBC", all_shows) == T]

names <- all_data %>%
  mutate(Name = Speaker_Match) %>%
  plyr::ddply(.,
              ~Name,
              summarize,
              Count = n()) %>%
  arrange(-Count)
   


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


names$Name[grepl(",", names$Name) != T] <- paste0(names$Name, ",")

split_data  <- unlist(strsplit(names$Name, ","))
names$lastnames   <- trimws(split_data[seq(from = 1, to = length(split_data)-1, by = 2)])
names$firstnames  <- trimws(split_data[seq(from = 2, to = length(split_data), by = 2)])


for (i in 1:length(LETTERS)){
  names$firstnames[names$firstnames == LETTERS[i]] <- NA
}

for (i in 1:length(LETTERS)){
  names$lastnames[names$lastnames == LETTERS[i]] <- NA
}

names <- names %>%
  filter(.,
         !is.na(firstnames))

## Separate the first and last names for text extraction purposes 
split_data  <- unlist(strsplit(names$Name, ","))
lastnames   <- trimws(split_data[seq(from = 1, to = length(split_data)-1, by = 2)])
firstnames  <- trimws(split_data[seq(from = 2, to = length(split_data), by = 2)])
lastnames <- c(lastnames, "WEIR")
firstnames <- c(firstnames, "BILL")
lastnames <- c(lastnames, "WEINER")
firstnames <- c(firstnames, "NANCY")
lastnames <- c(lastnames, "BARZ")
firstnames <- c(firstnames, "MIKE")
lastnames <- c(lastnames, "CASTRO")
firstnames <- c(firstnames, "MARYSOL")
lastnames <- c(lastnames, "SANDLER")
firstnames <- c(firstnames, "ADAM")
lastnames <- c(lastnames, "MARRIS")
firstnames <- c(firstnames, "JACQUELINE")
lastnames <- c(lastnames, "DESCHAINE")
firstnames <- c(firstnames, "ROB")
lastnames <- c(lastnames, "WINKLER")
firstnames <- c(firstnames, "KELLY")
lastnames <- c(lastnames, "DESCHAINE")
firstnames <- c(firstnames, "MEGAN")
lastnames <- c(lastnames, "DOLGOFF")
firstnames <- c(firstnames, "JOANNA")

remove_titles <- c("MR. ", "MRS. ", "DR. ", "DOCTOR", "PRESIDENT", "PRES. ", "NOMINEE", "SEN. ", "SENATOR ", "MR. ", "MRS. ", "MS. ", "GOVERNOR", "SENATOR", "PRESIDENT", "GOV. ", "SECRETARY ", "SEC'Y ", "SEC. ", "CONGRESSMAN", "CONGRESSWOMAN", "SPEAKER", "REP. ", "REPRESENTATIVE")

nbc_shows <- nbc_shows[30:90]
## Merge in cleaned speaker data
for (i in 1:length(nbc_shows))
{
  load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/Network Data/Final Clean/", nbc_shows[i]))
  temp$Speaker_Match <- NA
  temp$unique.observation <- as.character(temp$unique.observation)
  ## Change the text variable from factor to string
  temp$Text <- toupper(as.character(temp$Text))
  missing_speaker     <- filter(temp, is.na(Speaker_Match))
  
  
  
  not_missing_speaker <- filter(temp, !(unique.observation %in% missing_speaker$unique.observation))
  not_missing_speaker$New_Speaker4 <- not_missing_speaker$New_Speaker3
  
  if (nrow(missing_speaker) != 0){
    missing_speaker$Text_Extract <- T  
  }
  
  
  if (nrow(not_missing_speaker) != 0){
    not_missing_speaker$Text_Extract <- F  
  }
  
  
  ## Loop through the frequency of last names for each row
  for (j  in 1:length(lastnames)){
    if (nrow(missing_speaker) != 0){
      missing_speaker$last_name_flag  <- rep(F, nrow(missing_speaker))
      missing_speaker$first_name_flag <- rep(F, nrow(missing_speaker))
      missing_speaker$temp_text <- substr(toupper(missing_speaker$Text), 1, 30)
      
      for (q in 1:length(remove_titles)){
        missing_speaker$temp_text <- gsub(remove_titles[q], "", missing_speaker$temp_text)
      }
      
      missing_speaker$last_name_flag  <- grepl(lastnames[j], missing_speaker$temp_text)
      missing_speaker$first_name_flag <- grepl(firstnames[j], missing_speaker$temp_text)
      
      temp_missing <- subset(missing_speaker, 
                             last_name_flag == T & 
                               first_name_flag == T)
      temp_missing <- dplyr::select(temp_missing, -temp_text)
     
      if (j == round(length(lastnames)*0.2)){
        print(paste0("10% done with ", nbc_shows[i]))
      }
      
      
      
      if (j == round(length(lastnames)*0.4)){
        print(paste0("20% done with ", nbc_shows[i]))
      }
      
      
      if (j == round(length(lastnames)*0.6)){
        print(paste0("30% done with ", nbc_shows[i]))
      }
      
      
      if (j == round(length(lastnames)*0.8)){
        print(paste0("40% done with ", nbc_shows[i]))
      }
      
      
      if (nrow(temp_missing) != 0){
        temp_missing$Speaker_Match <- paste0(lastnames[j], ", ", firstnames[j])
        
        not_missing_speaker <- temp_missing %>%
          dplyr::select(.,
                        -first_name_flag,
                        -last_name_flag) %>%
          rbind(., 
                not_missing_speaker)
        missing_speaker <- filter(missing_speaker, 
                                  !(unique.observation %in% not_missing_speaker$unique.observation)) 
      }
    } 
    
    if (j == length(lastnames) & nrow(missing_speaker) != 0){
      temp <- missing_speaker %>%
        dplyr::select(.,
                      -first_name_flag,
                      -last_name_flag,
                      -temp_text) %>%
        rbind(., 
              not_missing_speaker) 
    } 
    if (j == length(lastnames) & nrow(missing_speaker) == 0){
      temp <- not_missing_speaker
    }
  }
  
  
  
   
  lastnames_check <- lastnames %in% c("H", "YO", "QUEST", "SCOTT", "MILLER")
  lastnames_v2    <- lastnames[lastnames_check == F]
  firstnames_v2   <- firstnames[lastnames_check == F]
  lastnames_v2    <- c("CARLSON", lastnames_v2)
  firstnames_v2   <- c("TUCKER", firstnames_v2)
  lastnames_v2    <- lastnames_v2[1:250]
  firstnames_v2   <- firstnames_v2[1:250]
  
  for (q  in 1:length(lastnames_v2)){
    missing_speaker     <- filter(temp, is.na(Speaker_Match))
    not_missing_speaker <- filter(temp, !(unique.observation %in% missing_speaker$unique.observation))
    if (nrow(missing_speaker) != 0){
      missing_speaker$last_name_flag  <- rep(F, nrow(missing_speaker))
      missing_speaker$temp_text <- substr(toupper(missing_speaker$Text), 1, 20)
      
      for (z in 1:length(remove_titles)){
        missing_speaker$temp_text <- gsub(remove_titles[z], "", missing_speaker$temp_text)
      }
      
      missing_speaker$last_name_flag  <- grepl(lastnames_v2[q], missing_speaker$temp_text)
      
      temp_missing <- subset(missing_speaker, 
                             last_name_flag == T)
      temp_missing <- dplyr::select(temp_missing, -temp_text)
      if (q == round(length(lastnames_v2)*0.1)){
        print(paste0("50% done with ", nbc_shows[i]))
      }
      
      
      
      if (q == round(length(lastnames_v2)*0.3)){
        print(paste0("60% done with ", nbc_shows[i]))
      }
      
      if (q == round(length(lastnames_v2)*0.5)){
        print(paste0("80% done with ", nbc_shows[i]))
      }
      
      
      
      if (q == round(length(lastnames_v2)*0.9)){
        print(paste0("90% done with ", nbc_shows[i]))
      }
      
      if (nrow(temp_missing) != 0){
        temp_missing$Speaker_Match <- paste0(lastnames_v2[q], ", ", firstnames_v2[q])
        
        not_missing_speaker <- temp_missing %>%
          dplyr::select(.,
                        #-first_name_flag,
                        -last_name_flag) %>%
          bind_rows(., 
                not_missing_speaker)
        missing_speaker <- filter(missing_speaker, 
                                  !(unique.observation %in% not_missing_speaker$unique.observation)) 
        
        temp <- missing_speaker %>%
          bind_rows(not_missing_speaker) %>%
          dplyr::select(-last_name_flag)
      }
    } 
    
    if (q == length(lastnames) & nrow(missing_speaker) != 0){
      temp <- missing_speaker %>%
        dplyr::select(.,
                      #-first_name_flag,
                      -last_name_flag,
                      -temp_text) %>%
        bind_rows(., 
              not_missing_speaker) 
    } 
    if (q == length(lastnames) & nrow(missing_speaker) == 0){
      temp <- not_missing_speaker
    }
  }
  
  last_fix_missing <- temp %>%
    filter(is.na(Speaker_Match) & !is.na(New_Speaker5))
  coded_speaker <- temp %>%
    filter(!(unique.observation %in% last_fix_missing$unique.observation))
  last_fix_missing$Speaker_Match = last_fix_missing$New_Speaker5
  
  temp <- bind_rows(last_fix_missing, coded_speaker)
  temp <- temp[,!(colnames(temp) %in% c("Text_Extract", "temp_text"))]
  save(temp, file = paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search/Check NBC/", nbc_shows[i]))
  print(paste0("Pct Files Processed ", round(i/length(nbc_shows), 3)))
}
