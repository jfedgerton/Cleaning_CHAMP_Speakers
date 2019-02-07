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
all_shows <- list.files(path = "CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search", 
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
                  Count >= 250)
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

remove_titles <- c("DR. ", "DOCTOR", "PRESIDENT", "PRES. ", "NOMINEE", "SEN. ", "SENATOR ", "MR. ", "MRS. ", "MS. ", "GOVERNOR", "SENATOR", "PRESIDENT", "GOV. ", "SECRETARY ", "SEC'Y ", "SEC. ", "CONGRESSMAN", "CONGRESSWOMAN", "SPEAKER", "REP. ", "REPRESENTATIVE")
drop <- !(all_shows %in% c("Names_and_show.Rda", "names_frequency.Rda", "Text_Search.Rda"))
all_shows <- all_shows[drop == T]
for (i in 8:17){
  ## Loop through all files 
  load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/", all_shows[i]))
  temp$unique.observation <- as.character(temp$unique.observation)
  ## Change the text variable from factor to string
  temp$Text <- toupper(as.character(temp$Text))
  missing_speaker     <- filter(temp, is.na(New_Speaker3))
  
  if (nrow(missing_speaker) != 0){
    missing_speaker$New_Speaker4 <- NA  
  }
  
  
  
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
      if (j == round(length(lastnames)*0.1)){
        print(paste0("10% done with ", all_shows[i]))
      }
      
      if (j == round(length(lastnames)*0.2)){
        print(paste0("20% done with ", all_shows[i]))
      }
      
      
      if (j == round(length(lastnames)*0.3)){
        print(paste0("30% done with ", all_shows[i]))
      }
      
      if (j == round(length(lastnames)*0.4)){
        print(paste0("40% done with ", all_shows[i]))
      }
      
      if (j == round(length(lastnames)*0.5)){
        print(paste0("50% done with ", all_shows[i]))
      }
      
      
      if (j == round(length(lastnames)*0.6)){
        print(paste0("60% done with ", all_shows[i]))
      }
      
      if (j == round(length(lastnames)*0.7)){
        print(paste0("70% done with ", all_shows[i]))
      }
      
      if (j == round(length(lastnames)*0.8)){
        print(paste0("80% done with ", all_shows[i]))
      }
      
      if (j == round(length(lastnames)*0.9)){
        print(paste0("90% done with ", all_shows[i]))
      }
      
      if (nrow(temp_missing) != 0){
        temp_missing$New_Speaker4 <- paste0(lastnames[j], ", ", firstnames[j])
        
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
  temp$New_Speaker4[temp$New_Speaker4 == "FREMD, VON"] <- "VON FREMD, MIKE"  
  temp$New_Speaker4[temp$New_Speaker4 == "SULEMAN, N"] <- "SULEMAN, NADYA" 
  temp$New_Speaker4[temp$New_Speaker4 == "CAROLINA PANTHERS CHEERLEADER"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "DURHAM, NORTH CAROLINA"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", ANDREW"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", CARRIE"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", CHARLOTTE"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", JOHN"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", MARIO RICCIO"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", MEIR"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", NAMTHIP"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", STUDENT"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", SUDJADNAN"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", SUWAT"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", TODD"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", TONY FARRANTE"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", UNIDENTIFIED"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == ", WILLA MAE"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, A US"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, ADEL"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, AI"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, ANNA"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, AOL"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, AOLTV"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, AYMAN"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, BALDWIN"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, BEGALA"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, BJ"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, CASEY"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, CPL"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, DONNA"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, DS"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, ERIKA"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, FBI"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, GD"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, GHAZI"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, HAFEZ"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, HANCOCKS"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, HUSSAIN"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, ICO TO"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, ID"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, IMAM GIUMAA"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, IN DEPTH"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, JANELLE"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, JEANNE"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, JJ"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, KAGAN"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, KAREN"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, KASICH"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, KELSEY SMITH"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, KOPPEL"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, MAHMOUD"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, ME"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, MIKE"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, MOWAFFAK"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, MUNAF"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, MUSHIR"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, NASA"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, NBC"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, NOW JAN"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, OK"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, SARS"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, SGT"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, SPC"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, US"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "A, WILL"] <- NA
  temp$New_Speaker4[temp$New_Speaker4 == "ABC, JAKE TAPPER"] <- "TAPPER, JACK"
  temp$New_Speaker4[temp$New_Speaker4 == "ABRAHAM, LYNNE M"] <- "ABRAHAM, LYNNE"
  temp$New_Speaker4[temp$New_Speaker4 == "BLITZER, WOLFF"] <- "BLITZER, WOLF"
  temp$New_Speaker4[temp$New_Speaker4 == "BLITZTER, WOLF"] <- "BLITZER, WOLF"
  temp$New_Speaker4[temp$New_Speaker4 == "FBLITZER, WOL"] <- "BLITZER, WOLF"
  temp$New_Speaker4[temp$New_Speaker4 == "HOST, WOLF BLITZER"] <- "BLITZER, WOLF"
  temp$New_Speaker4[temp$New_Speaker4 == "KING, LARRY,"] <- "KING, LARRY"
  temp$New_Speaker4[temp$New_Speaker4 == "KING, LARY"] <- "KING, LARRY"
  temp$New_Speaker4[temp$New_Speaker4 == "KING, LLEWELLYN"] <- "KING, LLEWELLYLN"
  temp$New_Speaker4[temp$New_Speaker4 == "OREILLY, B"] <- "OREILLY, BILL"
  temp$New_Speaker4[temp$New_Speaker4 == "OREILLY, BIILL"] <- "OREILLY, BILL"
  temp$New_Speaker4[temp$New_Speaker4 == "OREILLY, BILL"] <- "OREILLY, BILL"
  temp$New_Speaker4[temp$New_Speaker4 == "OREILLY, BILL,"] <- "OREILLY, BILL"
  temp$New_Speaker4[temp$New_Speaker4 == "SMITH, HARR"] <- "SMITH, HARRY"
  temp$New_Speaker4[temp$New_Speaker4 == "COOPER, ANDERCON"] <- "COOPER, ANDERSON"
  temp$New_Speaker4[temp$New_Speaker4 == "COOPER, ANDERON"] <- "COOPER, ANDERSON"
  temp$New_Speaker4[temp$New_Speaker4 == "COOPER, ANDERSOJN"] <- "COOPER, ANDERSON"
  temp$New_Speaker4[temp$New_Speaker4 == "COOPER, ANDERSON"] <- "COOPER, ANDERSON"
  temp$New_Speaker4[temp$New_Speaker4 == "COOPER, ANDERSON,"] <- "COOPER, ANDERSON"
  temp$New_Speaker4[temp$New_Speaker4 == "COOPER, ANDERSONS"] <- "COOPER, ANDERSON"
  temp$New_Speaker4[temp$New_Speaker4 == "DOBBS, LOUB"] <- "DOBBS, LOU"
  temp$New_Speaker4[temp$New_Speaker4 == "DOBBS, LOUD"] <- "DOBBS, LOU"
  temp$New_Speaker4[temp$New_Speaker4 == "MATHEWS, CHRIS"] <- "MATTHEWS, CHRIS"
  temp$New_Speaker4[temp$New_Speaker4 == "BECK, GLEN"] <- "BECK, GLENN"
  temp$New_Speaker4[temp$New_Speaker4 == "CHEN, JULE"] <- "CHEN, JULIE"
  temp$New_Speaker4[temp$New_Speaker4 == "CHEN, JULEI"] <- "CHEN, JULIE"
  temp$New_Speaker4[temp$New_Speaker4 == "CHEN, JULIE,"] <- "CHEN, JULIE"
  temp$New_Speaker4[temp$New_Speaker4 == "CHEN, JULIES"] <- "CHEN, JULIE"
  temp$New_Speaker4[temp$New_Speaker4 == "CHEN, JULLIE"] <- "CHEN, JULIE"
  temp$New_Speaker4[temp$New_Speaker4 == "SUSTEREN, VAM"] <- "VAN SUSTEREN, GRETA"
  temp$New_Speaker4[temp$New_Speaker4 == "SUSTEREN, VAN"] <- "VAN SUSTEREN, GRETA"
  temp$New_Speaker4[temp$New_Speaker4 == "SUSTEREN, VAN,"] <- "VAN SUSTEREN, GRETA"
  temp$New_Speaker4[temp$New_Speaker4 == "SUSTEREN, VANE"] <- "VAN SUSTEREN, GRETA"
  temp$New_Speaker4[temp$New_Speaker4 == "PHILLIPS, KYRAN"] <- "PHILLIPS, KYRA"
  temp$New_Speaker4[temp$New_Speaker4 == "GUMBEL, BYANT"] <- "GUMBEL, BRYANT"
  temp$New_Speaker4[temp$New_Speaker4 == "LEMON, DAN"] <- "LEMON, DON"
  temp$New_Speaker4[temp$New_Speaker4 == "LEMON, DOM"] <- "LEMON, DON"
  
  save(temp, file = paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search/", all_shows[i]))
}

name_find <- paste0(lastnames, ", ", firstnames)
for (i in 1:length(all_shows)){
  load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search/", all_shows[i]))
  temp <- dplyr::select(temp, -check)
  
  for (i in 1:length(lastnames)){
    temp$flag <- F
    temp$flag[temp$New_Speaker4 %in% name_find] <- T
    
    correct_names <- temp %>%
      filter(.,
             flag == T)
    incorrect_names <- temp %>%
      filter(.,
             !(unique.observation %in% correct_names$unique.observation))
    
  }
  
}


name_list <- list()
for (i in 1:length(all_shows)){
  load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search/", all_shows[i]))
  if (all_shows[i] != "Text_Search.Rda"){
    speakers <- temp %>%
      plyr::ddply(., 
                  ~New_Speaker4,
                  summarize, 
                  Count = n()) 
    speakers$Show <- substr(all_shows[i], 1, nchar(all_shows[i])-9)
    name_list[[i]] <- speakers 
  }
}

all_names <- do.call(rbind, name_list)
names_and_show <- all_names %>%
  filter(.,
         !is.na(New_Speaker4))  %>%         
  mutate(.,
         ID = paste0(New_Speaker4, "@", Show)) %>%
  plyr::ddply(.,
              ~ID, 
              summarize, 
              Count = sum(Count)) %>%
  filter(., 
         Count >= 50) 

split_data  <- unlist(strsplit(names_and_show$ID, "@"))
names_and_show$Names   <- trimws(split_data[seq(from = 1, to = length(split_data)-1, by = 2)])
names_and_show$show  <- trimws(split_data[seq(from = 2, to = length(split_data), by = 2)])
names_and_show <- names_and_show %>%
                    dplyr::select(., 
                                  -ID)

names_frequency <- all_names %>%
               dplyr::select(.,
                             -Show) %>%
               plyr::ddply(.,
                            ~New_Speaker4, 
                            summarize, 
                            Count = sum(Count))

save(names_and_show, file = "CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search/Names_and_show.Rda")
save(names_frequency, file = "CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search/names_frequency.Rda")
