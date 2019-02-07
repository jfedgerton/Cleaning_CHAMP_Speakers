str(all_data)
library('tidyverse')
load("C:/Users/jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Edgelist/all_data.Rda")

## Demographic information 

all_data$Show <- substr(all_data$Show_Name, 1, 4)
all_data$Show <- gsub("_", "", all_data$Show)

all_data <- all_data %>%
  filter(!(Show %in% c("name", "Name")))

years_network <- list()
for (i in 2000:2010)
{
  years_network[[i - 1999]] <- filter(all_data, year == i)
}

show_data <- list()
for (i in 1:length(years_network))
{
  for (j in 1:length(unique(years_network[[i]]$Show)))
  {
    show_data_utterance <- years_network[[i]] %>%
      plyr::ddply(.,
                  ~Show,
                  summarize, 
                  Total_Utterances        = n(),
                  Black_Utterances        = sum(race == 4),
                  Latinx_Utterances       = sum(race == 5),
                  Arab_Utterances         = sum(race == 2),
                  White_Utterances        = sum(race == 7),
                  Other_Race_Utterances   = sum(race %in% c(1:3, 6, 8:9)),
                  Male_Utterances         = sum(gender == 2),
                  Female_Utterances       = sum(gender == 1),
                  Democrat_Utterances     = sum(party == 1),
                  Republican_Utterances   = sum(party == 2),
                  Independent_Utterances  = sum(party == 3),
                  Third_Party_Utterances  = sum(party == 4),
                  Unaffiliated_Utterances = sum(!(party %in% c(1:4))))  %>%
      arrange(Show)
    show_data_utterance$Year <- unique(years_network[[i]]$year)
    
    unique_person <-  years_network[[i]] %>%
      distinct(Speaker_Match, .keep_all = T) %>%
      plyr::ddply(.,
                  ~Show,
                  summarize, 
                  Total_Unique        = n(),
                  Black_Unique        = sum(race == 4),
                  Latinx_Unique       = sum(race == 5),
                  Arab_Unique         = sum(race == 2),
                  White_Unique        = sum(race == 7),
                  Other_Race_Unique   = sum(race %in% c(1:3, 6, 8:9)),
                  Male_Unique         = sum(gender == 2),
                  Female_Unique       = sum(gender == 1),
                  Democrat_Unique     = sum(party == 1),
                  Republican_Unique   = sum(party == 2),
                  Independent_Unique  = sum(party == 3),
                  Third_Party_Unique  = sum(party == 4),
                  Unaffiliated_Unique = sum(!(party %in% c(1:4)))) %>%
      arrange(Show) %>%
      dplyr::select(-Show)
    
    show_data[[i]] <- cbind(show_data_utterance, unique_person)
  }
}


show_data <- list()
for (i in 1:length(years_network))
{
  for (j in 1:length(unique(years_network[[i]]$Show)))
  {
    crosstab_data <-  years_network[[i]] %>%
      distinct(Speaker_Match, .keep_all = T) %>%
      plyr::ddply(.,
                  ~Show,
                  summarize, 
                  Total_Unique        = n(),
                  
                  ## Black 
                  Black_Unique        = sum(race == 4),
                  Black_Democrat      = sum(race == 4 & party == 1),
                  Black_Women_Dem     = sum(race == 4 & gender == 2 & party == 1),
                  Black_Women_Rep     = sum(race == 4 & gender == 2 & party == 2),
                  Black_Republican    = sum(race == 4 & party == 2),
                  Black_Independent   = sum(race == 4 & party == 3),
                  Black_Third_Party   = sum(race == 4 & party == 4),
                  
                  ## Latinx 
                  Latinx_Unique        = sum(race == 5),
                  Latinx_Democrat      = sum(race == 5 & party == 1),
                  Latinx_Women_Dem     = sum(race == 5 & gender == 2 & party == 1),
                  Latinx_Women_Rep     = sum(race == 5 & gender == 2 & party == 2),
                  Latinx_Republican    = sum(race == 5 & party == 2),
                  Latinx_Independent   = sum(race == 5 & party == 3),
                  Latinx_Third_Party   = sum(race == 5 & party == 4),
                  
                  ## Arab 
                  Arab_Unique        = sum(race == 2),
                  Arab_Democrat      = sum(race == 2 & party == 1),
                  Arab_Women_Dem     = sum(race == 2 & gender == 2 & party == 1),
                  Arab_Women_Rep     = sum(race == 2 & gender == 2 & party == 2),
                  Arab_Republican    = sum(race == 2 & party == 2),
                  Arab_Independent   = sum(race == 2 & party == 3),
                  Arab_Third_Party   = sum(race == 2 & party == 4),
                  
                  ## White 
                  White_Unique        = sum(race == 7),
                  White_Democrat      = sum(race == 7 & party == 1),
                  White_Women_Dem     = sum(race == 7 & gender == 2 & party == 1),
                  White_Women_Rep     = sum(race == 7 & gender == 2 & party == 2),
                  White_Republican    = sum(race == 7 & party == 2),
                  White_Independent   = sum(race == 7 & party == 3),
                  White_Third_Party   = sum(race == 7 & party == 4), 
                  
                  ## Women 
                  White_Woman         = sum(gender == 2 & race == 7), 
                  Arab_Woman          = sum(gender == 2 & race == 2),
                  Black_Woman         = sum(gender == 2 & race == 4),
                  Latinx_Woman        = sum(gender == 2 & race == 5),
                  Democrat_Woman      = sum(gender == 2 & party == 1),
                  Republican_Woman    = sum(gender == 2 & party == 2)) %>%
      arrange(Show) %>%
      dplyr::select(-Show)
    
    show_data[[i]] <- cbind(show_data_utterance, unique_person)
  }
}

network_utterance_data_by_year <- do.call(rbind, show_data)
