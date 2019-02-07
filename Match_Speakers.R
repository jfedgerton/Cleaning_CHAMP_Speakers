memory.limit(size = 5000000000)
library('readr')
library('tidyverse')
library('RecordLinkage') 
setwd('C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/data frames to check')
load('Speakers.Rda')

Updated_Speaker_File <- all_speakers %>%
          #sort(., Speaker) %>%
          mutate(., Original_Field = Speaker) %>%
            separate(., Speaker, into = c("Last_Name", "First Name"), sep = "\\,")



pairs <-  compare.dedup(Updated_Speaker_File, phonetic = c(1, 2))
rpairs <- epiWeights(rpairs)
summary(rpairs)
