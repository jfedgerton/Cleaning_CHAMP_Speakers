setwd('C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/data frames to check/')
load('Speakers.Rda')
library('RecordLinkage')
library('tidyverse')

convert_speaker <- all_speakers %>% 
                     mutate(., 
                            Speaker_Conversion = toupper(Speaker), 
                            Speaker_Conversion = gsub("T\\. J\\.", "T\\.J\\.", Speaker_Conversion),
                            Speaker_Conversion = gsub('"', "", Speaker_Conversion),
                            Speaker_Conversion = gsub('ABBOTT\\.J', "ABBOTT\\. J", Speaker_Conversion),
                            Speaker_Conversion = gsub('GREGG', "GREG", Speaker_Conversion),
                            Speaker_Conversion = gsub('ABBOT, ED', "ABBOT, EDWARD", Speaker_Conversion), 
                            Formatted_Speaker = Speaker_Conversion) %>%
                       separate(., Speaker_Conversion, c("Last Name", "First Name"), sep = ",")

rpairsfuzzy <- compare.dedup(convert_speaker, blockfld = list("Last Name", "First Name")) %>%
                 epiWeights(.) %>%
                   getPairs(.) 

mutate(., Fuzzy_ID = sort(rep(1:(7896/3), 3)))

for (i in 1:nrow(rpairsfuzzy)){
  if (sum(i/3 == 1:2632) == 1){
    rpairsfuzzy[i,] <- NA 
  }
}



save(convert_speaker, file = "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/data frames to check/Probabalistic_Match.Rda")

all_names <- names(table(convert_speaker$Speaker_Conversion))
all_names <- all_names %>% as.data.frame(.)
all_names <- mutate(all_names, New_ID = 1:nrow(all_names))
colnames(all_names)[1] <- "Speaker_Conversion" 

New_ID_Speaker <- merge(convert_speaker, 
                        all_names, 
                        by = "Speaker_Conversion", 
                        all.x = T)
save(New_ID_Speaker, file = 'Convert_Speaker.Rda')
