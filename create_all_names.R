setwd("C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data")
## Import the files names as a vector
import.list <- list.files(path = "C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data", 
                          pattern = ".Rda", 
                          all.files = FALSE,
                          full.names = FALSE, 
                          recursive = FALSE,
                          ignore.case = FALSE, 
                          include.dirs = FALSE, 
                          no.. = FALSE)
source("C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Programs/fix_names_again.R")
source("C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Programs/fix_names_again_again.R")
library('tidyverse')
name_list <- list()
for (i in 1:length(import.list)){
  load(import.list[i])
  if (temp[1,]$year > 1999 &  temp[1,]$year < 2015){
    temp <- clean_names(temp)
    temp <- clean_names_again(temp)
    save(temp, file = import.list[i])
    freq_names <- as.data.frame(table(temp$New_Speaker2))
    
    if (nrow(freq_names) > 0){
      colnames(freq_names) <- c("Name", "Freq") 
      name_list[[i]] <- freq_names  
    }  
  }
  
}

all_names <- do.call(rbind, name_list)

over_150 <- all_names %>%
               filter(., Freq > 149) %>%
               plyr::ddply(.,
                           ~Name, 
                           summarize,
                           Count = sum(Freq))



save(over_150, file = "over_150.Rda")
save(all_names, file = "all_names.Rda")
xlsx::write.xlsx(all_names, file = "all_names.xlsx")
