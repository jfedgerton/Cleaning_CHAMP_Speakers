############################
#####   Missing years  #####
############################

missing_years_function <- function(input){
  require('tidyverse')
  source('C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/network_fix_function.R')
  source('C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/show_fix_function.R')
  
  data <- input 
  data <- data %>%
            mutate(., 
                   network = toupper(`Channel Name`),
                   show    = toupper(`Show Name`)) %>%
              filter(., Year > 1999, Year < 2015) %>%
              dplyr::select(., 
                            -`Channel Name`,
                            -`Show Name`) %>%
                show_fix_function(.) %>%
                  network_fix_function(.) %>%
                    mutate(., 
                           Code_ID = paste(network, show, sep = "_"))
  
  year_summary <- data %>%
                    plyr::ddply(., ~Code_ID, summarize,
                                min_year = min(Year),
                                max_year = max(Year))
  
  #year_summary$min_year[year_summary$min_year < 2000] <- 2000
  #year_summary$max_year[year_summary$max_year > 2015] <- 2014
  
  show_year_sequence <- list() 
  for (i in 1:nrow(year_summary)){
    show_year_sequence[[i]] <- year_summary[i,]$min_year:year_summary[i,]$max_year
  }
  names(show_year_sequence) <- year_summary$Code_ID
  
  data_check <- as.data.frame((matrix(0, ncol = 15, nrow = nrow(year_summary))))
  for(i in 1:ncol(data_check)){
    col_name <- paste0("X", i+1999)
    colnames(data_check)[i] <- col_name
  }
  
  data_check$Code_ID <- year_summary$Code_ID  
  data_check <- data_check[,c(16, 1:15)]
  
  years_we_have <- list()
  for (i in 1:nrow(year_summary)){
    temp <- year_summary[i,]$Code_ID
    years_we_have[[i]] <- as.vector(subset(data, Code_ID == temp)$Year)
  }
  names(years_we_have) <- year_summary$Code_ID
  missing_years_list <- list()
  
  for (i in 1:length(years_we_have)){
    if (names(years_we_have)[i] == names(show_year_sequence)[i]){
    missing_years_list[[i]] <- show_year_sequence[[i]] %in% years_we_have[[i]]
    }
  }
  
  years_we_need <- list()
  for (i in 1:length(missing_years_list)){
    years_we_need[[i]] <-  subset(show_year_sequence[[i]], missing_years_list[[i]] == F) 
  }
  names(years_we_need) <- names(years_we_have)
  for (i in 1:length(years_we_need)){
    for (j in 2000:2014){
      if (length(years_we_need[[i]]) != 0 ){
        check <- sum(years_we_need[[i]] %in% j) 
      if (check > 0){
        variable <- paste0("X", j)
        data_check[,c(variable)][data_check$Code_ID == names(years_we_need)[i]] <- 1
        }
      }
    }
  }
  data_check$X2000[data_check$X2000 > 0] <- 2000
  data_check$X2001[data_check$X2001 > 0] <- 2001
  data_check$X2002[data_check$X2002 > 0] <- 2002
  data_check$X2003[data_check$X2003 > 0] <- 2003
  data_check$X2004[data_check$X2004 > 0] <- 2004
  data_check$X2005[data_check$X2005 > 0] <- 2005
  data_check$X2006[data_check$X2006 > 0] <- 2006
  data_check$X2007[data_check$X2007 > 0] <- 2007
  data_check$X2008[data_check$X2008 > 0] <- 2008
  data_check$X2009[data_check$X2009 > 0] <- 2009
  data_check$X2010[data_check$X2010 > 0] <- 2010
  data_check$X2011[data_check$X2011 > 0] <- 2011
  data_check$X2012[data_check$X2012 > 0] <- 2012
  data_check$X2013[data_check$X2013 > 0] <- 2013
  data_check$X2014[data_check$X2014 > 0] <- 2014
  data_check$Missing = rowSums(data_check[,2:ncol(data_check)]) 
  data_check <- filter(data_check, Missing > 0)                       
  
  
  return(data_check)
}  

missing.years <- missing_years_function(all_data)
save(missing.years, file = "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/data frames to check/Missing_Years.Rda")
sum(missing.years[,2:16] != 0)
