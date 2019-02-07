##################################
####                          ####
####  Finding Missing Years   ####
####                          ####
##################################

rm(list=ls())
library('tidyverse')
## Set the working directory
setwd('C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs')
source('network_fix_function.R')
source('show_fix_function.R')
## Load the data from csv 1278
load('csv_1278_variable_check.Rda')
csv_1278 <- variable_data %>%
              mutate(., 
                     show = toupper(`Show Name`), 
                     network = toupper(`Channel Name`)) %>%
                network_fix_function(.) %>%
                  show_fix_function(.) %>%
                    dplyr::select(., 
                                  show, 
                                  network, 
                                  Year)

## Load the data from csv 3456
load('csv_3456_variable_check.Rda')
all_shows <- variable_data %>%
               mutate(., 
                      show = toupper(`Show Name`), 
                      network = toupper(`Channel Name`)) %>%
                 network_fix_function(.) %>%
                   show_fix_function(.) %>%
                     dplyr::select(., 
                                   show, 
                                   network, 
                                   Year) %>%
                               rbind(., csv_1278)


early_year_shows <- sum(all_shows < 2000)

show_names <- unique(all_shows[,c("show", "network")])
show_years <- as.data.frame(matrix(NA, nrow = nrow(show_names), ncol = 16))

for (i in 1:ncol(show_years)){
  colnames(show_years)[i] <- paste("X", i+1999, sep = "")
}
show_years$Channel <- show_names$network 
show_years$Show <- show_names$show
all_shows$Channel <- all_shows$network
all_shows$Show <- all_shows$show

all_shows <- dplyr::select(all_shows, -show, -network)
show_years <- show_years[,c(17:18, 1:16)]

for (i in 1:nrow(show_years)){
  for (j in 1:nrow(all_shows)){
    if (show_years[i,]$Channel == all_shows[j,]$Channel &
        show_years[i,]$Show    == all_shows[j,]$Show){
      
      if(all_shows[j,]$Year == 2000){
        show_years[i,]$X2000 = 1
      }
      if(all_shows[j,]$Year == 2001){
        show_years[i,]$X2001 = 1
      }
      if(all_shows[j,]$Year == 2002){
        show_years[i,]$X2002 = 1
      }
      if(all_shows[j,]$Year == 2003){
        show_years$X2003 = 1
      }
      if(all_shows[j,]$Year == 2004){
        show_years[i,]$X2004 = 1
      }
      if(all_shows[j,]$Year == 2005){
        show_years$X2005= 1
      }
      if(all_shows[j,]$Year == 2006){
        show_years[i,]$X2006 = 1
      }
      if(all_shows[j,]$Year == 2007){
        show_years[i,]$X2007 = 1
      }
      if(all_shows[j,]$Year == 2008){
        show_years[i,]$X2008 = 1
      }
      if(all_shows[j,]$Year == 2009){
        show_years[i,]$X2009 = 1
      }
      if(all_shows[j,]$Year == 2010){
        show_years[i,]$X2010 = 1
      }
      if(all_shows[j,]$Year == 2011){
        show_years[i,]$X2011 = 1
      }
      if(all_shows[j,]$Year == 2012){
        show_years[i,]$X2012 = 1
      }
      if(all_shows[j,]$Year == 2013){
        show_years[i,]$X2013 = 1
      }
      if(all_shows[j,]$Year == 2014){
        show_years[i,]$X2014 = 1
      }
      if(all_shows[j,]$Year == 2015){
        show_years[i,]$X2015 = 1
      }
    }
  }
}
save(show_years, file = "Show_Years.Rda")

