setwd("C:/Users/Jared-Edgerton/Dropbox/CHAMP-Net/Data/Show-Year CSVs/formatted_data")

coded_names_unc <- read_excel("coded_names_unc.xlsx")
coded_names_unc$Name <- trimws(toupper(paste0(coded_names_unc$Last_Name, ", ", coded_names_unc$First_Name)))

coded_names_unc$Name[is.na(coded_names_unc$First_Name)] <- NA
coded_names_unc$Name[is.na(coded_names_unc$Last_Name)] <- NA

for (i in 1:nrow(coded_names_unc)){
  if (is.na(coded_names_unc[i,]$Last_Name)){
    coded_names_unc[i,]$Name <- trimws(toupper(coded_names_unc[i,]$First_Name))
  }
}

coded_names_unc$Name <- gsub("\\.", "", coded_names_unc$Name) 
coded_names_unc$Name <- gsub(" GOV ", " ", coded_names_unc$Name) 
coded_names_unc$Name <- gsub(" SEN ", " ", coded_names_unc$Name) 
coded_names_unc$Name <- gsub(" REP ", " ", coded_names_unc$Name) 

names_we_have <- as.data.frame(unique(coded_names_unc$Name))
colnames(names_we_have) <- "Name"
save(names_we_have, file = "names_we_have.Rda")
load("all_names.Rda")
all_name