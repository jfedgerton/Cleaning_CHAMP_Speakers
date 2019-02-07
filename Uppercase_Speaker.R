csv_1278 <- list.files(path = "C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/format_csv_1278", pattern = NULL, all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)


for (i in 1:length(csv_1278)){
  import_file_pathway <- paste0("C:/Users/Jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/format_csv_1278/", csv_1278[i])
  temp <- suppressMessages(read_delim(import_file_pathway,
                                      delim = "\t", escape_double = FALSE, trim_ws = TRUE))
  temp$Speaker <- toupper(temp$Speaker)
  write.table(temp, file = import_file_pathway, sep = "\t", row.names = F)
}
  
  