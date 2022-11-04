library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)




# if the Google Sheets uses multiple delimiters to identify a string split, this function will perform multiple splits.
# the best option is probably to use consistent delimiters in Google Sheets for enumerations/examples
vstrsplit <- function(x, vsplit, ...){
  vsplit <- c("\n", "|", ",")
  for (s in vsplit) {
    x <- strsplit(x = x, split = s, fixed = TRUE)
    x <- unlist(x)
  }
  return(x)
}
rm(list = c("vstrsplit"))




# read in the data
url <- "https://docs.google.com/spreadsheets/d/1xfSQqRQIq6pGkJ5jzzv2QhetmX5boaEZoNECpDwXe5I"




# table metadata
meta_tsv <- tibble(
    entity="meta",
    required="TRUE",
    table=c("analysis", "gsr_file")
)
table_names <- meta_tsv$table
tables <- lapply(table_names, function(x) read_sheet(url, sheet=x, skip=1, col_types="c"))
names(tables) <- table_names
rm(list = c("table_names", "url"))




# keep only non-empty rows
for (i in 1:length(tables)) {
  tables[[i]] <- tables[[i]][apply(tables[[i]], 1, function(x){sum(!is.na(x))}) != 0, ]
}




# carry through the section labels due to format of Google Sheets document
# this could be changed in the Google Sheets file itself for clarity
for (i in 2:nrow(tables[[1]])) {
  if (is.na(tables[[1]]$Categories[i])) {
    tables[[1]]$Categories[i] <- tables[[1]]$Categories[i-1]
  }
}




# rename keys to be lower case, and replace slashes or spaces with underscores
for (i in 1:length(tables)) {
  names(tables[[i]]) <- tolower(names(tables[[i]]))
  names(tables[[i]]) <- gsub(" |/", "_", names(tables[[i]]))
}
names(meta_tsv) <- tolower(names(meta_tsv))
rm(list = c("i"))




# call in the sheets_to_list function that accepts two arguments:
# 1) the list describing which tables are in the Google Sheets file
# 2) the list of data tables corresponding to the first argument
source("sheets_to_list.R")
tab_list <- sheets_to_list(apply(meta_tsv[, -1], 1, as.list), tables)
rm(list = c("meta_tsv", "tables", "sheets_to_list"))
##############################################################################
# NOTE: this version uses "example" as the column name instead of "examples" #
##############################################################################



# initialize leading text
master <- list(
  list(
    # Overall File Details
    name = "PRIMED GSR Data Model",
    description = "Insert description here...",
    version = "1.3",
    
    # Data Table Details
    tables = tab_list
  )
)
rm(list = c("tab_list"))




# compile master file in JSON format
out <- toJSON(x = master,
              pretty = TRUE,
              auto_unbox = TRUE,
              unbox = TRUE)
rm(list = c("master"))




# unquote the logical parameters TRUE and FALSE
out <- gsub(pattern = ': \"TRUE\"',  replacement = ': true',  x = out)
out <- gsub(pattern = ': \"FALSE\"', replacement = ': false', x = out)




# view the final version
out




# save the final version
write(out, "PRIMED_GSR_data_model.json")

