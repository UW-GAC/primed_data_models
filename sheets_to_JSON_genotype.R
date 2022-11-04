# library packages
library(googlesheets4)
library(dplyr)
library(tidyr)
library(jsonlite)




# read in the data
url <- "https://docs.google.com/spreadsheets/d/1lwVMGT-TQaWbMWvi3hdqWuEthZvaKGOImINAqXguPaM"
meta <- read_sheet(url, sheet="Tables")
meta_tsv <- meta %>%
  mutate(entity="meta") %>%
  select(entity, required=Required, table=Table)




# pull tables that will be converted to JSON
table_names <- meta_tsv$table
tables <- lapply(table_names, function(x){read_sheet(url, sheet = x, col_types = "c")})
names(tables) <- table_names
rm(list = c("table_names", "url"))




# rename keys to be lower case, and replace slashes or spaces with underscores
for (i in 1:length(tables)) {
  names(tables[[i]]) <- tolower(names(tables[[i]]))
  names(tables[[i]]) <- gsub(" |/", "_", names(tables[[i]]))
}
names(meta) <- tolower(names(meta))
rm(list = c("i"))




# call in the sheets_to_list function that accepts two arguments:
# 1) the list describing which tables are in the Google Sheets file
# 2) the list of data tables corresponding to the first argument
source("sheets_to_list.R")
tab_list <- sheets_to_list(apply(meta, 1, as.list), tables)
rm(list = c("meta", "meta_tsv", "tables", "sheets_to_list"))




# initialize leading text
master <- list(
  list(
    # Overall File Details
    name = "PRIMED Genotype Data Model",
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
write(out, "PRIMED_genotype_data_model.json")



