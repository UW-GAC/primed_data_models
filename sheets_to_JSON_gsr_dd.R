library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)


# link to the data
url <- "https://docs.google.com/spreadsheets/d/1xfSQqRQIq6pGkJ5jzzv2QhetmX5boaEZoNECpDwXe5I"


# table metadata
tables <- list(read_sheet(url, sheet="GSR_files_DD", skip=1, col_types="c"))
meta <- tibble(
  required="TRUE",
  table=c("gsr_files_dd")
)
table_names <- meta$table
names(tables) <- table_names
rm(list = c("table_names", "url"))


# rename and reorder columns
for (i in 1:length(tables)) {
    tables[[i]] <- tables[[i]] %>%
        filter(!is.na(`Data type`)) %>% # keep only valid rows
        select(column = Column, 
               required = Required,
               description = Description, 
               data_type = `Data type`, 
               enumerations = Enumerations, 
               examples = Examples, 
               notes = `Notes/comments`)
}


# call in the sheets_to_list function that accepts two arguments:
# 1) the list describing which tables are in the Google Sheets file
# 2) the list of data tables corresponding to the first argument
source("sheets_to_list.R")
tab_list <- sheets_to_list(apply(meta, 1, as.list), tables)
rm(list = c("meta", "tables", "sheets_to_list"))


# initialize leading text
master <- list(
  list(
    # Overall File Details
    name = "PRIMED GSR Data Dictionary",
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
write(out, "PRIMED_GSR_data_dictionary.json")
