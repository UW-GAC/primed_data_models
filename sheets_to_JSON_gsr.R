library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)


# link to the data
url <- "https://docs.google.com/spreadsheets/d/1xfSQqRQIq6pGkJ5jzzv2QhetmX5boaEZoNECpDwXe5I"


# table metadata
meta <- tibble(
    table=c("analysis", "gsr_file"),
    required="TRUE"
)
table_names <- meta$table
tables <- lapply(table_names, function(x) read_sheet(url, sheet=x, skip=1, col_types="c"))
names(tables) <- table_names
rm(list = c("table_names", "url"))


# rename and reorder columns
for (i in 1:length(tables)) {
    tmp <- tables[[i]] %>%
        filter(!is.na(`Data type`)) # keep only valid rows
    if ("Multi-value delimeter" %in% names(tmp)) {
        tables[[i]] <- tmp %>%
            select(column = Column, 
               required = Required,
               description = Description, 
               data_type = `Data type`, 
               references = References, 
               enumerations = Enumerations, 
               multi_value_delimeter = `Multi-value delimeter`,
               examples = Examples, 
               notes = `Notes/comments`)
    } else {
        tables[[i]] <- tmp %>%
            select(column = Column, 
               required = Required,
               description = Description, 
               data_type = `Data type`, 
               references = References, 
               enumerations = Enumerations, 
               examples = Examples, 
               notes = `Notes/comments`)
    }
}
rm(list = c("tmp"))


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
    name = "PRIMED GSR Data Model",
    description = "Data model for Genomic Summary Results in the PRIMED consortium",
    version = "0.1.2",
    
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
