library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)


# link to the data
url <- "https://docs.google.com/spreadsheets/d/1xfSQqRQIq6pGkJ5jzzv2QhetmX5boaEZoNECpDwXe5I"
model_name <- "PRIMED GSR Data Dictionary"
model_description <- "Data dictionary for Genomic Summary Results in the PRIMED consortium"
model_version <- "0.1.3"


# table metadata
tables <- list(read_sheet(url, sheet="GSR_files_DD", skip=1, col_types="c"))
meta <- tibble(
    table=c("gsr_files_dd"),
    required="TRUE"
)
table_names <- meta$table
names(tables) <- table_names
rm(list = c("table_names", "url"))


# rename and reorder columns
for (i in 1:length(tables)) {
    tables[[i]] <- tables[[i]] %>%
        filter(!is.na(`Data type`)) %>% # keep only valid rows
        mutate(primary_key = ifelse(paste0(names(tables)[i], "_id") == Column, TRUE, NA)) %>%
        select(column = Column, 
               primary_key,
               required = Required,
               description = Description, 
               data_type = `Data type`, 
               enumerations = Enumerations, 
               examples = Examples, 
               notes = `Notes/comments`) %>%
        mutate(description=gsub('"', "'", description),
               notes=gsub('"', "'", notes)) # replace double with single quotes
}


# call in the sheets_to_list function that accepts two arguments:
# 1) the list describing which tables are in the Google Sheets file
# 2) the list of data tables corresponding to the first argument
source("sheets_to_list.R")
tab_list <- sheets_to_list(apply(meta, 1, as.list), tables)
rm(list = c("meta", "tables", "sheets_to_list"))


# initialize leading text
master <- list(
    # Overall File Details
    name = model_name,
    description = model_description,
    version = model_version,
    
    # Data Table Details
    tables = tab_list
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
