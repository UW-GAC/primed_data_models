library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)


# link to the data
url <- "https://docs.google.com/spreadsheets/d/1xfSQqRQIq6pGkJ5jzzv2QhetmX5boaEZoNECpDwXe5I"
model_name <- "PRIMED GSR Data Model"
model_description <- "Data model for Genomic Summary Results in the PRIMED consortium"
model_version <- "0.1.3"


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
        filter(!is.na(`Data type`)) %>% # keep only valid rows
        mutate(primary_key = ifelse(paste0(names(tables)[i], "_id") == Column, TRUE, NA)) %>%
        mutate(Description=gsub('"', "'", Description), # replace double with single quote
               Description=gsub('\n', ' ', Description), # replace newline with space
               `Notes/comments`=gsub('"', "'", `Notes/comments`), # replace double with single quote
               `Notes/comments`=gsub('\n', ' ', `Notes/comments`), # replace newline with space
               References=ifelse(grepl("omop_concept", References), NA, References)) # remove external table reference
    if ("Multi-value delimiter" %in% names(tmp)) {
        tables[[i]] <- tmp %>%
            select(column = Column, 
               primary_key,
               required = Required,
               description = Description, 
               data_type = `Data type`, 
               references = References, 
               enumerations = Enumerations, 
               multi_value_delimiter = `Multi-value delimiter`,
               examples = Examples, 
               notes = `Notes/comments`)
    } else {
        tables[[i]] <- tmp %>%
            select(column = Column, 
               primary_key,
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
write(out, "PRIMED_GSR_data_model.json")
