library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)


# link to the data
url <- "https://docs.google.com/spreadsheets/d/1xfSQqRQIq6pGkJ5jzzv2QhetmX5boaEZoNECpDwXe5I"
model_name <- "PRIMED GSR Data Model"
model_description <- "Data model for Genomic Summary Results in the PRIMED consortium"
model_version <- "2.0"


# table metadata
meta <- read_sheet(url, sheet="Tables", skip=1, col_types="c") %>%
    select(table=Table, required=Required)
table_names <- meta$table
tables <- lapply(table_names, function(x) read_sheet(url, sheet=x, skip=1, col_types="c"))
names(tables) <- table_names


# rename and reorder columns
for (i in 1:length(tables)) {
    tmp <- tables[[i]] %>%
        filter(!is.na(`Data type`)) %>% # keep only valid rows
        mutate(primary_key = ifelse(paste0(names(tables)[i], "_id") == Column, TRUE, NA)) %>%
        mutate(is_bucket_path = ifelse(Column == "file_path", TRUE, NA)) %>%
        mutate(Description=gsub('"', "'", Description), # replace double with single quote
               Description=gsub('\n', ' ', Description), # replace newline with space
               `Notes/comments`=gsub('"', "'", `Notes/comments`), # replace double with single quote
               `Notes/comments`=gsub('\n', ' ', `Notes/comments`), # replace newline with space
               References=ifelse(grepl("omop_concept", References), NA, References)) # remove external table reference
    
    lookup <- c(
        data_type = "Data type", 
        multi_value_delimiter = "Multi-value delimiter",
        notes = "Notes/comments"
    )
    tmp <- tmp %>%
        rename(any_of(lookup)) %>%
        rename_with(tolower)
    
    keep_cols <- c(
        "column", 
        "primary_key",
        "required",
        "description", 
        "data_type", 
        "references", 
        "enumerations", 
        "is_bucket_path",
        "multi_value_delimiter",
        "examples", 
        "notes"
    )
    tables[[i]] <- tmp %>%
        select(any_of(keep_cols))
}


# call in the sheets_to_list function that accepts two arguments:
# 1) the list describing which tables are in the Google Sheets file
# 2) the list of data tables corresponding to the first argument
source("sheets_to_list.R")
tab_list <- sheets_to_list(apply(meta, 1, as.list), tables)


# initialize leading text
master <- list(
    # Overall File Details
    name = model_name,
    description = model_description,
    version = model_version,
    
    # Data Table Details
    tables = tab_list
)


# compile master file in JSON format
out <- toJSON(x = master,
              pretty = TRUE,
              auto_unbox = TRUE,
              unbox = TRUE)


# unquote the logical parameters TRUE and FALSE
out <- gsub(pattern = ': \"TRUE\"',  replacement = ': true',  x = out)
out <- gsub(pattern = ': \"FALSE\"', replacement = ': false', x = out)


# view the final version
out


# save the final version
write(out, "PRIMED_GSR_data_model.json")
