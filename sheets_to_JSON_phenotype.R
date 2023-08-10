library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)


# link to the data
url <- "https://docs.google.com/spreadsheets/d/1kpWz-6QfjMPVtm62fQwm4hoxzXhR0dnKxVt02fbx9ks"
model_name <- "PRIMED Phenotype Data Model"
model_description <- "Data model for phenotype data in the PRIMED consortium"
model_version <-"1.2"


# table metadata
meta <- read_sheet(url, sheet="Description", skip=1) %>%
    select(table=Table, required=Required, url=Link)

#table_names <- meta$table
#tables <- lapply(table_names, function(x) read_sheet(url, sheet=x, skip=1))
#names(tables) <- table_names

table_info <- meta %>% select(table_name=table, table_url=url)
tables <- list()
for(i in 1:dim(table_info)[1]){
    url <- table_info$table_url[i]
    sheet_name <- table_info$table_name[i]
    tmp <- read_sheet(url, sheet=sheet_name, skip=1)
    tables[[i]] <- tmp
}
names(tables) <- table_info$table_name

rm(list = c("table_info", "url"))


# rename and reorder columns
for (i in 1:length(tables)) {
    tmp <- tables[[i]] %>%
        filter(!is.na(`Data type`)) %>% # keep only valid rows
        mutate(Required=as.logical(Required), # non-T/F values will be NA
               Description=gsub('"', "'", Description), # replace double with single quote
               Description=gsub('\n', ' ', Description), # replace newline with space
               `Notes/comments`=gsub('"', "'", `Notes/comments`), # replace double with single quote
               `Notes/comments`=gsub('\n', ' ', `Notes/comments`)) # replace newline with space
    if ("Primary key" %in% names(tmp)) {
        tmp <- tmp %>%
            rename(primary_key = `Primary key`)
    } else {
        tmp <- tmp %>%
            mutate(primary_key = ifelse(paste0(names(tables)[i], "_id") == Column, TRUE, NA))
    }
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
write(out, "PRIMED_phenotype_data_model.json")
