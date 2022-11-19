library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)


# link to the data
url <- "https://docs.google.com/spreadsheets/d/16_WvgGyUsQlnNmrZbJqqPlP2vMc9USDQYGAVU3u7JH4/edit#gid=1543372699"


# table metadata
meta <- tibble(
  table=c("subject", "cohort_data", "cohort_metadata", "pop_descriptors", "omop_person", "omop_measurement", "omop_concept"),
  required=TRUE
)

table_names <- meta$table
tables <- lapply(table_names, function(x) read_sheet(url, sheet=x, skip=1))
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
               references = References, 
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
    name = "PRIMED Phenotype Data Model",
    description = "Data model for phenotype data in the PRIMED consortium",
    version = "0",
    
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
