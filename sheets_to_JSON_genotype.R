# library packages
library(googlesheets4)
library(dplyr)
library(tidyr)
library(jsonlite)


url <- "https://docs.google.com/spreadsheets/d/1lwVMGT-TQaWbMWvi3hdqWuEthZvaKGOImINAqXguPaM"
model_name <- "PRIMED Genotype Data Model"
model_description <- "Data model for genotype data in the PRIMED consortium"
model_version <- "0.2.2"


# read in the data
meta <- read_sheet(url, sheet="Tables") %>%
  select(table=Table, required=Required)


# pull tables that will be converted to JSON
table_names <- meta$table
tables <- lapply(table_names, function(x){read_sheet(url, sheet = x, col_types = "c")})
names(tables) <- table_names
rm(list = c("table_names", "url"))


# rename and reorder columns
for (i in 1:length(tables)) {
    tables[[i]] <- tables[[i]] %>%
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
write(out, "PRIMED_genotype_data_model.json")
