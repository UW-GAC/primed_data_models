# option 1: list columns (enumerations or example) in the R code that will contain arrays from the "\n" break **preferred
# option 2: for any column, split on new lines to make an array


# library packages
library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)



# establish working directory
setwd("C:/Users/grant/Documents/UW/1_Autumn 22/Research Assistant")



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



# rename things as shown in the toy example
for (i in 1:length(tables)) {
  names(tables[[i]]) <- c("required", "name", "description", "references", "data_type", "enumerations", "examples", "notes_comments")
  tables[[i]] <- tables[[i]][, c("name", "description", "data_type", "references", "required", "enumerations", "examples", "notes_comments")]
}
names(meta) <- c("required", "name")
meta <- meta[, c("name", "required")]



# create list structure describing all of the tables and related variables
tab_list <- apply(meta, 1, as.list)
for (i in 1:length(tab_list)) {
  # insert data frame describing variables in each table
  tab_list[[i]] <- append(tab_list[[i]], list(data.frame(tables[[i]]))) # 
  names(tab_list[[i]])[length(tab_list[[i]])] <- "Variables"
  
  # coerce enumerations manually
  enum <- sapply(unlist(tab_list[[i]]$Variables$enumerations), function(x){strsplit(x, split = "\n")})
  enum <- unname(unlist(lapply(enum, function(x){ifelse(all(is.na(x)), NA, paste0('[', paste0(x, collapse = ', '), ']'))})))
  tab_list[[i]]$Variables$enumerations <- enum
}



# initialize leading text
master <- list(
  list(
    # Overall File Details
    name = "Example data model",
    description = "Example data model for PRIMED",
    version = "1.1",
    
    # Data Table Details
    tables = tab_list
  )
)



# compile master file in JSON format
out <- toJSON(x = master,
              pretty = TRUE,
              auto_unbox = TRUE,
              unbox = TRUE)



# remove outer brackets from the JSON file
# per solution provided at https://stackoverflow.com/questions/50240935/removing-brackets-from-ends-of-geojson-in-r
out <- substr(x = out, start = 2, stop = nchar(out) - 1)




# unquote the logical parameters TRUE and FALSE
out <- gsub(pattern = '\"TRUE\"',  replacement = 'true',  x = out)
out <- gsub(pattern = '\"FALSE\"', replacement = 'false', x = out)



# view the final version
prettify(out)



# save the final version
write(out, "PRIMED_genotype_data_model.json")


