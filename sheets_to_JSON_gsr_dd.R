library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

url <- "https://docs.google.com/spreadsheets/d/1xfSQqRQIq6pGkJ5jzzv2QhetmX5boaEZoNECpDwXe5I"


tables <- list(read_sheet(url, sheet="GSR_files_DD", skip=1, col_types="c"))
meta_tsv <- tibble(
  entity="meta",
  required="TRUE",
  table=c("gsr_files_dd")
)
table_names <- meta_tsv$table
names(tables) <- table_names
rm(list = c("table_names", "url"))




# keep only non-empty rows
for (i in 1:length(tables)) {
  tables[[i]] <- tables[[i]][apply(tables[[i]], 1, function(x){sum(!is.na(x))}) != 0, ]
}



# rename keys to be lower case, and replace slashes or spaces with underscores
for (i in 1:length(tables)) {
  names(tables[[i]]) <- tolower(names(tables[[i]]))
  names(tables[[i]]) <- gsub(" |/", "_", names(tables[[i]]))
}
names(meta_tsv) <- tolower(names(meta_tsv))
rm(list = c("i"))




# create list structure describing all of the tables and related variables
# establish the table names
tab_list <- apply(meta_tsv[, -1], 1, as.list)
for (i in 1:length(tab_list)) {
  # manually remove null entries from the table list
  tab_list[[i]] <- tab_list[[i]][sapply(tab_list[[i]], function(x){!all(is.na(x))})]
  
  # make each row of the data frame a list and make a list of the rows
  tab_list[[i]] <- append(tab_list[[i]], list(apply(tables[[i]], 1, as.list)))
  
  # label the key as "Variables"
  var_loc <- length(tab_list[[i]])
  names(tab_list[[i]])[var_loc] <- "variables"
  
  # split "enumerations" and "examples" into vectors according to line breaks in the Google Sheets file
  for (j in 1:nrow(tables[[i]])) {
    
    # coerce enumerations to a vector
    make_enum_vec <- ifelse(is.na(tab_list[[i]][[var_loc]][[j]]$enumerations), NA,
                            strsplit(as.character(unlist(tab_list[[i]][[var_loc]][[j]]$enumerations)), split = "\n"))
    tab_list[[i]][[var_loc]][[j]]$enumerations <- unlist(make_enum_vec)
    
    # coerce examples to a vector
    make_examp_vec <- ifelse(is.na(tab_list[[i]][[var_loc]][[j]]$examples), NA,
                             strsplit(as.character(unlist(tab_list[[i]][[var_loc]][[j]]$examples)), split = "\n"))
    tab_list[[i]][[var_loc]][[j]]$examples <- unlist(make_examp_vec)
    
    # manually remove null entries from the variable list
    tab_list[[i]][[var_loc]][[j]] <- tab_list[[i]][[var_loc]][[j]][sapply(tab_list[[i]][[var_loc]][[j]], function(x){!all(is.na(x))})]
  }
}
rm(list = c("make_enum_vec", "make_examp_vec", "meta_tsv", "tables", "i", "j", "var_loc"))




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


