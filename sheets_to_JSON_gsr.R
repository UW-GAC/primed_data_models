library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)


# link to the data
url <- "https://docs.google.com/spreadsheets/d/1xfSQqRQIq6pGkJ5jzzv2QhetmX5boaEZoNECpDwXe5I"


# table metadata
meta <- tibble(
    required="TRUE",
    table=c("analysis", "gsr_file")
)
table_names <- meta$table
tables <- lapply(table_names, function(x) read_sheet(url, sheet=x, skip=1, col_types="c"))
names(tables) <- table_names
rm(list = c("table_names", "url"))


# keep only non-empty rows
for (i in 1:length(tables)) {
  tables[[i]] <- tables[[i]][apply(tables[[i]], 1, function(x){sum(!is.na(x))}) != 0, ]
}


# carry through the section labels due to format of Google Sheets document
# this could be changed in the Google Sheets file itself for clarity
for (i in 2:nrow(tables[[1]])) {
  if (is.na(tables[[1]]$Categories[i])) {
    tables[[1]]$Categories[i] <- tables[[1]]$Categories[i-1]
  }
}


# assign new names to the columns in the Google Sheets file
"categories" -> "Categories"
"required" -> "Required"
"column" -> "Column"
"description" -> "Description"
"references" -> "References"
"data_type" -> "Data type"
"enumerations" -> "Enumerations"
"multi_value_delimeter" -> "Multi-value delimeter"
"examples" -> "Examples"
"notes_comments" -> "Notes/comments"


# manually check that names are in the correct order
expected_names <- c("Categories", "Column", "Description", "Data type", "Required", "References",	"Enumerations", "Multi-value delimeter", "Examples", "Notes/comments", "CC", "CAPE", "CARDINAL", "D-PRISM", "EPIC-PRS", "FFAIRR-PRS",	"PREVENT",	"PRIMED-Cancer", "...8")
for (i in 1:length(tables)) {
  # check if all table names are in the expected list
  if (all(names(tables[[i]]) %in% expected_names)) {
    # if so, re-order as according to the preferred ordering
    tables[[i]] <- tables[[i]][, intersect(names(tables[[i]]), expected_names)]
    # re-label the columns to the preferred notation
    colnames(tables[[i]]) <- sapply(intersect(names(tables[[i]]), expected_names),
                                    function(x){ifelse(exists(x), get(x), x)})
  } else {
    # otherwise, identify which columns were unexpected
    unexpected_names <- names(tables[[i]])[(!(names(tables[[i]]) %in% expected_names))]
    stop(paste0("the following columns in the Google Sheets file were not expected:\n       ",
                paste0(1:length(unexpected_names), ". ", unexpected_names, collapse = "\n       ")))
  }
}
rm(list = c("i", "expected_names", expected_names[sapply(expected_names, exists)]))


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
write(out, "PRIMED_GSR_data_model.json")
