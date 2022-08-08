library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

url <- "https://docs.google.com/spreadsheets/d/16_WvgGyUsQlnNmrZbJqqPlP2vMc9USDQYGAVU3u7JH4/edit#gid=1543372699"

# table metadata
meta_tsv <- tibble(
  entity="meta",
  required=TRUE,
  table=c("subject", "cohort_data", "cohort_metadata", "pop_desc", "omop_person", "omop_measurement", "omop_concept")
)

table_names <- meta_tsv$table
tables <- lapply(table_names, function(x) read_sheet(url, sheet=x, skip=1))
names(tables) <- table_names

tsv_format <- function(t) {
  tables[[t]] %>%
    filter(!is.na(`Data type`)) %>%
    mutate(entity="Table",
           table=t,
           pk=ifelse(paste0(t, "_id") == Column, TRUE, NA),
           type=ifelse(`Data type` == "enumeration", Column, `Data type`)) %>%
    select(entity, table,
           column=Column, type, required=Required,
           pk, ref=References,
           note=Description) %>%
    mutate(note=gsub('"', "'", note)) # replace double with single quote
}

out <- lapply(table_names, tsv_format) %>%
  bind_rows()

# enumerated values
enum_format <- function(t) {
  tables[[t]] %>%
    filter(!is.na(Enumerations)) %>%
    mutate(entity="enum") %>%
    select(entity,
           table=Column,
           column=Enumerations) %>%
    separate_rows(column, sep="\n") %>%
    mutate(column=str_trim(column))
}

enum_tsv <- lapply(table_names, enum_format) %>%
  bind_rows %>%
  distinct()

out <- bind_rows(out, enum_tsv, meta_tsv)

readr::write_tsv(out, file="PRIMED_phenotype_data_model_v0.tsv", na="", escape="none")
