library(googlesheets4)
library(dplyr)
library(tidyr)
library(stringr)

url <- "https://docs.google.com/spreadsheets/d/1xfSQqRQIq6pGkJ5jzzv2QhetmX5boaEZoNECpDwXe5I"

dd <- read_sheet(url, sheet="GSR_files_DD", skip=1)

tsv_format <- dd %>%
    filter(!is.na(`Data type`)) %>%
    mutate(entity="Table",
           table="data",
           pk=NA, ref=NA,
           required=ifelse(as.character(Required) == "TRUE", TRUE, NA),
           type=ifelse(`Data type` == "enumeration", Column, `Data type`)) %>%
    select(entity, table,
           column=Column, type, required,
           pk, ref,
           note=Description) %>%
    mutate(note=gsub('"', "'", note)) # replace double with single quote

# enumerated values
enum_format <- dd %>%
    filter(!is.na(Enumerations)) %>%
    mutate(entity="enum") %>%
    select(entity,
           table=Column,
           column=Enumerations) %>%
    separate_rows(column, sep="\n") %>%
    mutate(column=str_trim(column)) %>%
    distinct()

out <- bind_rows(tsv_format, enum_format)

readr::write_tsv(out, file="PRIMED_GSR_data_dictionary.tsv", na="", escape="none")
