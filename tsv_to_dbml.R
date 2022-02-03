remotes::install_github("UW-GAC/AnvilDataModels")

tsv <- "PRIMED_data_model_draft.txt"
dbml <- "PRIMED_data_model_draft.dbml"
AnvilDataModels::tsv_to_dbml(tsv, dbml)
