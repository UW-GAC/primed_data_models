#remotes::install_github("UW-GAC/AnvilDataModels")

prefix <- "PRIMED_genotype_data_model_v0"
prefix <- "PRIMED_GSR_data_model_draft"

AnvilDataModels::tsv_to_dbml(paste0(prefix, ".tsv"), 
                             paste0(prefix, ".dbml"))

# check that data model object can be created
AnvilDataModels::tsv_to_dm(paste0(prefix, ".tsv"))
