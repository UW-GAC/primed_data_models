#remotes::install_github("UW-GAC/AnvilDataModels")

#prefix <- "PRIMED_genotype_data_model"
prefix <- "PRIMED_phenotype_data_model"
#prefix <- "PRIMED_GSR_data_model"

# check that data model object can be created
AnvilDataModels::json_to_dm(paste0(prefix, ".json"))

AnvilDataModels::json_to_dbml(paste0(prefix, ".json"), 
                              paste0(prefix, ".dbml"))
