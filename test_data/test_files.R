library(dplyr)
library(readr)
library(tools)

# number of rows in test data
n <- 20

file_names <- c("cmqt_anthropometry", 
                "cmqt_lipids", 
                "cmqt_blood_pressure")

# truncated normal distribution
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}

set.seed(4)

subject <- tibble(
  subject_id = paste0("subject", 1:n),
  age_at_obs=round(rtnorm(n, 58, 5, 0, 90)),
  consent_code = sample(x = c("GRU", "HMB-IRB", "DS-CVD"), size = n, replace = TRUE),
  study_nickname = sample(x = c("UKBB", "JHS", "ARIC"), size = n, replace = TRUE), 
  dbgap_submission = c(rep(TRUE, 2), rep(FALSE, n-2)), 
  reported_sex = sample(x = c("Female", "Male", "Unknown", "Other"), size = n, replace = TRUE)
)

set.seed(4)

cmqt_anthropometry <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  height_1=rnorm(n, 165, 7), # height in cm
  weight_1=rnorm(n, 80, 5), # weight in kg
  bmi_1=weight_1 / (height_1 / 100)^2, # bmi in km/m^2
  waist_hip_ratio_1=rnorm(n, 0.8, 0.08)
)

set.seed(4)

cmqt_lipids <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  triglycerides_1=rnorm(n, 116, 13.6), # mg/dL
  hdl_1=rnorm(n, 55, 15),
  total_cholesterol_1=rnorm(n, 203, 41),
  ldl_1=rnorm(n, 122, 37),
  ldl_emerge_1=rnorm(n, 122, 37),
  non_hdl_1=rnorm(n, 81, 40),
)

set.seed(4)

cmqt_blood_pressure <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  systolic_bp_1=rnorm(n, 120, 20),
  diastolic_bp_1=rnorm(n, 80, 10),
  hypertension_1=ifelse(systolic_bp_1 > 140 & diastolic_bp_1 > 90, 1, 0)
)

# fill in table after uploading tsv files to anvil

bucket <- "gs://fc-e3b6ff37-761e-4e53-89c0-fb243b8bd8e5/test_data/"

readme <- tibble(
  read_me = c(NA)
)

phenotype_harmonized <- tibble(
  # phenotype_harmonized_id=
  domain=(file_names), 
  md5sum=as.vector(md5sum(paste0("test_data/", file_names, ".tsv"))), 
  file_path=paste0(bucket, file_names, '.tsv'), 
  file_readme_path=paste0(bucket, 'readme.tsv'), 
  n_subjects=rep(n, length(file_names)), 
  n_rows=rep(n, length(file_names)),
)

subject <- subject %>% select(-age_at_obs)

# cmqt_hematology <- tibble(
#   subject_id=rep(subject$subject_id),
#   age_at_obs=rep(subject$age_at_obs),
#   visit=rep(1, n),
#   rbc_1
#   hemoglobin_1=rnorm(n, 10, 20),
#   hematocrit_1=rnorm(n, 0.4, 0.4),
#   mcv_1
#   mch_1
#   mchc_1
#   rdw_1
#   wbc_1=rnorm(n, 100, 100),
#   basophil_count_1
#   eosinophil_count_1
#   lymphocyte_count_1
#   monocyte_count_1
#   neutrophil_count_1
#   platelet_count_1=rnorm(n, 800, 200)
#   mean_platelet_volume_1
# )

setwd("~/Downloads/primed_data_models")
write_tsv(readme, "test_data/readme.tsv")
write_tsv(subject, "test_data/subject.tsv")
write_tsv(cmqt_anthropometry, "test_data/cmqt_anthropometry.tsv")
write_tsv(cmqt_lipids, "test_data/cmqt_lipids.tsv")
write_tsv(cmqt_blood_pressure, "test_data/cmqt_blood_pressure.tsv")
write_tsv(phenotype_harmonized, "test_data/phenotype_harmonized.tsv")
# write_tsv(cmqt_hematology, "test_data/cmqt_hematology.tsv")
