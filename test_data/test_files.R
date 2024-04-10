library(dplyr)
library(readr)

n <- 20 # number of rows in test data

set.seed(4)

subject <- tibble(
  subject_id = paste0("subject", 1:n), 
  age_at_obs=round(runif(n, 20, 80))
)

cmqt_anthropometry <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep(1, n),
  height_1=rnorm(n, 500, 10), # height in cm
  weight_1=rnorm(n, 400, 5), # weight in kg 
  bmi_1=weight_1 / (height_1 / 100)^2, # bmi in km/m^2
  # waist_hip_ratio_1
)

cmqt_lipids <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep(1, n),
  triglycerides_1=rnorm(n, 600, 100), # mg/dL
  # hdl_1
  # total_cholesterol_1
  # ldl_1
  # non_hdl_1
)

cmqt_blood_pressure <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep(1, n),
  systolic_bp_1=rnorm(n, 100, 100),
  diastolic_bp_1=rnorm(n, 100, 100),
  # hypertension_1
)

cmqt_hematology <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep(1, n),
  # rbc_1
  hemoglobin_1=rnorm(n, 10, 20),
  hematocrit_1=rnorm(n, 0.4, 0.4),
  # mcv_1
  # mch_1
  # mchc_1
  # rdw_1
  wbc_1=rnorm(n, 100, 100),
  # basophil_count_1
  # eosinophil_count_1
  # lymphocyte_count_1
  # monocyte_count_1
  # neutrophil_count_1
  platelet_count_1=rnorm(n, 800, 200)
  # mean_platelet_volume_1
)

# setwd("~/Downloads/primed-file-checks/pheno_qc")
write_tsv(subject, "test_data/subject.tsv")
write_tsv(cmqt_anthropometry, "test_data/cmqt_anthropometry.tsv")
write_tsv(cmqt_lipids, "test_data/cmqt_lipids.tsv")
write_tsv(cmqt_blood_pressure, "test_data/cmqt_blood_pressure.tsv")
write_tsv(cmqt_hematology, "test_data/cmqt_hematology.tsv")
