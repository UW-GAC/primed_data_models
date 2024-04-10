library(dplyr)
library(readr)

# number of rows in test data
n <- 20

# truncated normal distribution
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}

set.seed(4)

subject <- tibble(
  subject_id = paste0("subject", 1:n),
  age_at_obs=round(rtnorm(n, 58, 5, 0, 90))
)

cmqt_anthropometry <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  height_1=rnorm(n, 165, 7), # height in cm
  weight_1=rnorm(n, 80, 5), # weight in kg
  bmi_1=weight_1 / (height_1 / 100)^2, # bmi in km/m^2
  waist_hip_ratio_1=rnorm(n, 0.8, 0.08)
)

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

cmqt_blood_pressure <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep(1, n),
  systolic_bp_1=rnorm(n, 120, 20),
  diastolic_bp_1=rnorm(n, 80, 10),
  hypertension_1=ifelse(systolic_bp_1 > 140 & diastolic_bp_1 > 90, 1, 0)
)

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

# setwd("~/Downloads/primed_data_models")
write_tsv(subject, "test_data/subject.tsv")
write_tsv(cmqt_anthropometry, "test_data/cmqt_anthropometry.tsv")
write_tsv(cmqt_lipids, "test_data/cmqt_lipids.tsv")
write_tsv(cmqt_blood_pressure, "test_data/cmqt_blood_pressure.tsv")
# write_tsv(cmqt_hematology, "test_data/cmqt_hematology.tsv")
