library(dplyr)
library(readr)
library(tools)

# number of rows in test data
n <- 20

file_names <- c(
  # "pilot",
  # "population_descriptor",
  "cmqt_flags",
  "cmqt_anthropometry",
  "cmqt_blood_pressure",
  "cmqt_lipids",
  "cmqt_hematology",
  "cmqt_glycemic",
  "cmqt_kidney_function",
  "diabetes_diabetes",
  "cvd_cad",
  "cancer_breast",
  "cancer_prostate",
  "family_history")

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

cmqt_flags <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  flag_pregnancy_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_acute_illness_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_bld_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_anemia_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_hiv_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_eskd_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_splenectomy_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_cirrhosis_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_fasting_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_lipids_med_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_bp_med_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_cvd_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_t2d_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_t1d_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
  flag_diabetes_other_1 = sample(x = c("no", "yes", "unknown", "data not collected"), size = n, replace = TRUE),
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

cmqt_blood_pressure <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  systolic_bp_1=rnorm(n, 120, 20),
  diastolic_bp_1=rnorm(n, 80, 10),
  hypertension_1=ifelse(systolic_bp_1 > 140 & diastolic_bp_1 > 90, 1, 0)
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

cmqt_hematology <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  rbc_1=rnorm(n, 4, 1.5),
  hemoglobin_1=rnorm(n, 13, 3),
  hematocrit_1=rnorm(n, 0.4, 0.4),
  mcv_1=(hematocrit_1 * 10) / rbc_1,
  mch_1=rnorm(n, 38, 2),
  mchc_1=rnorm(n, 34, 2),
  rdw_1=rnorm(n, 10, 1),
  wbc_1=rnorm(n, 8, 3),
  basophil_count_1=rnorm(n, 200, 100),
  eosinophil_count_1=rnorm(n, 200, 100),
  lymphocyte_count_1=rnorm(n, 1300, 1000),
  monocyte_count_1=rnorm(n, 450, 100),
  neutrophil_count_1=rnorm(n, 4000, 2000),
  platelet_count_1=rnorm(n, 800, 200),
  mean_platelet_volume_1=rnorm(n, 10, 2)
)

set.seed(4)

cmqt_glycemic <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  fasting_glucose_plasma_1=rnorm(n, 50, 20),
  fasting_glucose_serum_1=rnorm(n, 85, 10),
  fasting_insulin_1=rnorm(n, 36, 6),
  hba1c_1=rnorm(n, 3, 2)
)

set.seed(4)

cmqt_kidney_function <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  cystatin_c_1=rnorm(n, 3, 1),
  serum_creatinine_1=rnorm(n, .5, 0.25)
)

set.seed(4)

diabetes_diabetes <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  t1d_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.95, 0.05)),
  t2d_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.95, 0.05)),
)

set.seed(4)

cvd_cad <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  cad_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.95, 0.05)),
  cad_emerge_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.95, 0.05)),
  cad_emerge_mod_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.95, 0.05))
)

set.seed(4)

cancer_breast <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  breast_cancer_status_emerge_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.95, 0.05)),
  breast_cancer_status_registry_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.95, 0.05)),
  breast_cancer_status_survey_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.95, 0.05)),
  age_at_diagnosis_1=sapply(subject$age_at_obs, function(x) round(rtnorm(1, x, 5, x, 90))),
  year_at_diagnosis_1=round(rtnorm(n, 2010, 5, 1900, 2024)),
  breast_cancer_type_1=sample(x = c("unilateral", "bilateral"), size = n, replace = TRUE),
  cancer_behavior_1=sample(x = c("benign", "borderline", "in_situ", "invasive"), size = n, replace = TRUE),
  her2_1=sample(x = c("positive", "negative", "unknown"), size = n, replace = TRUE),
  pr_1=sample(x = c("positive", "negative", "unknown"), size = n, replace = TRUE),
  er_1=sample(x = c("positive", "negative", "unknown"), size = n, replace = TRUE),
  T_stage_clinical_1=sample(x = c("stage 1", "stage 2", "stage 3", "stage 4", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_pathological_1=sample(x = c("stage 1", "stage 2", "stage 3", "stage 4", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_uknown_1=sample(x = c("stage 1", "stage 2", "stage 3", "stage 4", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_clinical_2=sample(x = c("localized", "regional", "distant", "in_situ", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_pathological_2=sample(x = c("localized", "regional", "distant", "in_situ", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_unknown_2=sample(x = c("localized", "regional", "distant", "in_situ", "unstaged", "unknown"), size = n, replace = TRUE),
  nodal_involvement_1=sample(x = c("NX", "N0", "N1", "N2", "N3"), size = n, replace = TRUE),
  distant_metastasis_1=sample(x = c("MX", "M0", "M1"), size = n, replace = TRUE),
  stage_system_1=rep(NA, n),
  grade_clinical_1=sample(x = c("grade 1", "grade 2", "grade 3"), size = n, replace = TRUE),
  grade_pathological_1=sample(x = c("grade 1", "grade 2", "grade 3"), size = n, replace = TRUE),
  grade_unknown_1=sample(x = c("grade 1", "grade 2", "grade 3"), size = n, replace = TRUE),
  screening_history_1=sample(x = c(0, 1), size = n, replace = TRUE),
  recurrence_1=sample(x = c("recurrence_primary", "recurrence_second_primary", "unknown", "none"), size = n, replace = TRUE),
  surgery_1=sample(x = c(0, 1), size = n, replace = TRUE),
  radiotherapy_1=sample(x = c(0, 1), size = n, replace = TRUE),
  chemotherapy_1=sample(x = c(0, 1), size = n, replace = TRUE),
  hormone_therapy_1=sample(x = c("pharmaceutical", "surgical", "both", "none", "unknown"), size = n, replace = TRUE),
  NSAID_1=sample(x = c(0, 1), size = n, replace = TRUE),
  age_at_natural_menopause_1=sapply(subject$age_at_obs, function(x) round(rtnorm(1, x, 10, x, 90))),
  post_menopausal_hormone_use_1=sample(x = c(0, 1), size = n, replace = TRUE),
  parity_1=sample(x = c(0, 1, 2), size = n, replace = TRUE),
  age_at_first_birth_1=round(rtnorm(n, 28, 5, 0, 90)),
  age_at_menarche_1=round(rtnorm(n, 15, 3, 0, 90)),
  deceased_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.9, 0.1)),
  cause_of_death_breast_cancer_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.9, 0.1)),
  age_at_death_1=sapply(subject$age_at_obs, function(x) round(rtnorm(1, x, 20, x, 90))),
)

set.seed(4)

cancer_prostate <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep("visit_1", n),
  prostate_cancer_status_emerge_1=sample(x = c(0, 1), size = n, replace = TRUE),
  prostate_cancer_status_registry_1=sample(x = c(0, 1), size = n, replace = TRUE),
  prostate_cancer_status_survey_1=sample(x = c(0, 1), size = n, replace = TRUE),
  age_at_diagnosis_1=sapply(subject$age_at_obs, function(x) round(rtnorm(1, x, 5, x, 90))),
  year_at_diagnosis_1=round(rtnorm(n, 2010, 5, 1900, 2024)),
  cancer_behavior_1=sample(x = c("benign", "borderline", "in_situ", "invasive"), size = n, replace = TRUE),
  T_stage_clinical_1=sample(x = c("stage 1", "stage 2", "stage 3", "stage 4", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_pathological_1=sample(x = c("stage 1", "stage 2", "stage 3", "stage 4", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_uknown_1=sample(x = c("stage 1", "stage 2", "stage 3", "stage 4", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_clinical_2=sample(x = c("localized", "regional", "distant", "in_situ", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_pathological_2=sample(x = c("localized", "regional", "distant", "in_situ", "unstaged", "unknown"), size = n, replace = TRUE),
  T_stage_unknown_2=sample(x = c("localized", "regional", "distant", "in_situ", "unstaged", "unknown"), size = n, replace = TRUE),
  nodal_involvement_1=sample(x = c("NX", "N0", "N1", "N2", "N3"), size = n, replace = TRUE),
  distant_metastasis_1=sample(x = c("MX", "M0", "M1"), size = n, replace = TRUE),
  stage_system_1=rep(NA, n), 
  gleason_score_clinical_1=runif(n, 2, 10),
  gleason_score_pathological_1=runif(n, 2, 10),
  gleason_score_unknown_1=runif(n, 2, 10),
  psa_1=rtnorm(n, 1.5, 1, 0, 50),
  psa_at_diagnosis_1=rtnorm(n, 1.5, 1, 0, 50),
  screening_history_1=sample(x = c(0, 1), size = n, replace = TRUE),
  recurrence_1=sample(x = c("recurrence_primary", "recurrence_second_primary", "unknown", "none"), size = n, replace = TRUE),
  surgery_1=sample(x = c(0, 1), size = n, replace = TRUE),
  radiotherapy_1=sample(x = c(0, 1), size = n, replace = TRUE),
  chemotherapy_1=sample(x = c(0, 1), size = n, replace = TRUE),
  hormone_therapy_1=sample(x = c("pharmaceutical", "surgical", "both", "none", "unknown"), size = n, replace = TRUE),
  NSAID_1=sample(x = c(0, 1), size = n, replace = TRUE),
  deceased_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.9, 0.1)),
  cause_of_death_prostate_cancer_1=sample(x = c(0, 1), size = n, replace = TRUE, prob = c(0.9, 0.1)),
  age_at_death_1=sapply(subject$age_at_obs, function(x) round(rtnorm(1, x, 20, x, 90))),
)

set.seed(4)

family_history <- tibble(
  subject_id=rep(subject$subject_id),
  age_at_obs=rep(subject$age_at_obs),
  visit=rep(1, n),
  family_hx_cancer_breast_1=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_cancer_breast_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_cancer_prostate=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_cancer_prostate_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_cancer_pancreatic=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_cancer_pancreatic_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_cancer_colorectal=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_cancer_colorectal_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_cancer_lung=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_cancer_lung_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_cancer_any=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_type2_diabetes=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_type2_diabetes_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_myocardial_infarction=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_myocardial_infarction_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_dementia=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_dementia_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_type1_diabetes=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_type1_diabetes_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_asthma=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_asthma_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_stroke=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_stroke_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE),
  family_hx_heart_failure=sample(x = c(0, 1), size = n, replace = TRUE),
  family_hx_heart_failure_relatedness_1=sample(x = c("1st degree", "2nd degree", "unknown"), size = n, replace = TRUE)
)

# fill in table after uploading tsv files to anvil

bucket <- "gs://fc-e3b6ff37-761e-4e53-89c0-fb243b8bd8e5/test_data/"

readme <- tibble(
  read_me = c(NA)
)

subject <- subject %>% select(-age_at_obs)


setwd("~/Downloads/primed_data_models")
write_tsv(readme, "test_data/readme.tsv")
write_tsv(subject, "test_data/subject.tsv")
write_tsv(cmqt_flags, "test_data/cmqt_flags.tsv")
write_tsv(cmqt_anthropometry, "test_data/cmqt_anthropometry.tsv")
write_tsv(cmqt_blood_pressure, "test_data/cmqt_blood_pressure.tsv")
write_tsv(cmqt_lipids, "test_data/cmqt_lipids.tsv")
write_tsv(cmqt_hematology, "test_data/cmqt_hematology.tsv")
write_tsv(cmqt_glycemic, "test_data/cmqt_glycemic.tsv")
write_tsv(cmqt_kidney_function, "test_data/cmqt_kidney_function.tsv")
write_tsv(diabetes_diabetes, "test_data/diabetes_diabetes.tsv")
write_tsv(cvd_cad, "test_data/cvd_cad.tsv")
write_tsv(cancer_breast, "test_data/cancer_breast.tsv")
write_tsv(cancer_prostate, "test_data/cancer_prostate.tsv")
write_tsv(family_history, "test_data/family_history.tsv")

phenotype_harmonized <- tibble(
  # phenotype_harmonized_id=
  domain=(file_names), 
  md5sum=as.vector(md5sum(paste0("test_data/", file_names, ".tsv"))), 
  file_path=paste0(bucket, file_names, '.tsv'), 
  file_readme_path=paste0(bucket, 'readme.tsv'), 
  n_subjects=rep(n, length(file_names)), 
  n_rows=rep(n, length(file_names)),
)

write_tsv(phenotype_harmonized, "test_data/phenotype_harmonized.tsv")
