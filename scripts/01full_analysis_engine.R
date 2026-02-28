# ============================================================
# 01_full_analysis_engine.R
# Global Complete-Case Analytic Framework
# ============================================================

library(dplyr)
library(tidyr)
library(writexl)

if (!dir.exists("output")) dir.create("output")

source("functions_models.R")

# ------------------------------------------------------------
# 1. PREPARE DATA
# ------------------------------------------------------------

prep <- prepare_data(
  "Manuscript Analysis Dataset.csv",
  "codebook_levels.csv"
)

dat <- collapse_binary_outcomes(prep$data)
codebook <- prep$codebook

# ------------------------------------------------------------
# 2. DEFINE MODEL STRUCTURE
# ------------------------------------------------------------

structural_predictors <- c(
  "nci_center",
  "education",
  "agedx",
  "race_ethnicity",
  "geo_ruca",
  "insurance"
)

covariates_global  <- c("gender","participant_type")
covariates_patient <- c("gender")

binary_outcomes <- c(
  "clinical_trial_offer",
  "second_opinion_discussion",
  "tumor_storage_informed",
  "tumor_mutations_explained",
  "soc_recommended",
  "ttf_discussion",
  "tumor_testing_informed"
)

multinomial_outcomes <- c(
  "clinical_trial_offer_raw",
  "second_opinion_discussion_raw",
  "tumor_storage_informed_raw",
  "soc_recommended_raw",
  "ttf_discussion_raw",
  "tumor_testing_informed_raw"
)

# ------------------------------------------------------------
# 3. GLOBAL COMPLETE-CASE
# ------------------------------------------------------------

global_vars <- c(
  structural_predictors,
  covariates_global,
  binary_outcomes,
  multinomial_outcomes
)

dat_global <- dat %>%
  drop_na(all_of(global_vars))

cat("Global complete-case N =", nrow(dat_global), "\n")

# ------------------------------------------------------------
# 4. PRIMARY BINARY MODELS
# ------------------------------------------------------------

primary_models <- run_binary_models(
  dat_global,
  binary_outcomes,
  structural_predictors,
  covariates_global,
  codebook,
  include_n = TRUE
)

# ------------------------------------------------------------
# 5. SENSITIVITY — PATIENT ONLY
# ------------------------------------------------------------

dat_patients <- dat_global %>%
  filter(participant_type == 1)

sens_patient_models <- run_binary_models(
  dat_patients,
  binary_outcomes,
  structural_predictors,
  covariates_patient,
  codebook,
  include_n = TRUE
)

# ------------------------------------------------------------
# 6. SENSITIVITY — LOW RECALL
# ------------------------------------------------------------

dat_lowrisk <- dat_patients %>%
  filter(patient_cog_recall_risk == 0)

sens_lowrisk_models <- run_binary_models(
  dat_lowrisk,
  binary_outcomes,
  structural_predictors,
  covariates_patient,
  codebook,
  include_n = TRUE
)

# ------------------------------------------------------------
# 7. MULTINOMIAL MODELS
# ------------------------------------------------------------

multinom_models <- run_multinom_models(
  dat_global,
  multinomial_outcomes,
  structural_predictors,
  covariates_global,
  codebook,
  significant_only = FALSE
)

# ------------------------------------------------------------
# 8. EXPORT
# ------------------------------------------------------------

write_xlsx(
  list(
    Primary_Binary_Models = primary_models,
    Sensitivity_PatientOnly = sens_patient_models,
    Sensitivity_LowRecall = sens_lowrisk_models,
    Multinomial_Models = multinom_models
  ),
  file.path("output","full_analysis_output.xlsx")
)

cat("Full analysis engine complete.\n")