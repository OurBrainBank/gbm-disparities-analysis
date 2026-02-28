# ============================================================
# 02_manuscript_tables.R
# ============================================================

library(dplyr)
library(tidyr)
library(writexl)

if (!dir.exists("output")) dir.create("output")

source("functions_models.R")

prep <- prepare_data(
  "Manuscript Analysis Dataset.csv",
  "codebook_levels.csv"
)

dat <- collapse_binary_outcomes(prep$data)
codebook <- prep$codebook

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

global_vars <- c(
  structural_predictors,
  covariates_global,
  binary_outcomes,
  multinomial_outcomes
)

dat_global <- dat %>%
  drop_na(any_of(global_vars))

# TABLE 2
Table2 <- run_binary_models(
  dat_global,
  binary_outcomes[1:4],
  structural_predictors,
  covariates_global,
  codebook
)

# TABLE 3
Table3 <- run_binary_models(
  dat_global,
  binary_outcomes[5:7],
  structural_predictors,
  covariates_global,
  codebook
)

# eTABLE 2
eTable2 <- run_binary_models(
  dat_global,
  binary_outcomes,
  structural_predictors,
  covariates_global,
  codebook,
  include_n = TRUE
)

# eTABLE 3
dat_patients <- dat_global %>%
  filter(participant_type == 1)

dat_lowrisk <- dat_patients %>%
  filter(patient_cog_recall_risk == 0)

eTable3 <- bind_rows(
  run_binary_models(dat_patients,
                    binary_outcomes,
                    structural_predictors,
                    covariates_patient,
                    codebook,
                    include_n = TRUE) %>%
    mutate(Sensitivity = "Patients Only"),
  run_binary_models(dat_lowrisk,
                    binary_outcomes,
                    structural_predictors,
                    covariates_patient,
                    codebook,
                    include_n = TRUE) %>%
    mutate(Sensitivity = "Excluding Cognitive Recall Risk")
)

# eTABLE 4
eTable4 <- run_multinom_models(
  dat_global,
  multinomial_outcomes,
  structural_predictors,
  covariates_global,
  codebook,
  significant_only = TRUE
) %>%
  rename(`Comparison (vs Yes)` = Response)

write_xlsx(
  list(
    Table2_Primary = Table2,
    Table3_Secondary = Table3,
    eTable2_FullModels = eTable2,
    eTable3_Sensitivity = eTable3,
    eTable4_Multinomial = eTable4
  ),
  file.path("output","manuscript_tables.xlsx")
)

cat("Manuscript tables exported.\n")