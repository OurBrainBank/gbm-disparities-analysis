# ============================================================
# FUNCTIONS — FINAL MANUSCRIPT-STABLE VERSION
# Manual binary labeling + correct multinomial reference
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(broom)
library(stringr)
library(nnet)

# ============================================================
# MANUAL LABEL MAP FOR COLLAPSED BINARY OUTCOMES
# ============================================================

binary_outcome_labels <- list(
  clinical_trial_offer = list(
    outcome = "Clinical Trial Offer",
    levels  = c(
      "1" = "Offered clinical trial",
      "0" = "Not offered/no confirmed offer"
    )
  ),
  second_opinion_discussion = list(
    outcome = "Second Opinion Discussion",
    levels  = c(
      "1" = "Discussion occurred",
      "0" = "Not discussed/no confirmed discussion"
    )
  ),
  tumor_storage_informed = list(
    outcome = "Tumor Tissue Storage Informed",
    levels  = c(
      "1" = "Informed",
      "0" = "Not adequately informed"
    )
  ),
  tumor_testing_informed = list(
    outcome = "Tumor Testing Informed",
    levels  = c(
      "1" = "Informed",
      "0" = "Not adequately informed"
    )
  ),
  soc_recommended = list(
    outcome = "Standard-of-Care Recommended",
    levels  = c(
      "1" = "SOC recommended",
      "0" = "SOC not clearly recommended (no confirmed recommendation)"
    )
  ),
  ttf_discussion = list(
    outcome = "Tumor Treating Fields Discussion",
    levels  = c(
      "1" = "TTFields discussed",
      "0" = "Not discussed/no confirmed discussion"
    )
  )
)

# ============================================================
# 1. PREPARE DATA USING CODEBOOK
# ============================================================

prepare_data <- function(data_file, codebook_file){
  
  dat_raw  <- read_csv(data_file, show_col_types = FALSE)
  codebook <- read_csv(codebook_file, show_col_types = FALSE)
  
  codebook <- codebook %>%
    mutate(
      var = as.character(var),
      to_code = as.character(to_code),
      reference_level = as.character(reference_level)
    )
  
  rename_map <- codebook %>%
    distinct(var, label) %>%
    filter(!is.na(var), !is.na(label))
  
  dat <- dat_raw
  
  for(i in seq_len(nrow(rename_map))){
    if(rename_map$label[i] %in% names(dat)){
      names(dat)[names(dat) == rename_map$label[i]] <- rename_map$var[i]
    }
  }
  
  for(v in unique(codebook$var)){
    if(v %in% names(dat)){
      
      rec_map <- codebook %>% filter(var == v)
      
      lookup <- rec_map %>%
        select(from_level, to_code) %>%
        mutate(across(everything(), as.character))
      
      raw_vals <- as.character(dat[[v]])
      
      mapped_vals <- lookup$to_code[
        match(raw_vals, lookup$from_level)
      ]
      
      dat[[v]] <- factor(mapped_vals)
      
      ref_val <- rec_map %>%
        filter(!is.na(reference_level)) %>%
        pull(to_code)
      
      if(length(ref_val) == 1 &&
         ref_val %in% levels(dat[[v]])){
        dat[[v]] <- relevel(dat[[v]], ref_val)
      }
    }
  }
  
  list(data = dat, codebook = codebook)
}

# ============================================================
# 2. COLLAPSE RAW OUTCOMES TO BINARY
# ============================================================

collapse_binary_outcomes <- function(dat){
  
  collapse_binary <- function(x){
    x_char <- as.character(x)
    case_when(
      x_char == "1" ~ "1",
      x_char %in% c("0","2") ~ "0",
      TRUE ~ NA_character_
    )
  }
  
  raw_vars <- c(
    "clinical_trial_offer_raw",
    "second_opinion_discussion_raw",
    "tumor_storage_informed_raw",
    "soc_recommended_raw",
    "ttf_discussion_raw",
    "tumor_testing_informed_raw"
  )
  
  for(v in raw_vars){
    if(v %in% names(dat)){
      new_var <- str_remove(v, "_raw$")
      dat[[new_var]] <- factor(collapse_binary(dat[[v]]))
    }
  }
  
  dat
}

# ============================================================
# 3. FORMAT MODEL OUTPUT
# ============================================================

format_with_labels <- function(model_results,
                               outcome_name,
                               model_n,
                               codebook,
                               include_n,
                               response_level = NULL){
  
  if(outcome_name %in% names(binary_outcome_labels)){
    outcome_label <- binary_outcome_labels[[outcome_name]]$outcome
  } else {
    outcome_label <- codebook %>%
      filter(var == outcome_name) %>%
      distinct(label) %>%
      pull(label)
    
    if(length(outcome_label) == 0){
      outcome_label <- outcome_name
    }
    outcome_label <- outcome_label[1]
  }
  
  out <- model_results %>%
    filter(term != "(Intercept)") %>%
    mutate(
      var = str_remove(term, "[0-9]+$"),
      level_code = str_extract(term, "[0-9]+$")
    ) %>%
    left_join(
      codebook %>%
        select(var, to_code, to_label, label) %>%
        mutate(to_code = as.character(to_code)),
      by = c("var", "level_code" = "to_code")
    ) %>%
    mutate(
      Predictor = label,
      Category = if(outcome_name %in% names(binary_outcome_labels)){
        binary_outcome_labels[[outcome_name]]$levels[level_code]
      } else {
        to_label
      },
      Outcome = outcome_label,
      OR_CI = sprintf("%.2f (%.2f, %.2f)",
                      estimate, conf.low, conf.high),
      P = case_when(
        is.na(p.value) ~ NA_character_,
        p.value < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", p.value)
      ),
      n = if(include_n) model_n else NA
    )
  
  if(!is.null(response_level)){
    out <- out %>%
      mutate(Response = response_level) %>%
      select(Outcome, Response, Predictor, Category, OR_CI, P, n)
  } else {
    out <- out %>%
      select(Outcome, Predictor, Category, OR_CI, P, n)
  }
  
  return(out)
}

# ============================================================
# 4. RUN BINARY MODELS
# ============================================================

run_binary_models <- function(df, outcomes,
                              structural_predictors,
                              covariates,
                              codebook,
                              include_n = FALSE){
  
  map_dfr(outcomes, function(out){
    
    vars <- c(out, structural_predictors, covariates)
    model_df <- df %>% drop_na(any_of(vars))
    
    if(nrow(model_df) == 0) return(NULL)
    if(length(unique(model_df[[out]])) < 2) return(NULL)
    
    fit <- glm(
      reformulate(c(structural_predictors, covariates), out),
      data = model_df,
      family = binomial()
    )
    
    format_with_labels(
      tidy(fit, conf.int = TRUE, exponentiate = TRUE),
      out,
      nrow(model_df),
      codebook,
      include_n
    )
  })
}

# ============================================================
# 5. RUN MULTINOMIAL MODELS — YES AS REFERENCE
# ============================================================

run_multinom_models <- function(df, outcomes,
                                structural_predictors,
                                covariates,
                                codebook,
                                significant_only = FALSE){
  
  map_dfr(outcomes, function(out){
    
    vars <- c(out, structural_predictors, covariates)
    model_df <- df %>% drop_na(any_of(vars))
    
    if(nrow(model_df) == 0) return(NULL)
    if(length(unique(model_df[[out]])) < 3) return(NULL)
    
    # Force "Yes" (coded as 1) as reference
    model_df[[out]] <- relevel(model_df[[out]], ref = "1")
    
    fit <- multinom(
      reformulate(c(structural_predictors, covariates), out),
      data = model_df,
      trace = FALSE
    )
    
    res <- tidy(fit, conf.int = TRUE, exponentiate = TRUE)
    
    if(significant_only){
      res <- res %>% filter(p.value < 0.05)
    }
    
    map_dfr(unique(res$y.level), function(resp){
      
      resp_label <- codebook %>%
        filter(var == out,
               to_code == resp) %>%
        pull(to_label)
      
      resp_label <- paste0(resp_label, " (vs Yes)")
      
      format_with_labels(
        res %>% filter(y.level == resp),
        out,
        nrow(model_df),
        codebook,
        include_n = FALSE,
        response_level = resp_label
      )
    })
  })
}