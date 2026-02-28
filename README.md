# GBM Disparities Analysis

Reproducible R analysis scripts for the manuscript:

**Structural Differences in Communication and Access in Glioblastoma Care**

This repository contains the analytic code used to generate all primary, secondary, sensitivity, and multinomial regression models reported in the manuscript.

---

## Repository Structure

### `scripts/`

- **01_full_analysis_engine.R**  
  Runs the complete analytic pipeline and exports full model output.

- **02_manuscript_tables.R**  
  Generates manuscript-formatted tables.

- **functions_models.R**  
  Core modeling functions, data preparation, binary collapsing logic, and multinomial regression engine.

### `codebook_levels.csv`

Variable definitions, recoding rules, level mappings, and reference categories used in modeling.

---

## Analytic Framework

- Global complete-case analytic dataset  
- Multivariable logistic regression for binary outcomes  
- Multinomial logistic regression preserving original 3-level responses (Yes / No / Not sure)  
- “Yes” specified as the reference outcome for multinomial models  
- Structural predictors adjusted for gender and respondent type  
- Sensitivity analyses restricted to:
  - Patient respondents only  
  - Patients without elevated cognitive recall risk  

---

## Data Availability

The analytic dataset used for this manuscript is not publicly hosted in this repository.

Researchers may request access through **OurDataBank**:

https://www.ourbrainbank.org/ourdatabank/

Data access is subject to review and a data use agreement.

---

## Reproducibility

After obtaining the analytic dataset, place it in the working directory and run:

```r
source("scripts/functions_models.R")
source("scripts/02_manuscript_tables.R")
