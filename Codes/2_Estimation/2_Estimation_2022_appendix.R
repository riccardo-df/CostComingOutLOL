## @author: riccardo-df
## University of Rome Tor Vergata
## 09\08\2023

## The Cost of Coming Out - Synthetic Controls.

## This script runs the main analysis of the "The Cost of Coming Out" paper.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

# Settings --------------------------------------------
## Select champions.
champions <- c("Graves")

## Select outcome series.
outcome_colname_pool <- "pick_rate_pooled"
outcome_colname_regional <- "pick_rate"

bandwidth_pool <- 3
bandwidth_regional <- 3

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
donor_pools <- c("jungle", "middle", "top")
estimators <- c("sc", "sc_reg")
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- TRUE
n_boot <- 2
backdate <- 10

# covariates_pool <- c()
# covariates_regional <- c()
covariates_pool <- c("ban_level_sum", "win_level_sum", "gold_pooled", "kills_pooled", "assists_pooled", "deaths_pooled")
covariates_regional <- c("ban_level", "win_level", "gold_avg", "kills_avg", "assists_avg", "deaths_avg")

# Estimation --------------------------------------------------------------
## Loop over donor pools.
pooled_result_list <- list()
regional_result_list <- list()

counter <- 1

for (estimator in estimators) {
  for (pool in donor_pools) {
    cat("\n")
    cat("Estimator: ", estimator, " donor pool: ", pool, "\n", sep = "")
    cat("\n")

    pooled_result_list[[counter]] <- run_main_pooled(champions, outcome_colname_pool, pool, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth_pool, covariate_colnames = covariates_pool, max_date = max_date)
    regional_result_list[[counter]] <- run_main_regional(champions, outcome_colname_regional, pool, estimator, treatment_date, inference = inference, n_boot = n_boot, bandwidth = bandwidth_regional, covariate_colnames = covariates_regional, max_date = max_date)

    counter <- counter + 1
  }
}

# Plots -------------------------------------------------------------------
save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/2_Estimation/2022"

for (i in seq_len(length(regional_result_list))) {
  produce_plots_pooled(pooled_result_list[[i]], save_here)
  produce_plots_regional(regional_result_list[[i]], save_here)
}

# LATEX -------------------------------------------------------------------
produce_latex(pooled_result_list, regional_result_list)
