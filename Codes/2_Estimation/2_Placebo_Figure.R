## @author: riccardo-df
## University of Rome Tor Vergata
## 09\08\2023

## The Cost of Coming Out - Synthetic Controls.

## This script produces the placebo figure for the "The Cost of Coming Out" paper.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

# Run SC algorithm for all control champions ------------------------------
## Select champions.
champions <- setdiff(sort(unique(lol_champ_pool_dta$champion)), "Graves")

## Select outcome series.
outcome_colname_pool <- "pick_rate_pooled"

bandwidth_pool <- 3

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
estimator <- "sc_reg"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- FALSE
n_boot <- 200
backdate <- 10

covariates_pool <- c()

## Loop over donor pools. Each pool excludes Graves and the "treated" champion.
pooled_result_list <- list()
counter <- 1

for (champion in champions) {
  pool <- setdiff(champions, champion)

  pooled_result_list[[counter]] <- run_main_pooled(champions, outcome_colname_pool, pool, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth_pool, covariate_colnames = covariates_pool, max_date = max_date)
  counter <- counter + 1
}

# Plots -------------------------------------------------------------------
save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/2_Estimation/2023"

for (i in seq_len(length(pooled_result_list))) {
  produce_plots_pooled(pooled_result_list[[i]], save_here)
  produce_plots_regional(regional_result_list[[i]], save_here)
}
