## @author: riccardo-df
## University of Rome Tor Vergata
## 11\10\2023

## The Cost of Coming Out - Mechanisms (Graves' Performance).

## This script uses synthetic control methods to investigate whether Graves' performance changes at the treatment date.

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
outcome_colnames <- c("kd_ratio", "assists_pooled", "gold_pooled", "win_rate_pooled")

bandwidth <- 3

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
donors <- "all"
estimator <- "sc_reg"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- TRUE
n_boot <- 2
backdate <- 10

covariates <- c()

# Estimation --------------------------------------------------------------
## Loop over donor pools.
pooled_result_list <- list()
counter <- 1

for (outcome_colname in outcome_colnames) {
  cat("\n")
  cat("Outcome: ", outcome_colname, "\n", sep = "")
  cat("\n")

  pooled_result_list[[counter]] <- run_main_pooled(champions, outcome_colname, donors, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth, covariate_colnames = covariates, max_date = max_date)

  counter <- counter + 1
}

# Plots -------------------------------------------------------------------
save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Graves_Performance"

for (i in seq_len(length(pooled_result_list))) {
  produce_plots_pooled(pooled_result_list[[i]], save_here)
}
