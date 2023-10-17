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
champions <- "Graves"

## Select outcome series.
bandwidth <- 3

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
donors <- "all"
estimator <- "sc_reg"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- TRUE
n_boot <- 200
backdate <- 10

covariates <- c()
# covariates <- c("ban_rate_pooled", "win_rate_pooled", "gold_pooled", "kills_pooled", "assists_pooled", "deaths_pooled")

# Estimation --------------------------------------------------------------
pooled_result_kd_ratio <- run_main_pooled(champions, "kd_ratio", donors, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth, covariate_colnames = covariates, max_date = max_date)
pooled_result_win_rate <- run_main_pooled(champions, "win_rate_pooled", donors, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth, covariate_colnames = covariates, max_date = max_date)

# Plots -------------------------------------------------------------------
save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Graves_Performance"

produce_plots_pooled(pooled_result_kd_ratio, save_here)
produce_plots_pooled(pooled_result_win_rate, save_here)
