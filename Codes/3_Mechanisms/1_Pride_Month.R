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
champions <- c("LGB")

## Select outcome series.
outcome_colname <- "pick_rate_pooled"

bandwidth <- 3

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
donors <- "non_lgb"
estimator <- "sc_reg"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- TRUE
n_boot <- 2
backdate <- 10

covariates <- c()
# covariates_pool <- c("ban_level_sum", "win_level_sum", "gold_pooled", "kills_pooled", "assists_pooled", "deaths_pooled")
# covariates_pool <- c("ban_rate_pooled", "win_rate_pooled", "gold_pooled", "kills_pooled", "assists_pooled", "deaths_pooled")

# Estimation --------------------------------------------------------------
pooled_result <- run_main_pooled(champions, outcome_colname, donors, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth, covariate_colnames = covariates, max_date = max_date)

# Plots -------------------------------------------------------------------
save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Pride_Month"

produce_plots_pooled(pooled_result, save_here)

# LATEX -------------------------------------------------------------------
produce_latex_pooled(pooled_result)
