## @author: riccardo-df
## University of Rome Tor Vergata
## 30\06\2025

## The Cost of Coming Out - Main Results.

## This script produces ...

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
outcome_colname_pool <- "pick_rate_pooled"

bandwidth_pool <- 3

## Set SC estimator.
pool <- "all"
estimator <- "sc"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- FALSE
n_boot <- 200
backdate <- 10

covariates_pool <- c()

# Estimation --------------------------------------------------------------
pooled_result_bottom <- run_main_pooled(champions, outcome_colname_pool, pool, estimator, treatment_date,
                                        backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth_pool, covariate_colnames = covariates_pool,
                                        dataset = "below")

pooled_result_top <- run_main_pooled(champions, outcome_colname_pool, pool, estimator, treatment_date,
                                     backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth_pool, covariate_colnames = covariates_pool,
                                     dataset = "above")

# Plots -------------------------------------------------------------------
## Decide where to save the plots and call functions.
save_here <- "C:/Users/rdif/Dropbox/University/Research/Projects/Ongoing/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/2_Estimation/2022"

## Be careful, as plots are saved under the same filename and thus replaced at each call.
produce_plots_pooled(pooled_result_bottom, ylims = c(0, 40), palette = NULL, save_here)
produce_plots_pooled(pooled_result_top, ylims = c(0, 40), palette = NULL, save_here)
