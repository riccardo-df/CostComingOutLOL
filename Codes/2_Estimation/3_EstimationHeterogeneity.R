## @author: riccardo-df
## University of Rome Tor Vergata
## 09\08\2023

## The Cost of Coming Out - Main Results.

## This script replicates Section 4.2 and Section 4.3 and produces Figure 4.1, Figure 4.2, Figure A.2, and Table 4.1.

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

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
donor_pools <- c("all")
estimators <- c("sc")
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- FALSE
n_boot <- 200
backdate <- 10

covariates_pool <- c()

# Estimation --------------------------------------------------------------
pooled_result <- run_main_pooled(champions, outcome_colname_pool, pool, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth_pool, covariate_colnames = covariates_pool, max_date = max_date)

# Plots -------------------------------------------------------------------
## Extract "active" champions and construct palette for weights plots.
pooled_actives <- rownames(summary(pooled_result_list[[1]]$Graves$tau_hat)$controls)

palette <- pal_jco()(length(pooled_actives))
names(palette) <- pooled_actives

## Decide where to save the plots and call functions.
save_here <- "C:/Users/riccardo-df/Dropbox/University/Research/Projects/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/2_Estimation/2022"

produce_plots_pooled(pooled_result_list[[i]], ylims = c(0, 40), palette, save_here)
