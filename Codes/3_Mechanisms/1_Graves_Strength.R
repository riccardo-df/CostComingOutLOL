## @author: riccardo-df
## University of Rome Tor Vergata
## 11\10\2023

## The Cost of Coming Out - Mechanisms (Graves' Performance).

## This script replicates Section 5.1 and produces Figure 5.1.

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
outcome_colnames <- "win_rate_pooled"
bandwidth <- 3

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
donors <- "all"
estimator <- "sc"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- TRUE
n_boot <- 200
backdate <- 10

covariates <- c()

# Estimation --------------------------------------------------------------
pooled_result <- run_main_pooled(champions, outcome_colnames, donors, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth, covariate_colnames = covariates, max_date = max_date)

# Plots -------------------------------------------------------------------
unique_actives <- sort(rownames(summary(pooled_result$Graves$tau_hat)$controls))

palette <- pal_jco()(length(unique_actives))
names(palette) <- unique_actives

save_here <- "C:/Users/riccardo-df/Dropbox/University/Research/Projects/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Graves_Strength"

produce_plots_pooled(pooled_result, ylims = c(20, 80), palette, save_here)

# Point estimate and confidence intervals ---------------------------------
tau_hat <- summary(pooled_result$Graves$tau_hat)$estimate
se <- pooled_result$Graves$se_tau_hat
cil <- trimws(format(round(tau_hat - 1.96 * se, 3), nsmall = 3))
ciu <- trimws(format(round(tau_hat + 1.96 * se, 3), nsmall = 3))

cat("Point estimate: ", tau_hat, " [", cil, ", ", ciu, "] \n", sep = "")
