## @author: riccardo-df
## University of Rome Tor Vergata
## 09\08\2023

## The Cost of Coming Out - Mechanisms (LGBT Pride Month).

## This script replicates Section 5.4 and produces Figure 5.4.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "dplyr")
inst <- lapply(pkgs, library, character.only = TRUE)

# Settings --------------------------------------------
## Select champions.
champions <- "LGB"

## Select outcome series.
outcome_colname <- "pick_rate_pooled"
bandwidth <- 3

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
donors <- "non_lgb"
estimator <- "sc"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- TRUE
n_boot <- 200
backdate <- 10

covariates <- c()

# Estimation --------------------------------------------------------------
pooled_result <- run_main_pooled(champions, outcome_colname, donors, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth, covariate_colnames = covariates, max_date = max_date)

# Plots -------------------------------------------------------------------
unique_actives <- sort(rownames(summary(pooled_result$LGB$tau_hat)$controls))

palette <- pal_jco()(length(unique_actives))
names(palette) <- unique_actives

save_here <- "C:/Users/riccardo-df/Dropbox/University/Research/Projects/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Pride_Month"

produce_plots_pooled(pooled_result, ylims = c(0, 40), palette, save_here)

# Point estimate and confidence intervals ---------------------------------
tau_hat <-  summary(pooled_result$LGB$tau_hat)$estimate
se <- pooled_result$LGB$se_tau_hat
cil <- trimws(format(round(tau_hat - 1.96 * se, 3), nsmall = 3))
ciu <- trimws(format(round(tau_hat + 1.96 * se, 3), nsmall = 3))

cat("Point estimate: ", tau_hat, " [", cil, ", ", ciu, "] \n", sep = "")
