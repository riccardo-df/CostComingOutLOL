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

# Estimation --------------------------------------------
## Settings.
# Select champions.
champions <- c("Graves", "LGB")

# Select outcome series.
outcome_colname_pool <- "pick_rate_pooled"
outcome_colname_regional <- "pick_rate"

bandwidth_pool <- 5
bandwidth_regional <- 5

max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

# Set SC estimator.
donors <- c("Ahri", "Ezreal", "Nautilus", "Xayah")
estimator <- "sc_reg"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- TRUE
backdate <- 10

## Estimation.
pooled_results <- run_main_pooled(champions, outcome_colname_pool, donors, estimator, treatment_date, backdate, inference = inference, bandwidth = bandwidth_pool, max_date = max_date)
regional_results <- run_main_regional(champions, outcome_colname_regional, donors, estimator, treatment_date, bandwidth = bandwidth_regional, max_date = max_date)

# Plots -------------------------------------------------------------------
## Settings.
save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/2_Estimation"

## Produce plots.
produce_plots_pooled(pooled_results, save_here)
produce_plots_regional(regional_results, save_here)

# Some number -------------------------------------------------------------
## Average effect.
for (champion in names(pooled_results)[seq_len(length(champions))]) {
  cat("Average difference post-treatment for ", champion, " measured in ", if (pooled_results$outcome_colname == "pick_level_sum") "pick levels" else "pick rates", " is:
    Main fit         : ", round(summary(pooled_results[[champion]]$tau_hat)$estimate, 2), "
    Backdated fit    : ", round(summary(pooled_results[[champion]]$tau_hat_back)$estimate, 2), "

    Regions:
       Europe        : ", round(summary(regional_results[[champion]]$tau_hats$Europe)$estimate, 2), "
       Korea         : ", round(summary(regional_results[[champion]]$tau_hats$Korea)$estimate, 2), "
       Latin America : ", round(summary(regional_results[[champion]]$tau_hats$Latin_America)$estimate, 2), "
       North America : ", round(summary(regional_results[[champion]]$tau_hats$North_America)$estimate, 2), "\n\n", sep = "")
}
