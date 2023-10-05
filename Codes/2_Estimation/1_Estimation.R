## @author: riccardo-df
## University of Rome Tor Vergata
## 09\08\2023

## The Cost of Coming Out - Synthetic Controls.

## This script runs the main analysis of the "The Cost of Coming Out" paper.

## OPTION TO SMOOTH SERIES IN THE PACKAGE, AND TO SELECT THE TIME WINDOW.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
panel <- lol_champ_dta
pooled_panel <- lol_champ_pool_dta

# Estimation --------------------------------------------
champions <- c("Graves", "LGB")

pooled_results <- run_main_pooled(pooled_panel, champions, outcome_colname = "pick_level_sum", donor_pool = "non_lgb", estimator = "sc_reg",
                                  treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"), backdate = 10)
regional_results <- run_main_regional(panel, champions, outcome_colname  = "pick_level", donor_pool = "non_lgb", estimator  = "sc_reg",
                                      treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"))

# Plots -------------------------------------------------------------------
produce_plots_pooled(pooled_results)
produce_plots_regional(regional_results)

# Some number -------------------------------------------------------------
## Average effect.
for (champion in names(pooled_results)[seq_len(length(champions))]) {
  cat("Average difference post-treatment for ", champion, " measured in ", if (pooled_results$outcome_colname == "pick_level_sum") "pick levels" else "pick rates", " is:
    Main fit         : ", round(summary(pooled_results[[champion]]$tau_hat)$estimate, 2), "
    Backdated fit    : ", round(summary(pooled_results[[champion]]$tau_hat_back)$estimate, 2), "

    Regions:
       Europe        : ", round(summary(regional_results[[champion]]$Europe)$estimate, 2), "
       Korea         : ", round(summary(regional_results[[champion]]$Korea)$estimate, 2), "
       Latin America : ", round(summary(regional_results[[champion]]$Latin_America)$estimate, 2), "
       North America : ", round(summary(regional_results[[champion]]$North_America)$estimate, 2), "\n\n", sep = "")
}
