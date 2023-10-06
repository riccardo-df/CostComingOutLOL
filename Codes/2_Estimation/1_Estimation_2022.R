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
champions <- c("Graves")

## Select outcome series.
outcome_colname_pool <- "pick_level_sum"
outcome_colname_regional <- "pick_level"

bandwidth_pool <- 5
bandwidth_regional <- 5

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
donor_pools <- c("all", "non_lgb", "jungle", "middle", "top", "support", "adc")
estimator <- "sc"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- FALSE
backdate <- 10

covariates_pool <- c()
covariates_regional <- c()
# covariates_pool <- c("ban_level_sum", "win_level_sum", "gold_pooled", "kills_pooled", "assists_pooled", "deaths_pooled")
# covariates_regional <- c("ban_level", "win_level", "gold_avg", "kills_avg", "assists_avg", "deaths_avg")

# Estimation --------------------------------------------------------------
## Loop over donor pools.
pooled_result_list <- list()
regional_result_list <- list()

counter <- 1

for (pool in donor_pools) {
  cat("\n")
  cat("Donor pool set to ", pool, "\n", sep = "")
  cat("\n")

  pooled_result_list[[counter]] <- run_main_pooled(champions, outcome_colname_pool, pool, estimator, treatment_date, backdate, inference = inference, bandwidth = bandwidth_pool, covariate_colnames = covariates_pool, max_date = max_date)
  regional_result_list[[counter]] <- run_main_regional(champions, outcome_colname_regional, pool, estimator, treatment_date, bandwidth = bandwidth_regional, covariate_colnames = covariates_regional, max_date = max_date)

  counter <- counter + 1
}

# Plots -------------------------------------------------------------------
save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/2_Estimation/2022"

for (i in seq_len(length(pooled_result_list))) {
  produce_plots_pooled(pooled_result_list[[i]], save_here)
  produce_plots_regional(regional_result_list[[i]], save_here)
}

# # Some number -------------------------------------------------------------
# ## Average effect.
# for (champion in names(pooled_results)[seq_len(length(champions))]) {
#   cat("Average difference post-treatment for ", champion, " measured in ", if (pooled_results$outcome_colname == "pick_level_sum") "pick levels" else "pick rates", " is:
#     Main fit         : ", round(summary(pooled_results[[champion]]$tau_hat)$estimate, 2), "
#     Backdated fit    : ", round(summary(pooled_results[[champion]]$tau_hat_back)$estimate, 2), "
#
#     Regions:
#        Europe        : ", round(summary(regional_results[[champion]]$tau_hats$Europe)$estimate, 2), "
#        Korea         : ", round(summary(regional_results[[champion]]$tau_hats$Korea)$estimate, 2), "
#        Latin America : ", round(summary(regional_results[[champion]]$tau_hats$Latin_America)$estimate, 2), "
#        North America : ", round(summary(regional_results[[champion]]$tau_hats$North_America)$estimate, 2), "\n\n", sep = "")
# }
