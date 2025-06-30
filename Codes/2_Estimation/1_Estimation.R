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
outcome_colname_regional <- "pick_rate"

bandwidth_pool <- 3
bandwidth_regional <- 3

## Set SC estimator.
donor_pools <- c("all", "support_adc")
estimators <- c("sc", "sc_reg")
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- FALSE
n_boot <- 200
backdate <- 10

covariates_pool <- c()
covariates_regional <- c()

# Estimation --------------------------------------------------------------
## Loop over donor pools.
pooled_result_list <- list()
regional_result_list <- list()

counter <- 1

for (estimator in estimators) {
  for (pool in donor_pools) {
    cat("\n")
    cat("Estimator: ", estimator, " donor pool: ", pool, "\n", sep = "")
    cat("\n")

    pooled_result_list[[counter]] <- run_main_pooled(champions, outcome_colname_pool, pool, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth_pool, covariate_colnames = covariates_pool)
    # regional_result_list[[counter]] <- run_main_regional(champions, outcome_colname_regional, pool, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth_regional, covariate_colnames = covariates_regional)

    counter <- counter + 1
  }
}

# Plots -------------------------------------------------------------------
## Extract "active" champions and construct palette for weights plots.
pooled_actives <- rownames(summary(pooled_result_list[[1]]$Graves$tau_hat)$controls)
regional_actives <- unlist(lapply(regional_result_list[[1]]$Graves$tau_hats, function(y) { rownames(summary(y)$controls) }))
unique_actives <- sort(unique(c(pooled_actives, regional_actives)))

palette <- pal_jco()(length(unique_actives))
names(palette) <- unique_actives

## Decide where to save the plots and call functions.
save_here <- "C:/Users/rdif/Dropbox/University/Research/Projects/Ongoing/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/2_Estimation/2022"

for (i in seq_len(length(pooled_result_list))) {
  produce_plots_pooled(pooled_result_list[[i]], ylims = c(0, 40), palette, save_here)
  # produce_plots_regional(regional_result_list[[i]], ylims = c(0, 60), palette, save_here)
}

# LATEX -------------------------------------------------------------------
## Produce table.
produce_latex(pooled_result_list, regional_result_list)

## Compute percentages.
# Pooled.
graves_pre_avg_pooled <- lol_champ_pool_dta %>%
  filter(champion == "Graves" & day < treatment_date) %>%
  pull(pick_rate_pooled) %>%
  mean()

effects_pooled <- sapply(pooled_result_list, function(x) { as.numeric(x$Graves$tau_hat) })

percentages_pooled <- effects_pooled / graves_pre_avg_pooled * 100

# Regional (only for main specification).
graves_pre_avg_regional <- lol_champ_dta %>%
  filter(champion == "Graves" & day < treatment_date) %>%
  group_by(region) %>%
  mutate(avg = mean(pick_rate)) %>%
  distinct(region, .keep_all = TRUE) %>%
  select(region, avg)

effects_regional <- sapply(regional_result_list, function(x) { as.numeric(x$Graves$tau_hats) })

percentages_regional <- effects_regional[, 1] / graves_pre_avg_regional$avg * 100
names(percentages_regional) <- graves_pre_avg_regional$region

# Check LOO ---------------------------------------------------------------
## Extract main specification.
idx <- lapply(pooled_result_list, function(x) { data.frame("estimator" = x$estimator, "donor_pool" = x$donors) })
this_element <- which(sapply(idx, function(x) { is.element("sc", x) & is.element("all", x) }))
this_fit <- pooled_result_list[[this_element]]

## Investigate point estimates.
this_fit$Graves$tau_hat_drop$Ezreal # This is the series that diverges from the others.
this_fit$Graves$tau_hat_drop$Ahri
this_fit$Graves$tau_hat_drop$Nautilus
this_fit$Graves$tau_hat_drop$Jinx
