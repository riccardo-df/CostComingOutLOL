## @author: riccardo-df
## University of Rome Tor Vergata
## 01\01\2024

## The Cost of Coming Out - Synthetic Controls.

## This script produces the placebo figure for the "The Cost of Coming Out" paper.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

# Run SC algorithm for control champions ------------------------------
## Select outcome series.
outcome_colname_pool <- "pick_rate_pooled"

bandwidth_pool <- 3

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
estimator <- "sc"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- FALSE
n_boot <- 200
backdate <- 10

covariates_pool <- c()

# ## Select most played champions.
# choose_n <- lol_champ_pool_dta %>%
#   filter(day < treatment_date) %>%
#   pull(champion) %>%
#   unique() %>%
#   length()
#
# champions <- lol_champ_pool_dta %>%
#   filter(champion != "Graves" & day < treatment_date) %>%
#   group_by(champion) %>%
#   mutate(avg_pick_pre = mean(pick_rate_pooled)) %>%
#   ungroup() %>%
#   select(champion, avg_pick_pre) %>%
#   distinct(champion, .keep_all = TRUE) %>%
#   arrange(desc(avg_pick_pre)) %>%
#   top_n(choose_n) %>%
#   pull(champion) %>%
#   sort()

## Select all champions released before the treatment date.
champions <- setdiff(sort(unique(lol_champ_pool_dta$champion)), c("Naafiri", "Milio", "KSante", "Nilah", "Belveth"))

## Estimation.
pooled_results <- run_main_pooled(champions, outcome_colname_pool, champions, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth_pool, covariate_colnames = covariates_pool, max_date = max_date)

# Plot -------------------------------------------------------------------
## Plot and save.
save_here <- "C:/Users/riccardo-df/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/2_Estimation/2022"

rmses <- produce_plot_placebo(pooled_results, "Graves", to_plot_n = 50, ylims = c(-20, 20), save_here)

## Investigate distribution of placebo effects. First remove champions with zero fit.
rmses$pre %>%
  print(n = 1000)

rmses$post %>%
  print(n = 1000)

## Select best 50 fits
