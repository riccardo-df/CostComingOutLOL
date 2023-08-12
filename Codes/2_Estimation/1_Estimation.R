## @author: riccardo-df
## University of Rome Tor Vergata
## 05\05\2023

## LoL - SC Estimation.
## This script runs the main analysis of "The Cost of Coming Out."

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

# regional_fits <- construct_sc_lol_regional(panel, champions, outcome = "pick_level", donor_pool = "non_lgbtq", estimator = "sc_reg", covariate_names = c())
# lgbtq_regional_fit <- construct_sc_lol_regional_lgbtq(panel, outcome = "pick_level", estimator = "sc_reg")
#
# # Plots -------------------------------------------------------------------
# pooled_plots <- post_process_pooled(pooled_fits, pooled_panel)
# lgbtq_pooled_plots <- post_process_pooled_lgbtq(lgbtq_pooled_fit, pooled_panel)
# regional_plots <- post_process_regional(regional_fits, panel)
# lgbtq_regional_plots <- post_process_regional_lgbtq(lgbtq_regional_fit, panel)
# difference_plot <- post_process_difference(pooled_fits, lgbtq_pooled_fit, pooled_panel)
#
# # Arrange figures ---------------------------------------------------------
# ## Graves.
# # Pooled.
# pooled_plots$Graves$plot_main
# ggsave(paste0("C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/Figures/2_Estimation_results/graves_pooled.svg"), device = "svg")
#
# pooled_plots$Graves$plot_back
# ggsave(paste0("C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/Figures/2_Estimation_results/graves_pooled_back.svg"), device = "svg")
#
# pooled_plots$Graves$plot_weights
# ggsave(paste0("C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/Figures/2_Estimation_results/graves_pooled_weights.svg"), device = "svg")
#
# pooled_plots$Graves$plot_loo
# ggsave(paste0("C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/Figures/2_Estimation_results/graves_pooled_loo.svg"), device = "svg")
#
# # Regional.
# regional_plots$Graves
# ggsave(paste0("C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/Figures/2_Estimation_results/graves_regional.svg"), device = "svg")
#
# ## LGBTQ+ aggregate
# # Pooled.
# lgbtq_pooled_plots$plot_main
# ggsave(paste0("C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/Figures/3_Mechanisms/lgbtq_aggregate_pooled.svg"), device = "svg")
#
# lgbtq_pooled_plots$plot_back
# ggsave(paste0("C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/Figures/3_Mechanisms/lgbtq_aggregate_pooled_back.svg"), device = "svg")
#
# # Regional.
# lgbtq_regional_plots
# ggsave(paste0("C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/Figures/3_Mechanisms/lgbtq_aggregate_regional.svg"), device = "svg")
