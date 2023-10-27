## @author: riccardo-df
## University of Rome Tor Vergata
## 11\10\2023

## The Cost of Coming Out - Mechanisms (Players' Performance).

## This script produces plots to investigate whether players' performance changes at the treatment date and whether players playing Graves the most
## are different from other players.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

# Plots -------------------------------------------------------------------
## Settings.
n_groups <- 5
n_pre_matches <- 100
n_post_matches <- 50

treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Players_Performance"

## Produce plots.
mechanisms_plots_lol2(n_pre_matches, n_post_matches, treatment_date = treatment_date, min_date = min_date, max_date = max_date, save_here = save_here)

# Diff-in-Diff on performance measures ------------------------------------
## Fit estimators.
did_results <- did_players_performance(n_pre_matches, n_post_matches, treatment_date = treatment_date, min_date = min_date, max_date = max_date)

dr_reduce_graves_agg <- did::aggte(did_results$dr_reduce_graves, type = "simple")
dr_reduce_graves_covariates_agg <- did::aggte(did_results$dr_reduce_graves_covariates, type = "simple")
dr_drop_graves_agg <- did::aggte(did_results$dr_drop_graves, type = "simple")
dr_drop_graves_covariates_agg <- did::aggte(did_results$dr_drop_graves_covariates, type = "simple")

## Plots.
plot_reduce_unconditional <- plot_did(did_results$dr_reduce_graves, "Unconditional", "Reduce Graves")
plot_reduce_conditional <- plot_did(did_results$dr_reduce_graves_covariates, "Conditional", "Reduce Graves")
plot_drop_unconditional <- plot_did(did_results$dr_drop_graves, "Unconditional", "Drop Graves")
plot_drop_conditional <- plot_did(did_results$dr_drop_graves_covariates, "Conditional", "Drop Graves")

ggsave(paste0(save_here, "/", "did_players_performance.svg"), (plot_reduce_unconditional + plot_reduce_conditional) / (plot_drop_unconditional + plot_drop_conditional), device = "svg", width = 7, height = 7)

