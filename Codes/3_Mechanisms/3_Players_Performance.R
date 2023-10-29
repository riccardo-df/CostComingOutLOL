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
n_pre_matches <- 150

treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Players_Performance"

## Produce plots.
players_performance_plots_lol(n_pre_matches, treatment_date = treatment_date, min_date = min_date, max_date = max_date, save_here = save_here)

# Diff-in-Diff on performance measures ------------------------------------
## Fit estimators.
did_results <- did_players_performance(n_pre_matches, treatment_date = treatment_date, min_date = min_date, max_date = max_date)

## LATEX.
latex_did(did_results, 1986)

## Plot.
plot_did(did_results, save_here)
