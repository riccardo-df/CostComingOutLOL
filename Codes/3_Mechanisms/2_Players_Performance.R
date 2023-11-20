## @author: riccardo-df
## University of Rome Tor Vergata
## 11\10\2023

## The Cost of Coming Out - Mechanisms (Players' Performance).

## This script replicates Section 5.2 and Section 5.3 and produces Figure 5.2, Figure 5.3, and Figure A.3.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

# Plots -------------------------------------------------------------------
## Settings.
n_pre_matches <- 100

treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Players_Performance"

## Plot.
players_performance_plots_lol(n_pre_matches, treatment_date = treatment_date, min_date = min_date, max_date = max_date, save_here = save_here)

# Diff-in-Diff on performance measures ------------------------------------
## Fit estimators.
filter <- "prior_users"
did_results <- did_players_performance(n_pre_matches, filter = filter, treatment_date = treatment_date, min_date = min_date, max_date = max_date)

## Plot.
plot_did(did_results, save_here)
