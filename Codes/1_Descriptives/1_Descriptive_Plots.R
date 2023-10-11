## @author: riccardo-df
## University of Rome Tor Vergata
## 08\08\2023

## The Cost of Coming Out - Descriptive Plots.

## This script produces a bunch of figures to investigate the dynamics of pick, ban, and win variables of the champions of interest.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

# Plots -------------------------------------------------------------------
## Settings.
lgb_champions <- c("Nami", "Leona", "Diana", "Neeko")
champions <- c("Graves", lgb_champions)

bandwidth <- 5

min_date <- as.POSIXct("2022-01-01")
max_date <- as.POSIXct("2023-08-01")

treatment_date1 <- NULL
treatment_date2 <- NULL

save_here_champions <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Champions"
save_here_players <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Players"

## Produce plots.
champions_descriptive_plots_lol(champions, treatment_date1 = treatment_date1, treatment_date2 = treatment_date2, min_date = min_date, max_date = max_date, bandwidth = bandwidth, save_here = save_here_champions)
champions_performance_plots_lol(champions, min_date = min_date, max_date = max_date, bandwidth = bandwidth, save_here = save_here_champions)
players_descriptive_plots_lol(min_date = min_date, max_date = max_date, bandwidth = bandwidth, save_here = save_here_players)
