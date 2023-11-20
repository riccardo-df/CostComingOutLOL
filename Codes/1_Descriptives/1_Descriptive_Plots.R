## @author: riccardo-df
## University of Rome Tor Vergata
## 08\08\2023

## The Cost of Coming Out - Descriptive Plots.

## This script produces Figure A.1 and a bunch of other figures not appearing in the paper that we used to investigate the dynamics of the variables of the champions of interest.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

# Plots -------------------------------------------------------------------
## Settings.
champions <- c("Graves", "TwistedFate")

bandwidth_champions <- 5
bandwidth_players <- 3

min_date <- as.POSIXct("2022-01-01")
max_date <- as.POSIXct("2022-07-13")

treatment_date1 <- NULL
treatment_date2 <- NULL

save_here_champions <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Champions"
save_here_players <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Players"

## Produce plots.
champions_descriptive_plots_lol(champions, treatment_date1 = treatment_date1, treatment_date2 = treatment_date2, min_date = min_date, max_date = max_date, bandwidth = bandwidth_champions, ylims_rates = c(0, 40), save_here = save_here_champions)
champions_performance_plots_lol(champions, min_date = min_date, max_date = max_date, bandwidth = bandwidth_champions, save_here = save_here_champions)
players_descriptive_plots_lol(min_date = min_date, max_date = max_date, bandwidth = bandwidth_players, save_here = save_here_players)
