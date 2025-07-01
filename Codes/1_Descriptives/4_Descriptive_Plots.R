## @author: riccardo-df
## University of Southern Denmark
## 30\06\2025

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
champions <- c("Graves")

bandwidth_champions <- 5

treatment_date <- NULL

save_here_champions <- "C:/Users/rdif/Dropbox/University/Research/Projects/Ongoing/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Champions"
save_here_players <- "C:/Users/rdif/Dropbox/University/Research/Projects/Ongoing/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Players"

## Produce plots for character metrics. Switch "dataset" to select which sample of players to use (see documentation for more details). Be careful, as each call saves the plots using the same filenames.
champions_descriptive_plots_lol(champions,
                                treatment_date,
                                bandwidth = bandwidth_champions,
                                ylims_rates = c(0, 40),
                                save_here = save_here_champions,
                                dataset = "above")

## Produce plots for player activity.
players_descriptive_plots_lol(save_here = save_here_players)
