## @author: riccardo-df
## University of Rome Tor Vergata
## 30\06\2025

## The Cost of Coming Out - Most Played Characters.

## This script produces ...

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "ggplot2")
inst <- lapply(pkgs, library, character.only = TRUE)

## Define time events.
treatment_date <- as.POSIXct("2022-06-01")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Path to save plots.
save_here <- "C:/Users/riccardo-df/Dropbox/University/Research/Projects/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Pick_Rates"

# Descriptves -------------------------------------------------------------
## Basic correlations before coming-out event.
lol_player_dta %>%
  filter(day < treatment_date) %>%
  select(graves_rate, win_rate, n_matches, n_hours) %>%
  cor()

