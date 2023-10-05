## @author: riccardo-df
## University of Rome Tor Vergata
## 08\08\2023

## The Cost of Coming Out - Graves Performance.

## This script produces a figure the performance over time of the champions of interest.
## Users can select champions in line 19.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

# Performance measures -----------------------------------------------------------
## Settings.
lgb_champions <- c("Nami", "Leona", "Diana", "Neeko")
champions <- c("Graves", lgb_champions)

save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives"

## Produce plot.
performance_plots_lol(champions, save_here = save_here)
