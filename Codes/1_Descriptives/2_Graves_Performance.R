## @author: riccardo-df
## University of Rome Tor Vergata
## 08\08\2023

## The Cost of Coming Out - Graves Performance.

## This script produces a figure displaying Graves performance over time.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
pooled_panel <- lol_champ_pool_dta

# Performance measures -----------------------------------------------------------
champions <- c("Graves", "Leona", "Diana", "Neeko", "Nami")
performance_plots_lol(champions, pooled_panel)
