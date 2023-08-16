## @author: riccardo-df
## University of Rome Tor Vergata
## 08\08\2023

## The Cost of Coming Out - Descriptive Plots.

## This script produces a bunch of figures to investigate the dynamics of pick, ban, and win variables of the champions of interest.
## Users can select champions in lines 24-25.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
champ_panel <- lol_champ_dta
champ_panel_pool <- lol_champ_pool_dta

# Plots -------------------------------------------------------------------
## Settings.
lgb_champions <- c("Nami", "Leona", "Diana", "Neeko")
champions <- c("Graves", lgb_champions)

descriptive_plots_lol(champions, champ_panel, champ_panel_pool)
descriptive_plots_lol_lgb(champ_panel_pool)
