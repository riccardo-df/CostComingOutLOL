## @author: riccardo-df
## University of Rome Tor Vergata
## 08\08\2023

## The Cost of Coming Out - Outcome Plots.

## This script produces several plots to investigate the dynamics of pick, ban, and win variables of the champions of interest.
## Users can select champions in lines 20-21 and run the whole script.

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

outcome_plots_lol(champions, champ_panel, champ_panel_pool)
outcome_plots_lol_lgb(champ_panel_pool)
