## @author: riccardo-df
## University of Southern Denmark
## 11\10\2023

## The Cost of Coming Out - Mechanisms (Players' Skills and Performance and Belveth).

## This script replicates Section 5.2 and Section 5.3 and produces Figure 5.2, Figure 5.3, and Figure A.3.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
# pkgs <- c("CostComingOutLOL")
# inst <- lapply(pkgs, library, character.only = TRUE)
library(devtools)
load_all()

# Settings ----------------------------------------------------------------
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
save_here <- "C:/Users/rdif/Dropbox/University/Research/Projects/Ongoing/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Players_Skills_Performance"

# Players' skills ---------------------------------------------------------
## Compare prior and non-prior users.
players_performance_plots_lol(treatment_date, save_here)

# Players' performance ----------------------------------------------------
## Assess impact of Pride Month on prior users' performance.
did_results <- did_players_performance(treatment_date)
plot_did(did_results, save_here)

# Belveth -----------------------------------------------------------------
## Check preferences for Belveth.
belveth(save_here)
