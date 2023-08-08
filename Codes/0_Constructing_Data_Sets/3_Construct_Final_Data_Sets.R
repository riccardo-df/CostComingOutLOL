## @author: riccardo-df
## University of Rome Tor Vergata
## 07\08\2023

## The Cost of Coming Out - Constructing the final data sets.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("LOLpaper", "data.table")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
clean_dta <- fread("C:/Users/difra/Dropbox/University/Data_Sets/LoL/lol_cleaned_dta.csv")

# Final data sets ---------------------------------------------------------
construct_lol_champion_pooled_data(clean_dta)

