## @author: riccardo-df
## University of Rome Tor Vergata
## 07\08\2023

## The Cost of Coming Out - Cleaning the raw data.
## No need to run this script, as the clean data set is bundled within the LOLpaper package.
## This script is here only for archival purposes.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("LOLpaper", "data.table")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
raw_dta <- fread("C:/Users/difra/Dropbox/University/Data_Sets/LoL/lol_raw1.csv")

# Data cleaning -----------------------------------------------------------
clean_lol_data(raw_dta)
