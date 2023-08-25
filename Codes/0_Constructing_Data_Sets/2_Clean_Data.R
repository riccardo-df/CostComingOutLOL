## @author: riccardo-df
## University of Rome Tor Vergata
## 07\08\2023

## The Cost of Coming Out - Cleaning the raw data.

## This script cleans the raw data pulled from Riot API. Run "help(clean_lol_data)" for details.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "data.table")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
raw_dta <- fread("C:/Users/difra/Dropbox/University/Data_Sets/LoL/Raw/lol_raw.csv")

# Data cleaning -----------------------------------------------------------
clean_lol_data(raw_dta)
