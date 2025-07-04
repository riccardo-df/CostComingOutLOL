## @author: riccardo-df
## University of Southern Denmark
## 07\08\2023

## The Cost of Coming Out - Pulling data from Riot API.

## This script pulls raw data from Riot API. Run "help(pull_lol_data)" for details. The raw data used for the paper is available from the authors on request.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL")
inst <- lapply(pkgs, library, character.only = TRUE)

# Pull data ----------------------------------------------------------
## Settings.
api_key <- "INSERT YOUR KEY"
server <- "euw1" # Change this to target different servers.
queue <- "RANKED_SOLO_5x5"
rank <- "CHALLENGER"
division <- "I"
min_sec <- 2
max_sec <- 2

## Call pulling function.
pull_lol_data(server, queue, rank, division, api_key, min_sec, max_sec)
