## @author: riccardo-df
## University of Rome Tor Vergata
## 07\08\2023

## The Cost of Coming Out - Constructing the final data sets.

## This script constructs the data sets used for the analysis of the "The Cost of Coming Out" paper.
## These data set are bundled in our package, so you do not need to run this script.
## Run "help(construct_lol_champion_data)", "help(construct_lol_champion_pooled_data)", and "help(construct_lol_player_data)" for details.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "data.table")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
clean_dta <- fread("C:/Users/difra/Dropbox/University/Data_Sets/LoL/lol_cleaned_dta.csv")

## Descriptives.
cat("N. players is ", length(unique(clean_dta$player_puiid)), "\n", sep = "")
cat("N. matches is ", length(unique(clean_dta$match_id)), "\n", sep = "")

# Final data sets ---------------------------------------------------------
construct_lol_champion_data(clean_dta) # The output of this function is bundled in the package.
construct_lol_champion_pooled_data(lol_champ_dta) # Both the input and the output of this function are bundled in the package.
construct_lol_player_data(clean_dta) # The output of this function is bundled in the package.

## Descriptives.
cat("Champion data set:
     N. days:      ", length(unique(lol_champ_dta$day)), "
     N. champions: ", length(unique(lol_champ_dta$champion)), "
     N. matches:   ", lol_champ_dta %>% distinct(day, region, .keep_all = TRUE) %>% pull(n_matches) %>% sum(), "

Champion pooled data set:
     N. days:      ", length(unique(lol_champ_pool_dta$day)), "
     N. champions: ", length(unique(lol_champ_pool_dta$champion)), "
     N. matches:   ", lol_champ_pool_dta %>% distinct(day, .keep_all = TRUE) %>% pull(n_matches_sum) %>% sum(), "

Player data set:
     N. days:      ", length(unique(lol_player_dta$day)), "
     N. players:   ", length(unique(lol_player_dta$id)), "
     N. matches:   ", lol_player_dta %>% pull(n_matches) %>% sum() / 10, "

    \n", sep = "")
