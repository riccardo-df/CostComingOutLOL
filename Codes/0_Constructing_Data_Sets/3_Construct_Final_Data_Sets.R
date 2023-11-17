## @author: riccardo-df
## University of Rome Tor Vergata
## 07\08\2023

## The Cost of Coming Out - Constructing the final data sets.

## This script constructs the data sets used for the analysis of the "The Cost of Coming Out" paper.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "data.table")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
clean_dta <- fread("C:/Users/difra/Dropbox/University/Data_Sets/LoL/Clean/lol_cleaned.csv")

## Descriptives.
cat("N. players is ", length(unique(clean_dta$player_puiid)), "\n", sep = "")
cat("N. matches is ", length(unique(clean_dta$match_id)), "\n", sep = "")

# Final data sets ---------------------------------------------------------
## The output of these functions are bundled in the package, so you do not need to run this section.
construct_lol_champion_data(clean_dta)
construct_lol_champion_pooled_data(lol_champ_dta)
construct_lol_player_data(clean_dta)

lol_player_dta <- fread("lol_player_dta.csv")
usethis::use_data(lol_player_dta, compress = "xz", overwrite = TRUE)

# Descriptives ------------------------------------------------------------
lol_champ_dta <- lol_champ_dta %>%
  filter(day < as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d"))
lol_champ_pool_dta <- lol_champ_pool_dta %>%
  filter(day < as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d"))
lol_player_dta <- lol_player_dta %>%
  filter(day < as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d"))

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
     N. matches:   ", lol_player_dta %>% pull(n_matches) %>% sum() / 10, "\n", sep = "")
