## @author: riccardo-df
## University of Rome Tor Vergata
## 07\08\2023

## The Cost of Coming Out - Constructing the final data sets.

## This script constructs the data sets used in the paper.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "data.table")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
clean_dta <- fread("C:/Users/rdif/Dropbox/University/Research/Data_Sets/LoL/Clean/lol_cleaned.csv")

## Descriptives.
cat("N. players is ", length(unique(clean_dta$player_puiid)), "\n", sep = "")
cat("N. matches is ", length(unique(clean_dta$match_id)), "\n", sep = "")

# Final data sets ---------------------------------------------------------
# ## Count how many daily hours each player plays. Because of matchmaking and character selection, this is massively underestimating time spent on LoL.
# clean_dta <- clean_dta %>%
#   group_by(day, player_puiid) %>%
#   mutate(hours_played = sum(duration) / 60) %>%
#   ungroup()
#
# ## Count how many daily matches each player plays.
# clean_dta <- clean_dta %>%
#   group_by(day, player_puiid) %>%
#   mutate(matches_played = n_distinct(match_id)) %>%
#   ungroup() %>%
#   group_by(player_puiid) %>%
#   mutate(avg_daily_matches = mean(matches_played)) %>%
#   ungroup()

## Construct estimation data sets. The output of these functions are bundled in the package, so you do not need to run this chunk.
construct_lol_champion_data(clean_dta)
construct_lol_champion_pooled_data(lol_champ_dta)
construct_lol_player_data(clean_dta)
construct_lol_match_data(clean_dta)

# Descriptives ------------------------------------------------------------
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

Match data set:
     N. days:      ", length(unique(lol_match_dta$day)), "
     N. matches:   ", lol_match_dta %>% pull(match_id) %>% unique() %>% length(), "\n", sep = "")
