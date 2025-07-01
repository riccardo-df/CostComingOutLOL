## @author: riccardo-df
## University of Southern Denmark
## 01\07\2025

## The Cost of Coming Out - Constructing the final data sets.

## This script constructs the data sets used in the paper. Such data sets are already bundled in our R package, so no need to run again this script.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "data.table", "devtools", "ggplot2", "tidyr")
inst <- lapply(pkgs, library, character.only = TRUE)

## Loading data.
clean_dta <- fread("C:/Users/rdif/Dropbox/University/Research/Data_Sets/LoL/Clean/lol_cleaned.csv")

## Define time events.
treatment_date <- as.POSIXct("2022-06-01")

## Descriptives.
cat("N. players is ", length(unique(clean_dta$player_puiid)), "\n", sep = "")
cat("N. matches is ", length(unique(clean_dta$match_id)), "\n", sep = "")

# Final data sets (DO NOT RUN) --------------------------------------------
## Construct estimation data sets and bundle.
construct_lol_champion_data(clean_dta)
lol_champ_dta <- fread("lol_champ_dta.csv")
use_data(lol_champ_dta, compress = "xz")

construct_lol_champion_pooled_data(lol_champ_dta)
lol_champ_pool_dta <- fread("lol_champ_pool_dta.csv")
use_data(lol_champ_pool_dta, compress = "xz")

construct_lol_player_data(clean_dta)
lol_player_dta <- fread("lol_player_dta.csv")
use_data(lol_player_dta, compress = "xz")

## Re-apply the champion functions to compute pick rates using only subsets of players (defined according to their engagement).
# Split players according to quartiles of pre-treatment win rates. Then merge back. Filter players with at least 10 matches.
player_skill <- clean_dta %>%
  filter(day < treatment_date) %>%
  group_by(player_puiid) %>%
  summarise(tot_matches = n_distinct(match_id),
            win_rate = mean(win),
            avg_gold = mean(gold),
            kda_ratio = mean((kills + assists) / pmax(deaths, 1)),
            avg_level = mean(player_level),
            .groups = "drop") %>%
  filter(tot_matches >= 20) %>%
  mutate(skill_group = ifelse(win_rate <= median(win_rate), "Below median", "Above median"))

clean_dta_tagged <- clean_dta %>%
  left_join(player_skill, by = "player_puiid") %>%
  filter(!is.na(skill_group))

cat("N. unique players left is ", length(unique(clean_dta_tagged$player_puiid)), "\n", sep = "")

# Plot histogram of total matches and hours below/above median.
plot_player_skill <- clean_dta_tagged %>%
  distinct(player_puiid, .keep_all = TRUE) %>%
  pivot_longer(cols = c(tot_matches, win_rate), names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                           tot_matches = "Total matches",
                           win_rate = "Win rate"),
         variable = factor(variable, levels = c("Win rate", "Total matches")),
         skill_group = factor(skill_group, levels = c("Below median", "Above median"))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 100, fill = "#4E79A7", color = "black", alpha = 0.8) +
  facet_grid(cols = vars(variable), rows = vars(skill_group), scales = "free") +
  theme_bw(base_size = 13) +
  labs(title = "", x = NULL, y = "Count") +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

save_here <- "C:/Users/rdif/Dropbox/University/Research/Projects/Ongoing/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Players"
ggsave(paste0(save_here, "/", "plot_player_skill_median.pdf"), plot_player_skill, width = 10, height = 7)

# Filter by below/above and apply construction function. Bundle data sets.
dta_below <- clean_dta_tagged %>%
  filter(skill_group == "Below median")
construct_lol_champion_data(dta_below, filename = "lol_champ_dta_belowM.csv")
construct_lol_champion_pooled_data(fread("lol_champ_dta_belowM.csv"), filename = "lol_champ_pool_dta_belowM.csv")
lol_champ_pool_dta_belowM <- fread("lol_champ_pool_dta_belowM.csv")
use_data(lol_champ_pool_dta_belowM, compress = "xz")

dta_above <- clean_dta_tagged %>%
  filter(skill_group == "Above median")
construct_lol_champion_data(dta_above, filename = "lol_champ_dta_aboveM.csv")
construct_lol_champion_pooled_data(fread("lol_champ_dta_aboveM.csv"), filename = "lol_champ_pool_dta_aboveM.csv")
lol_champ_pool_dta_aboveM <- fread("lol_champ_pool_dta_aboveM.csv")
use_data(lol_champ_pool_dta_aboveM, compress = "xz")

# Descriptives ------------------------------------------------------------
## Overview.
cat("Champion data set:
     N. days:      ", length(unique(lol_champ_dta$day)), "
     N. champions: ", length(unique(lol_champ_dta$champion)), "
     N. matches:   ", lol_champ_dta %>% distinct(day, region, .keep_all = TRUE) %>% pull(n_matches) %>% sum(), "

Champion pooled data set:
     N. days:      ", length(unique(lol_champ_pool_dta$day)), "
     N. champions: ", length(unique(lol_champ_pool_dta$champion)), "
     N. matches:   ", lol_champ_pool_dta %>% distinct(day, .keep_all = TRUE) %>% pull(n_matches_sum) %>% sum(), "

Champion pooled data set (below median):
     N. days:      ", length(unique(lol_champ_pool_dta_bottom$day)), "
     N. champions: ", length(unique(lol_champ_pool_dta_bottom$champion)), "
     N. matches:   ", lol_champ_pool_dta_bottom %>% distinct(day, .keep_all = TRUE) %>% pull(n_matches_sum) %>% sum(), "

Champion pooled data set (above median):
     N. days:      ", length(unique(lol_champ_pool_dta_top$day)), "
     N. champions: ", length(unique(lol_champ_pool_dta_top$champion)), "
     N. matches:   ", lol_champ_pool_dta_top %>% distinct(day, .keep_all = TRUE) %>% pull(n_matches_sum) %>% sum(), "

Player data set:
     N. days:      ", length(unique(lol_player_dta$day)), "
     N. players:   ", length(unique(lol_player_dta$id)), "\n", sep = "")

## Total player time.
total_hours_all_players <- sum(clean_dta$duration) / 60
cat("Total player-hours in sample:", round(total_hours_all_players, 0), "\n")

## Impute 2022 US federal minimum wage.
min_wage <- 7.25
opportunity_cost_usd <- total_hours_all_players * min_wage
cat("Imputed opportunity cost:", round(opportunity_cost_usd, 0), "USD\n")

