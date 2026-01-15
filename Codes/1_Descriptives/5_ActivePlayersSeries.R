## @author: riccardo-df
## University of Southern Denmark
## 15\01\2026

## The Cost of Coming Out - Descriptive Plots.

## This script produces ...

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
# pkgs <- c("CostComingOutLOL")
# inst <- lapply(pkgs, library, character.only = TRUE)
library(devtools)
load_all()

## Define time events.
treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

# Define prior users ------------------------------------------------------
## Add before/after treatment dummy.
lol_player_dta2 <- lol_player_dta %>%
  mutate(disclosure = ifelse(day > treatment_date, 1, 0))

## Compute players' total number of matches before/after.
lol_player_dta2 <- lol_player_dta2 %>%
  group_by(id) %>%
  mutate(n_matches_pre = sum(n_matches * (1 - disclosure)),
         n_matches_post = sum(n_matches * disclosure)) %>%
  ungroup() %>%
  select(day, disclosure, id, graves_rate, graves_ban_rate,
         top_rate, jungle_rate, mid_rate, bottom_rate, support_rate, lgb_rate,
         n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

## Define prior users.
treated_controls <- lol_player_dta2 %>%
  group_by(id) %>%
  mutate(avg_graves_rate_pre = sum(graves_rate * (1 - disclosure)) / sum(1 - disclosure),
         prior_user = avg_graves_rate_pre >= 5) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE) %>%
  select(prior_user, id)

cat("N. players is ", length(unique(lol_player_dta$id)), " of which:
  ", treated_controls %>% distinct(id, .keep_all = TRUE) %>% pull(prior_user) %>% sum(), " was playing Graves at least 5% before the disclosure (prior users)
  ", length(unique(lol_player_dta$id))- treated_controls %>% distinct(id, .keep_all = TRUE) %>% pull(prior_user) %>% sum(), " has played Graves less than 5% before the disclosure (non-prior users) \n", sep = "")

lol_player_dta2 <- lol_player_dta2 %>%
  left_join(treated_controls, by = "id") %>%
  select(day, id, disclosure, prior_user, graves_rate, graves_ban_rate,
         top_rate, jungle_rate, mid_rate, bottom_rate, support_rate, lgb_rate,
         n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

# Daily n. players --------------------------------------------------------
## Compute the n. daily players.
daily_all <- lol_player_dta2 %>%
  group_by(day) %>%
  summarise(n_players = n_distinct(id),
            .groups = "drop") %>%
  mutate(series = "All players")

## Same, but split by prior Graves' usage.
daily_groups <- lol_player_dta2 %>%
  group_by(day, prior_user) %>%
  summarise(n_players = n_distinct(id), 
            .groups = "drop") %>%
  mutate(series = if_else(prior_user, "Prior users", "Non-prior users")) %>%
  select(day, n_players, series)

## Bundle together.
daily_active_3lines <- bind_rows(
  daily_all %>% select(day, n_players, series),
  daily_groups
)

## Plot.
rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

plot_daily_players_3lines <- daily_active_3lines %>%
  ggplot(aes(x = day, y = n_players, color = series, linetype = series)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
  annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
  xlab("") + ylab("Daily active players") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%Y") +
  scale_color_manual(values = c("All players" = "#2B2B2B", "Prior users" = "#0072B2", "Non-prior users" = "firebrick")) +
  scale_linetype_manual(values = c("All players" = "solid", "Prior users" = "dotted", "Non-prior users" = "dashed")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 13, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 13),
        strip.text = element_text(size = 16, face = "bold"),
        legend.position = c(0.16, 0.86),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 13),
        legend.key.width = unit(2, "lines"))

ggsave("daily_active_players_series.pdf", plot_daily_players_3lines, width = 13, height = 6)
