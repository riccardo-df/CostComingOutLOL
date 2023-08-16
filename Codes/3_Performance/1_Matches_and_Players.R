## @author: riccardo-df
## University of Rome Tor Vergata
## 13\08\2023

## The Cost of Coming Out - Number of daily matches and players.

## This script produces one figure to investigate the dynamics of the number of matches and players of League of Legends.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "dplyr", "ggplot2", "gridExtra")
inst <- lapply(pkgs, library, character.only = TRUE)

## Define time events.
pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

rainbow <- adjustcolor(matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

## Loading data. Focus on period before world championship.
player_panel <- lol_player_dta %>%
  filter(day < as.POSIXct("2022-07-15"))

# # Number of matches and players series ----------------------------
## 1.) Pooled.
trans_coef <- 5
matches_color <- "tomato"
players_color <- "#00BFC4"

plot_pooled <- player_panel %>%
  group_by(day) %>%
  mutate(total_matches = sum(n_matches) / 10,
         total_players = n_distinct(id)) %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = total_matches, color = "Matches"), linewidth = 1) +
  geom_line(aes(y = total_players / trans_coef, color = "Players"), linewidth = 1) +
  annotation_raster(rainbow, xmin = pride_month_2022_begin, xmax = pride_month_2022_end, ymin = -Inf, ymax = Inf) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous(name = "N. daily matches", sec.axis = sec_axis(~ . * trans_coef, name = "N. daily players")) +
  scale_color_manual(name = "Colors", values = c("Matches" = matches_color, "Players" = players_color)) +
  xlab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.11, 0.9), legend.title = element_blank(), legend.direction = "vertical")
ggsave("n_matches_players.svg", plot = plot_pooled, device = "svg")
