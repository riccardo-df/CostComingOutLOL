## @author: riccardo-df
## University of Rome Tor Vergata
## 08\08\2023

## The Cost of Coming Out - Graves Performance.

## This script produces a figure displaying Graves performance over time.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "dplyr", "ggplot2", "grid", "gridExtra")
inst <- lapply(pkgs, library, character.only = TRUE)

## Define time events.
pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")
rainbow <- adjustcolor(matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

## Loading data.
pooled_panel <- lol_champ_pool_dta

# Graves' performance measures -----------------------------------------------------------
## 1.) Kill/Death ratio.
p_kd <- pooled_panel %>%
  filter(champion == "Graves") %>%
  mutate(kd_ratio = kills_pooled / deaths_pooled) %>%
  replace(is.na(.), 0) %>%
  ggplot(aes(x = as.POSIXct(day), y = kd_ratio, group = champion, color = champion)) +
  annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
  geom_line(color = "tomato", linewidth = 1) +
  xlab("") + ylab("Kills/deaths") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

## 2.) Assists.
p_assists <- pooled_panel %>%
  filter(champion == "Graves") %>%
  ggplot(aes(x = as.POSIXct(day), y = assists_pooled, group = champion, color = champion)) +
  annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
  geom_line(color = "tomato", linewidth = 1) +
  xlab("") + ylab("Assists") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

## 3.) Gold earned.
p_gold <- pooled_panel %>%
  filter(champion == "Graves") %>%
  ggplot(aes(x = as.POSIXct(day), y = gold_pooled, group = champion, color = champion)) +
  annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
  geom_line(color = "tomato", linewidth = 1) +
  xlab("") + ylab("Gold earned") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

## 4.) Win rates.
p_win <- pooled_panel %>%
  filter(champion == "Graves") %>%
  ggplot(aes(x = as.POSIXct(day), y = win_rate_pooled, group = champion, color = champion)) +
  annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
  geom_line(color = "tomato", linewidth = 1) +
  xlab("") + ylab(paste0("Win rate")) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

## 5.) Grid.
ggsave("graves_performance_pooled.svg", plot = grid.arrange(p_kd, p_assists, p_gold, p_win, top = textGrob("Graves")), device = "svg")
