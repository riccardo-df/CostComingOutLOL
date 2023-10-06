## @author: riccardo-df
## University of Rome Tor Vergata
## 08\08\2023

## The Cost of Coming Out - Google Trends.

## This script produces a figure displaying the Google search interest for key queries.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("gtrendsR", "dplyr", "ggplot2", "gridExtra", "Cairo")
inst <- lapply(pkgs, library, character.only = TRUE)

## Define time events.
treatment_week <- as.POSIXct("2022-05-29", tryFormats = "%Y-%m-%d")
pride_month_2022_begin <- as.POSIXct("2022-05-29", tryFormats = "%Y-%m-%d")
pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")
rainbow <- adjustcolor(matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)
wc_2022_begin <- as.POSIXct("2022-09-29", tryFormats = "%Y-%m-%d")
wc_2022_end <- as.POSIXct("2022-11-05", tryFormats = "%Y-%m-%d")

## Set path to save plot.
save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives"

# Google trends -----------------------------------------------------------
search_interest <- gtrends("lol Graves", time = "2022-01-01 2022-12-31", gprop = "web", geo = "", onlyInterest = TRUE)$interest_over_time
search_interest_gay <- gtrends("Graves gay", time = "2022-01-01 2022-12-31", gprop = "web", geo = "", onlyInterest = TRUE)$interest_over_time

search_interest_graves <- search_interest %>%
  filter(keyword == "lol Graves")
search_interest_others <- search_interest %>%
  filter(keyword != "lol Graves")

plot_one <- search_interest_gay %>%
  ggplot(aes(x = date, y = hits)) +
  geom_line(color = "#69b3a2", linewidth = 1.2) +
  geom_vline(xintercept = as.POSIXct(treatment_week), linetype = 4) +
  annotate(geom = "rect", xmin = as.POSIXct(wc_2022_begin), xmax = as.POSIXct(wc_2022_end), ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.2) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  xlab("") + ylab("Search interest") + ggtitle("'Graves gay'") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "italic"), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot_two <- search_interest_graves %>%
  ggplot(aes(x = date, y = hits)) +
  geom_line(color = "#69b3a2", linewidth = 1.2) +
  geom_vline(xintercept = as.POSIXct(treatment_week), linetype = 4) +
  annotate(geom = "rect", xmin = as.POSIXct(wc_2022_begin), xmax = as.POSIXct(wc_2022_end), ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.2) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  xlab("") + ylab("Search interest") + ggtitle("'lol Graves'") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "italic"), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(save_here, "/", "google_trends_time_grid.svg"), plot = grid.arrange(plot_one, plot_two), device = CairoSVG)
