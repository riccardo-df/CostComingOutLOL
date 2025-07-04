## @author: riccardo-df
## University of Southern Denmark
## 08\08\2023

## The Cost of Coming Out - Google Trends.

## This script produces Figure 2.1.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("dplyr", "ggplot2")
inst <- lapply(pkgs, library, character.only = TRUE)

## Define time events.
treatment_week <- as.POSIXct("2022-05-29", tryFormats = "%Y-%m-%d")
pride_month_2022_begin <- as.POSIXct("2022-05-29", tryFormats = "%Y-%m-%d")
pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")
rainbow <- adjustcolor(matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)
wc_2022_begin <- as.POSIXct("2022-09-29", tryFormats = "%Y-%m-%d")
wc_2022_end <- as.POSIXct("2022-11-05", tryFormats = "%Y-%m-%d")

## Set path to save plot.
save_here <- "C:/Users/riccardo-df/Dropbox/University/Research/Projects/Cost_Coming_Out/2_Data_Collection/CostComingOutLOL/Figures/1_Descriptives/Champions"

# Manual collection -------------------------------------------------------
## Import and handle data.
library(reshape2)

search_interest <- read.csv("lol_characters.csv", skip = 2)
search_interest_gay <- read.csv("characters_gay.csv", skip = 2)

colnames(search_interest) <- colnames(search_interest_gay) <- c("week", "Graves", "Twisted_Fate")

search_interest$Graves <- as.numeric(search_interest$Graves)
search_interest$Twisted_Fate <- as.numeric(search_interest$Twisted_Fate)

search_interest_gay$Graves <- as.numeric(search_interest_gay$Graves)
search_interest_gay$Twisted_Fate <- as.numeric(search_interest_gay$Twisted_Fate)

search_interest$keyword <- "lol"
search_interest_gay$keyword <- "gay"

plot_dta <- bind_rows(search_interest, search_interest_gay) %>%
  replace(is.na(.), 0) %>%
  melt(id.vars = c("week", "keyword"), measure.vars = c("Graves", "Twisted_Fate"), variable.name = "champion", value.name = "hits") %>%
  mutate(graves = factor(grepl("Graves", keyword), levels = c(TRUE, FALSE), labels = c("Graves", "Twisted Fate")),
         gay_query = factor(grepl("gay", keyword), levels = c(TRUE, FALSE), labels = c("[champion] gay", "LoL [champion]")))
plot_dta$week <- as.POSIXct(plot_dta$week)

## Paper.
plot_graves <- plot_dta %>%
  filter(champion == "Graves") %>%
  mutate(gay_query = factor(grepl("gay", keyword), levels = c(TRUE, FALSE), labels = c("Graves gay", "LoL Graves"))) %>%
  ggplot(aes(x = week, y = hits, color = champion)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = as.POSIXct(treatment_week), linetype = 4) +
  annotate(geom = "rect", xmin = as.POSIXct(wc_2022_begin), xmax = as.POSIXct(wc_2022_end), ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.2) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(vars(gay_query), nrow = 2) +
  xlab("") + ylab("Search interest") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "italic"), axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(size = 15, face = "italic"),
        legend.position = "none")

plot_graves_twisted <- plot_dta %>%
  ggplot(aes(x = week, y = hits, color = champion)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = as.POSIXct(treatment_week), linetype = 4) +
  annotate(geom = "rect", xmin = as.POSIXct(wc_2022_begin), xmax = as.POSIXct(wc_2022_end), ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.2) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(vars(gay_query), nrow = 2) +
  xlab("") + ylab("Search interest") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "italic"), axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(size = 15, face = "italic"),
        legend.position = c(0.12, 0.92), legend.title = element_blank())

## Slides.
plot_graves_slides <- plot_dta %>%
  filter(champion == "Graves" & keyword == "gay") %>%
  ggplot(aes(x = week, y = hits, color = champion)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = as.POSIXct(treatment_week), linetype = 4) +
  annotate(geom = "rect", xmin = as.POSIXct(wc_2022_begin), xmax = as.POSIXct(wc_2022_end), ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.2) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_color_brewer(palette = "Set2") +
  xlab("") + ylab("Search interest") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "italic"), axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(size = 15, face = "italic"),
        legend.position = "none")

## Export.
ggsave(paste0(save_here, "/", "google_trends_time_grid_graves.pdf"), plot = plot_graves, width = 7, height = 7)
ggsave(paste0(save_here, "/", "google_trends_time_grid_graves_twisted.pdf"), plot = plot_graves_twisted, width = 7, height = 7)
ggsave(paste0(save_here, "/", "google_trends_time_graves_slides.pdf"), plot = plot_graves_slides, width = 12, height = 7)
