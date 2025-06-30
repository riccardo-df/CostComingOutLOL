## @author: riccardo-df
## University of Rome Tor Vergata
## 11\10\2023

## The Cost of Coming Out - Mechanisms (Players' Skills and Performance and Belveth).

## This script replicates Section 5.2 and Section 5.3 and produces Figure 5.2, Figure 5.3, and Figure A.3.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "did")
inst <- lapply(pkgs, library, character.only = TRUE)

treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")

# Match outcomes ----------------------------------------------------------
## Victory.
# Arrange estimation data.
did_dta_victory <- lol_match_dta %>%
  mutate(gname = ifelse(graves_team, as.numeric(treatment_date), 0),
         victory = as.numeric(victory))

# Estimation.
dr_results_victory <- att_gt(yname = "victory", tname = "day_no", gname = "gname",
                             xformla = ~ 1,
                             data = did_dta_victory, panel = FALSE, allow_unbalanced_panel = FALSE)

# Plot.
n_times <- length(unique(dr_results_victory$t))
times <- unique(dr_results_victory$t)

results <-  data.frame(year = as.POSIXct(times, origin = "1970-01-01"))

results$att <- dr_results_victory$att
results$att.se <- dr_results_victory$se
results$post <- as.factor(1 * (results$year >= treatment_date))
results$c <- dr_results_victory$c
alp_moderate_reduction <- dr_results_victory$alp
c.point_moderate_reduction <- stats::qnorm(1 - alp_moderate_reduction / 2)
results$plot_post <- as.factor(1 * (results$year >= (as.Date(treatment_date) - 10)))

plot_pre <- results %>%
  dplyr::filter(plot_post == 0) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = att, ymin = (att - c * att.se), ymax = (att + c * att.se))) +
  ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
  ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
  ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%m-%Y") +
  ggplot2::scale_color_manual(drop = FALSE, values = c("#e87d72", "#56bcc2"), breaks = c(0, 1), labels = c("Pre", "Post")) +
  ggplot2::xlab("") + ggplot2::ylab(expression(italic("ATT ( t )"))) +
  ggplot2::theme_bw() +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 10, face = "bold"), strip.text.y = ggplot2::element_text(size = 10, face = "italic"),
        legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))

plot_post <- results %>%
  dplyr::filter(plot_post == 1) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = att, ymin = (att - c * att.se), ymax = (att + c * att.se))) +
  ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
  ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
  ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%m-%Y") +
  ggplot2::scale_color_manual(drop = FALSE, values = c("#e87d72", "#56bcc2"), breaks = c(0, 1), labels = c("Pre", "Post")) +
  ggplot2::xlab("") + ggplot2::ylab(expression(italic("ATT ( t )"))) +
  ggplot2::theme_bw() +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 10, face = "bold"), strip.text.y = ggplot2::element_text(size = 10, face = "italic"),
        legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))

## Surrender.
# Arrange estimation data.
did_dta_surrender <- lol_match_dta %>%
  mutate(gname = ifelse(graves_match, as.numeric(treatment_date), 0),
         surrender = as.numeric(surrender)) %>%
  distinct(match_id, .keep_all = TRUE)

# Estimation.
dr_results_surrender <- att_gt(yname = "surrender", tname = "day_no", gname = "gname",
                               xformla = ~ 1,
                               data = did_dta_surrender, panel = FALSE, allow_unbalanced_panel = FALSE)

# Plot.
n_times <- length(unique(dr_results_surrender$t))
times <- unique(dr_results_surrender$t)

results <-  data.frame(year = as.POSIXct(times, origin = "1970-01-01"))

results$att <- dr_results_surrender$att
results$att.se <- dr_results_surrender$se
results$post <- as.factor(1 * (results$year >= treatment_date))
results$c <- dr_results_surrender$c
alp_moderate_reduction <- dr_results_surrender$alp
c.point_moderate_reduction <- stats::qnorm(1 - alp_moderate_reduction / 2)
results$plot_post <- as.factor(1 * (results$year >= (as.Date(treatment_date) - 10)))

plot_pre <- results %>%
  dplyr::filter(plot_post == 0) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = att, ymin = (att - c * att.se), ymax = (att + c * att.se))) +
  ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
  ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
  ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%m-%Y") +
  ggplot2::scale_color_manual(drop = FALSE, values = c("#e87d72", "#56bcc2"), breaks = c(0, 1), labels = c("Pre", "Post")) +
  ggplot2::xlab("") + ggplot2::ylab(expression(italic("ATT ( t )"))) +
  ggplot2::theme_bw() +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 10, face = "bold"), strip.text.y = ggplot2::element_text(size = 10, face = "italic"),
        legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))

plot_post <- results %>%
  dplyr::filter(plot_post == 1) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = att, ymin = (att - c * att.se), ymax = (att + c * att.se))) +
  ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
  ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
  ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%m-%Y") +
  ggplot2::scale_color_manual(drop = FALSE, values = c("#e87d72", "#56bcc2"), breaks = c(0, 1), labels = c("Pre", "Post")) +
  ggplot2::xlab("") + ggplot2::ylab(expression(italic("ATT ( t )"))) +
  ggplot2::theme_bw() +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 10, face = "bold"), strip.text.y = ggplot2::element_text(size = 10, face = "italic"),
        legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))

## Duration.
# Arrange estimation data.
did_dta_duration <- lol_match_dta %>%
  mutate(gname = ifelse(graves_match, as.numeric(treatment_date), 0)) %>%
  distinct(match_id, .keep_all = TRUE)

# Estimation.
dr_results_duration <- att_gt(yname = "duration", tname = "day_no", gname = "gname",
                              xformla = ~ 1,
                              data = did_dta_duration, panel = FALSE, allow_unbalanced_panel = FALSE)

# Plot.
n_times <- length(unique(dr_results_duration$t))
times <- unique(dr_results_duration$t)

results <-  data.frame(year = as.POSIXct(times, origin = "1970-01-01"))

results$att <- dr_results_duration$att
results$att.se <- dr_results_duration$se
results$post <- as.factor(1 * (results$year >= treatment_date))
results$c <- dr_results_duration$c
alp_moderate_reduction <- dr_results_duration$alp
c.point_moderate_reduction <- stats::qnorm(1 - alp_moderate_reduction / 2)
results$plot_post <- as.factor(1 * (results$year >= (as.Date(treatment_date) - 10)))

plot_pre <- results %>%
  dplyr::filter(plot_post == 0) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = att, ymin = (att - c * att.se), ymax = (att + c * att.se))) +
  ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
  ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
  ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%m-%Y") +
  ggplot2::scale_color_manual(drop = FALSE, values = c("#e87d72", "#56bcc2"), breaks = c(0, 1), labels = c("Pre", "Post")) +
  ggplot2::xlab("") + ggplot2::ylab(expression(italic("ATT ( t )"))) +
  ggplot2::theme_bw() +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 10, face = "bold"), strip.text.y = ggplot2::element_text(size = 10, face = "italic"),
        legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))

plot_post <- results %>%
  dplyr::filter(plot_post == 1) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = att, ymin = (att - c * att.se), ymax = (att + c * att.se))) +
  ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
  ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
  ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
  ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%m-%Y") +
  ggplot2::scale_color_manual(drop = FALSE, values = c("#e87d72", "#56bcc2"), breaks = c(0, 1), labels = c("Pre", "Post")) +
  ggplot2::xlab("") + ggplot2::ylab(expression(italic("ATT ( t )"))) +
  ggplot2::theme_bw() +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 10, face = "bold"), strip.text.y = ggplot2::element_text(size = 10, face = "italic"),
        legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))
