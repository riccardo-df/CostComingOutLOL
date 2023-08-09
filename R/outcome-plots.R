#' LoL Outcome Plots
#'
#' Produces plots for the pick, ban, and win variables of the champions of interest.
#'
#' @param champions Character vector with the champions of interest.
#' @param lol_champ_dta Data set bundled in the package.
#' @param lol_champ_pool_dta Data set bundled in the package.
#' @param max_date Object of class \code{POSIXct}. Where to cut the series.
#'
#' @import dplyr ggplot2
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{construct_lol_champion_data}}, \code{\link{construct_lol_champion_pooled_data}}, \code{\link{\code{\link{construct_lol_player_data}}}}
#'
#' @export
outcome_plots_lol <- function(champions, lol_champ_dta, lol_champ_pool_dta, max_date = as.POSIXct("2022-07-15")) {
  ## 0.) Handling inputs and checks.
  if (sum(!(champions %in% unique(lol_champ_dta$champion))) > 1 | sum(!(champions %in% unique(lol_champ_pool_dta$champion)))) stop("Invalid 'champions'. One or more champions are not in the data sets.", call. = FALSE)
  if (max_date > max(lol_champ_dta$day) | max_date > max(lol_champ_pool_dta$day)) stop("Invalid 'max_date'. It is larger than the most recent day in one or both data sets.")

  treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")
  rainbow <- adjustcolor(matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  lol_champ_dta <- lol_champ_dta %>%
    dplyr::filter(day < max_date)

  lol_champ_pool_dta <- lol_champ_pool_dta %>%
    dplyr::filter(day < max_date)

  ## 1.) Plots for pooled variables.
  # 1a.) Picks.
  plot_pick_level_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_level_sum, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick level")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave("plot_pick_level_pooled.svg", plot_pick_level_pooled, device = "svg")

  plot_pick_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick rate")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave("plot_pick_rate_pooled.svg", plot_pick_rate_pooled, device = "svg")

  # 1b.) Bans.
  plot_ban_level_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_level_sum, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban level")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave("plot_ban_level_pooled.svg", plot_ban_level_pooled, device = "svg")

  plot_ban_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban rate")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave("plot_ban_rate_pooled.svg", plot_ban_rate_pooled, device = "svg")

  # 1c.) Wins.
  plot_win_level_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_level_sum, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win level")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave("plot_win_level_pooled.svg", plot_win_level_pooled, device = "svg")

  plot_win_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win rate")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave("plot_win_rate_pooled.svg", plot_win_rate_pooled, device = "svg")

  ## 2.) Regional plots.
  for (my_champion in champions) {
    # 2a.) Picks.
    regional_plot_pick_level <- lol_champ_dta %>%
      dplyr::filter(champion == my_champion) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_level, group = champion, color = champion)) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::facet_wrap(vars(region), ncol = 2) +
      ggplot2::xlab("") + ggplot2::ylab(paste0("Pick level")) + ggplot2::ggtitle(my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    ggplot2::ggsave(paste0(my_champion, "_regional_plot_pick_level.svg"), regional_plot_pick_level, device = "svg")

    regional_plot_pick_rate <- lol_champ_dta %>%
      dplyr::filter(champion == my_champion) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_rate, group = champion, color = champion)) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::facet_wrap(vars(region), ncol = 2) +
      ggplot2::xlab("") + ggplot2::ylab(paste0("Pick rate")) + ggplot2::ggtitle(my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    ggplot2::ggsave(paste0(my_champion, "_regional_plot_pick_rate.svg"), regional_plot_pick_rate, device = "svg")

    # 2b.) Bans.
    regional_plot_ban_level <- lol_champ_dta %>%
      dplyr::filter(champion == my_champion) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_level, group = champion, color = champion)) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::facet_wrap(vars(region), ncol = 2) +
      ggplot2::xlab("") + ggplot2::ylab(paste0("Ban level")) + ggplot2::ggtitle(my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    ggplot2::ggsave(paste0(my_champion, "_regional_plot_ban_level.svg"), regional_plot_ban_level, device = "svg")

    regional_plot_ban_rate <- lol_champ_dta %>%
      dplyr::filter(champion == my_champion) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_rate, group = champion, color = champion)) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::facet_wrap(vars(region), ncol = 2) +
      ggplot2::xlab("") + ggplot2::ylab(paste0("Ban rate")) + ggplot2::ggtitle(my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    ggplot2::ggsave(paste0(my_champion, "_regional_plot_ban_rate.svg"), regional_plot_ban_rate, device = "svg")

    # 3c.) Wins.
    regional_plot_win_level <- lol_champ_dta %>%
      dplyr::filter(champion == my_champion) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_level, group = champion, color = champion)) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::facet_wrap(vars(region), ncol = 2) +
      ggplot2::xlab("") + ggplot2::ylab(paste0("Win level")) + ggplot2::ggtitle(my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    ggplot2::ggsave(paste0(my_champion, "_regional_plot_win_level.svg"), regional_plot_win_level, device = "svg")

    regional_plot_win_rate <- lol_champ_dta %>%
      dplyr::filter(champion == my_champion) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_rate, group = champion, color = champion)) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::facet_wrap(vars(region), ncol = 2) +
      ggplot2::xlab("") + ggplot2::ylab(paste0("Win rate")) + ggplot2::ggtitle(my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    ggplot2::ggsave(paste0(my_champion, "_regional_plot_win_rate.svg"), regional_plot_win_rate, device = "svg")
  }

  cat("Figures are saved at ", getwd(), "\n", sep = "")
}


#' LoL Outcome Plots (LGB Aggregate)
#'
#' Produces plots for the pick, ban, and win variables of the LGB aggregate, constructed by averaging variables for the LGB champions
#' 'Nami', 'Leona', 'Diana', and 'Neeko'.
#'
#' @param lol_champ_pool_dta Data set bundled in the package.
#' @param max_date Object of class \code{POSIXct}. Where to cut the series.
#'
#' @import dplyr ggplot2 reshape2
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{pull_lol_data}}, \code{\link{construct_lol_champion_data}}, \code{\link{construct_lol_champion_pooled_data}}, \code{\link{\code{\link{construct_lol_player_data}}}}
#'
#' @export
outcome_plots_lol_lgb <- function(lol_champ_pool_dta, max_date = as.POSIXct("2022-07-15")) {
  ## 0.) Handling inputs and checks.
  if (max_date > max(lol_champ_dta$day) | max_date > max(lol_champ_pool_dta$day)) stop("Invalid 'max_date'. It is larger than the most recent day in one or both data sets.")

  treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")
  rainbow <- adjustcolor(matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  lgb_champions <- c("Nami", "Leona", "Diana", "Neeko")

  lol_champ_pool_dta <- lol_champ_pool_dta %>%
    dplyr::filter(day < max_date)

  lgb_aggregate <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% lgb_champions) %>%
    dplyr::group_by(day) %>%
    dplyr::mutate(lgb_pick_level = mean(pick_level_sum),
                  lgb_pick_rate = mean(pick_rate_pooled),
                  lgb_ban_level = mean(ban_level_sum),
                  lgb_ban_rate = mean(ban_rate_pooled),
                  lgb_win_level = mean(win_level_sum),
                  lgb_win_rate = mean(win_rate_pooled)) %>%
    dplyr::distinct(day, .keep_all = TRUE) %>%
    dplyr::select(day, lgb_pick_level, lgb_pick_rate, lgb_ban_level, lgb_ban_rate, lgb_win_level, lgb_win_rate)

  colnames(lgb_aggregate) <- c("day", "pick_level", "pick_rate", "ban_level", "ban_rate", "win_level", "win_rate")

  lgb_aggregate <- lgb_aggregate %>%
    reshape2::melt(id.vars = "day")

  ## 1.) Plots.
  plot <- lgb_aggregate %>%
    ggplot2::ggplot(aes(x = as.POSIXct(day), y = value)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(variable), scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave("plot_lgb_aggregate.svg", plot, device = "svg")

  cat("Figures are saved at ", getwd(), "\n", sep = "")
}
