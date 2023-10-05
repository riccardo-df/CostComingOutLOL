#' LoL Descriptive Plots
#'
#' Produces plots for the pick, ban, and win variables of the champions of interest.
#'
#' @param champions Character vector with the champions of interest.
#' @param treatment_date1 Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param treatment_date2 Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces nice plots.
#'
#' @details
#' \code{treatment_date1}, \code{treatment_date2}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.
#'
#' @import dplyr ggplot2 grDevices
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{descriptive_plots_lol_lgb}} \code{\link{performance_plots_lol}}
#'
#' @export
descriptive_plots_lol <- function(champions,
                                  treatment_date1 = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"),
                                  treatment_date2 = as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d"),
                                  min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-09-12"),
                                  save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  if (sum(!(champions %in% unique(lol_champ_dta$champion))) > 1 | sum(!(champions %in% unique(lol_champ_pool_dta$champion)))) stop("Invalid 'champions'. One or more champions are not in the data sets.", call. = FALSE)

  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

  pride_month_2023_begin <- as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2023_end <- as.POSIXct("2023-06-30", tryFormats = "%Y-%m-%d")

  plot_2022_rainbow <- (as.Date(min_date) + 1 < as.Date(pride_month_2022_begin) + 1) & (as.Date(max_date) + 1 > as.Date(pride_month_2022_end) + 1)
  plot_2023_rainbow <- (as.Date(min_date) + 1 < as.Date(pride_month_2023_begin) + 1) & (as.Date(max_date) + 1 > as.Date(pride_month_2023_end) + 1)

  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  lol_champ_dta <- lol_champ_dta %>%
    dplyr::filter(min_date < day & day < max_date)

  lol_champ_pool_dta <- lol_champ_pool_dta %>%
    dplyr::filter(min_date < day & day < max_date)

  ## 1.) Plots for pooled variables.
  # 1a.) Picks.
  plot_pick_level_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_level_sum, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick level")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_level_pooled.svg"), plot_pick_level_pooled, device = "svg")

  plot_pick_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick rate")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_rate_pooled.svg"), plot_pick_rate_pooled, device = "svg")

  # 1b.) Bans.
  plot_ban_level_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_level_sum, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban level")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_level_pooled.svg"), plot_ban_level_pooled, device = "svg")

  plot_ban_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban rate")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_rate_pooled.svg"), plot_ban_rate_pooled, device = "svg")

  # 1c.) Wins.
  plot_win_level_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_level_sum, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win level")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_win_level_pooled.svg"), plot_win_level_pooled, device = "svg")

  plot_win_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win rate")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_win_rate_pooled.svg"), plot_win_rate_pooled, device = "svg")

  ## 2.) Regional plots.
  # 2a.) Picks.
  plot_pick_level_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_level, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick level")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%Y-%m") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_level_regional.svg"), plot_pick_level_regional, device = "svg")

  plot_pick_rate_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_rate, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick rate")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%Y-%m") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_rate_regional.svg"), plot_pick_rate_regional, device = "svg")

  # 2b.) Bans.
  plot_ban_level_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_level, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban level")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%Y-%m") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_level_regional.svg"), plot_ban_level_regional, device = "svg")

  plot_ban_rate_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_rate, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban rate")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%Y-%m") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_rate_regional.svg"), plot_ban_rate_regional, device = "svg")

  # 2c.) Wins.
  plot_win_level_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_level, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win level")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%Y-%m") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_win_level_regional.svg"), plot_win_level_regional, device = "svg")

  plot_win_rate_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% champions) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_rate, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win rate")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%Y-%m") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_win_rate_regional.svg"), plot_win_rate_regional, device = "svg")

  ## 3.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' LoL Descriptive Plots (LGB Aggregate)
#'
#' Produces plots for the pick, ban, and win variables of the LGB composite unit, constructed by averaging variables for the LGB champions
#' 'Nami', 'Leona', 'Diana', and 'Neeko'.
#'
#' @param treatment_date1 Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param treatment_date2 Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces nice plots.
#'
#' @details
#' \code{treatment_date1}, \code{treatment_date2}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.
#'
#' @import dplyr ggplot2 reshape2 grDevices
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{descriptive_plots_lol}} \code{\link{performance_plots_lol}}
#'
#' @export
descriptive_plots_lol_lgb_composite <- function(treatment_date1 = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"),
                                      treatment_date2 = as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d"),
                                      min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-09-12"),
                                      save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

  pride_month_2023_begin <- as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2023_end <- as.POSIXct("2023-06-30", tryFormats = "%Y-%m-%d")

  plot_2022_rainbow <- (as.Date(min_date) + 1 < as.Date(pride_month_2022_begin) + 1) & (as.Date(max_date) + 1 > as.Date(pride_month_2022_end) + 1)
  plot_2023_rainbow <- (as.Date(min_date) + 1 < as.Date(pride_month_2023_begin) + 1) & (as.Date(max_date) + 1 > as.Date(pride_month_2023_end) + 1)

  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  lgb_champions <- c("Nami", "Leona", "Diana", "Neeko")

  lol_champ_pool_dta <- lol_champ_pool_dta %>%
    dplyr::filter(min_date < day & day < max_date)

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
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(vars(variable), scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_lgb_aggregate.svg"), plot, device = "svg")

  ## 2.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' LoL Performance Plots
#'
#' Produces plots for kill-to-death ratio, number of assists, number of kills, and win rates of the champions of interest.
#'
#' @param champions Character vector with the champions of interest.
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces a nice plot.
#'
#' @details
#' \code{treatment_date1}, \code{treatment_date2}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.
#'
#' @import dplyr ggplot2 grDevices Cairo reshape2
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{descriptive_plots_lol}} \code{\link{descriptive_plots_lol_lgb}}
#'
#' @export
performance_plots_lol <- function(champions,
                                  min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-09-12"),
                                  save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  if (sum(!(champions %in% unique(lol_champ_dta$champion))) > 1 | sum(!(champions %in% unique(lol_champ_pool_dta$champion)))) stop("Invalid 'champions'. One or more champions are not in the data sets.", call. = FALSE)

  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

  pride_month_2023_begin <- as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2023_end <- as.POSIXct("2023-06-30", tryFormats = "%Y-%m-%d")

  plot_2022_rainbow <- (as.Date(min_date) + 1 < as.Date(pride_month_2022_begin) + 1) & (as.Date(max_date) + 1 > as.Date(pride_month_2022_end) + 1)
  plot_2023_rainbow <- (as.Date(min_date) + 1 < as.Date(pride_month_2023_begin) + 1) & (as.Date(max_date) + 1 > as.Date(pride_month_2023_end) + 1)

  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  lol_champ_pool_dta <- lol_champ_pool_dta %>%
    dplyr::filter(min_date < day & day < max_date)

  ## 1.) Plots.
  for (i in seq_len((length(champions)))) {
    my_champion <- champions[i]

    plot_dta <- lol_champ_pool_dta %>%
      dplyr::filter(champion == my_champion) %>%
      dplyr::mutate(kd_ratio = kills_pooled / deaths_pooled) %>%
      replace(is.na(.), 0) %>%
      dplyr::select(day, kd_ratio, assists_pooled, gold_pooled, win_rate_pooled)
    colnames(plot_dta) <- c("day", "Kills/Deaths", "Assists", "Gold", "Win Rate")

    plot <- plot_dta %>%
      reshape2::melt(id.vars = "day", measure.vars = c("Kills/Deaths", "Assists", "Gold", "Win Rate")) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = value)) +
      ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line(color = "tomato", linewidth = 0.5) +
      ggplot2::facet_grid(rows = vars(variable), scales = "free") +
      ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::ggtitle(my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "none")

    ggsave(paste0(save_here, "/", tolower(my_champion), "_performance_pooled.svg"), plot = plot, device = Cairo::CairoSVG)
  }

  ## 2.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}
