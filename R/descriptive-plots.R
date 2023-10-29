#' LoL Descriptive Plots
#'
#' Produces plots for the pick, ban, and win variables of the champions of interest. It also produces plots to investigate the most common auxiliary role given the main role.
#'
#' @param champions Character vector with the champions of interest.
#' @param treatment_date1 Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param treatment_date2 Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#' @param bandwidth Parameter controlling the amount of smoothing.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces nice plots.
#'
#' @details
#' \code{treatment_date1}, \code{treatment_date2}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' \code{\link{champions_descriptive_plots_lol}} always includes in the plots an LGB composite unit constructed by averaging the variables values of the champions
#' Nami, Leona, Diana, and Neeko.\cr
#'
#' The series are smoothed using a Nadaraya–Watson kernel regression. The user can control the amount of smoothing by setting the \code{bandwidth} parameter. The larger parameter, the smoother the series.
#' An infinitesimal bandwidth amounts to no smoothing.\cr
#'
#' @import dplyr ggplot2 grDevices
#' @importFrom stats ksmooth
#' @importFrom stats time
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{champions_performance_plots_lol}} \code{\link{players_descriptive_plots_lol}}
#'
#' @export
champions_descriptive_plots_lol <- function(champions,
                                  treatment_date1 = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"),
                                  treatment_date2 = as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d"),
                                  min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-08-01"),
                                  bandwidth = 0.01, save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  champion <- NULL
  pick_level_sum <- NULL
  pick_rate_pooled <- NULL
  ban_level_sum <- NULL
  ban_rate_pooled <- NULL
  win_level_sum <- NULL
  win_rate_pooled <- NULL
  lgb_pick_level <- NULL
  lgb_pick_rate <- NULL
  lgb_ban_level <- NULL
  lgb_ban_rate <- NULL
  lgb_win_level <- NULL
  lgb_win_rate <- NULL
  main_role <- NULL
  aux_role <- NULL
  region <- NULL
  pick_level <- NULL
  pick_rate <- NULL
  ban_level <- NULL
  ban_rate <- NULL
  win_level <- NULL
  win_rate <- NULL
  kills_pooled <- NULL
  deaths_pooled <- NULL
  kd_ratio <- NULL
  value <- NULL
  variable <- NULL

  if (sum(!(champions %in% unique(lol_champ_dta$champion))) > 1 | sum(!(champions %in% unique(lol_champ_pool_dta$champion)))) stop("Invalid 'champions'. One or more champions are not in the data sets.", call. = FALSE)
  if (bandwidth <= 0) stop("Invalid 'bandwidth'. This must be a positive number.", call. = FALSE)

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

  ## 1.) Construct composite LGB unit and bind rows.
  lgb_champions <- c("Nami", "Leona", "Diana", "Neeko")

  lgb_aggregate_pool <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% lgb_champions) %>%
    dplyr::group_by(day) %>%
    dplyr::mutate(lgb_pick_level = mean(pick_level_sum),
                  lgb_pick_rate = mean(pick_rate_pooled),
                  lgb_ban_level = mean(ban_level_sum),
                  lgb_ban_rate = mean(ban_rate_pooled),
                  lgb_win_level = mean(win_level_sum),
                  lgb_win_rate = mean(win_rate_pooled)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(champion = "Composite LGB") %>%
    dplyr::distinct(day, .keep_all = TRUE) %>%
    dplyr::select(day, champion, lgb_pick_level, lgb_pick_rate, lgb_ban_level, lgb_ban_rate, lgb_win_level, lgb_win_rate, main_role, aux_role)

  colnames(lgb_aggregate_pool) <- c("day", "champion", "pick_level_sum", "pick_rate_pooled", "ban_level_sum", "ban_rate_pooled", "win_level_sum", "win_rate_pooled", "main_role", "aux_role")

  lol_champ_pool_dta <- lol_champ_pool_dta %>%
    dplyr::select(day, champion, pick_level_sum, pick_rate_pooled, ban_level_sum, ban_rate_pooled, win_level_sum, win_rate_pooled, main_role, aux_role) %>%
    dplyr::bind_rows(lgb_aggregate_pool)

  lgb_aggregate_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% lgb_champions) %>%
    dplyr::group_by(region, day) %>%
    dplyr::mutate(lgb_pick_level = mean(pick_level),
                  lgb_pick_rate = mean(pick_rate),
                  lgb_ban_level = mean(ban_level),
                  lgb_ban_rate = mean(ban_rate),
                  lgb_win_level = mean(win_level),
                  lgb_win_rate = mean(win_rate)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(champion = "Composite LGB") %>%
    dplyr::distinct(region, day, .keep_all = TRUE) %>%
    dplyr::select(region, day, champion, lgb_pick_level, lgb_pick_rate, lgb_ban_level, lgb_ban_rate, lgb_win_level, lgb_win_rate)

  colnames(lgb_aggregate_regional) <- c("region", "day", "champion", "pick_level", "pick_rate", "ban_level", "ban_rate", "win_level", "win_rate")

  lol_champ_dta <- lol_champ_dta %>%
    dplyr::select(region, day, champion, pick_level, pick_rate, ban_level, ban_rate, win_level, win_rate) %>%
    dplyr::bind_rows(lgb_aggregate_regional)

  ## 2.) Smooth the series.
  to_smooth_pool <- c("pick_level_sum", "pick_rate_pooled", "ban_level_sum", "ban_rate_pooled", "win_level_sum", "win_rate_pooled")
  to_smooth_regional <- c("pick_level", "pick_rate", "ban_level", "ban_rate", "win_level", "win_rate")

  lol_champ_pool_dta <- lol_champ_pool_dta %>%
    dplyr::group_by(champion) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(to_smooth_pool), function(x) { stats::ksmooth(stats::time(x), x, "normal", bandwidth = bandwidth)$y }))

  lol_champ_dta <- lol_champ_dta %>%
    dplyr::group_by(region, champion) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(to_smooth_regional), function(x) { stats::ksmooth(stats::time(x), x, "normal", bandwidth = bandwidth)$y }))

  ## 3.) Plots for pooled variables.
  # 3a.) Picks.
  plot_pick_level_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_level_sum, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick level")) +
    ggplot2::scale_x_datetime(date_breaks = "2 month", date_labels = "%m-%Y") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_level_pooled.svg"), plot_pick_level_pooled, device = Cairo::CairoSVG, width = 7, height = 7)

  plot_pick_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick rate")) +
    ggplot2::scale_x_datetime(date_breaks = "2 month", date_labels = "%m-%Y") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_rate_pooled.svg"), plot_pick_rate_pooled, device = Cairo::CairoSVG, width = 7, height = 7)

  # 3b.) Bans.
  plot_ban_level_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_level_sum, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban level")) +
    ggplot2::scale_x_datetime(date_breaks = "2 month", date_labels = "%m-%Y") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_level_pooled.svg"), plot_ban_level_pooled, device = Cairo::CairoSVG, width = 7, height = 7)

  plot_ban_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban rate")) +
    ggplot2::scale_x_datetime(date_breaks = "2 month", date_labels = "%m-%Y") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_rate_pooled.svg"), plot_ban_rate_pooled, device = Cairo::CairoSVG, width = 7, height = 7)

  # 3c.) Wins.
  plot_win_level_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_level_sum, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win level")) +
    ggplot2::scale_x_datetime(date_breaks = "2 month", date_labels = "%m-%Y") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_win_level_pooled.svg"), plot_win_level_pooled, device = Cairo::CairoSVG, width = 7, height = 7)

  plot_win_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win rate")) +
    ggplot2::scale_x_datetime(date_breaks = "2 month", date_labels = "%m-%Y") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_win_rate_pooled.svg"), plot_win_rate_pooled, device = Cairo::CairoSVG, width = 7, height = 7)

  ## 4.) Regional plots.
  # 4a.) Picks.
  plot_pick_level_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_level, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick level")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%m-%Y") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_level_regional.svg"), plot_pick_level_regional, device = Cairo::CairoSVG, width = 7, height = 7)

  plot_pick_rate_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_rate, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick rate")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%m-%Y") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_rate_regional.svg"), plot_pick_rate_regional, device = Cairo::CairoSVG, width = 7, height = 7)

  # 4b.) Bans.
  plot_ban_level_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_level, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban level")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%m-%Y") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_level_regional.svg"), plot_ban_level_regional, device = Cairo::CairoSVG, width = 7, height = 7)

  plot_ban_rate_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_rate, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban rate")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%m-%Y") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_rate_regional.svg"), plot_ban_rate_regional, device = Cairo::CairoSVG, width = 7, height = 7)

  # 4c.) Wins.
  plot_win_level_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_level, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win level")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%m-%Y") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_win_level_regional.svg"), plot_win_level_regional, device = Cairo::CairoSVG, width = 7, height = 7)

  plot_win_rate_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = win_rate, group = champion, color = champion)) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Win rate")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%m-%Y") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_win_rate_regional.svg"), plot_win_rate_regional, device = Cairo::CairoSVG, width = 7, height = 7)

  ## 5.) Auxiliary role.
  plot_aux_role <- lol_champ_pool_dta %>%
    dplyr::distinct(champion, .keep_all = TRUE) %>%
    dplyr::group_by(main_role, aux_role) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::arrange(main_role, desc(count)) %>%
    ggplot2::ggplot(ggplot2::aes(x = aux_role, y = count)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Count")) +
    ggplot2::facet_grid(vars(main_role), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_aux_role.svg"), plot_aux_role, device = Cairo::CairoSVG, width = 7, height = 7)

  ## 6.) Talk to the user.
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
#' @param bandwidth Parameter controlling the amount of smoothing.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces a nice plot.
#'
#' @details
#' \code{treatment_date1}, \code{treatment_date2}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' The series are smoothed using a Nadaraya–Watson kernel regression. The user can control the amount of smoothing by setting the \code{bandwidth} parameter. The larger parameter, the smoother the series.
#' An infinitesimal bandwidth amounts to no smoothing.\cr
#'
#' @import dplyr ggplot2 grDevices Cairo reshape2
#' @importFrom stats ksmooth
#' @importFrom stats time
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{champions_descriptive_plots_lol}} \code{\link{players_descriptive_plots_lol}}
#'
#' @export
champions_performance_plots_lol <- function(champions,
                                  min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-08-01"),
                                  bandwidth = 0.01, save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  lol_champ_dta <- lol_champ_dta
  champion <- NULL
  kills_pooled <- NULL
  deaths_pooled <- NULL
  . <- NULL
  kd_ratio <- NULL
  win_rate_pooled <- NULL
  value <- NULL
  variable <- NULL

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
      dplyr::select(day, kd_ratio, win_rate_pooled)
    colnames(plot_dta) <- c("day", "Kills/Deaths", "Win Rate")

    to_smooth_pool <- c("Kills/Deaths", "Win Rate")

    plot_dta <- plot_dta %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(to_smooth_pool), function(x) { stats::ksmooth(stats::time(x), x, "normal", bandwidth = bandwidth)$y }))

    plot <- plot_dta %>%
      reshape2::melt(id.vars = "day", measure.vars = c("Kills/Deaths", "Win Rate")) %>%
      ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = value)) +
      ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line(color = "tomato", linewidth = 0.5) +
      ggplot2::facet_grid(rows = vars(variable), scales = "free") +
      ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::ggtitle(my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%Y") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.position = "none")

    ggsave(paste0(save_here, "/", tolower(my_champion), "_performance_pooled.svg"), plot = plot, device = Cairo::CairoSVG, width = 7, height = 7)
  }

  ## 2.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' LoL Performance Plots
#'
#' Produces plots for kill-to-death ratio, number of assists, number of kills, and win rates of the champions of interest.
#'
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#' @param bandwidth Parameter controlling the amount of smoothing.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces a nice plot.
#'
#' @details
#' \code{treatment_date1}, \code{treatment_date2}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' The series are smoothed using a Nadaraya–Watson kernel regression. The user can control the amount of smoothing by setting the \code{bandwidth} parameter. The larger parameter, the smoother the series.
#' An infinitesimal bandwidth amounts to no smoothing.\cr
#'
#' @import dplyr ggplot2 grDevices Cairo reshape2
#' @importFrom stats ksmooth
#' @importFrom stats time
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{champions_descriptive_plots_lol}} \code{\link{champions_performance_plots_lol}}
#'
#' @export
players_descriptive_plots_lol <- function(min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-08-01"),
                                          bandwidth = 0.01, save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  lol_champ_dta <- lol_champ_dta
  n_matches <- NULL
  total_matches <- NULL
  total_players <- NULL

  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

  pride_month_2023_begin <- as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2023_end <- as.POSIXct("2023-06-30", tryFormats = "%Y-%m-%d")

  plot_2022_rainbow <- (as.Date(min_date) + 1 < as.Date(pride_month_2022_begin) + 1) & (as.Date(max_date) + 1 > as.Date(pride_month_2022_end) + 1)
  plot_2023_rainbow <- (as.Date(min_date) + 1 < as.Date(pride_month_2023_begin) + 1) & (as.Date(max_date) + 1 > as.Date(pride_month_2023_end) + 1)

  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(min_date < day & day < max_date)

  ## 1.) Plot.
  trans_coef <- 5
  matches_color <- "tomato"
  players_color <- "#00BFC4"

  plot <- lol_player_dta %>%
    dplyr::group_by(day) %>%
    dplyr::mutate(total_matches = sum(n_matches) / 10,
                  total_players = n_distinct(id)) %>%
    dplyr::distinct(day, .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c("total_matches", "total_players")), function(x) { stats::ksmooth(stats::time(x), x, "normal", bandwidth = bandwidth)$y })) %>%
    ggplot2::ggplot(ggplot2::aes(x = day)) +
    ggplot2::geom_line(ggplot2::aes(y = total_matches, color = "Matches"), linewidth = 0.8) +
    ggplot2::geom_line(aes(y = total_players / trans_coef, color = "Players"), linewidth = 0.8) +
    ggplot2::annotation_raster(rainbow, xmin = pride_month_2022_begin, xmax = pride_month_2022_end, ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(rainbow, xmin = pride_month_2023_begin, xmax = pride_month_2023_end, ymin = -Inf, ymax = Inf) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%Y") +
    ggplot2::scale_y_continuous(name = "N. daily matches", sec.axis = sec_axis(~ . * trans_coef, name = "N. daily players")) +
    ggplot2::scale_color_manual(name = "Colors", values = c("Matches" = matches_color, "Players" = players_color)) +
    ggplot2::xlab("") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = c(0.11, 0.9), legend.title = ggplot2::element_blank(), legend.direction = "vertical")
  ggplot2::ggsave(paste0(save_here, "/", "n_matches_players.svg"), plot, device = Cairo::CairoSVG, width = 7, height = 7)

  ## 2.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}
