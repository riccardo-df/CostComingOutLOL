#' LoL Champions' Descriptive Plots
#'
#' Produces plots for the pick, ban, and win variables of the champions of interest.
#'
#' @param champions Character vector with the champions of interest.
#' @param treatment_date1 Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param treatment_date2 Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param bandwidth Parameter controlling the amount of smoothing.
#' @param ylims_levels Vector storing lower and upper limit for the y-axis (valid for the variables measured in levels).
#' @param ylims_rates Vector storing lower and upper limit for the y-axis (valid for the variables measured in rates).
#' @param save_here String denoting the path where to save the figures.
#' @param dataset String controlling which data set to use. If \code{"main"}, we use \code{lol_champ_pool_dta}. If \code{"below"}, we use \code{lol_champ_pool_dta_belowM}. If \code{"above"}, we use \code{lol_champ_pool_dta_aboveM}.
#'
#' @return
#' Produces nice plots.
#'
#' @details
#' \code{treatment_date1} and \code{treatment_date2}, must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' If one of the element of \code{champions} is \code{"LGB"}, \code{\link{champions_descriptive_plots_lol}} includes in the plots an LGB composite unit constructed by averaging the variables values of the
#' champions Diana, Leona, Nami, and Neeko. These are the champions confirmed to be LGBT before Graves' disclosure. See Section 2.2 and Section 5.4 of the paper for more details.\cr
#'
#' The series are smoothed using a Nadaraya–Watson kernel regression. The user can control the amount of smoothing by setting the \code{bandwidth} parameter. The larger parameter, the smoother the series.
#' An infinitesimal bandwidth amounts to no smoothing.
#'
#' @import dplyr ggplot2 grDevices
#' @importFrom stats ksmooth
#' @importFrom stats time
#'
#' @author Riccardo Di Francesco
#'
#' @export
champions_descriptive_plots_lol <- function(champions,
                                  treatment_date1 = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"),
                                  treatment_date2 = as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d"),
                                  bandwidth = 0.01, ylims_rates = c(0, 100), save_here = getwd(),
                                  dataset = "main") {
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
  if (!(dataset %in% c("main", "below", "above"))) stop("Invalid 'dataset'. This must be one of 'main', 'below', 'above'.", call. = FALSE)

  if (dataset == "main") {
    lol_champ_pool_dta <- lol_champ_pool_dta
  } else if (dataset == "below") {
    lol_champ_pool_dta <- lol_champ_pool_dta_belowM
  } else if (dataset == "above") {
    lol_champ_pool_dta <- lol_champ_pool_dta_aboveM
  }

  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  ## 1.) If necessary, construct composite LGB unit and bind rows.
  if ("LGB" %in% champions) {
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
  }

  ## 2.) Smooth the series.
  to_smooth_pool <- c("pick_rate_pooled", "ban_rate_pooled", "win_rate_pooled")
  to_smooth_regional <- c("pick_rate", "ban_rate", "win_rate")

  lol_champ_pool_dta <- lol_champ_pool_dta %>%
    dplyr::group_by(champion) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(to_smooth_pool), function(x) { stats::ksmooth(stats::time(x), x, "normal", bandwidth = bandwidth)$y }))

  lol_champ_dta <- lol_champ_dta %>%
    dplyr::group_by(region, champion) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(to_smooth_regional), function(x) { stats::ksmooth(stats::time(x), x, "normal", bandwidth = bandwidth)$y }))

  ## 3.) Plots for pooled variables.
  # 3a.) Picks.
  plot_pick_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::ylim(ylims_rates[1], ylims_rates[2]) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick rate")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%Y") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", strip.text = ggplot2::element_text(size = 10, face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_rate_pooled.pdf"), plot_pick_rate_pooled, width = 7, height = 7)

  # 3b.) Bans.
  plot_ban_rate_pooled <- lol_champ_pool_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_rate_pooled, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::ylim(ylims_rates[1], ylims_rates[2]) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban rate")) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%Y") +
    ggplot2::facet_wrap(vars(champion)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", strip.text = ggplot2::element_text(size = 10, face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_rate_pooled.pdf"), plot_ban_rate_pooled, width = 7, height = 7)

  ## 4.) Regional plots.
  # 4a.) Picks.
  plot_pick_rate_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = pick_rate, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Pick rate")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%m-%Y") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", strip.text = ggplot2::element_text(size = 10, face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_pick_rate_regional.pdf"), plot_pick_rate_regional, width = 7, height = 7)

  # 4b.) Bans.
  plot_ban_rate_regional <- lol_champ_dta %>%
    dplyr::filter(champion %in% c(champions, "Composite LGB")) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(day), y = ban_rate, group = champion, color = champion)) +
    ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(color = "tomato") +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date1), linetype = 4) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date2), linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab(paste0("Ban rate")) +
    ggplot2::scale_x_datetime(date_breaks = "3 month", date_labels = "%m-%Y") +
    ggplot2::facet_grid(cols = vars(champion), rows = vars(region), scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", strip.text = ggplot2::element_text(size = 10, face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(paste0(save_here, "/", "plot_ban_rate_regional.pdf"), plot_ban_rate_regional, width = 7, height = 7)

  ## 5.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' LoL Players' Behavior Plots
#'
#' Produces plots for the number of matches and hours played.
#'
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces a nice plot.
#'
#' @details
#' \code{treatment_date1} and must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' @import dplyr ggplot2 grDevices Cairo reshape2 tidyr
#'
#' @author Riccardo Di Francesco
#'
#' @export
players_descriptive_plots_lol <- function(save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  lol_player_dta <- lol_player_dta
  tot_hours <- NULL

  ## 1.) Compute total matches and hours played by each player.
  lol_player_dta <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(tot_matches = sum(n_matches),
                  tot_hours = sum(n_hours)) %>%
    dplyr::ungroup()

  ## 2.) Arrange plotting data.
  totals_dta <- lol_player_dta %>%
    dplyr::distinct(id, tot_matches, tot_hours) %>%
    tidyr::pivot_longer(cols = c(tot_matches, tot_hours), names_to = "variable", values_to = "value") %>%
    dplyr::mutate(level = "Player",
                  variable = dplyr::recode(variable,
                                           tot_matches = "Total matches",
                                           tot_hours = "Total hours"))

  daily_dta <- lol_player_dta %>%
    dplyr::select(id, n_matches, n_hours) %>%
    tidyr::pivot_longer(cols = c(n_matches, n_hours), names_to = "variable", values_to = "value") %>%
    dplyr::mutate(level = "Daily",
                  variable = dplyr::recode(variable,
                                           n_matches = "Daily matches",
                                           n_hours = "Daily hours"))

  hist_dta <- bind_rows(totals_dta, daily_dta)

  ## 3.) Plot.
  plot_player_activity <- hist_dta %>%
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 100, fill = "#4E79A7", color = "black", alpha = 0.8) +
    ggplot2::facet_wrap(~ variable, scales = "free", ncol = 2) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::labs(title = "", x = NULL, y = "Count") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                   panel.grid.minor = ggplot2::element_blank())
  ggplot2::ggsave(paste0(save_here, "/", "plot_player_activity.pdf"), plot_player_activity, width = 10, height = 7)

  ## 3.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}
