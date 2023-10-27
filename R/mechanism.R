#' LoL Mechanisms Plots
#'
#' Divide players into groups according to their pre-treatment pick rates for Graves and produces plots showing the average performance of players in each group
#' before and after the treatment.
#'
#' @param n_groups How many groups to form.
#' @param n_pre_matches How many matches before \code{treatment_date} players must have played to be kept in the data set.
#' @param n_post_matches How many matches before \code{treatment_date} players must have played to be kept in the data set.
#' @param treatment_date Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces nice plots.
#'
#' @details
#' \code{treatment_date}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' Groups are formed by cutting the Graves' pre-treatment pick rates distribution into \code{n_groups} buckets.\cr
#'
#' Players that have played less than \code{n_pre_matches} before \code{treatment_date} or less than \code{n_post_matches} between \code{treatment_date} and \code{max_date} are dropped. The number of
#' players remaining in the data set is printed.
#'
#' @import dplyr reshape2 ggplot2
#'
#' @author Riccardo Di Francesco
#'
#' @export
mechanisms_plots_lol <- function(n_groups, n_pre_matches, n_post_matches,
                                 treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"), min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-08-01"),
                                 save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(min_date < day & day < max_date) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(treatment = ifelse(day > treatment_date, 1, 0),
                  n_matches_pre = sum(n_matches * (1 - treatment)),
                  n_matches_post = sum(n_matches * treatment)) %>%
    dplyr::filter(n_matches_pre >= n_pre_matches & n_matches_post >= n_post_matches) %>%
    dplyr::select(day, treatment, id, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg) %>%
    dplyr::ungroup()

  never_graves <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(never = sum(graves_rate) == 0,
                  never_pre = sum(graves_rate * (1 - treatment)) == 0,
                  never_post = sum(graves_rate * treatment) == 0) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::mutate(n_never_graves = sum(never),
                  n_never_graves_pre = sum(never_pre),
                  n_never_graves_post = sum(never_post)) %>%
    dplyr::select(n_never_graves, n_never_graves_pre, n_never_graves_post) %>%
    dplyr::distinct()

  cat("N. players is ", length(unique(lol_player_dta$id)), " of which:
  ", never_graves$n_never_graves, " has never played Graves
  ", never_graves$n_never_graves_pre, " has never played Graves before the treatment
  ", never_graves$n_never_graves_post, " has never played Graves after the treatment \n", sep = "")

  plot_never <- data.frame("treatment" = c("Pre-treatment", "Post-treatment"), "n_never_graves" = c(never_graves$n_never_graves_pre, never_graves$n_never_graves_post)) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(treatment, levels = c("Pre-treatment", "Post-treatment")), y = n_never_graves, fill = factor(treatment, levels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(stat = "identity") +
    scale_fill_manual(values = c("dodgerblue", "tomato")) +
    ggplot2::xlab("") + ggplot2::ylab("N. players never picking Graves") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none")
  ggplot2::ggsave(paste0(save_here, "/", "plot_never_graves.svg"), plot_never, device = Cairo::CairoSVG, width = 7, height = 7)

  ## 2.) Compute Graves' pre- and post-treatment pick rates, and use quantiles of pre rates to group units.
  pick_rates <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_graves_rate_pre = sum(graves_rate * (1 - treatment)) / sum(1 - treatment),
                  avg_graves_rate_post = sum(graves_rate * treatment) / sum(treatment)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::mutate(quantile_pre = dplyr::ntile(avg_graves_rate_pre, n_groups)) %>%
    dplyr::select(id, avg_graves_rate_pre, avg_graves_rate_post, quantile_pre)

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(pick_rates, by = c("id")) %>%
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, quantile_pre, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  ## 3.) Distribution of pre-treatment Graves' pick rates.
  plot_avg_rates_distribution <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(avg_graves_rate_pre, avg_graves_rate_post) %>%
    reshape2::melt() %>%
    ggplot2::ggplot(ggplot2::aes(x = value, fill = factor(variable, levels = c("avg_graves_rate_pre", "avg_graves_rate_post"), labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_density(alpha = 0.4, linewidth = 1) +
    scale_fill_manual(values = c("dodgerblue", "tomato")) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = c(.95, .95), legend.justification = c("right", "top"),
          legend.box.just = "right", legend.title = ggplot2::element_blank())
  ggplot2::ggsave(paste0(save_here, "/", "plot_graves_prerates_distribution.svg"), plot_avg_rates_distribution, device = "svg", width = 7, height = 7)

  ## 3.) Average players' pick rates for Graves in each bucket.
  plot_avg_rates_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = mean(avg_graves_rate_pre),
           post_treatment = mean(avg_graves_rate_post)) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "mean")

  plot_avg_rates_se_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = sd(avg_graves_rate_pre) / sqrt(n()),
           post_treatment = sd(avg_graves_rate_post) / sqrt(n())) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "se")

  plot_avg_rates_buckets <- plot_avg_rates_buckets_dta %>%
    dplyr::left_join(plot_avg_rates_se_buckets_dta, by = c("quantile_pre", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(quantile_pre), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average Graves' pick rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = c(0.05, 0.95), legend.title = ggplot2::element_blank(),
          legend.direction = "vertical", legend.justification = c("left", "top"))

  ## 5.) Average players' performance in each bucket.
  # 5a.) Average number of daily matches.
  n_matches <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_n_matches_pre = sum(n_matches * (1 - treatment)) / sum(1 - treatment),
                  avg_n_matches_post = sum(n_matches * treatment) / sum(treatment)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(id, avg_n_matches_pre, avg_n_matches_post)

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(n_matches, by = c("id")) %>%
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, avg_n_matches_pre, avg_n_matches_post, quantile_pre, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  plot_avg_n_matches_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = mean(avg_n_matches_pre),
                  post_treatment = mean(avg_n_matches_post)) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "mean")

  plot_avg_n_matches_se_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = sd(avg_n_matches_pre) / sqrt(n()),
                  post_treatment = sd(avg_n_matches_post) / sqrt(n())) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "se")

  plot_avg_n_matches_buckets <- plot_avg_n_matches_buckets_dta %>% ## ISSUE: AFTER TREATMENT WE HAVE A SHORTER TIME SPAN. ALSO, SEASONALITY MAYBE HINDER INTERPRETATION.
    dplyr::left_join(plot_avg_n_matches_se_buckets_dta, by = c("quantile_pre", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(quantile_pre), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average number of daily matches") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", legend.title = ggplot2::element_blank(),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  # 5b.) Average win rate.
  win_rates <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_win_rate_pre = sum(win_rate * (1 - treatment)) / sum(1 - treatment),
                  avg_win_rate_post = sum(win_rate * treatment) / sum(treatment)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(id, avg_win_rate_pre, avg_win_rate_post)

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(win_rates, by = c("id")) %>%
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, avg_n_matches_pre, avg_n_matches_post, avg_win_rate_pre, avg_win_rate_post, quantile_pre, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  plot_avg_win_rate_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = mean(avg_win_rate_pre),
           post_treatment = mean(avg_win_rate_post)) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "mean")

  plot_avg_win_rate_se_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = sd(avg_win_rate_pre) / sqrt(n()),
           post_treatment = sd(avg_win_rate_post) / sqrt(n())) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "se")

  plot_win_rate_buckets <- plot_avg_win_rate_buckets_dta %>%
    dplyr::left_join(plot_avg_win_rate_se_buckets_dta, by = c("quantile_pre", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(quantile_pre), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average win rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", legend.title = ggplot2::element_blank(),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  # 5c.) Average kills/deaths ratio.
  kd_ratios <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_kills_pre = sum(kills_avg * (1 - treatment)) / sum(1 - treatment),
                  avg_kills_post = sum(kills_avg * treatment) / sum(treatment),
                  avg_deaths_pre = sum(deaths_avg * (1 - treatment)) / sum(1 - treatment),
                  avg_deaths_post = sum(deaths_avg * treatment) / sum(treatment)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::mutate(avg_kd_ratio_pre = avg_kills_pre / avg_deaths_pre,
                  avg_kd_ratio_post = avg_kills_post / avg_deaths_post) %>%
    dplyr::select(id, avg_kd_ratio_pre, avg_kd_ratio_post)

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(kd_ratios, by = c("id")) %>%
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, avg_n_matches_pre, avg_n_matches_post, avg_win_rate_pre, avg_win_rate_post, avg_kd_ratio_pre, avg_kd_ratio_post, quantile_pre, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  plot_avg_kd_ratio_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = mean(avg_kd_ratio_pre),
                  post_treatment = mean(avg_kd_ratio_post)) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "mean")

  plot_avg_kd_ratio_se_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = sd(avg_kd_ratio_pre) / sqrt(n()),
                  post_treatment = sd(avg_kd_ratio_post) / sqrt(n())) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "se")

  plot_kd_ratio_buckets <- plot_avg_kd_ratio_buckets_dta %>% ## ISSUE: DIFFERENT ROLES GENERALLY HAVE DIFFERENT KD RATIOS.
    dplyr::left_join(plot_avg_kd_ratio_se_buckets_dta, by = c("quantile_pre", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(quantile_pre), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average kills/deaths") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", legend.title = ggplot2::element_blank(),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  # 5d.) Average gold earned.
  gold <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_gold_pre = sum(gold_avg * (1 - treatment)) / sum(1 - treatment),
                  avg_gold_post = sum(gold_avg * (treatment)) / sum(treatment)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(id, avg_gold_pre, avg_gold_post)

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(gold, by = c("id")) %>%
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, avg_n_matches_pre, avg_n_matches_post, avg_win_rate_pre, avg_win_rate_post, avg_kd_ratio_pre, avg_kd_ratio_post, avg_gold_pre, avg_gold_post, quantile_pre, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  plot_avg_gold_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = mean(avg_gold_pre),
                  post_treatment = mean(avg_gold_post)) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "mean")

  plot_avg_gold_se_buckets_dta <- lol_player_dta %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(quantile_pre) %>%
    dplyr::arrange(quantile_pre) %>%
    dplyr::mutate(pre_treatment = sd(avg_gold_pre) / sqrt(n()),
                  post_treatment = sd(avg_gold_post) / sqrt(n())) %>%
    dplyr::select(quantile_pre, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("quantile_pre"), variable.name = "time", value.name = "se")

  plot_gold_buckets <- plot_avg_gold_buckets_dta %>% ## ISSUE: DIFFERENT ROLES GENERALLY EARN DIFFERENTLY.
    dplyr::left_join(plot_avg_gold_se_buckets_dta, by = c("quantile_pre", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(quantile_pre), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average gold") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", legend.title = ggplot2::element_blank(),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  ## 5e.) Export grid.
  ggplot2::ggsave(paste0(save_here, "/", "players_performance_by_group.svg"), gridExtra::grid.arrange(plot_avg_rates_buckets, plot_avg_n_matches_buckets, plot_kd_ratio_buckets, plot_win_rate_buckets), device = "svg", width = 7, height = 7)

  ## 6.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' LoL Mechanisms Plots
#'
#' Divide players into treated and control groups according to whether they never picked Graves pre-treatment and produces plots showing the average performance of players
#' in each group before and after the treatment.
#'
#' @param n_pre_matches How many matches before \code{treatment_date} players must have played to be kept in the data set.
#' @param n_post_matches How many matches before \code{treatment_date} players must have played to be kept in the data set.
#' @param treatment_date Object of class \code{POSIXct}. Where to display a dashed vertical line. Set to \code{NULL} if you do not want this line.
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces nice plots.
#'
#' @details
#' \code{treatment_date}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' Players that have played less than \code{n_pre_matches} before \code{treatment_date} or less than \code{n_post_matches} between \code{treatment_date} and \code{max_date} are dropped. The number of
#' players remaining in the data set is printed.
#'
#' @import dplyr reshape2 ggplot2 patchwork
#'
#' @author Riccardo Di Francesco
#'
#' @export
mechanisms_plots_lol2 <- function(n_pre_matches, n_post_matches,
                                 treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"), min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-08-01"),
                                 save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(min_date < day & day < max_date) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(disclosure = ifelse(day > treatment_date, 1, 0),
                  n_matches_pre = sum(n_matches * (1 - disclosure)),
                  n_matches_post = sum(n_matches * disclosure)) %>%
    dplyr::filter(n_matches_pre >= n_pre_matches & n_matches_post >= n_post_matches) %>%
    dplyr::select(day, disclosure, id, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg) %>%
    dplyr::ungroup()

  ## 1.) Define prior users.
  treated_controls <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(prior_user = sum(graves_rate * (1 - disclosure)) != 0) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(prior_user, id)

  cat("N. players is ", length(unique(lol_player_dta$id)), " of which:
  ", treated_controls %>% distinct(id, .keep_all = TRUE) %>% pull(prior_user) %>% sum(), " was playing Graves before the disclosure (treatment group)
  ", length(unique(lol_player_dta$id))- treated_controls %>% distinct(id, .keep_all = TRUE) %>% pull(prior_user) %>% sum(), " has never played Graves before the disclosure (control group) \n", sep = "")

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(treated_controls, by = "id") %>%
    dplyr::select(day, id, disclosure, prior_user, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  ## 2.) Average players' pick rates for Graves in each group.
  plot_avg_rates_buckets_dta <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_graves_rate_pre = sum(graves_rate * (1 - disclosure)) / sum(1 - disclosure),
                  avg_graves_rate_post = sum(graves_rate * disclosure) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = mean(avg_graves_rate_pre),
                  post_treatment = mean(avg_graves_rate_post)) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "mean")

  plot_avg_rates_se_buckets_dta <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_graves_rate_pre = sum(graves_rate * (1 - disclosure)) / sum(1 - disclosure),
                  avg_graves_rate_post = sum(graves_rate * disclosure) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = sd(avg_graves_rate_pre) / sqrt(n()),
                  post_treatment = sd(avg_graves_rate_post) / sqrt(n())) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "se")

  plot_avg_rates_buckets <- plot_avg_rates_buckets_dta %>%
    dplyr::left_join(plot_avg_rates_se_buckets_dta, by = c("prior_user", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(prior_user, levels = c(FALSE, TRUE), labels = c("Non-prior users", "Prior users")), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average Graves' pick rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = c(0.05, 0.95), legend.title = ggplot2::element_blank(),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  ## 3.) Average number of matches in each group.
  plot_avg_n_matches_buckets_dta <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_n_matches_pre = sum(n_matches * (1 - disclosure)) / sum(1 - disclosure),
                  avg_n_matches_post = sum(n_matches * disclosure) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = mean(avg_n_matches_pre),
                  post_treatment = mean(avg_n_matches_post)) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "mean")

  plot_avg_n_matches_se_buckets_dta <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_n_matches_pre = sum(n_matches * (1 - disclosure)) / sum(1 - disclosure),
                  avg_n_matches_post = sum(n_matches * disclosure) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = sd(avg_n_matches_pre) / sqrt(n()),
                  post_treatment = sd(avg_n_matches_post) / sqrt(n())) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "se")

  plot_avg_n_matches_buckets <- plot_avg_n_matches_buckets_dta %>% ## ISSUE: AFTER TREATMENT WE HAVE A SHORTER TIME SPAN. ALSO, SEASONALITY MAYBE HINDER INTERPRETATION.
    dplyr::left_join(plot_avg_n_matches_se_buckets_dta, by = c("prior_user", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(prior_user, levels = c(FALSE, TRUE), labels = c("Non-prior users", "Prior users")), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average number of daily matches") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", legend.title = ggplot2::element_blank(),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  ## 4.) Average players' win rate in each group.
  plot_avg_win_rate_buckets_dta <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_win_rate_pre = sum(win_rate * (1 - disclosure)) / sum(1 - disclosure),
                  avg_win_rate_post = sum(win_rate * disclosure) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = mean(avg_win_rate_pre),
                  post_treatment = mean(avg_win_rate_post)) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "mean")

  plot_avg_win_rate_se_buckets_dta <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_win_rate_pre = sum(win_rate * (1 - disclosure)) / sum(1 - disclosure),
                  avg_win_rate_post = sum(win_rate * disclosure) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = sd(avg_win_rate_pre) / sqrt(n()),
                  post_treatment = sd(avg_win_rate_post) / sqrt(n())) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "se")

  plot_win_rate_buckets <- plot_avg_win_rate_buckets_dta %>%
    dplyr::left_join(plot_avg_win_rate_se_buckets_dta, by = c("prior_user", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(prior_user, levels = c(FALSE, TRUE), labels = c("Non-prior users", "Prior users")), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average win rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", legend.title = ggplot2::element_blank(),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  ## 5.) Average players' kills/deaths ratio.
  kd_ratio_dta <- lol_player_dta %>%
    dplyr::mutate(kd_ratio = kills_avg / deaths_avg) %>%
    replace(is.na(.), 0)
  kd_ratio_dta$kd_ratio[is.infinite(kd_ratio_dta$kd_ratio )] <- 0

  plot_avg_kd_ratio_buckets_dta <- kd_ratio_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_kd_ratio_pre = sum(kd_ratio * (1 - disclosure)) / sum(1 - disclosure),
                  avg_kd_ratio_post = sum(kd_ratio * disclosure) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = mean(avg_kd_ratio_pre),
                  post_treatment = mean(avg_kd_ratio_post)) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "mean")

  plot_avg_kd_ratio_se_buckets_dta <- kd_ratio_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_kd_ratio_pre = sum(kd_ratio * (1 - disclosure)) / sum(1 - disclosure),
                  avg_kd_ratio_post = sum(kd_ratio * disclosure) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = sd(avg_kd_ratio_pre) / sqrt(n()),
                  post_treatment = sd(avg_kd_ratio_post) / sqrt(n())) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "se")

  plot_kd_ratio_buckets <- plot_avg_kd_ratio_buckets_dta %>% ## ISSUE: DIFFERENT ROLES GENERALLY HAVE DIFFERENT KD RATIOS.
    dplyr::left_join(plot_avg_kd_ratio_se_buckets_dta, by = c("prior_user", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(prior_user, levels = c(FALSE, TRUE), labels = c("Non-prior users", "Prior users")), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average kills/deaths") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", legend.title = ggplot2::element_blank(),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  ## 6.) Average players' gold earned in each group.
  plot_avg_gold_buckets_dta <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_gold_pre = sum(gold_avg * (1 - disclosure)) / sum(1 - disclosure),
                  avg_gold_post = sum(gold_avg * (disclosure)) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = mean(avg_gold_pre),
                  post_treatment = mean(avg_gold_post)) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "mean")

  plot_avg_gold_se_buckets_dta <- kd_ratio_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_gold_pre = sum(gold_avg * (1 - disclosure)) / sum(1 - disclosure),
                  avg_gold_post = sum(gold_avg * (disclosure)) / sum(disclosure)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(pre_treatment = sd(avg_gold_pre) / sqrt(n()),
                  post_treatment = sd(avg_gold_post) / sqrt(n())) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "se")

  plot_gold_buckets <- plot_avg_gold_buckets_dta %>% ## ISSUE: DIFFERENT ROLES GENERALLY EARN DIFFERENTLY.
    dplyr::left_join(plot_avg_gold_se_buckets_dta, by = c("prior_user", "time")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(prior_user, levels = c(FALSE, TRUE), labels = c("Non-prior users", "Prior users")), y = mean, fill = factor(time, labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::xlab("") + ggplot2::ylab("Average gold") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", legend.title = ggplot2::element_blank(),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  ## 7.) Export grid.
  ggplot2::ggsave(paste0(save_here, "/", "players_performance_by_group.svg"), plot_avg_rates_buckets / (plot_avg_n_matches_buckets + plot_win_rate_buckets), device = "svg", width = 7, height = 7)

  ## 8.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' LoL Impact on Players Performance
#'
#' Uses a diff-in-diff strategy to investigate the impact of the coming-out event on players' performance.
#'
#' @param n_pre_matches How many matches before \code{treatment_date} players must have played to be kept in the data set.
#' @param n_post_matches How many matches before \code{treatment_date} players must have played to be kept in the data set.
#' @param treatment_date Object of class \code{POSIXct}. The date of the treatment.
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#'
#' @return
#' Prints code for a LATEX table.
#'
#' @details
#' We define two versions of the treatment.
#' \describe{
#'    \item{\code{ReduceGraves}}{Players that reduce their average pick rate for Graves following his disclosure are considered treated.}
#'    \item{\code{DropGraves}}{Players that had a non-zero average pick rate for Graves before his disclosure and transition to a zero pick rate after are considered treated.}
#' }
#'
#' Two-way fixed effect regressions are employed to estimate the impact of the coming-out event on the performance of treated players. These regressions are consistent for the average treatment
#' effect on the treated under the assumptions of no anticipation and parallel trends. Standard errors are obtained by clustering.\cr
#'
#' We also use the estimator of Callaway and Sant’Anna (2021) as a robustness check. This is implemented using the \code{\link[did]{att_gt}} function. Technical details are given in the associated documentation.
#' To summarize ideas, this estimates the average treatment effect on the treated for all time t > \code{treatment_date} (effects before that date are also estimated and are useful to check the plausibility
#' of the parallel trend assumption). We fit the estimator both unconditionally and conditional on pre-treatment covariates (average kills, assists, deaths, gold earned, and matches played each day).\cr
#'
#' \code{treatment_date}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' Players that have played less than \code{n_pre_matches} before \code{treatment_date} or less than \code{n_post_matches} between \code{treatment_date} and \code{max_date} are dropped. The number of
#' players remaining in the data set is printed.
#'
#' @import dplyr fixest did
#' @importFrom lubridate month
#'
#' @author Riccardo Di Francesco
#'
#' @export
did_players_performance <- function(n_pre_matches, n_post_matches,
                                    treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"), min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-08-01")) {
  ## 0.) Handling inputs and checks.
  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(min_date < day & day < max_date) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(disclosure = ifelse(day > treatment_date, 1, 0),
                  n_matches_pre = sum(n_matches * (1 - disclosure)),
                  n_matches_post = sum(n_matches * disclosure)) %>%
    dplyr::filter(n_matches_pre >= n_pre_matches & n_matches_post >= n_post_matches) %>%
    dplyr::select(day, disclosure, id, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg) %>%
    dplyr::ungroup()

  ## 1.) Assign treatment status.
  treated_controls <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_graves_rate_pre = sum(graves_rate * (1 - disclosure)) / sum(1 - disclosure),
                  avg_graves_rate_post = sum(graves_rate * disclosure) / sum(disclosure),
                  reduce_graves = as.numeric(avg_graves_rate_post < avg_graves_rate_pre),
                  drop_graves = as.numeric(avg_graves_rate_pre > 0 & avg_graves_rate_post == 0)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(id, reduce_graves, drop_graves)

  cat("N. players is ", length(unique(lol_player_dta$id)), " of which:
  ", treated_controls %>% distinct(id, .keep_all = TRUE) %>% pull(reduce_graves) %>% sum(), " reduces their pick rates for Graves (treatment group 1)
  ", treated_controls %>% distinct(id, .keep_all = TRUE) %>% pull(drop_graves) %>% sum(), " completely stopped playing Graves (treatment group 2) \n\n", sep = "")

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(treated_controls, by = "id") %>%
    dplyr::select(day, id, disclosure, reduce_graves, drop_graves, graves_rate, graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  ## 2.) Arrange estimation data.
  estimation_dta <- lol_player_dta %>%
    dplyr::mutate(reduce_graves_int = disclosure * reduce_graves,
                  drop_graves_int = disclosure * drop_graves) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(mean_n_matches_pre = sum(n_matches * (1 - disclosure) / sum(1 - disclosure)),
                  mean_gold_pre = sum(gold_avg * (1 - disclosure)) / sum(1 - disclosure),
                  mean_kills_pre = sum(kills_avg * (1 - disclosure)) / sum(1 - disclosure),
                  mean_assists_pre = sum(assists_avg * (1 - disclosure)) / sum(1 - disclosure),
                  mean_deaths_pre = sum(deaths_avg * (1 - disclosure)) / sum(1 - disclosure)) %>%
    dplyr::select(day, id, win_rate, disclosure, reduce_graves, reduce_graves_int, drop_graves, drop_graves_int, mean_n_matches_pre, mean_gold_pre, mean_kills_pre, mean_assists_pre, mean_deaths_pre) %>%
    dplyr::mutate(n_matches_pre = mean_n_matches_pre * disclosure,
                  gold_pre = mean_gold_pre * disclosure,
                  kills_pre = mean_kills_pre * disclosure,
                  assists_pre = mean_assists_pre * disclosure,
                  deaths_pre = mean_deaths_pre * disclosure) %>%
    dplyr::ungroup()

  ## 3.) Two-way fixed effects regressions.
  twfe_results <- fixest::feols(win_rate ~ 0 + sw(reduce_graves_int, drop_graves_int) | day + id, data = estimation_dta, panel.id = c("id", "day"), vcov = "twoway")

  ## 4.) Doubly-robust DiD. First, we need to transform variables to numeric and assign date of first treatment (treatment_date for those that are treated).
  estimation_dta <- estimation_dta %>%
    dplyr::mutate(day_no = as.numeric(day),
                  id_no = as.numeric(factor(id)),
                  gname_reduce = ifelse(reduce_graves == 1, as.numeric(treatment_date), 0),
                  gname_drop = ifelse(drop_graves == 1, as.numeric(treatment_date), 0))

  dr_results_reduce <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "gname_reduce",
                                              xformla = ~ 1,
                                              data = estimation_dta, panel = FALSE, allow_unbalanced_panel = TRUE)

  dr_results_reduce_covariates <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "gname_reduce",
                                              xformla = ~ mean_n_matches_pre + mean_gold_pre + mean_kills_pre + mean_assists_pre + mean_deaths_pre,
                                              data = estimation_dta, panel = FALSE, allow_unbalanced_panel = TRUE)

  dr_results_drop <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "gname_drop",
                                            xformla = ~ 1,
                                            data = estimation_dta, panel = FALSE, allow_unbalanced_panel = TRUE)

  dr_results_drop_covariates <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "gname_drop",
                                   xformla = ~ mean_n_matches_pre + mean_gold_pre + mean_kills_pre + mean_assists_pre + mean_deaths_pre,
                                   data = estimation_dta, panel = FALSE, allow_unbalanced_panel = TRUE)

  ## 5.) Output.
  return(list("twfe" = twfe_results,
              "dr_reduce_graves" = dr_results_reduce,
              "dr_reduce_graves_covariates" = dr_results_reduce_covariates,
              "dr_drop_graves" = dr_results_drop,
              "dr_drop_graves_covariates" = dr_results_drop_covariates))
}


#' Diff-in-Diff Plots
#'
#' Plots the time ATTs estimated by \code{\link{did_players_performance}}.
#'
#' @param att_gt_results One of the results of \code{\link{did_players_performance}}.
#' @param title String, title of the plot.
#' @param subtitle String, subtitle of the plot.
#' @param treatment_date Object of class \code{POSIXct}. The date of the treatment. Must be the same used when calling \code{\link{did_players_performance}} for the result to make sense.
#'
#' @return
#' Returns a nice plot.
#'
#' @import ggplot2
#'
#' @author Riccardo Di Francesco
#'
#' @export
plot_did <- function(att_gt_results, title, subtitle, treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")) {
  ## 0.) Handling inputs and checks.
  if (!inherits(att_gt_results, "MP")) stop("Invalid 'att_gt_results'. This must be an object of class 'MP'.", call. = FALSE)

  n_times <- length(unique(att_gt_results$t))
  times <- unique(att_gt_results$t)

  results <- data.frame(year = as.POSIXct(times, origin = "1970-01-01"))
  results$att <- att_gt_results$att
  results$att.se <- att_gt_results$se
  results$post <- as.factor(1*(results$year >= treatment_date))
  results$c <- att_gt_results$c

  alp <- att_gt_results$alp
  c.point <- qnorm(1 - alp / 2)

  plot <- ggplot2::ggplot(results, ggplot2::aes(x = year, y = att, ymin = (att - c * att.se), ymax = (att + c * att.se))) +
    ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
    ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
    ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    ggplot2::scale_color_manual(drop = FALSE, values = c("#e87d72", "#56bcc2"), breaks = c(0, 1), labels = c("Pre", "Post")) +
    ggplot2::xlab("") + ggplot2::ylab("ATT") + ggplot2::ggtitle(label = title, subtitle = subtitle) +
    ggplot2::theme_bw() +
    theme(plot.title = ggplot2::element_text(color = "black", face = "bold", size = 12), plot.subtitle = ggplot2::element_text(color = "black", face = "italic", size = 9),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 15),
          legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))

  return(plot)
}
