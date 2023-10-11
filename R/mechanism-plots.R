#' LoL Descriptive Plots
#'
#' Produces plots
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
#' players remaining in the data set is printed.\cr
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
    dplyr::select(day, treatment, id, Graves_rate, Graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg) %>%
    dplyr::ungroup()

  never_graves <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(never = sum(Graves_rate) == 0,
                  never_pre = sum(Graves_rate * (1 - treatment)) == 0,
                  never_post = sum(Graves_rate * treatment) == 0) %>%
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
    dplyr::mutate(avg_graves_rate_pre = sum(Graves_rate * (1 - treatment)) / sum(1 - treatment),
                  avg_graves_rate_post = sum(Graves_rate * treatment) / sum(treatment)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::mutate(quantile_pre = dplyr::ntile(avg_graves_rate_pre, n_groups)) %>%
    dplyr::select(id, avg_graves_rate_pre, avg_graves_rate_post, quantile_pre)

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(pick_rates, by = c("id")) %>%
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, quantile_pre, Graves_rate, Graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

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
  ggplot2::ggsave(paste0(save_here, "/", "graves_pick_rates_by_group.svg"), plot_avg_rates_buckets, device = "svg", width = 7, height = 7)

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
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, avg_n_matches_pre, avg_n_matches_post, quantile_pre, Graves_rate, Graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

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
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, avg_n_matches_pre, avg_n_matches_post, avg_win_rate_pre, avg_win_rate_post, quantile_pre, Graves_rate, Graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

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
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, avg_n_matches_pre, avg_n_matches_post, avg_win_rate_pre, avg_win_rate_post, avg_kd_ratio_pre, avg_kd_ratio_post, quantile_pre, Graves_rate, Graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

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
    dplyr::select(day, id, treatment, avg_graves_rate_pre, avg_graves_rate_post, avg_n_matches_pre, avg_n_matches_post, avg_win_rate_pre, avg_win_rate_post, avg_kd_ratio_pre, avg_kd_ratio_post, avg_gold_pre, avg_gold_post, quantile_pre, Graves_rate, Graves_ban_rate, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

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
  ggplot2::ggsave(paste0(save_here, "/", "players_performance_by_group.svg"), gridExtra::grid.arrange(plot_avg_n_matches_buckets, plot_win_rate_buckets, plot_kd_ratio_buckets, plot_gold_buckets), device = "svg", width = 7, height = 7)

  ## 6.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}
