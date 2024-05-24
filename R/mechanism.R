#' LoL Players' Performance Plots
#'
#' Divides players into two groups according to whether they never picked Graves pre-treatment (prior and non-prior users) and produces plots showing the average performance of players
#' in each group before and after the treatment. It also produces a plot to investigate which positions are the most played in each group before and after the treatment. The same
#' plot is then repeated only for those prior users that are "treated" as discussed in the documentation of the \code{\link{did_players_performance}}.
#'
#' @param n_pre_matches How many matches before \code{treatment_date} players must have played to be kept in the data set.
#' @param treatment_date Object of class \code{POSIXct}. When the treatment took place.
#' @param min_date Object of class \code{POSIXct}. When to start the series.
#' @param max_date Object of class \code{POSIXct}. When to end the series.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Produces nice plots.
#'
#' @details
#' \code{treatment_date}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' Players that have played less than \code{n_pre_matches} before \code{treatment_date} or that never played after are dropped. The number of players remaining in the data set is printed in the console.
#'
#' @import dplyr reshape2 ggplot2 patchwork
#' @importFrom stats sd
#'
#' @author Riccardo Di Francesco
#'
#' @export
players_performance_plots_lol <- function(n_pre_matches,
                                 treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"), min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-08-01"),
                                 save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  n_matches <- NULL
  disclosure <- NULL
  n_matches_pre <- NULL
  n_matches_post <- NULL
  graves_rate <- NULL
  graves_ban_rate <- NULL
  win_rate <- NULL
  gold_avg <- NULL
  kills_avg <- NULL
  assists_avg <- NULL
  deaths_avg <- NULL
  prior_user <- NULL
  avg_graves_rate_pre <- NULL
  avg_graves_rate_post <- NULL
  pre_treatment <- NULL
  avg_n_matches_pre <- NULL
  avg_n_matches_post <- NULL
  avg_win_rate_pre <- NULL
  avg_win_rate_post <- NULL
  . <- NULL
  kd_ratio <- NULL
  kd_ratio <- NULL
  kd_ratio <- NULL
  post_treatment <- NULL

  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(min_date < day & day < max_date) %>%
    dplyr::mutate(disclosure = ifelse(day > treatment_date, 1, 0))

  keep_these_players <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(n_matches_pre = sum(n_matches * (1 - disclosure)),
                  n_matches_post = sum(n_matches * disclosure)) %>%
    dplyr::filter(n_matches_pre >= n_pre_matches & n_matches_post > 0) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(id, n_matches_pre, n_matches_post) %>%
    dplyr::ungroup()

  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(id %in% keep_these_players$id) %>%
    dplyr::left_join(keep_these_players, by = "id") %>%
    dplyr::mutate(disclosure = ifelse(day > treatment_date, 1, 0)) %>%
    dplyr::select(day, disclosure, id, graves_rate, graves_ban_rate, top, jungle, mid, bottom, support, lgb, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  ## 1.) Define prior users.
  treated_controls <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_graves_rate_pre = sum(graves_rate * (1 - disclosure)) / sum(1 - disclosure),
                  prior_user = avg_graves_rate_pre >= 5) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(prior_user, id)

  cat("N. players is ", length(unique(lol_player_dta$id)), " of which:
  ", treated_controls %>% distinct(id, .keep_all = TRUE) %>% pull(prior_user) %>% sum(), " was playing Graves at least 5% before the disclosure (prior users)
  ", length(unique(lol_player_dta$id))- treated_controls %>% distinct(id, .keep_all = TRUE) %>% pull(prior_user) %>% sum(), " has played Graves less than 5% before the disclosure (non-prior users) \n", sep = "")

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(treated_controls, by = "id") %>%
    dplyr::select(day, id, disclosure, prior_user, graves_rate, graves_ban_rate, top, jungle, mid, bottom, support, lgb, n_matches, n_matches_pre, n_matches_post, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

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
    dplyr::mutate(pre_treatment = stats::sd(avg_graves_rate_pre) / sqrt(n()),
                  post_treatment = stats::sd(avg_graves_rate_post) / sqrt(n())) %>%
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
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = c(0.05, 0.97), legend.title = ggplot2::element_blank(),
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
    dplyr::mutate(pre_treatment = stats::sd(avg_n_matches_pre) / sqrt(n()),
                  post_treatment = stats::sd(avg_n_matches_post) / sqrt(n())) %>%
    dplyr::select(prior_user, pre_treatment, post_treatment) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user"), variable.name = "time", value.name = "se")

  plot_avg_n_matches_buckets <- plot_avg_n_matches_buckets_dta %>%
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
    dplyr::mutate(pre_treatment = stats::sd(avg_win_rate_pre) / sqrt(n()),
                  post_treatment = stats::sd(avg_win_rate_post) / sqrt(n())) %>%
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

  ## 5.) Count positions chosen by each group.
  plot_positions_buckets_dta_pre <- lol_player_dta %>%
    dplyr::filter(disclosure == 0) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(top_rate = top / n_matches * 100,
                  jungle_rate = jungle / n_matches * 100,
                  mid_rate = mid / n_matches * 100,
                  bottom_rate = bottom / n_matches * 100,
                  support_rate = support / n_matches * 100,
                  lgb_rate = lgb / n_matches * 100,
                  avg_top_rate_pre = mean(top_rate),
                  avg_jungle_rate_pre = mean(jungle_rate),
                  avg_mid_rate_pre = mean(mid_rate),
                  avg_bottom_rate_pre = mean(bottom_rate),
                  avg_support_rate_pre = mean(support_rate),
                  avg_lgb_rate_pre = mean(lgb_rate)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(top_lane = mean(avg_top_rate_pre),
                  jungle_lane = mean(avg_jungle_rate_pre),
                  mid_lane = mean(avg_mid_rate_pre),
                  bottom_lane = mean(avg_bottom_rate_pre),
                  support_lane = mean(avg_support_rate_pre),
                  lgb_lane = mean(avg_lgb_rate_pre)) %>%
    dplyr::select(prior_user, disclosure, top_lane, jungle_lane, mid_lane, bottom_lane, support_lane) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user", "disclosure"), variable.name = "position", value.name = "mean")

  plot_positions_buckets_dta_post <- lol_player_dta %>%
    dplyr::filter(disclosure == 1) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(top_rate = top / n_matches * 100,
                  jungle_rate = jungle / n_matches * 100,
                  mid_rate = mid / n_matches * 100,
                  bottom_rate = bottom / n_matches * 100,
                  support_rate = support / n_matches * 100,
                  lgb_rate = lgb / n_matches * 100,
                  avg_top_rate_post = mean(top_rate),
                  avg_jungle_rate_post = mean(jungle_rate),
                  avg_mid_rate_post = mean(mid_rate),
                  avg_bottom_rate_post = mean(bottom_rate),
                  avg_support_rate_post = mean(support_rate),
                  avg_lgb_rate_post = mean(lgb_rate)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(top_lane = mean(avg_top_rate_post),
                  jungle_lane = mean(avg_jungle_rate_post),
                  mid_lane = mean(avg_mid_rate_post),
                  bottom_lane = mean(avg_bottom_rate_post),
                  support_lane = mean(avg_support_rate_post),
                  lgb_lane = mean(avg_lgb_rate_post)) %>%
    dplyr::select(prior_user, disclosure, top_lane, jungle_lane, mid_lane, bottom_lane, support_lane) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user", "disclosure"), variable.name = "position", value.name = "mean")

  plot_positions_buckets_dta <- plot_positions_buckets_dta_pre %>%
    dplyr::bind_rows(plot_positions_buckets_dta_post)

  plot_positions_se_buckets_dta_pre <- lol_player_dta %>%
    dplyr::filter(disclosure == 0) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(top_rate = top / n_matches * 100,
                  jungle_rate = jungle / n_matches * 100,
                  mid_rate = mid / n_matches * 100,
                  bottom_rate = bottom / n_matches * 100,
                  support_rate = support / n_matches * 100,
                  lgb_rate = lgb / n_matches * 100,
                  avg_top_rate_pre = mean(top_rate),
                  avg_jungle_rate_pre = mean(jungle_rate),
                  avg_mid_rate_pre = mean(mid_rate),
                  avg_bottom_rate_pre = mean(bottom_rate),
                  avg_support_rate_pre = mean(support_rate),
                  avg_lgb_rate_pre = mean(lgb_rate)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(top_lane = stats::sd(avg_top_rate_pre) / sqrt(n()),
                  jungle_lane = stats::sd(avg_jungle_rate_pre) / sqrt(n()),
                  mid_lane = stats::sd(avg_mid_rate_pre) / sqrt(n()),
                  bottom_lane = stats::sd(avg_bottom_rate_pre) / sqrt(n()),
                  support_lane = stats::sd(avg_support_rate_pre) / sqrt(n()),
                  lgb_lane = stats::sd(avg_lgb_rate_pre) / sqrt(n())) %>%
    dplyr::select(prior_user, disclosure, top_lane, jungle_lane, mid_lane, bottom_lane, support_lane) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user", "disclosure"), variable.name = "position", value.name = "se")

  plot_positions_se_buckets_dta_post <- lol_player_dta %>%
    dplyr::filter(disclosure == 1) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(top_rate = top / n_matches * 100,
                  jungle_rate = jungle / n_matches * 100,
                  mid_rate = mid / n_matches * 100,
                  bottom_rate = bottom / n_matches * 100,
                  support_rate = support / n_matches * 100,
                  lgb_rate = lgb / n_matches * 100,
                  avg_top_rate_post = mean(top_rate),
                  avg_jungle_rate_post = mean(jungle_rate),
                  avg_mid_rate_post = mean(mid_rate),
                  avg_bottom_rate_post = mean(bottom_rate),
                  avg_support_rate_post = mean(support_rate),
                  avg_lgb_rate_post = mean(lgb_rate)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::group_by(prior_user) %>%
    dplyr::mutate(top_lane = stats::sd(avg_top_rate_post) / sqrt(n()),
                  jungle_lane = stats::sd(avg_jungle_rate_post) / sqrt(n()),
                  mid_lane = stats::sd(avg_mid_rate_post) / sqrt(n()),
                  bottom_lane = stats::sd(avg_bottom_rate_post) / sqrt(n()),
                  support_lane = stats::sd(avg_support_rate_post) / sqrt(n()),
                  lgb_lane = stats::sd(avg_lgb_rate_post) / sqrt(n())) %>%
    dplyr::select(prior_user, disclosure, top_lane, jungle_lane, mid_lane, bottom_lane, support_lane) %>%
    dplyr::distinct() %>%
    reshape2::melt(id.vars = c("prior_user", "disclosure"), variable.name = "position", value.name = "se")

  plot_positions_se_buckets_dta <- plot_positions_se_buckets_dta_pre %>%
    dplyr::bind_rows(plot_positions_se_buckets_dta_post)

  plot_positions_buckets <- plot_positions_buckets_dta %>%
    dplyr::left_join(plot_positions_se_buckets_dta, by = c("prior_user", "disclosure", "position")) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(position, levels = c("top_lane", "jungle_lane", "mid_lane", "bottom_lane", "support_lane"), labels = c("Top", "Jungle", "Mid", "Bottom", "Support")), y = mean, fill = factor(disclosure, levels = c(0, 1), labels = c("Pre-treatment", "Post-treatment")))) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.2, position = position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = c("#807F7F", "#BF504D")) +
    ggplot2::facet_wrap(vars(factor(prior_user, levels = c(FALSE, TRUE), labels = c("Non-prior users", "Prior users"))), nrow = 2) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.position = c(0.04, 0.98), legend.title = ggplot2::element_blank(), strip.text.x = ggplot2::element_text(size = 10, face = "italic"),
                   legend.direction = "vertical", legend.justification = c("left", "top"))

  ## 5.) Export plots.
  ggplot2::ggsave(paste0(save_here, "/", "players_performance_by_group.pdf"), plot_avg_rates_buckets / (plot_avg_n_matches_buckets + plot_win_rate_buckets), width = 7, height = 7)
  ggplot2::ggsave(paste0(save_here, "/", "players_position_by_group.pdf"), plot_positions_buckets, width = 7, height = 7)

  ## 6.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' LoL Impact on Players Performance
#'
#' Uses a diff-in-diff strategy to investigate the impact of the coming-out event on players' performance.
#'
#' @param n_pre_matches How many matches before \code{treatment_date} players must have played to be kept in the data set.
#' @param filter Which players to retain for the analysis.
#' @param treatment_date Object of class \code{POSIXct}. The date of the treatment.
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#'
#' @return
#' Returns a list with \code{treatment_date} and all the diff-in-diff results. The user can post-process the output using the \code{\link{plot_did}} function.
#'
#' @details
#' We define three versions of the treatment.
#'
#' \describe{
#'    \item{\code{Small reduction}}{Players that reduce their average pick rate for Graves by any amount within (0%, 25%].)}
#'    \item{\code{Moderate reduction}}{Players that reduce their average pick rate for Graves by any amount within (25%, 50%].}
#'    \item{\code{Substantial reduction}}{Players that reduce their average pick rate for Graves by any amount within (50%, 100%].}
#' }
#'
#' We keep the control group fixed as it is always composed of those prior-users that do not reduce their pick rates for Graves at all.\cr
#'
#' The estimators of Callaway and Sant’Anna (2021) are employed to estimate the impact of the coming-out event on the performance of treated players. This is implemented using the \code{\link[did]{att_gt}} function.
#' Technical details are given in the associated documentation. To summarize, we identify and estimate the average treatment effect on the treated for all time t > \code{treatment_date}. "Effects" before that date are
#' also estimated and are useful to check the plausibility of the parallel trend assumption. We consider both the unconditional estimator and the doubly-robust estimator that conditions on pre-treatment covariates
#' (average kills, assists, deaths, gold earned, and matches played each day).\cr
#'
#' \code{treatment_date}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' Players that have played less than \code{n_pre_matches} before \code{treatment_date} or that never played after are dropped. The number of players remaining in the data set is printed in the console.
#' Among these, only "prior_users" players are considerd, defined as those that used to play Graves at least 5% of their matches before his disclosure.
#'
#' @import dplyr fixest did
#' @importFrom lubridate month
#'
#' @author Riccardo Di Francesco
#'
#' @export
did_players_performance <- function(n_pre_matches,
                                    treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d"), min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-08-01")) {
  ## 0.) Handling inputs and checks.
  n_matches <- NULL
  disclosure <- NULL
  n_matches_pre <- NULL
  n_matches_post <- NULL
  graves_rate <- NULL
  graves_ban_rate <- NULL
  win_rate <- NULL
  gold_avg <- NULL
  kills_avg <- NULL
  assists_avg <- NULL
  deaths_avg <- NULL
  avg_graves_rate_post <- NULL
  avg_graves_rate_pre <- NULL
  any_reduction <- NULL
  complete_abandonment <- NULL
  mean_n_matches_pre <- NULL
  mean_gold_pre <- NULL
  mean_kills_pre <- NULL
  mean_assists_pre <- NULL
  mean_deaths_pre <- NULL

  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(min_date < day & day < max_date) %>%
    dplyr::mutate(disclosure = ifelse(day > treatment_date, 1, 0))
  lol_player_dta$day <- as.POSIXct(lol_player_dta$day, tryFormats = "%Y-%m-%d")

  keep_these_players <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(n_matches_pre = sum(n_matches * (1 - disclosure)),
                  n_matches_post = sum(n_matches * disclosure),
                  avg_graves_rate_pre = sum(graves_rate * (1 - disclosure)) / sum(1 - disclosure),
                  prior_user = avg_graves_rate_pre >= 5) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_matches_pre >= n_pre_matches & n_matches_post > 0 & prior_user) %>%
    dplyr::distinct(id, .keep_all = TRUE)

  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(id %in% keep_these_players$id) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(disclosure = ifelse(day > treatment_date, 1, 0)) %>%
    dplyr::select(day, disclosure, id, graves_rate, graves_ban_rate, n_matches, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg) %>%
    dplyr::ungroup()

  ## 1.) Assign treatment status.
  treated_controls <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_graves_rate_pre = sum(graves_rate * (1 - disclosure)) / sum(1 - disclosure),
                  avg_graves_rate_post = sum(graves_rate * disclosure) / sum(disclosure),
                  overall_reduction = ifelse(avg_graves_rate_post < avg_graves_rate_pre, (avg_graves_rate_pre - avg_graves_rate_post) / avg_graves_rate_pre * 100, 0),
                  no_reduction = overall_reduction == 0,
                  moderate_reduction = overall_reduction > 0 & overall_reduction <= 75,
                  substantial_reduction = overall_reduction > 75) %>%
                  # no_reduction = as.numeric(avg_graves_rate_post >= avg_graves_rate_pre),
                  # small_reduction = as.numeric(avg_graves_rate_post < avg_graves_rate_pre & avg_graves_rate_post >= 0.75 * avg_graves_rate_pre),
                  # moderate_reduction = as.numeric(avg_graves_rate_post < 0.75 * avg_graves_rate_pre & avg_graves_rate_post >= 0.5 * avg_graves_rate_pre),
                  # substantial_reduction = as.numeric(avg_graves_rate_post < 0.5 * avg_graves_rate_pre)
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(id, avg_graves_rate_pre, avg_graves_rate_post, overall_reduction, no_reduction, moderate_reduction, substantial_reduction)

  cat("N. observations is ", dim(lol_player_dta)[1], "
N. players is ", length(unique(lol_player_dta$id)), " of which:
  ", treated_controls %>% dplyr::distinct(id, .keep_all = TRUE) %>% dplyr::pull(no_reduction) %>% sum(), " did not reduce their pick rates for Graves
  ", treated_controls %>% dplyr::distinct(id, .keep_all = TRUE) %>% dplyr::pull(moderate_reduction) %>% sum(), " reduced their pick rates for Graves by (0%, 75%]
  ", treated_controls %>% dplyr::distinct(id, .keep_all = TRUE) %>% dplyr::pull(substantial_reduction) %>% sum(), " reduced their pick rates for Graves by (75%, 100%] \n\n", sep = "")

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(treated_controls, by = "id") %>%
    dplyr::select(day, id, disclosure, no_reduction, moderate_reduction, substantial_reduction, graves_rate, graves_ban_rate, n_matches, win_rate, gold_avg, kills_avg, assists_avg, deaths_avg)

  ## 2.) Arrange estimation data.
  estimation_dta <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(mean_n_matches_pre = sum(n_matches * (1 - disclosure) / sum(1 - disclosure)),
                  mean_gold_pre = sum(gold_avg * (1 - disclosure)) / sum(1 - disclosure),
                  mean_kills_pre = sum(kills_avg * (1 - disclosure)) / sum(1 - disclosure),
                  mean_assists_pre = sum(assists_avg * (1 - disclosure)) / sum(1 - disclosure),
                  mean_deaths_pre = sum(deaths_avg * (1 - disclosure)) / sum(1 - disclosure)) %>%
    dplyr::select(day, id, win_rate, disclosure, no_reduction, moderate_reduction, substantial_reduction, mean_n_matches_pre, mean_gold_pre, mean_kills_pre, mean_assists_pre, mean_deaths_pre) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(day_no = as.numeric(day),
                  id_no = as.numeric(factor(id)),
                  no_reduction_no = ifelse(no_reduction == 1, as.numeric(treatment_date), 0),
                  # small_reduction_no = ifelse(small_reduction == 1, as.numeric(treatment_date), 0),
                  moderate_reduction_no = ifelse(moderate_reduction == 1, as.numeric(treatment_date), 0),
                  substantial_reduction_no = ifelse(substantial_reduction == 1, as.numeric(treatment_date), 0))

  ## 3.) Doubly-robust DiD. Subset to avoid including "treated with less intensity" in the control group.
  # Small reduction.
  # estimation_dta_small_reduction <- estimation_dta %>%
  #   dplyr::filter(no_reduction == 1 | small_reduction == 1)
  #
  # dr_results_small_reduction <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "small_reduction_no",
  #                                           xformla = ~ 1,
  #                                           data = estimation_dta_small_reduction, panel = TRUE, allow_unbalanced_panel = TRUE)
  #
  # dr_results_small_reduction_covariates <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "small_reduction_no",
  #                                                      xformla = ~ mean_n_matches_pre + mean_gold_pre + mean_kills_pre + mean_assists_pre + mean_deaths_pre,
  #                                                      data = estimation_dta_small_reduction, panel = TRUE, allow_unbalanced_panel = TRUE)

  # Moderate reduction.
  estimation_dta_moderate_reduction <- estimation_dta %>%
    dplyr::filter(no_reduction == 1 | moderate_reduction == 1)

  dr_results_moderate_reduction <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "moderate_reduction_no",
                                                  xformla = ~ 1,
                                                  data = estimation_dta_moderate_reduction, panel = TRUE, allow_unbalanced_panel = TRUE)

  dr_results_moderate_reduction_covariates <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "moderate_reduction_no",
                                                             xformla = ~ mean_n_matches_pre + mean_gold_pre + mean_kills_pre + mean_assists_pre + mean_deaths_pre,
                                                             data = estimation_dta_moderate_reduction, panel = TRUE, allow_unbalanced_panel = TRUE)

  # Substantial reduction.
  estimation_dta_substantial_reduction <- estimation_dta %>%
    dplyr::filter(no_reduction == 1 | substantial_reduction == 1)

  dr_results_substantial_reduction <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "substantial_reduction_no",
                                                  xformla = ~ 1,
                                                  data = estimation_dta_substantial_reduction, panel = TRUE, allow_unbalanced_panel = TRUE)

  dr_results_substantial_reduction_covariates <- did::att_gt(yname = "win_rate", tname = "day_no", idname = "id_no", gname = "substantial_reduction_no",
                                                             xformla = ~ mean_n_matches_pre + mean_gold_pre + mean_kills_pre + mean_assists_pre + mean_deaths_pre,
                                                             data = estimation_dta_substantial_reduction, panel = TRUE, allow_unbalanced_panel = TRUE)


  ## 5.) Output.
  return(list("treatment_date" = treatment_date,
              # "dr_small_reduction" = dr_results_small_reduction,
              # "dr_small_reduction_covariates" = dr_results_small_reduction_covariates,
              "dr_moderate_reduction" = dr_results_moderate_reduction,
              "dr_moderate_reduction_covariates" = dr_results_moderate_reduction_covariates,
              "dr_substantial_reduction" = dr_results_substantial_reduction,
              "dr_substantial_reduction_covariates" = dr_results_substantial_reduction_covariates))
}


#' Diff-in-Diff Table
#'
#' Prints LATEX code for a nice table displaying the aggregated results of \code{\link{did_players_performance}}.
#'
#' @param did_results The results of \code{\link{did_players_performance}}.
#' @param seed To make results reproducible.
#'
#' @return
#' Prints LATEX code.
#'
#' @details
#' To summarize results, we report an average of the estimated ATT(t) for all t greater than \code{treatment_date} used when calling \code{\link{did_players_performance}}.
#'
#' @import did dplyr stringr
#'
#' @author Riccardo Di Francesco
#'
#' @export
latex_did <- function(did_results, seed = 1986) {
  ## Aggregate time ATTs.
  set.seed(seed)
#
#   dr_small_reduction_agg <- did::aggte(did_results$dr_small_reduction, type = "simple")
#   dr_small_reduction_covariates_agg <- did::aggte(did_results$dr_small_reduction_covariates, type = "simple")
  dr_moderate_reduction_agg <- did::aggte(did_results$dr_moderate_reduction, type = "simple")
  dr_moderate_reduction_covariates_agg <- did::aggte(did_results$dr_moderate_reduction_covariates, type = "simple")
  dr_substantial_reduction_agg <- did::aggte(did_results$dr_substantial_reduction, type = "simple")
  dr_substantial_reduction_covariates_agg <- did::aggte(did_results$dr_substantial_reduction_covariates, type = "simple")

  # dr_small_reduction_point <- dr_small_reduction_agg$overall.att
  # dr_small_reduction_covariates_point <- dr_small_reduction_covariates_agg$overall.att
  dr_moderate_reduction_point <- dr_moderate_reduction_agg$overall.att
  dr_moderate_reduction_covariates_point <- dr_moderate_reduction_covariates_agg$overall.att
  dr_substantial_reduction_point <- dr_substantial_reduction_agg$overall.att
  dr_substantial_reduction_covariates_point <- dr_substantial_reduction_covariates_agg$overall.att

  # dr_small_reduction_se <- dr_small_reduction_agg$overall.se
  # dr_small_reduction_covariates_se <- dr_small_reduction_covariates_agg$overall.se
  dr_moderate_reduction_se <- dr_moderate_reduction_agg$overall.se
  dr_moderate_reduction_covariates_se <- dr_moderate_reduction_covariates_agg$overall.se
  dr_substantial_reduction_se <- dr_substantial_reduction_agg$overall.se
  dr_substantial_reduction_covariates_se <- dr_substantial_reduction_covariates_agg$overall.se

  # dr_small_reduction_cil <- dr_small_reduction_point - 1.96 * dr_small_reduction_se
  # dr_small_reduction_covariates_cil <- dr_small_reduction_covariates_point - 1.96 * dr_small_reduction_covariates_se
  dr_moderate_reduction_cil <- dr_moderate_reduction_point - 1.96 * dr_moderate_reduction_se
  dr_moderate_reduction_covariates_cil <- dr_moderate_reduction_covariates_point - 1.96 * dr_moderate_reduction_covariates_se
  dr_substantial_reduction_cil <- dr_substantial_reduction_point - 1.96 * dr_substantial_reduction_se
  dr_substantial_reduction_covariates_cil <- dr_substantial_reduction_covariates_point - 1.96 * dr_substantial_reduction_covariates_se

  # dr_small_reduction_ciu <- dr_small_reduction_point + 1.96 * dr_small_reduction_se
  # dr_small_reduction_covariates_ciu <- dr_small_reduction_covariates_point + 1.96 * dr_small_reduction_covariates_se
  dr_moderate_reduction_ciu <- dr_moderate_reduction_point + 1.96 * dr_moderate_reduction_se
  dr_moderate_reduction_covariates_ciu <- dr_moderate_reduction_covariates_point + 1.96 * dr_moderate_reduction_covariates_se
  dr_substantial_reduction_ciu <- dr_substantial_reduction_point + 1.96 * dr_substantial_reduction_se
  dr_substantial_reduction_covariates_ciu <- dr_substantial_reduction_covariates_point + 1.96 * dr_substantial_reduction_covariates_se

  ## Extract information.
  atts <- format(round(c(dr_moderate_reduction_point, dr_moderate_reduction_covariates_point, dr_substantial_reduction_point, dr_substantial_reduction_covariates_point), 3), nsmall = 3)
  cils <- format(round(c(dr_moderate_reduction_cil, dr_moderate_reduction_covariates_cil, dr_substantial_reduction_cil, dr_substantial_reduction_covariates_cil), 3), nsmall = 3)
  cius <- format(round(c(dr_moderate_reduction_ciu, dr_moderate_reduction_covariates_ciu, dr_substantial_reduction_ciu, dr_substantial_reduction_covariates_ciu), 3), nsmall = 3)

  # n_players_small <- length(unique(did_results$dr_small_reduction$DIDparams$data$id_no))
  n_players_moderate <- length(unique(did_results$dr_moderate_reduction$DIDparams$data$id_no))
  n_players_substantial <- length(unique(did_results$dr_substantial_reduction$DIDparams$data$id_no))

  # n_observations_small <- dim(did_results$dr_small_reduction$DIDparams$data)[1]
  n_observations_moderate <- dim(did_results$dr_moderate_reduction$DIDparams$data)[1]
  n_observations_substantial <- dim(did_results$dr_substantial_reduction$DIDparams$data)[1]

  treatment_date <- did_results$treatment_date

  # n_treated_small_reduction <- did_results$dr_small_reduction$DIDparams$data %>%
  #   dplyr::filter(day_no > as.numeric(treatment_date)) %>%
  #   dplyr::distinct(id_no, .keep_all = TRUE) %>%
  #   dplyr::mutate(n_treated = sum(small_reduction_no != 0)) %>%
  #   dplyr::pull(n_treated) %>%
  #   unique()

  n_treated_moderate_reduction <- did_results$dr_moderate_reduction$DIDparams$data %>%
    dplyr::filter(day_no > as.numeric(treatment_date)) %>%
    dplyr::distinct(id_no, .keep_all = TRUE) %>%
    dplyr::mutate(n_treated = sum(moderate_reduction_no != 0)) %>%
    dplyr::pull(n_treated) %>%
    unique()

  n_treated_substantial_reduction <- did_results$dr_substantial_reduction$DIDparams$data %>%
    dplyr::filter(day_no > as.numeric(treatment_date)) %>%
    dplyr::distinct(id_no, .keep_all = TRUE) %>%
    dplyr::mutate(n_treated = sum(substantial_reduction_no != 0)) %>%
    dplyr::pull(n_treated) %>%
    unique()

  ## LATEX.
  cat("\\begingroup
  \\setlength{\\tabcolsep}{8pt}
  \\renewcommand{\\arraystretch}{1.1}
  \\begin{table}[H]
    \\centering
    \\begin{adjustbox}{width = 1\\textwidth}
    \\begin{tabular}{@{\\extracolsep{5pt}}l c c c c}
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      & \\multicolumn{2}{c}{\\textit{Moderate Reduction}} & \\multicolumn{2}{c}{\\textit{Substantial Reduction}} \\\\ \\cmidrule{2-3} \\cmidrule{4-5}
      & (1) & (2) & (3) & (4) \\\\

      \\midrule

      \\multirow{2}{*}{$\\overline{ATT \\left( t \\right)}$} & ", stringr::str_sub(paste(paste0(atts, " &"), collapse = " "), end = -3), " \\\\
      &", stringr::str_sub(paste(paste0("[", cils, ", ", cius, "] &"), collapse = " "), end = -3), " \\\\

      \\midrule

      Conditional PT & & \\checkmark & & \\checkmark \\\\
      Players & ", stringr::str_sub(paste(paste0(rep(n_players_moderate, 2), " &"), collapse = " "), end = -3), " & ", stringr::str_sub(paste(paste0(rep(n_players_substantial, 2), " &"), collapse = " "), end = -3), " \\\\
      Treated & ", paste0(rep(n_treated_moderate_reduction, 2), " & "), stringr::str_sub(paste(paste0(rep(n_treated_substantial_reduction, 2), " &"), collapse = " "), end = -3), " \\\\
      Observations & ", stringr::str_sub(paste(paste0(rep(n_observations_moderate, 2), " &"), collapse = " "), end = -3), " & ", stringr::str_sub(paste(paste0(rep(n_observations_substantial, 2), " &"), collapse = " "), end = -3), " \\\\

      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]

      \\end{tabular}
      \\end{adjustbox}
      \\caption{Point estimates and $95\\%$ confidence intervals for $\\overline{ATT \\left( t \\right)}$. Standard errors are clustered at the player level and computed using the multiplier bootstrap. Columns marked with checkmarks under 'Conditional PT' display the results obtained with the doubly-robust approach. The remaining columns display the results obtained with the unconditional estimator.}
      \\label{table_did_performance_measures}
    \\end{table}
\\endgroup \n", sep = "")
#   cat("\\begingroup
#   \\setlength{\\tabcolsep}{8pt}
#   \\renewcommand{\\arraystretch}{1.1}
#   \\begin{table}[H]
#     \\centering
#     \\begin{adjustbox}{width = 1\\textwidth}
#     \\begin{tabular}{@{\\extracolsep{5pt}}l c c c c c c}
#       \\\\[-1.8ex]\\hline
#       \\hline \\\\[-1.8ex]
#       & \\multicolumn{2}{c}{\\textit{Small Reduction}} & \\multicolumn{2}{c}{\\textit{Moderate Reduction}} & \\multicolumn{2}{c}{\\textit{Substantial Reduction}} \\\\ \\cmidrule{2-3} \\cmidrule{4-5} \\cmidrule{6-7}
#       & (1) & (2) & (3) & (4) & (5) & (6) \\\\
#
#       \\midrule
#
#       \\multirow{2}{*}{$\\overline{ATT \\left( t \\right)}$} & ", stringr::str_sub(paste(paste0(atts, " &"), collapse = " "), end = -3), " \\\\
#       &", stringr::str_sub(paste(paste0("[", cils, ", ", cius, "] &"), collapse = " "), end = -3), " \\\\
#
#       \\midrule
#
#       Conditional PT & & \\checkmark & & \\checkmark & & \\checkmark \\\\
#       Players & ", stringr::str_sub(paste(paste0(rep(n_players_small, 2), " &"), collapse = " "), end = -3), " & ", stringr::str_sub(paste(paste0(rep(n_players_moderate, 2), " &"), collapse = " "), end = -3), " & ", stringr::str_sub(paste(paste0(rep(n_players_substantial, 2), " &"), collapse = " "), end = -3), " \\\\
#       Treated & ", paste0(rep(n_treated_small_reduction, 2), " & "), paste0(rep(n_treated_moderate_reduction, 2), " & "), stringr::str_sub(paste(paste0(rep(n_treated_substantial_reduction, 2), " &"), collapse = " "), end = -3), " \\\\
#       Observations & ", stringr::str_sub(paste(paste0(rep(n_observations_small, 2), " &"), collapse = " "), end = -3), " & ", stringr::str_sub(paste(paste0(rep(n_observations_moderate, 2), " &"), collapse = " "), end = -3), " & ", stringr::str_sub(paste(paste0(rep(n_observations_substantial, 2), " &"), collapse = " "), end = -3), " \\\\
#
#       \\\\[-1.8ex]\\hline
#       \\hline \\\\[-1.8ex]
#
#       \\end{tabular}
#       \\end{adjustbox}
#       \\caption{Point estimates and $95\\%$ confidence intervals for $\\overline{ATT \\left( t \\right)}$. Standard errors are clustered at the player level and computed using the multiplier bootstrap. Columns marked with checkmarks under 'Conditional PT' display the results obtained with the doubly-robust approach. The remaining columns display the results obtained with the unconditional estimator.}
#       \\label{table_did_performance_measures}
#     \\end{table}
# \\endgroup \n", sep = "")
}


#' Diff-in-Diff Plots
#'
#' Plots the time ATTs estimated by \code{\link{did_players_performance}}.
#'
#' @param did_results The results of \code{\link{did_players_performance}}.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Returns a nice plot.
#'
#' @import ggplot2
#' @importFrom stats qnorm sd
#'
#' @author Riccardo Di Francesco
#'
#' @export
plot_did <- function(did_results, save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  att <- NULL
  att.se <- NULL
  post <- NULL

  treatment_date <- did_results$treatment_date
  n_times <- length(unique(did_results$dr_moderate_reduction$t))
  times <- unique(did_results$dr_moderate_reduction$t)

  results_moderate_reduction <- results_substantial_reduction <- data.frame(year = as.POSIXct(times, origin = "1970-01-01"))

  # results_small_reduction$att <- did_results$dr_small_reduction$att
  # results_small_reduction$att.se <- did_results$dr_small_reduction$se
  # results_small_reduction$post <- as.factor(1 * (results_small_reduction$year >= treatment_date))
  # results_small_reduction$c <- did_results$dr_small_reduction$c
  # alp_small_reduction <- did_results$dr_small_reduction$alp
  # c.point_small_reduction <- stats::qnorm(1 - alp_small_reduction / 2)
  # results_small_reduction$treatment_type <- "Small reduction"
  # results_small_reduction$parallel_type <- "Unconditional"
  # results_small_reduction$plot_post <- as.factor(1 * (results_small_reduction$year >= (as.Date(treatment_date) - 10)))

  results_moderate_reduction$att <- did_results$dr_moderate_reduction$att
  results_moderate_reduction$att.se <- did_results$dr_moderate_reduction$se
  results_moderate_reduction$post <- as.factor(1 * (results_moderate_reduction$year >= treatment_date))
  results_moderate_reduction$c <- did_results$dr_moderate_reduction$c
  alp_moderate_reduction <- did_results$dr_moderate_reduction$alp
  c.point_moderate_reduction <- stats::qnorm(1 - alp_moderate_reduction / 2)
  results_moderate_reduction$treatment_type <- "Moderate reduction"
  results_moderate_reduction$parallel_type <- "Unconditional"
  results_moderate_reduction$plot_post <- as.factor(1 * (results_moderate_reduction$year >= (as.Date(treatment_date) - 10)))

  results_substantial_reduction$att <- did_results$dr_substantial_reduction$att
  results_substantial_reduction$att.se <- did_results$dr_substantial_reduction$se
  results_substantial_reduction$post <- as.factor(1 * (results_substantial_reduction$year >= treatment_date))
  results_substantial_reduction$c <- did_results$dr_substantial_reduction$c
  alp_substantial_reduction <- did_results$dr_substantial_reduction$alp
  c.point_substantial_reduction <- stats::qnorm(1 - alp_substantial_reduction / 2)
  results_substantial_reduction$treatment_type <- "Substantial reduction"
  results_substantial_reduction$parallel_type <- "Unconditional"
  results_substantial_reduction$plot_post <- as.factor(1 * (results_substantial_reduction$year >= (as.Date(treatment_date) - 10)))


  ## 1.) Produce and save plot.
  plot_pre <- results_moderate_reduction %>%
    dplyr::bind_rows(results_substantial_reduction) %>%
    dplyr::filter(plot_post == 0) %>%
    dplyr::mutate(treatment_factor = factor(treatment_type, levels = c("Moderate reduction", "Substantial reduction"))) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = att, ymin = (att - c * att.se), ymax = (att + c * att.se))) +
    ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
    ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
    ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
    ggplot2::facet_grid(rows = vars(treatment_factor)) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%d-%m-%Y") +
    ggplot2::scale_color_manual(drop = FALSE, values = c("#e87d72", "#56bcc2"), breaks = c(0, 1), labels = c("Pre", "Post")) +
    ggplot2::xlab("") + ggplot2::ylab(expression(italic("ATT ( t )"))) +
    ggplot2::theme_bw() +
    theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 10, face = "bold"), strip.text.y = ggplot2::element_text(size = 10, face = "italic"),
          legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))
  ggplot2::ggsave(paste0(save_here, "/", "players_performance_did_pre.pdf"), plot_pre, width = 7, height = 7)

  plot_post <- results_moderate_reduction %>%
    dplyr::bind_rows(results_substantial_reduction) %>%
    dplyr::filter(plot_post == 1) %>%
    dplyr::mutate(treatment_factor = factor(treatment_type, levels = c("Small reduction", "Moderate reduction", "Substantial reduction"))) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = att, ymin = (att - c * att.se), ymax = (att + c * att.se))) +
    ggplot2::geom_point(ggplot2::aes(colour = post), size = 1.5) +
    ggplot2::geom_errorbar(ggplot2::aes(colour = post), width = 0.1) +
    ggplot2::geom_hline(aes(yintercept = 0), linetype = "dashed") +
    ggplot2::facet_grid(rows = vars(treatment_factor)) +
    ggplot2::scale_x_datetime(date_breaks = "1 week", date_labels = "%d-%m-%Y") +
    ggplot2::scale_color_manual(drop = FALSE, values = c("#e87d72", "#56bcc2"), breaks = c(0, 1), labels = c("Pre", "Post")) +
    ggplot2::xlab("") + ggplot2::ylab(expression(italic("ATT ( t )"))) +
    ggplot2::theme_bw() +
    theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 10, face = "bold"), strip.text.y = ggplot2::element_text(size = 10, face = "italic"),
          legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))
  ggplot2::ggsave(paste0(save_here, "/", "players_performance_did_post.pdf"), plot_post, width = 7, height = 7)

  ## 6.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' LoL Belveth
#'
#' Check whether Graves' prior and non-prior users switched to Belveth.
#'
#' @param n_pre_matches How many matches before \code{treatment_date} players must have played to be kept in the data set.
#' @param min_date Object of class \code{POSIXct}. Where to start the series.
#' @param max_date Object of class \code{POSIXct}. Where to end the series.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' None. It produces nice plots.
#'
#' @details
#' We consider only prior-users. We classify them into four different categories:
#'
#' \describe{
#'    \item{\code{No reduction}}{Players that did not reduce their average pick rate for Graves.}
#'    \item{\code{Moderate reduction}}{Players that reduce their average pick rate for Graves by any amount within (0%, 75%].}
#'    \item{\code{Substantial reduction}}{Players that reduce their average pick rate for Graves by any amount within (75%, 100%].}
#' }
#'
#' \code{min_date} and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' Players that have played less than \code{n_pre_matches} before \code{treatment_date} or that never played after are dropped. The number of players remaining in the data set is printed in the console.
#'
#' @import dplyr ggplot2
#'
#' @author Riccardo Di Francesco
#'
#' @export
belveth <- function(n_pre_matches,
                    min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-08-01"),
                    save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  n_matches <- NULL
  n_matches_pre <- NULL
  n_matches_post <- NULL
  graves_rate <- NULL
  graves_ban_rate <- NULL
  belveth_rate <- NULL
  belveth_ban_rate <- NULL
  win_rate <- NULL
  gold_avg <- NULL
  kills_avg <- NULL
  assists_avg <- NULL
  deaths_avg <- NULL
  avg_graves_rate_post <- NULL
  avg_graves_rate_pre <- NULL
  any_reduction <- NULL
  complete_abandonment <- NULL
  mean_n_matches_pre <- NULL
  mean_gold_pre <- NULL
  mean_kills_pre <- NULL
  mean_assists_pre <- NULL
  mean_deaths_pre <- NULL

  belveth_date <- as.POSIXct("2022-06-09")
  coming_out_date <- as.POSIXct("2022-06-01")

  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(min_date < day & day < max_date) %>%
    dplyr::mutate(belveth_released = as.numeric(day >= belveth_date),
                  disclosure = as.numeric(day > coming_out_date))

  keep_these_players <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(n_matches_pre = sum(n_matches * (1 - disclosure)),
                  n_matches_post = sum(n_matches * disclosure),
                  avg_graves_rate_pre = sum(graves_rate * (1 - disclosure)) / sum(1 - disclosure),
                  prior_user = avg_graves_rate_pre >= 5) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_matches_pre >= n_pre_matches & n_matches_post > 0 & prior_user) %>%
    dplyr::distinct(id, .keep_all = TRUE)

  lol_player_dta <- lol_player_dta %>%
    dplyr::filter(id %in% keep_these_players$id) %>%
    dplyr::select(day, belveth_released, disclosure, id, graves_rate, belveth_rate) %>%
    dplyr::ungroup()

  ## 1.) Assign "treatment" status.
  treated_controls <- lol_player_dta %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(avg_graves_rate_pre = sum(graves_rate * (1 - disclosure)) / sum(1 - disclosure),
                  avg_graves_rate_post = sum(graves_rate * disclosure) / sum(disclosure),
                  overall_reduction = ifelse(avg_graves_rate_post < avg_graves_rate_pre, (avg_graves_rate_pre - avg_graves_rate_post) / avg_graves_rate_pre * 100, 0),
                  no_reduction = overall_reduction == 0,
                  moderate_reduction = overall_reduction > 0 & overall_reduction <= 75,
                  substantial_reduction = overall_reduction > 75) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(id, overall_reduction, no_reduction, moderate_reduction, substantial_reduction)

  lol_player_dta <- lol_player_dta %>%
    dplyr::left_join(treated_controls, by = "id") %>%
    dplyr::select(day, id, belveth_released, disclosure, overall_reduction, no_reduction, moderate_reduction, substantial_reduction, graves_rate, belveth_rate)

  cat("N. observations is ", dim(lol_player_dta)[1], "
N. players is ", length(unique(lol_player_dta$id)), " of which:
  ", treated_controls %>% dplyr::distinct(id, .keep_all = TRUE) %>% dplyr::pull(no_reduction) %>% sum(), " did not reduce their pick rates for Graves
  ", treated_controls %>% dplyr::distinct(id, .keep_all = TRUE) %>% dplyr::pull(moderate_reduction) %>% sum(), " reduced their pick rates for Graves by (0%, 75%]
  ", treated_controls %>% dplyr::distinct(id, .keep_all = TRUE) %>% dplyr::pull(substantial_reduction) %>% sum(), " reduced their pick rates for Graves by (75%, 100%] \n\n", sep = "")

  plot_overall_reduction <- treated_controls %>%
    ggplot2::ggplot(ggplot2::aes(x = overall_reduction)) +
    geom_histogram(fill = "dodgerblue", color = "black", alpha = 0.8) +
    ggplot2::xlab("") + ggplot2::ylab("Reduction in average Graves' pick rate (percentage)") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.title = ggplot2::element_blank(), strip.text.x = ggplot2::element_text(size = 10, face = "italic"),
                   axis.text.x = ggplot2::element_text(angle = 0), legend.direction = "vertical", legend.justification = c("left", "top"))
  ggplot2::ggsave(paste0(save_here, "/", "players_graves_reduction.pdf"), plot_overall_reduction, width = 7, height = 7)

  ## 3.) Plot.
  prior_users_no_reduction_dta <- lol_player_dta %>%
    dplyr::filter(belveth_released == 1 & no_reduction == 1) %>%
    dplyr::mutate(treatment_status = "prior_users_no_reduction",
                  avg_belveth_rate = mean(belveth_rate)) %>%
    dplyr::select(treatment_status, avg_belveth_rate) %>%
    dplyr::distinct()

  prior_users_moderate_reduction_dta <- lol_player_dta %>%
    dplyr::filter(belveth_released == 1 & moderate_reduction == 1) %>%
    dplyr::mutate(treatment_status = "prior_users_moderate_reduction",
                  avg_belveth_rate = mean(belveth_rate)) %>%
    dplyr::select(treatment_status, avg_belveth_rate) %>%
    dplyr::distinct()

  prior_users_substantial_reduction_dta <- lol_player_dta %>%
    dplyr::filter(belveth_released == 1 & substantial_reduction == 1) %>%
    dplyr::mutate(treatment_status = "prior_users_substantial_reduction",
                  avg_belveth_rate = mean(belveth_rate)) %>%
    dplyr::select(treatment_status, avg_belveth_rate) %>%
    dplyr::distinct()

  plot_belveth_buckets_prior_users <- prior_users_no_reduction_dta %>%
    dplyr::bind_rows(prior_users_moderate_reduction_dta, prior_users_substantial_reduction_dta) %>%
    dplyr::mutate(players = "Prior users") %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(treatment_status, levels = c("prior_users_no_reduction", "prior_users_moderate_reduction", "prior_users_substantial_reduction"), labels = c("No reduction", "Moderate reduction", "Substantial reduction")), y = avg_belveth_rate)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity", fill = "dodgerblue") +
    ggplot2::facet_grid(cols = vars(players)) +
    ggplot2::xlab("") + ggplot2::ylab("Average Bel'Veth's pick rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.title = ggplot2::element_blank(), strip.text.x = ggplot2::element_text(size = 10, face = "italic"),
                   axis.text.x = ggplot2::element_text(angle = 0), legend.direction = "vertical", legend.justification = c("left", "top"))
  ggplot2::ggsave(paste0(save_here, "/", "players_belveth_barplot.pdf"), plot_belveth_buckets_prior_users, width = 7, height = 7)

  ## 4.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}
