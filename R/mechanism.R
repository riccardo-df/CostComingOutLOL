#' LoL Players' Performance Plots
#'
#' Divide players into treated and control groups according to whether they never picked Graves pre-treatment (prior and non prior users) and produces plots showing the average performance of players
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
#' @importFrom stats sd
#'
#' @author Riccardo Di Francesco
#'
#' @export
players_performance_plots_lol <- function(n_pre_matches, n_post_matches,
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
    dplyr::mutate(pre_treatment = stats::sd(avg_n_matches_pre) / sqrt(n()),
                  post_treatment = stats::sd(avg_n_matches_post) / sqrt(n())) %>%
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

  ## 5.) Export grid.
  ggplot2::ggsave(paste0(save_here, "/", "players_performance_by_group.svg"), plot_avg_rates_buckets / (plot_avg_n_matches_buckets + plot_win_rate_buckets), device = "svg", width = 7, height = 7)

  ## 6.) Talk to the user.
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
#' The estimators of Callaway and Sant’Anna (2021) are employed to estimate the impact of the coming-out event on the performance of treated players. This is implemented using the \code{\link[did]{att_gt}} function.
#' Technical details are given in the associated documentation. To summarize, we identify and estimate the average treatment effect on the treated for all time t > \code{treatment_date}. "Effects" before that date are
#' also estimated and are useful to check the plausibility of the parallel trend assumption. We consider both the unconditional estimator and the doubly-robust estimator that conditions on pre-treatment covariates
#' (average kills, assists, deaths, gold earned, and matches played each day).\cr
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
  reduce_graves <- NULL
  drop_graves <- NULL
  mean_n_matches_pre <- NULL
  mean_gold_pre <- NULL
  mean_kills_pre <- NULL
  mean_assists_pre <- NULL
  mean_deaths_pre <- NULL

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
    dplyr::group_by(id) %>%
    dplyr::mutate(mean_n_matches_pre = sum(n_matches * (1 - disclosure) / sum(1 - disclosure)),
                  mean_gold_pre = sum(gold_avg * (1 - disclosure)) / sum(1 - disclosure),
                  mean_kills_pre = sum(kills_avg * (1 - disclosure)) / sum(1 - disclosure),
                  mean_assists_pre = sum(assists_avg * (1 - disclosure)) / sum(1 - disclosure),
                  mean_deaths_pre = sum(deaths_avg * (1 - disclosure)) / sum(1 - disclosure)) %>%
    dplyr::select(day, id, win_rate, disclosure, reduce_graves, drop_graves, mean_n_matches_pre, mean_gold_pre, mean_kills_pre, mean_assists_pre, mean_deaths_pre) %>%
    dplyr::mutate(n_matches_pre = mean_n_matches_pre * disclosure,
                  gold_pre = mean_gold_pre * disclosure,
                  kills_pre = mean_kills_pre * disclosure,
                  assists_pre = mean_assists_pre * disclosure,
                  deaths_pre = mean_deaths_pre * disclosure) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(day_no = as.numeric(day),
                  id_no = as.numeric(factor(id)),
                  gname_reduce = ifelse(reduce_graves == 1, as.numeric(treatment_date), 0),
                  gname_drop = ifelse(drop_graves == 1, as.numeric(treatment_date), 0))

  ## 3.) Doubly-robust DiD.
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
  return(list("dr_reduce_graves" = dr_results_reduce,
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
#' @importFrom stats qnorm sd
#'
#' @author Riccardo Di Francesco
#'
#' @export
plot_did <- function(att_gt_results, title, subtitle, treatment_date = as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")) {
  ## 0.) Handling inputs and checks.
  att <- NULL
  att.se <- NULL
  post <- NULL

  if (!inherits(att_gt_results, "MP")) stop("Invalid 'att_gt_results'. This must be an object of class 'MP'.", call. = FALSE)

  n_times <- length(unique(att_gt_results$t))
  times <- unique(att_gt_results$t)

  results <- data.frame(year = as.POSIXct(times, origin = "1970-01-01"))
  results$att <- att_gt_results$att
  results$att.se <- att_gt_results$se
  results$post <- as.factor(1*(results$year >= treatment_date))
  results$c <- att_gt_results$c

  alp <- att_gt_results$alp
  c.point <- stats::qnorm(1 - alp / 2)

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
