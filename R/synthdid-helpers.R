#' Construct Donor Pool
#'
#' Subsets the provided data to include only the champion under investigation and the desired units in the donor pool.
#'
#' @param dta One of the champion data sets bundled in the package (\code{\link{lol_champ_pool_dta}} or \code{\link{lol_champ_dta}}).
#' @param donors Which units to include in the donor pool. See the details section below.
#' @param my_champion Champion under investigation. We need to include it as well.
#'
#' @return
#' \code{dta} filtered to contain only the champion under investigation and the desired donors.
#'
#' @details
#' \code{donors} must be a character vector with one or more champions contained in \code{dta}. Alternatively, one of the following special strings can be used:
#'
#' \itemize{
#'    \item{"all" }{This includes all champions.}
#'    \item{"non_lgb" }{This includes all champions expect Graves, Twisted Fate, Diana, Leona, Nami, and Neeko.}
#'    \item{"jungle" }{This includes all champions whose main role is Jungle.}
#'    \item{"middle" }{This includes all champions whose main role is Middle.}
#'    \item{"top" }{This includes all champions whose main role is Top.}
#'    \item{"support" }{This includes all champions whose main role is Support.}
#'    \item{"adc" }{This includes all champions whose main role is Adc.}
#'    \item{"main_role"}{This includes all champions whose main role is the same as the champion of interest.}
#'    \item{"aux_role"}{This includes all champions whose auxiliary role is the same as the champion of interest.}
#' }
#'
#' Almost all of these choices (exceptions are \code{"all"} and \code{"non_lgb"}) can be combined with the prefix \code{"non"} to exclude champions from a particular role.
#' For instance, setting \code{donors} to \code{non_jungle} excludes from the donor pool all champions whose main role is jungle.
#'
#' @import dplyr
#'
#' @author Riccardo Di Francesco
#'
#' @keywords internal
construct_donor_pool <- function(dta, donors, my_champion) {
  ## 0.) Handling inputs and checks.
  champion <- NULL
  main_role <- NULL
  aux_role <- NULL

  if (length(donors) == 1) {
    if (!(donors %in% c("all", "non_lgb", "main_role", "aux_role", "jungle", "middle", "top", "support", "adc", "support_adc", "top_jungle_middle", "non_main_role", "non_aux_role", "non_jungle", "non_middle", "non_top", "non_support", "non_adc"))) stop("Invalid 'donors'. Call 'help(run_main_pooled)' to check valid inputs.", call. = FALSE)
  } else {
    if (sum(!(donors %in% unique(dta$champion))) > 0) stop("Invalid 'donors'. One or more champions are not found in 'dta'.", call. = FALSE)
  }

  ## 2.) Subset dta.
  if (length(donors) == 1) {
    if (donors == "all") {
      my_subset <- dta
    } else if (donors == "non_lgb") {
      lgb_champions <- c("Graves", "TwistedFate", "Nami", "Leona", "Diana", "Neeko")
      exclude_these <- lgb_champions[lgb_champions != my_champion]

      my_subset <- dta %>%
        dplyr::filter(!(champion %in% exclude_these))
    } else if (donors == "main_role") {
      temp_main_role <- dta %>%
        dplyr::filter(champion == my_champion) %>%
        dplyr::pull(main_role) %>%
        unique()

      my_subset <- dta %>%
        dplyr::filter(main_role == temp_main_role | champion == my_champion)
    } else if (donors == "aux_role") {
      temp_aux_role <- dta %>%
        dplyr::filter(champion == my_champion) %>%
        dplyr::pull(aux_role) %>%
        unique()

      my_subset <- dta %>%
        dplyr::filter(main_role == temp_aux_role | champion == my_champion)
    } else if (donors == "jungle") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "JUNGLE" | champion == my_champion)
    } else if (donors == "middle") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "MIDDLE" | champion == my_champion)
    } else if (donors == "top") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "TOP" | champion == my_champion)
    } else if (donors == "support") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "UTILITY" | champion == my_champion)
    } else if (donors == "adc") {
      my_subset <- dta %>%
        dplyr::filter(main_role == "BOTTOM" | champion == my_champion)
    } else if (donors == "support_adc") {
      my_subset <- dta %>%
        dplyr::filter(main_role %in% c("UTILITY", "BOTTOM") | champion == my_champion)
    } else if (donors == "top_jungle_middle") {
      my_subset <- dta %>%
        dplyr::filter(main_role %in% c("TOP", "JUNGLE", "MIDDLE") | champion == my_champion)
    } else if (donors == "non_main_role") {
      temp_main_role <- dta %>%
        dplyr::filter(champion == my_champion) %>%
        dplyr::pull(main_role) %>%
        unique()

      my_subset <- dta %>%
        dplyr::filter(main_role != temp_main_role | champion == my_champion)
    } else if (donors == "non_aux_role") {
      temp_aux_role <- dta %>%
        dplyr::filter(champion == my_champion) %>%
        dplyr::pull(aux_role) %>%
        unique()

      my_subset <- dta %>%
        dplyr::filter(main_role != temp_aux_role | champion == my_champion)
    } else if (donors == "non_jungle") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "JUNGLE" | champion == my_champion)
    } else if (donors == "non_middle") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "MIDDLE" | champion == my_champion)
    } else if (donors == "non_top") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "TOP" | champion == my_champion)
    } else if (donors == "non_support") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "UTILITY" | champion == my_champion)
    } else if (donors == "non_adc") {
      my_subset <- dta %>%
        dplyr::filter(main_role != "BOTTOM" | champion == my_champion)
    }
  } else {
    my_subset <- dta %>%
      dplyr::filter(champion %in% c(donors, my_champion))
  }

  ## 3.) Output.
  return(my_subset)
}


#' Regularized Synthetic Control
#'
#' Synthetic control with a ridge penalty, as defined in Algorithm 1 of Arkhangelsky et al. (2021). Taken from
#' \href{https://synth-inference.github.io/synthdid/articles/paper-results.html}{https://synth-inference.github.io/synthdid/articles/paper-results.html}.
#'
#' @param Y Observation matrix.
#' @param N0 Number of control units (control units are in the rows 1-N0 of Y).
#' @param T0 Number of pre-treatment periods (they correspond to columns 1-T0 of Y).
#' @param X Optional 3D array of time-varying covariates. Shape should be N X T X C for C covariates.
#'
#' @import dplyr ggplot2
#'
#' @author Riccardo Di Francesco
#'
#' @export
sc_estimate_reg <- function(Y, N0, T0, X = array(dim = c(dim(Y), 0))) {
  sc_estimate(Y, N0, T0, X = X, eta.omega = ((nrow(Y) - N0) * (ncol(Y) - T0))^(1/4))
}



#' Call Synthdid Estimation Function
#'
#' Calls one of the \code{synthdid} estimation functions.
#'
#' @param dta One of the champion data sets bundled in the package (\code{\link{lol_champ_pool_dta}} or \code{\link{lol_champ_dta}}).
#' @param outcome_colname Name of the column of \code{dta} storing the outcome of interest.
#' @param estimator Which estimator to use. Must be one of "sc" (standard synthetic control), "sc_reg" (sc + a ridge penalty), "synthdid" (synthetic diff-in-diff).
#' @param covariate_colnames Character vector with the names of the columns storing the time-varying covariates for which we want to adjust for.
#'
#' @details
#' Before calling the estimation function, \code{\link{call_synthdid}} processes \code{dta} to generate the required objects.\cr
#'
#' If \code{covariate_colnames} is non-empty, \code{\link{call_synthdid}} regresses the outcomes on the covariates, on time fixed effects, and unit fixed effects, with the regression estimated without
#' using observations in which the treatment takes place. Then, \code{\link{call_synthdid}} uses the estimated coefficients to compute the residuals of the outcomes for all observations (also those in which
#' the treatment takes place), and uses these residuals rather than the original outcomes for estimation.
#'
#' @import synthdid xsynthdid
#'
#' @author Riccardo Di Francesco
#'
#' @keywords internal
call_synthdid <- function(dta, outcome_colname, estimator, covariate_colnames) {
  ## 0.) Handling inputs and checks.
  if (!(estimator %in% c("sc", "sc_reg", "sdid"))) stop("Invalid 'estimator'. This must be one of 'sc', 'sc_reg', 'sdid'.", call. = FALSE)

  unit <- which(colnames(dta) == "champion")
  time <- which(colnames(dta) == "day_no")
  outcome <- which(colnames(dta) == outcome_colname)
  treatment <- which(colnames(dta) == "treatment")

  ## 1.) If necessary, adjust the outcomes.
  if (length(covariate_colnames) != 0) {
    dta$y_adjusted <- xsynthdid::adjust.outcome.for.x(dta, unit = unit, time = time, outcome = outcome, treatment = treatment, x = covariate_colnames)
    outcome <- which(colnames(dta) == "y_adjusted")
  }

  ## 2.) Process data.
  setup <- synthdid::panel.matrices(as.data.frame(dta), unit = unit, time = time, outcome = outcome, treatment = treatment)

  ## 3.) Run estimation.
  if (estimator == "sc") {
    tau_hat <- synthdid::sc_estimate(setup$Y, setup$N0, setup$T0)
  } else if (estimator == "sc_reg") {
    tau_hat <- sc_estimate_reg(setup$Y, setup$N0, setup$T0)
  } else if (estimator == "sdid") {
    tau_hat <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
  }

  return(tau_hat)
}


#' Synthetic Outcome
#'
#' Constructs the synthetic outcome using the estimation results stored in an \code{\link[synthdid]{synthdid_estimate}} object.
#'
#' @param object, An \code{\link[synthdid]{synthdid_estimate}} object.
#' @param dta Data set used to estimate \code{object}.
#' @param unit_colname Name of the column of \code{dta} storing the units of observation.
#' @param outcome_colname Name of the column of \code{dta} storing the outcome of interest.
#' @param time_colname Name of the column of \code{dta} storing the time variable.
#'
#' @return
#' A list with the following elements:
#' \describe{
#' \item{weights}{An (nw)x2 tibble (with nw = number of non-zero estimated weights), with one column for the identities and one column for the contributions of units in the donor pool with a non-zero estimated weight.}
#' \item{synth_outcome}{An Tx2 tibble, with one column for the time periods and one column for the synthetic outcome.}
#' }
#'
#' @author Riccardo Di Francesco
#'
#' @import dplyr
#' @importFrom stats weighted.mean
#'
#' @export
construct_synth_outcome <- function(object, dta, unit_colname, outcome_colname, time_colname) {
  ## 0.) Handling inputs and checks.
  weight <- NULL

  if (!any(c(unit_colname, outcome_colname, time_colname) %in% colnames(dta))) stop("One or more of 'unit_colname', 'outcome_colname', and 'time_colname' are not found in 'dta'.", call. = FALSE)
  if(!inherits(object, "synthdid_estimate")) stop("Invalid 'object'. This must be of class 'synthdid_estimate'.", call. = FALSE)

  ## 1.) Extract non-zero weights and corresponding units.
  weights <- dplyr::as_tibble(data.frame("unit" = rownames(synthdid_controls(object)),
                                         "weight" = synthdid_controls(object)[, 1]))
  colnames(weights)[1] <- unit_colname

  ## 2.) Get outcomes over time for the actual contributors.
  contributors <- weights[, 1] %>%
    pull()

  outcomes_and_weights <- dta %>%
    dplyr::filter(!!as.symbol(unit_colname) %in% contributors) %>%
    dplyr::left_join(weights, by = unit_colname) %>%
    dplyr::select(all_of(c(time_colname, unit_colname, outcome_colname)), "weight")

  ## 3.) Construct synthetic outcome as weighted mean of actual outcomes.
  synth_outcome_results <- outcomes_and_weights %>%
    dplyr::group_by_at(time_colname) %>%
    dplyr::mutate(synth_outcome = stats::weighted.mean(get(outcome_colname), weight)) %>%
    dplyr::select(all_of(time_colname), "synth_outcome") %>%
    dplyr::distinct()

  ## 4.) Output.
  return(list("weights" = weights, "synth_outcome" = synth_outcome_results))
}


#' Placebo Plot
#'
#' Produced a placebo plot for synthetic control estimators.
#'
#' @param pooled_results Output of \code{\link{run_main_pooled}} called on several distinct characters.
#' @param main_champion String denoting the actual treated champion.
#' @param drop_overfit Pre-treatment RMSE threshold. Drop characters that feature a pre-treatment RMSE lower than this to omit overfit specifications.
#' @param ylims Vector storing lower and upper limit for the y-axis.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Save some nice plots and returns a list storing two tibbles, one with the pre-treatment and one with the post-treatment root mean squared errors of all fits.
#'
#' @import dplyr ggplot2 ggsci grDevices
#' @importFrom stats reorder
#' @importFrom gridExtra arrangeGrob
#' @importFrom lubridate year
#' @importFrom Metrics rmse
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{produce_plots_regional}} \code{\link{produce_latex_pooled}} \code{\link{produce_latex_regional}}
#'
#' @export
produce_plot_placebo <- function(pooled_results, main_champion, drop_overfit = 1, ylims = c(0, 100), save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  champion <- NULL
  day_no <- NULL
  smooth_outcome <- NULL
  lgb_smooth_outcome <- NULL
  synth_outcome <- NULL
  weight <- NULL

  outcome_colname <- pooled_results$outcome_colname
  estimator <- pooled_results$estimator
  donors <- pooled_results$donors
  treatment_date <- pooled_results$treatment_date
  year <- lubridate::year(treatment_date)
  n_back_days <- as.numeric(summary(pooled_results[[1]]$tau_hat)$dimensions["T0"] - summary(pooled_results[[1]]$tau_hat_back)$dimensions["T0"] - 1)
  treatment_date_back <- as.Date(treatment_date) - n_back_days
  champions <- names(pooled_results)[!(names(pooled_results) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))]

  if (!(main_champion %in% champions)) stop("Invalid 'main_champion'. This is not found among 'pooled_results'.", call. = FALSE)
  if (!as.numeric(drop_overfit)) stop("Invalid 'drop_overfit'. This must be numeric", call. = FALSE)

  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

  pride_month_2023_begin <- as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2023_end <- as.POSIXct("2023-06-30", tryFormats = "%Y-%m-%d")

  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  if (outcome_colname %in% c("pick_rate_pooled", "pick_rate_mean")) {
    y_label <- "Pick rate"
  } else if (outcome_colname %in% c("pick_rate_pooled", "pick_rate_mean")) {
    y_label <- "Pick level"
  } else if (outcome_colname == "win_rate_pooled") {
    y_label <- "Win rate"
  } else if (outcome_colname == "gold_pooled") {
    y_label <- "Gold"
  } else if (outcome_colname == "assists_pooled") {
    y_label <- "Assists"
  } else if (outcome_colname == "kd_ratio") {
    y_label <- "Kills/deaths"
  }

  ## 1.) Construct synthetic outcomes and compute RMSEs.
  synth_outcomes <- lapply(pooled_results[!(names(pooled_results) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) { construct_synth_outcome(x$tau_hat, x$dta, "champion", "smooth_outcome", "day")$synth_outcome })
  synth_outcomes <- mapply(function(x, y) { x %>% mutate(champion = y) }, synth_outcomes, names(synth_outcomes), SIMPLIFY = FALSE) %>%
    dplyr::bind_rows()

  dta <- pooled_results[[1]]$dta %>%
    dplyr::filter(champion %in% champions) %>%
    dplyr::left_join(synth_outcomes, by = c("day", "champion")) %>%
    dplyr::mutate(gaps = smooth_outcome - synth_outcome)

  rmses_pre <- dta %>%
    dplyr::filter(day < treatment_date) %>%
    dplyr::group_by(champion) %>%
    dplyr::mutate(rmse_pre = Metrics::rmse(smooth_outcome, synth_outcome)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(champion, .keep_all = TRUE) %>%
    dplyr::select(champion, rmse_pre) %>%
    dplyr::arrange(rmse_pre)

  rmses_post <- dta %>%
    dplyr::filter(day > treatment_date) %>%
    dplyr::group_by(champion) %>%
    dplyr::mutate(rmse_post = Metrics::rmse(smooth_outcome, synth_outcome)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(champion, .keep_all = TRUE) %>%
    dplyr::select(champion, rmse_post) %>%
    dplyr::arrange(rmse_post)

  ## 2.) Drop champions with a too low pre-RMSE.
  to_keep <- rmses_pre %>%
    dplyr::mutate(drop = ifelse(rmse_pre < drop_overfit, 1, 0)) %>%
    dplyr::filter(!drop) %>%
    dplyr::pull(champion)

  ## 2.) Placebo plot.
  plot_2022_rainbow <- (as.Date(min(dta$day)) < as.Date(pride_month_2022_begin) + 1) & (as.Date(max(dta$day)) > as.Date(pride_month_2022_end) + 1)
  plot_2023_rainbow <- (as.Date(min(dta$day)) < as.Date(pride_month_2023_begin) + 1) & (as.Date(max(dta$day)) > as.Date(pride_month_2023_end) + 1)

  plot_dta_main <- dta %>%
    dplyr::filter(champion == main_champion)

  plot_dta_aux <- dta %>%
    dplyr::filter(champion %in% to_keep & champion != main_champion)

  plot_main <- plot_dta_aux %>%
    ggplot2::ggplot(ggplot2::aes(x = day, y = gaps, group = champion, color = "Controls")) +
    ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(linewidth = 0.2) +
    ggplot2::geom_line(data = plot_dta_main, ggplot2::aes(y = gaps, col = "Graves"), linewidth = 1) +
    ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    ggplot2::geom_hline(yintercept = 0, linetype = 4) +
    ggplot2::xlab("") + ggplot2::ylab("Gaps") +
    ggplot2::ylim(ylims[1], ylims[2]) +
    ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%Y") +
    ggplot2::scale_color_manual(name = "Colors", values = c("Controls" = "gray", "Graves" = "black"), breaks = c("Graves", "Controls")) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text = ggplot2::element_text(size = 10, face = "bold"),
                   legend.position = c(0.11, 0.9), legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))
  ggplot2::ggsave(paste0(save_here, "/", tolower(main_champion), "_placebo.pdf"), plot_main, width = 9, height = 7)

  ## 3. Histogram of pre- and post-treatment RMSEs.
  rmses_ratio <- rmses_pre %>%
    dplyr::filter(champion %in% to_keep) %>%
    dplyr::left_join(rmses_post, by = "champion") %>%
    dplyr::mutate(rmse_ratio = rmse_post / rmse_pre)

  plot_rmses_dta <- rmses_ratio %>%
    dplyr::arrange(desc(rmse_ratio)) %>%
    dplyr::mutate(champion = factor(champion, levels = champion))

  plot_ratios <- plot_rmses_dta %>%
    ggplot2::ggplot(ggplot2::aes(x = champion, y = rmse_ratio)) +
    ggplot2::geom_segment(ggplot2::aes(xend = champion, yend = 0),
                          color = ifelse(plot_rmses_dta$champion == main_champion , "orange", "grey"),
                          linewidth = ifelse(plot_rmses_dta$champion == main_champion, 1.3, 0.7)) +
    ggplot2::geom_point(color = ifelse(plot_rmses_dta$champion == main_champion , "orange", "grey"),
                        size = ifelse(plot_rmses_dta$champion == main_champion, 5, 2)) +
    ggplot2::xlab("") + ggplot2::ylab("RMSE ratio") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()
  ggplot2::ggsave(paste0(save_here, "/", tolower(main_champion), "_rmses_ratios.pdf"), plot_ratios, width = 9, height = 7)

  ## 4.) Talk to the user and output.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")

  return(list("pre" = rmses_pre, "post" = rmses_post))
}
