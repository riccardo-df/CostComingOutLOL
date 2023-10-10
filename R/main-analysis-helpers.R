#' Cost of Coming Out - Main Analysis
#'
#' Produced plots displaying the results of the main analysis of the Cost of Coming Out paper performed by \code{\link{run_main_pooled}}.
#'
#' @param pooled_results Output of \code{\link{run_main_pooled}}
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Save some nice plots.
#'
#' @import dplyr ggplot2 ggsci grDevices Cairo
#' @importFrom stats reorder
#' @importFrom gridExtra arrangeGrob
#' @importFrom lubridate year
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{produce_plots_regional}} \code{\link{produce_latex_pooled}} \code{\link{produce_latex_regional}}
#'
#' @export
produce_plots_pooled <- function(pooled_results, save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  outcome_colname <- pooled_results$outcome_colname
  estimator <- pooled_results$estimator
  donors <- pooled_results$donors
  treatment_date <- pooled_results$treatment_date
  year <- lubridate::year(treatment_date)
  n_back_days <- as.numeric(summary(pooled_results[[1]]$tau_hat)$dimensions["T0"] - summary(pooled_results[[1]]$tau_hat_back)$dimensions["T0"] - 1)
  treatment_date_back <- as.Date(treatment_date) - n_back_days
  champions <- names(pooled_results)[!(names(pooled_results) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))]

  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

  pride_month_2023_begin <- as.POSIXct("2023-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2023_end <- as.POSIXct("2023-06-30", tryFormats = "%Y-%m-%d")

  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  if (outcome_colname %in% c("pick_rate_pooled", "pick_rate_mean")) y_label <- "Pick rate" else y_label <- "Pick level"

  ## 1.) Construct synthetic outcomes.
  synth_outcomes <- lapply(pooled_results[!(names(pooled_results) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) { construct_synth_outcome(x$tau_hat, x$dta, "champion", "smooth_outcome", "day") })
  synth_outcomes_back <- lapply(pooled_results[!(names(pooled_results) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) { construct_synth_outcome(x$tau_hat_back, x$dta, "champion", "smooth_outcome", "day") })
  synth_outcomes_drop <- lapply(pooled_results[!(names(pooled_results) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) { lapply(x$tau_hat_drop, function(z) { construct_synth_outcome(z, x$dta, "champion", "smooth_outcome", "day") }) } )

  ## 2.) Champions' plots.
  for (i in seq_len(length(champions))) {
    # 2a.) Select champion and handle data accordingly.
    my_champion <- champions[i]

    dta <- pooled_results[[my_champion]]$dta

    plot_2022_rainbow <- (as.Date(min(dta$day)) < as.Date(pride_month_2022_begin) + 1) & (as.Date(max(dta$day)) > as.Date(pride_month_2022_end) + 1)
    plot_2023_rainbow <- (as.Date(min(dta$day)) < as.Date(pride_month_2023_begin) + 1) & (as.Date(max(dta$day)) > as.Date(pride_month_2023_end) + 1)

    if (my_champion == "LGB") {
      lgb_avg_outcome <- dta %>%
        dplyr::filter(champion %in% c("Nami", "Leona", "Diana", "Neeko")) %>%
        dplyr::group_by(day_no) %>%
        dplyr::mutate(lgb_smooth_outcome = mean(smooth_outcome)) %>%
        dplyr::distinct(day, .keep_all = TRUE) %>%
        dplyr::select(day, day_no, lgb_smooth_outcome)

      colnames(lgb_avg_outcome) <- c("day", "day_no", "smooth_outcome")
      lgb_avg_outcome$champion <- "LGB"

      dta <- dta %>%
        dplyr::select(day, day_no, smooth_outcome) %>%
        dplyr::bind_rows(lgb_avg_outcome)
    }

    plot_dta <- dta %>%
      dplyr::filter(champion == my_champion)

    # 2b.) Main fit.
    plot_main <- plot_dta %>%
      ggplot2::ggplot(ggplot2::aes(x = day, y = smooth_outcome, color = "Actual")) +
      ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_line(data = synth_outcomes[[my_champion]]$synth_outcome, ggplot2::aes(y = synth_outcome, col = "Synthetic"), linewidth = 1) +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::xlab("") + ggplot2::ylab(y_label) + ggplot2::ggtitle(if (my_champion == "LGB") "Composite LGB" else my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Actual" = "tomato")) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = c(0.11, 0.9), legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))
    ggplot2::ggsave(paste0(save_here, "/", tolower(my_champion), "_pooled_", estimator, "_", donors, "_main", year, ".svg"), plot_main, device = Cairo::CairoSVG, width = 7, height = 7)

    # 2b.) Weights for the main fit.
    plot_weights <- synth_outcomes[[my_champion]]$weights %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(champion, -sort(weight)), y = weight, fill = champion)) +
      ggplot2::geom_bar(position = "dodge", stat = "identity") +
      ggplot2::coord_flip() +
      ggsci::scale_fill_jco() +
      ggplot2::xlab("") + ggplot2::ylab("Weight") + ggplot2::ggtitle(if (my_champion == "LGB") "Composite LGB" else my_champion) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical")
    ggplot2::ggsave(paste0(save_here, "/", tolower(my_champion), "_pooled_", estimator, "_", donors, "_weights", year, ".svg"), plot_weights, device = Cairo::CairoSVG, width = 7, height = 7)

    # 2d.) Backdate exercise.
    plot_back <- plot_dta %>%
      ggplot2::ggplot(ggplot2::aes(x = day, y = smooth_outcome, color = "Actual")) +
      ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_line(data = synth_outcomes_back[[my_champion]]$synth_outcome, ggplot2::aes(y = synth_outcome, col = "Synthetic"), linewidth = 1) +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date_back), linetype = 4, col = "gray", linewidth = 1) +
      ggplot2::xlab("") + ggplot2::ylab(y_label) + ggplot2::ggtitle(if (my_champion == "LGB") "Composite LGB" else my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Actual" = "tomato")) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = c(0.11, 0.79), legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))

    # 2e.) Leave-one-out exercise.
    if (length(pooled_results[[my_champion]]$tau_hat_drop) != 0) {
      temp_drop <- synth_outcomes_drop[[my_champion]] %>%
        lapply(function(x) { x$synth_outcome }) %>%
        dplyr::bind_rows(.id = "groups")
      colnames(temp_drop)[1] <- "champion"

      plot_drop <- plot_dta %>%
        ggplot2::ggplot(ggplot2::aes(x = day, y = smooth_outcome, color = "Actual")) +
        ggplot2::annotation_raster(if (plot_2022_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
        ggplot2::annotation_raster(if (plot_2023_rainbow) rainbow else "white", xmin = as.POSIXct(pride_month_2023_begin), xmax = as.POSIXct(pride_month_2023_end), ymin = -Inf, ymax = Inf) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_line(data = synth_outcomes[[my_champion]]$synth_outcome, ggplot2::aes(y = synth_outcome, col = "Synthetic"), linewidth = 1) +
        ggplot2::geom_line(data = temp_drop, ggplot2::aes(y = synth_outcome, group = champion, col = "Synthetic LOO"), linetype = "dashed", linewidth = 0.5) +
        ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
        ggplot2::xlab("") + ggplot2::ylab(y_label) + ggplot2::ggtitle(if (my_champion == "LGB") "Other LGB" else my_champion) +
        ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
        ggplot2::theme_bw() +
        ggplot2::scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Synthetic LOO" = "gray", "Actual" = "tomato")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              legend.position = c(0.12, 0.77), legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))

      plot_robustness <- gridExtra::arrangeGrob(plot_back, plot_drop, ncol = 1)
    } else {
      plot_robustness <- plot_back
    }

    ggplot2::ggsave(paste0(save_here, "/", tolower(my_champion), "_pooled_", estimator, "_", donors, "_robustness", year, ".svg"), plot_robustness, device = Cairo::CairoSVG, width = 7, height = 7)
  }

  ## 4.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' Cost of Coming Out - Main Analysis
#'
#' Produced plots displaying the results of the main analysis of the Cost of Coming Out paper performed by \code{\link{run_main_regional}}.
#'
#' @param regional_results Output of \code{\link{run_main_regional}}.
#' @param save_here String denoting the path where to save the figures.
#'
#' @return
#' Save some nice plots.
#'
#' @import dplyr ggplot2 ggsci grDevices Cairo
#' @importFrom lubridate year
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{produce_plots_pooled}} \code{\link{produce_latex_pooled}} \code{\link{produce_latex_regional}}
#'
#' @export
produce_plots_regional <- function(regional_results, save_here = getwd()) {
  ## 0.) Handling inputs and checks.
  outcome_colname <- regional_results$outcome_colname
  estimator <- regional_results$estimator
  donors <- regional_results$donors
  treatment_date <- regional_results$treatment_date
  year <- lubridate::year(treatment_date)
  champions <- names(regional_results)[!(names(regional_results) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))]

  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")

  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  if (outcome_colname %in% c("pick_rate", "pick_rate")) y_label <- "Pick rate" else y_label <- "Pick level"

  ## 1.) Construct synthetic outcomes.
  synth_outcomes <- lapply(regional_results[!(names(regional_results) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) {
    mapply(function(y, z) { construct_synth_outcome(y, z, "champion", "smooth_outcome", "day") }, y = x$tau_hats, z = x$dtas, SIMPLIFY = FALSE)
  })

  ## 2.) Plot.
  for (i in seq_len(length(champions))) {
    my_champion <- champions[i]

    regional_panels <- regional_results[[my_champion]]$dtas

    if (my_champion == "LGB") {
      lgb_avg_outcome <- lapply(regional_panels, function(x) {
        x %>%
          dplyr::filter(champion %in% c("Nami", "Leona", "Diana", "Neeko")) %>%
          dplyr::group_by(day_no) %>%
          dplyr::mutate(lgb_smooth_outcome = mean(smooth_outcome)) %>%
          dplyr::distinct(day, .keep_all = TRUE) %>%
          select(region, day, day_no, lgb_smooth_outcome)
      })

      lgb_avg_outcome <- lapply(lgb_avg_outcome, function(x) {
        colnames(x) <- c("region", "day", "day_no", "smooth_outcome")
        x$champion <- "LGB"
        x
      })

      lgb_avg_outcome <- dplyr::bind_rows(lgb_avg_outcome)

      dta <- regional_panels %>%
        dplyr::bind_rows() %>%
        dplyr::select(region, day, day_no, champion, smooth_outcome) %>%
        dplyr::bind_rows(lgb_avg_outcome)
    } else {
      dta <- regional_panels %>%
        dplyr::bind_rows()
    }

    plot_dta <- dta %>%
      dplyr::filter(champion == my_champion)

    plot_synth_outcomes <- lapply(synth_outcomes[[my_champion]], function(x) { x$synth_outcome }) %>%
      dplyr::bind_rows(.id = "groups")
    colnames(plot_synth_outcomes)[1] <- "region"
    plot_synth_outcomes$region <- factor(plot_synth_outcomes$region, levels = c("Europe", "Korea", "Latin_America", "North_America"), labels = c("Europe", "Korea", "Latin America", "North America"))

    plot_main <- plot_dta %>%
      dplyr::mutate(region = factor(region, levels = c("Europe", "Korea", "Latin_America", "North_America"), labels = c("Europe", "Korea", "Latin America", "North America"))) %>%
      ggplot2::ggplot(ggplot2::aes(x = day, y = smooth_outcome, color = "Actual")) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::geom_line(data = plot_synth_outcomes, ggplot2::aes(y = synth_outcome, col = "Synthetic"), linewidth = 0.6) +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::facet_wrap(~region, ncol = 2) +
      ggplot2::xlab("") + ggplot2::ylab(y_label) + ggplot2::ggtitle(if (my_champion == "LGB") "Other LGB" else my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Actual" = "tomato")) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), strip.text.x = ggplot2::element_text(size = 15),
            legend.position = c(0.11, 0.38), legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))
    ggplot2::ggsave(paste0(save_here, "/", tolower(my_champion), "_regional_", estimator, "_", donors, "_main", year, ".svg"), plot_main, device = Cairo::CairoSVG, width = 7, height = 7)
  }

  ## 3.) Talk to the user.
  cat("\n")
  cat("Figures are saved at ", save_here, "\n", sep = "")
}


#' Cost of Coming Out - Main Analysis
#'
#' Produced LATEX code for a table displaying the results of the main analysis of the Cost of Coming Out paper performed by \code{\link{run_main_pooled}}.
#'
#' @param pooled_result_list A list storing the output of \code{\link{run_main_pooled}}. Each element must store the results obtained with a different donor pool. Everything else must be equal for the table to make sense.
#'
#' @return
#' Prints LATEX code for a nice table.
#'
#' @details
#' Different synthetic control specifications can be used by calling \code{\link{run_main_pooled}} multiple times in a for loop, with some parameters (e.g., donor pool, estimator)
#' changing over the iterations. \code{pooled_result_list} must be a list storing the results from a single iteration. Please order the loop according to the order you want to display in the table.\cr
#'
#' While some parameters can change, others must be fixed over the specifications. These include the treatment date, the champion assigned to the treatment, the outcome series, and the bandwidths.
#'
#' @import dplyr stringr
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{produce_plots_pooled}} \code{\link{produce_plots_regional}} \code{\link{produce_latex_regional}}
#'
#' @export
produce_latex_pooled <- function(pooled_result_list) {
  ## 0.) Handling inputs and checks.
  outcome_colnames <- sapply(pooled_result_list, function(x) { x$outcome_colname })
  outcome_colname <- outcome_colnames[1]
  treatment_dates <- lapply(pooled_result_list, function(x) { x$treatment_date })
  treatment_date <- treatment_dates[[1]]
  champions <- sapply(pooled_result_list, function(x) { names(x)[!(names(x) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))] })
  my_champion <- champions[1]
  bandwidths <- sapply(pooled_result_list, function(x) { x$bandwidth })
  bandwidth <- bandwidths[1]

  donor_pools <- sapply(pooled_result_list, function(x) { x$donors })
  estimators <- sapply(pooled_result_list, function(x) { x$estimator })
  dtas <- lapply(pooled_result_list, function(x) { x[[my_champion]]$dta })
  dta <- dtas[[1]] %>%
    dplyr::filter(champion == my_champion & day < treatment_date) %>%
    dplyr::select(day, smooth_outcome)

  if (length(unique(outcome_colnames)) != 1) stop("'pooled_result_list' has been constructed using different treatment dates. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(unlist(treatment_dates))) != 1) stop("'pooled_result_list' has been constructed using different treatment dates. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(champions)) != 1) stop("'pooled_result_list' has been constructed using different treated champions. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(bandwidths)) != 1) stop("'pooled_result_list' has been constructed using different bandwidths. Maybe you want two different tables.", call. = FALSE)

  synth_outcomes_dta <- lapply(pooled_result_list, function(z) { lapply(z[!(names(z) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) { construct_synth_outcome(x$tau_hat, x$dta, "champion", "smooth_outcome", "day") }) })
  synth_outcomes <- lapply(synth_outcomes_dta, function(x) { x[[my_champion]]$synth_outcome %>% dplyr::filter(day < treatment_date) })
  n_donors <- sapply(synth_outcomes_dta, function(x) { x[[my_champion]]$weights %>% nrow()})

  ## 1.) Print LATEX code.
  n_col <- length(pooled_result_list) + 1
  n_estimators <- length(unique(estimators))
  n_donor_pools <- length(unique(donor_pools))

  donor_pools[donor_pools == "all"] <- "All champions"
  donor_pools[donor_pools == "adc"] <- "Only ADC"
  donor_pools[donor_pools == "support"] <- "Only Support"
  donor_pools[donor_pools == "jungle"] <- "Only Jungle"
  donor_pools[donor_pools == "top"] <- "Only Top"
  donor_pools[donor_pools == "middle"] <- "Only Middle"

  estimators[estimators == "sc"] <- "Synthetic Controls"
  estimators[estimators == "sc_reg"] <- "Regularized Synthetic Controls"

  cmid_points_start <- seq(2, n_donor_pools * n_estimators, by = n_donor_pools)
  cmid_points_end <- seq(2+n_donor_pools-1, n_col, by = n_donor_pools)

  tau_hats <- sapply(pooled_result_list, function(x) { x[[my_champion]]$tau_hat })
  ses <- sapply(pooled_result_list, function(x) { x[[my_champion]]$se_tau_hat })

  if (is.list(ses)) {
    cils <- cius <- rep(NA, n_col)
  } else {
    cils <- trimws(format(round(tau_hats - 1.96 * ses, 3), nsmall = 3))
    cius <- trimws(format(round(tau_hats + 1.96 * ses, 3), nsmall = 3))
  }

  tau_hats <- format(round(tau_hats, 3), nsmall = 3)
  rmses <- format(round(sapply(synth_outcomes, function(x) { Metrics::rmse(dta$smooth_outcome, x$synth_outcome) }), 3), nsmall = 3)

  cat("\\begingroup
  \\setlength{\\tabcolsep}{8pt}
  \\renewcommand{\\arraystretch}{1.1}
  \\begin{table}[b!]
    \\centering
    \\begin{adjustbox}{width = 1\\textwidth}
    \\begin{tabular}{@{\\extracolsep{5pt}}l ", rep("c ", n_col), "}
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      & ", stringr::str_sub(paste(paste0("\\multicolumn{", n_donor_pools, "}{c}{\\textit{", unique(estimators), "}} &"), collapse = " "), end = -3), " \\\\ ", paste0("\\cmidrule{", cmid_points_start, "-", cmid_points_end, "} "), "
      & ", stringr::str_sub(paste(paste(paste0(donor_pools, " &"), collapse = " "), collapse = " "), end = -3), " \\\\
      \\addlinespace[2pt]
      \\hline \\\\[-1.8ex] \n\n", sep = "")

  cat("      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils, ", ", cius, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses, " &"), collapse = " "), end = -3), " \\\\ \n", sep = "")

  cat("\n      \\addlinespace[3pt]
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      \\end{tabular}
      \\end{adjustbox}
      \\caption{Point estimates and $95\\%$ confidence intervals for $\\hat{\\tau}$. Additionally, the number of donors receiving a non-zero weight and the pre-treatment root mean squared error are displayed.}
      \\label{table_estimation_results_pooled}
    \\end{table}
\\endgroup")
}


#' Cost of Coming Out - Main Analysis
#'
#' Produced LATEX code for a table displaying the results of the main analysis of the Cost of Coming Out paper performed by \code{\link{run_main_regional}}.
#'
#' @param regional_result_list A list storing the output of \code{\link{run_main_regional}}. Each element must store the results obtained with a different donor pool. Everything else must be equal for the table to make sense.
#'
#' @return
#' Prints LATEX code for a nice table.
#'
#' @details
#' Different synthetic control specifications can be used by calling \code{\link{run_main_pooled}} multiple times in a for loop, with some parameters (e.g., donor pool, estimator)
#' changing over the iterations. \code{regional_result_list} must be a list storing the results from a single iteration. Please order the loop according to the order you want to display in the table.\cr
#'
#' While some parameters can change, others must be fixed over the specifications. These include the treatment date, the champion assigned to the treatment, the outcome series, and the bandwidths.
#'
#' @import dplyr stringr Metrics
#' @importFrom stats setNames
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{produce_plots_pooled}} \code{\link{produce_plots_regional}} \code{\link{produce_latex_pooled}}
#'
#' @export
produce_latex_regional <- function(regional_result_list) {
  ## 0.) Handling inputs and checks.
  outcome_colnames <- sapply(regional_result_list, function(x) { x$outcome_colname })
  outcome_colname <- outcome_colnames[1]
  treatment_dates <- lapply(regional_result_list, function(x) { x$treatment_date })
  treatment_date <- treatment_dates[[1]]
  champions <- sapply(regional_result_list, function(x) { names(x)[!(names(x) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))] })
  my_champion <- champions[1]
  bandwidths <- sapply(regional_result_list, function(x) { x$bandwidth })
  bandwidth <- bandwidths[1]

  donor_pools <- sapply(regional_result_list, function(x) { x$donors })
  estimators <- sapply(regional_result_list, function(x) { x$estimator })
  dtas <- lapply(regional_result_list, function(x) { dplyr::bind_rows(x[[my_champion]]$dta) })
  regional_dta <- dtas[[1]] %>%
    dplyr::filter(champion == my_champion & day < treatment_date) %>%
    dplyr::select(region, day, smooth_outcome)

  if (length(unique(outcome_colnames)) != 1) stop("'regional_result_list' has been constructed using different treatment dates. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(unlist(treatment_dates))) != 1) stop("'regional_result_list' has been constructed using different treatment dates. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(champions)) != 1) stop("'regional_result_list' has been constructed using different treated champions. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(bandwidths)) != 1) stop("'regional_result_list' has been constructed using different bandwidths. Maybe you want two different tables.", call. = FALSE)

  synth_outcomes_dta <- lapply(regional_result_list, function(x) {
    lapply(x[!(names(x) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) {
      mapply(function(y, z) { construct_synth_outcome(y, z, "champion", "smooth_outcome", "day") }, y = x$tau_hats, z = x$dtas, SIMPLIFY = FALSE)
    })
  })

  synth_outcomes <- lapply(synth_outcomes_dta, function(x) { lapply(x[[my_champion]], function(y) { y$synth_outcome %>% dplyr::filter(day < treatment_date) }) %>% dplyr::bind_rows(.id = "groups") })
  synth_outcomes <- lapply(synth_outcomes, stats::setNames, c("region", "day", "synth_outcome"))
  n_donors <- lapply(synth_outcomes_dta, function(x) { lapply(x[[my_champion]], function(y) { y$weights %>% nrow() }) %>% unlist() })

  ## 1.) Print LATEX code.
  n_col <- length(regional_result_list) + 1
  n_estimators <- length(unique(estimators))
  n_donor_pools <- length(unique(donor_pools))

  donor_pools[donor_pools == "all"] <- "All champions"
  donor_pools[donor_pools == "adc"] <- "Only ADC"
  donor_pools[donor_pools == "support"] <- "Only Support"
  donor_pools[donor_pools == "jungle"] <- "Only Jungle"
  donor_pools[donor_pools == "top"] <- "Only Top"
  donor_pools[donor_pools == "middle"] <- "Only Middle"

  estimators[estimators == "sc"] <- "Synthetic Controls"
  estimators[estimators == "sc_reg"] <- "Regularized Synthetic Controls"

  cmid_points_start <- seq(2, n_donor_pools * n_estimators, by = n_donor_pools)
  cmid_points_end <- seq(2+n_donor_pools-1, n_col, by = n_donor_pools)

  tau_hats <- lapply(regional_result_list, function(x) { x[[my_champion]]$tau_hats %>% unlist() })
  ses <- lapply(regional_result_list, function(x) { x[[my_champion]]$ses %>% unlist() })

  if (length(ses) == 1) {
    cils <- cius <- vector("list", length = n_col)
  } else {
    cils <- format(round(mapply(function(x, y) { x - 1.96 * y }, x = tau_hats, y = ses), 3), nsmall = 3)
    cius <- format(round(mapply(function(x, y) { x + 1.96 * y }, x = tau_hats, y = ses), 3), nsmall = 3)
  }

  tau_hats <- lapply(tau_hats, function(x) { format(round(x, 3), nsmall = 3) })
  rmses <- lapply(synth_outcomes, function(x) {
    x %>% left_join(regional_dta, by = c("region", "day")) %>% dplyr::group_by(region) %>% dplyr::mutate(rmse = Metrics::rmse(smooth_outcome, synth_outcome)) %>% dplyr::pull(rmse) %>% unique() })

  tau_hats_europe <- sapply(tau_hats, function(x) { x["Europe"] })
  cils_europe <- cils[1, ]
  cius_europe <- cius[1, ]
  n_donors_europe <- sapply(n_donors, function(x) { x["Europe"] })
  rmses_europe <- trimws(format(round(sapply(rmses, function(x) { x[1] }), 3), nsmall = 3))

  tau_hats_korea <- sapply(tau_hats, function(x) { x["Korea"] })
  cils_korea <- cils[2, ]
  cius_korea <- cius[2, ]
  n_donors_korea <- sapply(n_donors, function(x) { x["Korea"] })
  rmses_korea <- trimws(format(round(sapply(rmses, function(x) { x[2] }), 3), nsmall = 3))

  tau_hats_latin_am <- sapply(tau_hats, function(x) { x["Latin_America"] })
  cils_latin_am <- cils[3, ]
  cius_latin_am <- cius[3, ]
  n_donors_latin_am <- sapply(n_donors, function(x) { x["Latin_America"] })
  rmses_latin_am <- trimws(format(round(sapply(rmses, function(x) { x[3] }), 3), nsmall = 3))

  tau_hats_north_am <- sapply(tau_hats, function(x) { x["North_America"] })
  cils_north_am <- cils[4, ]
  cius_north_am <- cius[4, ]
  n_donors_north_am <- sapply(n_donors, function(x) { x["North_America"] })
  rmses_north_am <- trimws(format(round(sapply(rmses, function(x) { x[4] }), 3), nsmall = 3))

  cat("\\begingroup
  \\setlength{\\tabcolsep}{8pt}
  \\renewcommand{\\arraystretch}{1.1}
  \\begin{table}[b!]
    \\centering
    \\begin{adjustbox}{width = 1\\textwidth}
    \\begin{tabular}{@{\\extracolsep{5pt}}l ", rep("c ", n_col), "}
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      & ", stringr::str_sub(paste(paste0("\\multicolumn{", n_donor_pools, "}{c}{\\textit{", unique(estimators), "}} &"), collapse = " "), end = -3), " \\\\ ", paste0("\\cmidrule{", cmid_points_start, "-", cmid_points_end, "} "), "
      & ", stringr::str_sub(paste(paste(paste0(donor_pools, " &"), collapse = " "), collapse = " "), end = -3), " \\\\
      \\addlinespace[2pt]
      \\hline \\\\[-1.8ex] \n\n", sep = "")

  cat("      \\multicolumn{", n_col, "}{l}{\\textbf{\\small Panel 1: \\textit{Europe}}} \\\\
      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats_europe, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils_europe, ", ", cius_europe, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors_europe, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses_europe, " &"), collapse = " "), end = -3), " \\\\ \\cmidrule{", 1, "-", n_col, "} \n\n", sep = "")

  cat("      \\multicolumn{", n_col, "}{l}{\\textbf{\\small Panel 2: \\textit{Korea}}} \\\\
      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats_korea, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils_korea, ", ", cius_korea, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors_korea, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses_korea, " &"), collapse = " "), end = -3), " \\\\ \\cmidrule{", 1, "-", n_col, "} \n\n", sep = "")

  cat("      \\multicolumn{", n_col, "}{l}{\\textbf{\\small Panel 3: \\textit{Latin America}}} \\\\
      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats_latin_am, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils_latin_am, ", ", cius_latin_am, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors_latin_am, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses_latin_am, " &"), collapse = " "), end = -3), " \\\\ \\cmidrule{", 1, "-", n_col, "} \n\n", sep = "")

  cat("      \\multicolumn{", n_col, "}{l}{\\textbf{\\small Panel 4: \\textit{North America}}} \\\\
      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats_north_am, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils_north_am, ", ", cius_north_am, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors_north_am, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses_north_am, " &"), collapse = " "), end = -3), " \\\\ \n\n", sep = "")

  cat("\n      \\addlinespace[3pt]
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      \\end{tabular}
      \\end{adjustbox}
      \\caption{Point estimates and $95\\%$ confidence intervals for $\\hat{\\tau}$. Additionally, the number of donors receiving a non-zero weight and the pre-treatment root mean squared error are displayed. Each panel reports reports the results obtained using only matches from a particular region. Each column corresponds to a different specification, with the specifications differing solely in the employed estimator and donor pool.}
      \\label{table_estimation_results_regional}
    \\end{table}
\\endgroup")
}



#' Cost of Coming Out - Main Analysis
#'
#' Produced LATEX code for a table displaying the results of the main analysis of the Cost of Coming Out paper performed by \code{\link{run_main_pooled}} and \code{\link{run_main_regional}}.
#'
#' @param pooled_result_list A list storing the output of \code{\link{run_main_pooled}}. Each element must store the results obtained with a different donor pool or estimator. Everything else must be equal for the table to make sense.
#' @param regional_result_list A list storing the output of \code{\link{run_main_regional}}. Each element must store the results obtained with a different donor pool or estimator. Everything else must be equal for the table to make sense.
#'
#' @return
#' Prints LATEX code for a nice table.
#'
#' @details
#' Different synthetic control specifications (e.g., donor pool, estimator) can be used when calling \code{\link{run_main_pooled}} and \code{\link{run_main_regional}}. \code{pooled_result_list} and
#' \code{regional_result_list} must be lists with each element storing the results from a single specification. Please make sure that the lists use the specification order. Failure to comply will raise an error.\cr
#'
#' The order of the columns of the table will follow the order of the lists.\cr
#'
#' While some parameters can change, others must be fixed over the specifications. These include the treatment date, the champion assigned to the treatment, the outcome series, and the bandwidths.
#'
#' @import dplyr stringr Metrics
#' @importFrom stats setNames
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{produce_plots_pooled}} \code{\link{produce_plots_regional}} \code{\link{produce_latex_pooled}}
#'
#' @export
produce_latex <- function(pooled_result_list, regional_result_list) {
  ## 0.) Handling inputs and checks.
  # Pooled.
  outcome_colnames_pooled <- sapply(pooled_result_list, function(x) { x$outcome_colname })
  outcome_colname_pooled <- if (outcome_colnames_pooled[1] == "pick_level_sum") "Pick level" else if (outcome_colnames_pooled[1] == "pick_rate_pooled") "Pick rate"
  treatment_dates_pooled <- lapply(pooled_result_list, function(x) { x$treatment_date })
  treatment_date_pooled <- treatment_dates_pooled[[1]]
  champions_pooled <- sapply(pooled_result_list, function(x) { names(x)[!(names(x) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))] })
  my_champion_pooled <- champions_pooled[1]
  bandwidths_pooled <- sapply(pooled_result_list, function(x) { x$bandwidth })
  bandwidth_pooled <- bandwidths_pooled[1]

  donor_pools_pooled <- sapply(pooled_result_list, function(x) { x$donors })
  estimators_pooled <- sapply(pooled_result_list, function(x) { x$estimator })
  dtas_pooled <- lapply(pooled_result_list, function(x) { x[[my_champion_pooled]]$dta })
  dta_pooled <- dtas_pooled[[1]] %>%
    dplyr::filter(champion == my_champion_pooled & day < treatment_date) %>%
    dplyr::select(day, smooth_outcome)

  if (length(unique(outcome_colnames_pooled)) != 1) stop("'pooled_result_list' has been constructed using different treatment dates. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(unlist(treatment_dates_pooled))) != 1) stop("'pooled_result_list' has been constructed using different treatment dates. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(champions_pooled)) != 1) stop("'pooled_result_list' has been constructed using different treated champions. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(bandwidths_pooled)) != 1) stop("'pooled_result_list' has been constructed using different bandwidths. Maybe you want two different tables.", call. = FALSE)

  synth_outcomes_dta_pooled <- lapply(pooled_result_list, function(z) { lapply(z[!(names(z) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) { construct_synth_outcome(x$tau_hat, x$dta, "champion", "smooth_outcome", "day") }) })
  synth_outcomes_pooled <- lapply(synth_outcomes_dta_pooled, function(x) { x[[my_champion_pooled]]$synth_outcome %>% dplyr::filter(day < treatment_date) })
  n_donors_pooled <- sapply(synth_outcomes_dta_pooled, function(x) { x[[my_champion_pooled]]$weights %>% nrow()})

  # Regional.
  outcome_colnames_regional <- sapply(regional_result_list, function(x) { x$outcome_colname })
  outcome_colname_regional <- if (outcome_colnames_regional[1] == "pick_level") "Pick level" else if (outcome_colnames_regional[1] == "pick_rate") "Pick rate"
  treatment_dates_regional <- lapply(regional_result_list, function(x) { x$treatment_date })
  treatment_date_regional <- treatment_dates_regional[[1]]
  champions_regional <- sapply(regional_result_list, function(x) { names(x)[!(names(x) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))] })
  my_champion_regional <- champions_regional[1]
  bandwidths_regional <- sapply(regional_result_list, function(x) { x$bandwidth })
  bandwidth_regional <- bandwidths_regional[1]

  donor_pools_regional <- sapply(regional_result_list, function(x) { x$donors })
  estimators_regional <- sapply(regional_result_list, function(x) { x$estimator })
  dtas_regional <- lapply(regional_result_list, function(x) { dplyr::bind_rows(x[[my_champion_regional]]$dta) })
  regional_dta <- dtas_regional[[1]] %>%
    dplyr::filter(champion == my_champion_pooled & day < treatment_date) %>%
    dplyr::select(region, day, smooth_outcome)

  if (length(unique(outcome_colnames_regional)) != 1) stop("'regional_result_list' has been constructed using different treatment dates. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(unlist(treatment_dates_regional))) != 1) stop("'regional_result_list' has been constructed using different treatment dates. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(champions_regional)) != 1) stop("'regional_result_list' has been constructed using different treated champions. Maybe you want two different tables.", call. = FALSE)
  if (length(unique(bandwidths_regional)) != 1) stop("'regional_result_list' has been constructed using different bandwidths. Maybe you want two different tables.", call. = FALSE)

  synth_outcomes_dta_regional <- lapply(regional_result_list, function(x) {
    lapply(x[!(names(x) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) {
      mapply(function(y, z) { construct_synth_outcome(y, z, "champion", "smooth_outcome", "day") }, y = x$tau_hats, z = x$dtas, SIMPLIFY = FALSE)
    })
  })

  synth_outcomes_regional <- lapply(synth_outcomes_dta_regional, function(x) { lapply(x[[my_champion_regional]], function(y) { y$synth_outcome %>% dplyr::filter(day < treatment_date) }) %>% dplyr::bind_rows(.id = "groups") })
  synth_outcomes_regional <- lapply(synth_outcomes_regional, stats::setNames, c("region", "day", "synth_outcome"))
  n_donors_regional <- lapply(synth_outcomes_dta_regional, function(x) { lapply(x[[my_champion_regional]], function(y) { y$weights %>% nrow() }) %>% unlist() })

  # Pooled/Regional.
  if (length(pooled_result_list) != length(regional_result_list)) stop("'pooled_result_list' and 'regional_result_list' are of different lengths.", call. = FALSE)
  if (outcome_colname_pooled != outcome_colname_regional) stop("Different outcomes have been used in the pooled and regional specifications. Please change that.", call. = FALSE)
  if (treatment_date_pooled != treatment_date_regional) stop("Different treatment dates have been used in the pooled and regional specifications. Please change that.", call. = FALSE)
  if (my_champion_pooled != my_champion_regional) stop("Different outcomes have been used in the pooled and regional specifications. Please change that.", call. = FALSE)
  if (outcome_colname_pooled != outcome_colname_regional) stop("Different outcomes have been used in the pooled and regional specifications. Please change that.", call. = FALSE)
  if (bandwidth_regional != bandwidth_regional) stop("Different outcomes have been used in the pooled and regional specifications. Please change that.", call. = FALSE)
  if (sum(mapply(function(x, y) { x != y }, x = donor_pools_pooled, y = donor_pools_regional)) > 0) stop("There are issues with the ordering of the lists. Check the donor pool specifications.", call. = FALSE)
  if (sum(mapply(function(x, y) { x != y }, x = estimators_pooled, y = estimators_regional)) > 0) stop("There are issues with the ordering of the lists. Check the estimator specifications.", call. = FALSE)
  if (sum(mapply(function(x, y) { x != y }, x = donor_pools_pooled, y = donor_pools_regional)) > 0) stop("There are issues with the ordering of the lists. Check the donor pool specifications.", call. = FALSE)

  ## 1.) Print LATEX code.
  n_col <- length(pooled_result_list) + 1
  n_estimators <- length(unique(estimators))
  donor_pools <- rename_latex(donor_pools_pooled)
  n_donor_pools <- length(unique(donor_pools))
  my_champion <- my_champion_regional

  donor_pools[donor_pools == "all"] <- "All champions"
  donor_pools[donor_pools == "adc"] <- "Only ADC"
  donor_pools[donor_pools == "support"] <- "Only Support"
  donor_pools[donor_pools == "jungle"] <- "Only Jungle"
  donor_pools[donor_pools == "top"] <- "Only Top"
  donor_pools[donor_pools == "middle"] <- "Only Middle"

  estimators[estimators == "sc"] <- "Synthetic Controls"
  estimators[estimators == "sc_reg"] <- "Regularized Synthetic Controls"

  cmid_points_start <- seq(2, n_donor_pools * n_estimators, by = n_donor_pools)
  cmid_points_end <- seq(2+n_donor_pools-1, n_col, by = n_donor_pools)

  tau_hats_pooled <- sapply(pooled_result_list, function(x) { x[[my_champion]]$tau_hat })
  ses_pooled <- sapply(pooled_result_list, function(x) { x[[my_champion]]$se_tau_hat })

  if (is.list(ses_pooled)) {
    cils_pooled <- cius_pooled <- rep(NA, n_col)
  } else {
    cils_pooled <- trimws(format(round(tau_hats_pooled - 1.96 * ses_pooled, 3), nsmall = 3))
    cius_pooled <- trimws(format(round(tau_hats_pooled + 1.96 * ses_pooled, 3), nsmall = 3))
  }

  tau_hats_pooled <- format(round(tau_hats_pooled, 3), nsmall = 3)
  rmses_pooled <- format(round(sapply(synth_outcomes_pooled, function(x) { Metrics::rmse(dta_pooled$smooth_outcome, x$synth_outcome) }), 3), nsmall = 3)

  tau_hats_regional <- lapply(regional_result_list, function(x) { x[[my_champion]]$tau_hats %>% unlist() })
  ses_regional <- lapply(regional_result_list, function(x) { x[[my_champion]]$ses %>% unlist() })

  if (length(ses_regional) == 1) {
    cils_regional <- cius_regional <- vector("list", length = n_col)
  } else {
    cils_regional <- format(round(mapply(function(x, y) { x - 1.96 * y }, x = tau_hats_regional, y = ses_regional), 3), nsmall = 3)
    cius_regional <- format(round(mapply(function(x, y) { x + 1.96 * y }, x = tau_hats_regional, y = ses_regional), 3), nsmall = 3)
  }

  tau_hats_regional <- lapply(tau_hats_regional, function(x) { format(round(x, 3), nsmall = 3) })
  rmses_regional <- lapply(synth_outcomes_regional, function(x) {
    x %>% left_join(regional_dta, by = c("region", "day")) %>% dplyr::group_by(region) %>% dplyr::mutate(rmse = Metrics::rmse(smooth_outcome, synth_outcome)) %>% dplyr::pull(rmse) %>% unique() })

  tau_hats_europe <- sapply(tau_hats_regional, function(x) { x["Europe"] })
  cils_europe <- cils_regional[1, ]
  cius_europe <- cius_regional[1, ]
  n_donors_europe <- sapply(n_donors_regional, function(x) { x["Europe"] })
  rmses_europe <- trimws(format(round(sapply(rmses_regional, function(x) { x[1] }), 3), nsmall = 3))

  tau_hats_korea <- sapply(tau_hats_regional, function(x) { x["Korea"] })
  cils_korea <- cils_regional[2, ]
  cius_korea <- cius_regional[2, ]
  n_donors_korea <- sapply(n_donors_regional, function(x) { x["Korea"] })
  rmses_korea <- trimws(format(round(sapply(rmses_regional, function(x) { x[2] }), 3), nsmall = 3))

  tau_hats_latin_am <- sapply(tau_hats_regional, function(x) { x["Latin_America"] })
  cils_latin_am <- cils_regional[3, ]
  cius_latin_am <- cius_regional[3, ]
  n_donors_latin_am <- sapply(n_donors_regional, function(x) { x["Latin_America"] })
  rmses_latin_am <- trimws(format(round(sapply(rmses_regional, function(x) { x[3] }), 3), nsmall = 3))

  tau_hats_north_am <- sapply(tau_hats_regional, function(x) { x["North_America"] })
  cils_north_am <- cils_regional[4, ]
  cius_north_am <- cius_regional[4, ]
  n_donors_north_am <- sapply(n_donors_regional, function(x) { x["North_America"] })
  rmses_north_am <- trimws(format(round(sapply(rmses_regional, function(x) { x[4] }), 3), nsmall = 3))

  cat("\\begingroup
  \\setlength{\\tabcolsep}{8pt}
  \\renewcommand{\\arraystretch}{1.1}
  \\begin{table}[b!]
    \\centering
    \\begin{adjustbox}{width = 1\\textwidth}
    \\begin{tabular}{@{\\extracolsep{5pt}}l ", rep("c ", n_col), "}
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      & ", stringr::str_sub(paste(paste0("\\multicolumn{", n_donor_pools, "}{c}{\\textit{", unique(estimators), "}} &"), collapse = " "), end = -3), " \\\\ ", paste0("\\cmidrule{", cmid_points_start, "-", cmid_points_end, "} "), "
      & ", stringr::str_sub(paste(paste(paste0(donor_pools, " &"), collapse = " "), collapse = " "), end = -3), " \\\\
      \\addlinespace[2pt]
      \\hline \\\\[-1.8ex] \n\n", sep = "")

  cat("      \\multicolumn{", n_col, "}{l}{\\textbf{\\small Panel 1: \\textit{All}}} \\\\
      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats_pooled, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils_pooled, ", ", cius_pooled, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors_pooled, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses_pooled, " &"), collapse = " "), end = -3), " \\\\ \\cmidrule{", 1, "-", n_col, "} \n\n", sep = "")

  cat("      \\multicolumn{", n_col, "}{l}{\\textbf{\\small Panel 2: \\textit{Europe}}} \\\\
      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats_europe, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils_europe, ", ", cius_europe, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors_europe, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses_europe, " &"), collapse = " "), end = -3), " \\\\ \\cmidrule{", 1, "-", n_col, "} \n\n", sep = "")

  cat("      \\multicolumn{", n_col, "}{l}{\\textbf{\\small Panel 3: \\textit{Korea}}} \\\\
      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats_korea, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils_korea, ", ", cius_korea, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors_korea, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses_korea, " &"), collapse = " "), end = -3), " \\\\ \\cmidrule{", 1, "-", n_col, "} \n\n", sep = "")

  cat("      \\multicolumn{", n_col, "}{l}{\\textbf{\\small Panel 4: \\textit{Latin America}}} \\\\
      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats_latin_am, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils_latin_am, ", ", cius_latin_am, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors_latin_am, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses_latin_am, " &"), collapse = " "), end = -3), " \\\\ \\cmidrule{", 1, "-", n_col, "} \n\n", sep = "")

  cat("      \\multicolumn{", n_col, "}{l}{\\textbf{\\small Panel 5: \\textit{North America}}} \\\\
      $\\hat{\\tau}$ & ", stringr::str_sub(paste(paste0(tau_hats_north_am, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", stringr::str_sub(paste(paste0("[", cils_north_am, ", ", cius_north_am, "] &"), collapse = " "), end = -3), " \\\\
      N. Donors & ", stringr::str_sub(paste(paste0(n_donors_north_am, " &"), collapse = " "), end = -3), " \\\\
      RMSE & ", stringr::str_sub(paste(paste0(rmses_north_am, " &"), collapse = " "), end = -3), " \\\\ \n\n", sep = "")

  cat("\n      \\addlinespace[3pt]
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      \\end{tabular}
      \\end{adjustbox}
      \\caption{Point estimates and $95\\%$ confidence intervals for $\\hat{\\tau}$. Additionally, the number of donors receiving a non-zero weight and the pre-treatment root mean squared error are displayed. The first panel reports the results obtained using all the observed matches. The remaining four panels report the results obtained using only matches from a particular region. Each column corresponds to a different specification, with the specifications differing solely in the employed estimator and donor pool.}
      \\label{table_estimation_results}
    \\end{table}
\\endgroup")
}
