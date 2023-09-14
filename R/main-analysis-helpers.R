#' Construct Donor Pool
#'
#' Subsets the provided data to include only the champion under investigation and the desired units in the donor pool.
#'
#' @param dta One of the champion data sets bundled in the package (\code{\link{lol_champ_pool_dta}} or \code{\link{lol_champ_dta}}).
#' @param donor_pool Which units to include in the donor pool. Must be either "all" or "non_lgb". The latter excludes Nami, Leona, Diana, and Neeko.
#' @param my_champion Champion under investigation. We need to include it as well.
#'
#' @import dplyr
#'
#' @author Riccardo Di Francesco
#'
#' @keywords internal
construct_donor_pool <- function(dta, donor_pool, my_champion) {
  ## 0.) Handling inputs and checks.
  if (!(donor_pool %in% c("all", "non_lgb"))) stop("Invalid 'donor_pool'. This must be either 'all' or 'non_lgb'.", call. = FALSE)

  ## 2.) Subset dta.
  if (donor_pool == "all") {
    my_subset <- dta
  } else if (donor_pool == "non_lgb") {
    lgb_champions <- c("Graves", "Nami", "Leona", "Diana", "Neeko")
    exclude_these <- lgb_champions[lgb_champions != my_champion]

    my_subset <- dta %>%
      dplyr::filter(!(champion %in% exclude_these))
  }

  ## 3.) Output.
  return(my_subset)
}


#' Cost of Coming Out - Main Analysis
#'
#' Produced plots displaying the results of the main analysis of the Cost of Coming Out paper performed by \code{\link{run_main_pooled}}.
#'
#' @param pooled_results Output of \code{\link{run_main_pooled}}.
#'
#' @return
#' Save some nice plots.
#'
#' @import dplyr ggplot2 ggsci gridExtra grDevices stats Cairo
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{produce_plots_regional}}
#'
#' @export
produce_plots_pooled <- function(pooled_results) {
  ## 0.) Handling inputs and checks.
  outcome_colname <- pooled_results$outcome_colname
  dta <- pooled_results$dta
  treatment_date <- pooled_results$treatment_date
  n_back_days <- as.numeric(summary(pooled_results[[1]]$tau_hat)$dimensions["T0"] - summary(pooled_results[[1]]$tau_hat_back)$dimensions["T0"] - 1)
  treatment_date_back <- as.Date(treatment_date) - n_back_days
  champions <- names(pooled_results)[!(names(pooled_results) %in% c("outcome_colname", "dta", "treatment_date"))]

  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")
  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  if (outcome_colname %in% c("pick_rate_pooled", "pick_rate_mean")) y_label <- "Pick rate" else y_label <- "Pick level"

  ## 1.) Construct synthetic outcomes.
  synth_outcomes <- lapply(pooled_results[!(names(pooled_results) %in% c("outcome_colname", "dta", "treatment_date"))], function(x) { construct_synth_outcome(x$tau_hat, dta, "champion", outcome_colname, "day") })
  synth_outcomes_back <- lapply(pooled_results[!(names(pooled_results) %in% c("outcome_colname", "dta", "treatment_date"))], function(x) { construct_synth_outcome(x$tau_hat_back, dta, "champion", outcome_colname, "day") })
  synth_outcomes_drop <- lapply(pooled_results[!(names(pooled_results) %in% c("outcome_colname", "dta", "treatment_date"))], function(x) { lapply(x$tau_hat_drop, function(z) { construct_synth_outcome(z, dta, "champion", outcome_colname, "day") }) } )

  ## 2.) Champions' plots and RMSE.
  for (i in seq_len(length(champions))) {
    # 2a.) Select champion and handle data accordingly.
    my_champion <- champions[i]

    if (my_champion == "LGB") {
      lgb_avg_outcome <- dta %>%
        dplyr::filter(champion %in% c("Nami", "Leona", "Diana", "Neeko")) %>%
        dplyr::group_by(day_no) %>%
        dplyr::mutate(lgb_pick_level_sum = mean(pick_level_sum),
                      lgb_pick_rate_pooled = mean(pick_rate_pooled)) %>%
        dplyr::distinct(day, .keep_all = TRUE) %>%
        dplyr::select(day, day_no, lgb_pick_level_sum, lgb_pick_rate_pooled)

      colnames(lgb_avg_outcome) <- c("day", "day_no", "pick_level_sum", "pick_rate_pooled")
      lgb_avg_outcome$champion <- "LGB"

      dta <- dta %>%
        dplyr::select(day, day_no, pick_level_sum, pick_rate_pooled, champion) %>%
        dplyr::bind_rows(lgb_avg_outcome)
    }

    plot_dta <- dta %>%
      dplyr::filter(champion == my_champion)

    # 2b.) Main fit.
    plot_main <- plot_dta %>%
      ggplot2::ggplot(ggplot2::aes(x = day, y = .data[[outcome_colname]], color = "Actual")) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_line(data = synth_outcomes[[my_champion]]$synth_outcome, ggplot2::aes(y = synth_outcome, col = "Synthetic"), linewidth = 1) +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::xlab("") + ggplot2::ylab(y_label) + ggplot2::ggtitle(if (my_champion == "LGB") "Composite LGB" else my_champion) +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Actual" = "tomato")) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = c(0.11, 0.9), legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))
    ggplot2::ggsave(paste0(tolower(my_champion), "_pooled_main.svg"), plot_main, device = Cairo::CairoSVG)

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
    ggplot2::ggsave(paste0(tolower(my_champion), "_pooled_main_weights.svg"), plot_weights, device = Cairo::CairoSVG)

    # 2d.) Backdate exercise.
    plot_back <- plot_dta %>%
      ggplot2::ggplot(ggplot2::aes(x = day, y = .data[[outcome_colname]], color = "Actual")) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
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
        ggplot2::ggplot(ggplot2::aes(x = day, y = .data[[outcome_colname]], color = "Actual")) +
        ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
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

    ggplot2::ggsave(paste0(tolower(my_champion), "_pooled_robustness.svg"), plot_robustness, device = Cairo::CairoSVG)

    # # 2f.) RMSE pre-treatment.
    # actual_outcome <- plot_dta %>%
    #   dplyr::select(day, all_of(outcome_colname)) %>%
    #   filter(day < treatment_date + 1)
    # colnames(actual_outcome)[2] <- "actual_outcome"
    #
    # synth_outcome <- synth_outcomes[[my_champion]]$synth_outcome %>%
    #   filter(day < treatment_date + 1)
    # colnames(synth_outcome)[2] <- "synth_outcome"
    #
    # rmse_pre <- sqrt(dim(synth_outcome)[1]^(-1) * sum((actual_outcome$actual_outcome - synth_outcome$synth_outcome)^2))
  }

  ## 4.) If Graves and LGB are present, plot also the difference in their effects.
  ## Notice that, if the condition below is true, the routine already computed the average outcome of LGB champions.
  if (all(c("Graves", "LGB") %in% champions)) {
    graves_actual_outcome <- dta %>%
      dplyr::filter(champion == "Graves") %>%
      dplyr::select(day, all_of(outcome_colname))
    colnames(graves_actual_outcome)[2] <- "graves_actual_outcome"

    graves_synth_outcome <- synth_outcomes$Graves$synth_outcome
    colnames(graves_synth_outcome)[2] <- "graves_synth_outcome"

    graves_diff_dta <- graves_actual_outcome %>%
      dplyr::left_join(graves_synth_outcome, by = "day") %>%
      dplyr::mutate(graves_diff = graves_actual_outcome - graves_synth_outcome)

    lgb_actual_outcome <- dta %>%
      dplyr::filter(champion == "LGB") %>%
      dplyr::select(day, all_of(outcome_colname))
    colnames(lgb_actual_outcome)[2] <- "lgb_actual_outcome"

    lgb_synth_outcome <- synth_outcomes$LGB$synth_outcome
    colnames(lgb_synth_outcome)[2] <- "lgb_synth_outcome"

    lgb_diff_dta <- lgb_actual_outcome %>%
      dplyr::left_join(lgb_synth_outcome, by = "day") %>%
      dplyr::mutate(lgb_diff = lgb_actual_outcome - lgb_synth_outcome)

    diff_dta <- graves_diff_dta %>%
      dplyr::left_join(lgb_diff_dta, by = "day") %>%
      dplyr::mutate(diff_diff = graves_diff - lgb_diff)

    plot_diff <- diff_dta %>%
      ggplot2::ggplot(ggplot2::aes(x = day, y = diff_diff, color = "Series")) +
      ggplot2::annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
      ggplot2::xlab("") + ggplot2::ylab(expression(hat(tau)[t]^L - hat(gamma)[t]^L)) + ggplot2::ggtitle("") +
      ggplot2::scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      ggplot2::scale_color_manual(name = "Colors", values = c("Series" = "#69b3a2")) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "none", legend.title = ggplot2::element_blank(), legend.direction = "vertical", legend.text = element_text(size = 7))
    ggplot2::ggsave("difference_graves_lgb.svg", plot_diff, device = Cairo::CairoSVG)
  }

  cat("Figures are saved at ", getwd(), "\n", sep = "")
}


#' Cost of Coming Out - Main Analysis
#'
#' Produced plots displaying the results of the main analysis of the Cost of Coming Out paper performed by \code{\link{run_main_regional}}.
#'
#' @param regional_results Output of \code{\link{run_main_regional}}.
#'
#' @return
#' Save some nice plots.
#'
#' @import dplyr ggplot2 ggsci gridExtra grDevices Cairo
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{produce_plots_pooled}}
#'
#' @export
produce_plots_regional <- function(regional_results) {
  ## 0.) Handling inputs and checks.
  outcome_colname <- regional_results$outcome_colname
  dta <- regional_results$dta
  treatment_date <- regional_results$treatment_date
  champions <- names(regional_results)[!(names(regional_results) %in% c("outcome_colname", "dta", "treatment_date"))]

  regions <- unique(dta$region)
  regional_panels <- sapply(regions, function(x) {reg_panel <- dta %>% dplyr::filter(region == x)}, simplify = FALSE)

  pride_month_2022_begin <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
  pride_month_2022_end <- as.POSIXct("2022-06-30", tryFormats = "%Y-%m-%d")
  rainbow <- grDevices::adjustcolor(matrix(grDevices::hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50), alpha.f = 0.4)

  if (outcome_colname %in% c("pick_rate", "pick_rate")) y_label <- "Pick rate" else y_label <- "Pick level"

  ## 1.) Construct synthetic outcomes.
  synth_outcomes <- lapply(regional_results[!(names(regional_results) %in% c("outcome_colname", "dta", "treatment_date"))], function(x) {
    mapply(function(y, z) { construct_synth_outcome(y, z, "champion", outcome_colname, "day") }, y = x, z = regional_panels, SIMPLIFY = FALSE)
  })

  ## 2.) Plot.
  for (i in seq_len(length(champions))) {
    my_champion <- champions[i]

    if (my_champion == "LGB") {
      lgb_avg_outcome <- lapply(regional_panels, function(x) {
        x %>%
          dplyr::filter(champion %in% c("Nami", "Leona", "Diana", "Neeko")) %>%
          dplyr::group_by(day_no) %>%
          dplyr::mutate(lgb_pick_level = mean(pick_level),
                        lgb_pick_rate = mean(pick_rate)) %>%
          dplyr::distinct(day, .keep_all = TRUE) %>%
          select(region, day, day_no, lgb_pick_level, lgb_pick_rate)
      })

      lgb_avg_outcome <- lapply(lgb_avg_outcome, function(x) {
        colnames(x) <- c("region", "day", "day_no", "pick_level", "pick_rate")
        x$champion <- "LGB"
        x
      })

      lgb_avg_outcome <- dplyr::bind_rows(lgb_avg_outcome)

      dta <- dta %>%
        dplyr::select(region, day, day_no, pick_level, pick_rate, champion) %>%
        dplyr::bind_rows(lgb_avg_outcome)
    }

    plot_dta <- dta %>%
      dplyr::filter(champion == my_champion)

    plot_synth_outcomes <- lapply(synth_outcomes[[my_champion]], function(x) { x$synth_outcome }) %>%
      dplyr::bind_rows(.id = "groups")
    colnames(plot_synth_outcomes)[1] <- "region"

    # 2a.) Main fit.
    plot_main <- plot_dta %>%
      ggplot2::ggplot(ggplot2::aes(x = day, y = .data[[outcome_colname]], color = "Actual")) +
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
    ggplot2::ggsave(paste0(tolower(my_champion), "_regional_main.svg"), plot_main, device = Cairo::CairoSVG, width = 7, height = 7)
  }

  cat("Figures are saved at ", getwd(), "\n", sep = "")
}
