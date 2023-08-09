## @author: riccardo-df
## University of Rome Tor Vergata
## 05\05\2023

## LoL - Useful functions to post-process the results from construct_sc_lol_pooled, construct_sc_lol_pooled_lgbtq, and construct_sc_lol_regional.



post_process_pooled_lgbtq <- function(fit, pooled_panel) {
  ## INPUTS:
  #   - fit, the output of construct_sc_lol_pooled_lgbtq.
  #   - pooled_panel, our data set "pooled_champion.csv", as a tibble.

  ## OUTPUT:
  #   - None, save plots.

  ## Handling inputs and checks.
  outcome <- fit$outcome_string
  n_back_days <- summary(fit$tau_hat)$dimensions["T0"] - summary(fit$tau_hat_back)$dimensions["T0"] - 1

  if (outcome %in% c("pick_rate_pooled", "pick_rate_mean")) {
    y_label <- "Pick rate"
  } else if (outcome == "pick_level_sum") {
    y_label <- "Pick level"
  } else {
    stop("Invalid 'outcome'. This must be either 'pick_level_sum' or 'pick_rate_pooled'.", call. = FALSE)
  }

  ## Extract weights.
  weights <- data.frame("champion" = rownames(summary(fit$tau_hat)$controls), "weight" = summary(fit$tau_hat)$controls)
  weights_back <- data.frame("champion" = rownames(summary(fit$tau_hat_back)$controls), "weight" = summary(fit$tau_hat_back)$controls)

  ## Generate synthetic outcomes.
  synth_outcome <- pooled_panel %>%
    filter(champion %in% rownames(weights)) %>%
    left_join(weights, by = "champion") %>%
    group_by(day) %>%
    mutate(synt_outcome = weighted.mean(get(outcome), estimate.1)) %>%
    select(day, "synt_outcome") %>%
    distinct()

  synth_outcome_back <- pooled_panel %>%
    filter(champion %in% rownames(weights_back)) %>%
    left_join(weights_back, by = "champion") %>%
    group_by(day) %>%
    mutate(synt_outcome = weighted.mean(get(outcome), estimate.1)) %>%
    select(day, "synt_outcome") %>%
    distinct()

  ## Plots.
  plot_main <- fit$data %>%
    filter(champion == "LGBTQ+") %>%
    ggplot(aes(x = day, y = .data[[outcome]], color = "Actual")) +
    annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    geom_line() +
    geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    xlab("") + ylab(y_label) + ggtitle("Other LGBTQ+") +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_line(data = synth_outcome, aes(y = synt_outcome, col = "Synthetic")) +
    scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Actual" = "tomato")) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.11, 0.85), legend.title = element_blank(),
          legend.direction = "vertical", legend.key.size = unit(0.5, 'cm'))

   plot_back <- fit$data %>%
     filter(champion == "LGBTQ+") %>%
     ggplot(aes(x = day, y = .data[[outcome]], color = "Actual")) +
     annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
     geom_line() +
     geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
     geom_vline(xintercept = as.POSIXct(as.Date(treatment_date) - n_back_days), linetype = 4) +
     xlab("") + ylab(y_label) + ggtitle("Other LGB") +
     scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
     theme_bw() +
     theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
           axis.text.x = element_text(angle = 45, hjust = 1)) +
     geom_line(data = synth_outcome_back, aes(y = synt_outcome, col = "Synthetic")) +
     scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Actual" = "tomato")) +
     theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.11, 0.85), legend.title = element_blank(),
           legend.direction = "vertical", legend.key.size = unit(0.5, 'cm'))

   plot_weights <- weights %>%
    ggplot(aes(x = reorder(champion, -sort(estimate.1)), y = estimate.1, fill = champion)) +
    geom_bar(position = "dodge", stat = "identity") +
    coord_flip() +
    scale_fill_jco() +
    xlab("") + ylab("Weight") + ggtitle("Other LGBTQ+") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none", legend.title = element_blank(),
          legend.direction = "vertical", axis.text.x = element_text(angle = 45, hjust = 1))

  ## Output.
   return(list("plot_main" = plot_main, "plot_back" = plot_back, "plot_weights" = plot_weights))
}


post_process_regional <- function(fits, panel) {
  ## INPUTS:
  #   - fits, the output of construct_sc_lol_regional.
  #   - panel, our data set "daily_champion.csv", as a tibble.

  ## OUTPUT:
  #   - None, save plots.

  ## Handling inputs and checks.
  outcome <- fits$outcome_string

  if (outcome == "pick_rate") {
    y_label <- "Pick rate"
  } else if (outcome == "pick_level") {
    y_label <- "Pick level"
  } else {
    stop("Invalid 'outcome'. This must be either 'pick_level' or 'pick_rate'.", call. = FALSE)
  }

  ## Extract weights.
  weights <- lapply(fits[names(fits) != "outcome_string"], function(x) { lapply(x, function(y) {data.frame("champion" = rownames(summary(y)$controls), "weight" = summary(y)$controls)})})
  weights <- lapply(weights, function(x) {mapply(function(z, y) {tibble(z, "region" = y)}, z = x, y = names(weights[[1]]), SIMPLIFY = FALSE)})
  weights <- lapply(weights, function(x) {bind_rows(x)})

  ## Generate synthetic outcomes.
  synth <- lapply(weights, function(x) {
    synth_outcome <- panel %>%
      filter(champion %in% unique(x$champion)) %>%
      left_join(x, by = c("champion", "region")) %>%
      replace(is.na(.), 0) %>% # If some champions appear in one region and not in others, we get nas.
      group_by(day, region) %>%
      mutate(synt_outcome = weighted.mean(get(outcome), estimate.1)) %>%
      select(region, day, "synt_outcome") %>%
      distinct()
  })

  ## Plot.
  champions <- names(fits)[names(fits) != "outcome_string"]
  plots <- list()

  for (i in seq_len(length(champions))) {
    my_champion <- champions[i]

    plots[[i]] <- panel %>%
      filter(champion == my_champion) %>%
      ggplot(aes(x = day, y = .data[[outcome]], color = "Actual")) +
               annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
               geom_line() +
               geom_line(data = synth[[my_champion]], aes(y = synt_outcome, col = "Synthetic")) +
               facet_wrap(~region, ncol = 2) +
               geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
               xlab("") + ylab(y_label) + ggtitle(my_champion) +
               scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
               scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Actual" = "tomato")) +
               theme_bw() +
               theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.09, 0.93), legend.title = element_blank(),
                  legend.direction = "vertical", legend.key.size = unit(0.5, 'cm'), axis.text.x = element_text(angle = 45, hjust = 1))
  }

  ## Output.
  names(plots) <- champions
  return(plots)
}


post_process_regional_lgbtq <- function(fits, panel) {
  ## INPUTS:
  #   - fits, the output of construct_sc_lol_regional_lgbtq.
  #   - panel, our data set "daily_champion.csv", as a tibble.

  ## OUTPUT:
  #   - None, save plots.

  ## Handling inputs and checks.
  outcome <- fits$outcome_string

  if (outcome == "pick_rate") {
    y_label <- "Pick rate"
  } else if (outcome == "pick_level") {
    y_label <- "Pick level"
  } else {
    stop("Invalid 'outcome'. This must be either 'pick_level' or 'pick_rate'.", call. = FALSE)
  }

  ## Extract weights.
  weights <- lapply(fits$fits, function(x) {data.frame("champion" = rownames(summary(x)$controls), "weight" = summary(x)$controls)})
  weights <- mapply(function(x, y) {tibble(x, "region" = y)}, x = weights, y = names(weights), SIMPLIFY = FALSE)
  weights <- bind_rows(weights)

  ## Generate synthetic outcomes.
  synth_outcome <- panel %>%
    left_join(weights, by = c("champion", "region")) %>%
    replace(is.na(.), 0) %>% # If some champions appear in one region and not in others, we get nas.
    group_by(day, region) %>%
    mutate(synt_outcome = weighted.mean(get(outcome), estimate.1)) %>%
    select(region, day, synt_outcome) %>%
    arrange(region, day) %>%
    distinct()

  ## Plot.
  plot <- fits$data %>%
    bind_rows() %>%
    filter(champion == "LGBTQ+") %>%
    ggplot(aes(x = day, y = .data[[outcome]], color = "Actual")) +
    annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
    geom_line() +
    geom_line(data = synth_outcome, aes(y = synt_outcome, col = "Synthetic")) +
    facet_wrap(~region, ncol = 2) +
    geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
    xlab("") + ylab(y_label) + ggtitle("Other LGB") +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
    scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Actual" = "tomato")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.60, 0.93), legend.title = element_blank(),
          legend.direction = "vertical", legend.key.size = unit(0.4, 'cm'), axis.text.x = element_text(angle = 45, hjust = 1))

  ## Output.
  return(plot)
}


# post_process_pooled <- function(graves_fits, lgbt_fits, pooled_panel) {
#   ## INPUTS:
#   #   - graves_fits, the output of construct_sc_lol ran on (at least) Graves.
#   #   - lgbt_fits, the output of construct_sc_lol_pooled_lgbtq.
#   #   - pooled_panel, our data set "pooled_champion.csv", as a tibble.
#
#   ## OUTPUT:
#   #   - List of plots. Each element corresponds to a champion and stores three plots, one for the main fit, one for the backdating exercise, and one for the weights of the main fit.
#
#   ## Handling inputs and checks.
#   outcome1 <- graves_fits$outcome_string
#   outcome2 <- lgbt_fits$outcome_string
#
#   if (outcome1 != outcome2) stop("You used different outcomes for Graves and LGBT aggregate.", call. = FALSE)
#
#   outcome <- outcome1
#
#   if (outcome %in% c("pick_rate_pooled", "pick_rate_mean")) {
#     y_label <- "Pick rate"
#   } else if (outcome == "pick_level_sum") {
#     y_label <- "Pick level"
#   } else {
#     stop("Invalid 'outcome'. This must be one of 'pick_level_sum', 'pick_rate_pooled', 'pick_rate_mean'.", call. = FALSE)
#   }
#
#   if (any(names(graves_fits) == "Graves")) stop("I cannot find Graves.", call. = FALSE)
#
#   ## Extract weights.
#   weights_graves <- data.frame(lapply(graves_fits[names(graves_fits) == "Graves"], function(x) {data.frame("champion" = rownames(summary(x$tau_hat)$controls), "weight" = summary(x$tau_hat)$controls)}))
#   colnames(weights_graves) <- c("champion", "weight")
#   weights_lgbt <- data.frame("champion" = rownames(summary(lgbt_fits$tau_hat)$controls), "weight" = summary(lgbt_fits$tau_hat)$controls)
#   colnames(weights_lgbt) <- c("champion", "weight")
#
#   ## Generate synthetic outcomes.
#   synth_outcome <- pooled_panel %>%
#     filter(champion %in% rownames(weights_graves)) %>%
#     left_join(weights_graves, by = "champion") %>%
#     group_by(day) %>%
#     mutate(synt_outcome = weighted.mean(get(outcome), weight)) %>%
#     select(day, "synt_outcome") %>%
#     distinct()
#
#   synth_outcome_lgbt <- pooled_panel %>%
#     filter(champion %in% rownames(weights_lgbt)) %>%
#     left_join(weights_lgbt, by = "champion") %>%
#     group_by(day) %>%
#     mutate(synt_outcome = weighted.mean(get(outcome), weight)) %>%
#     select(day, "synt_outcome") %>%
#     distinct()
#
#   ## Estimate effects.
#   actual_outcome <- pooled_panel %>%
#     filter(champion == "Graves") %>%
#     select(day, all_of(outcome)) %>%
#     pull(all_of(outcome))
#   tau_hat_graves <- actual_outcome - synth_outcome$synt_outcome
#
#   actual_outcome_lgbt <- lgbt_fits$data %>%
#     filter(champion == "LGBTQ+") %>%
#     select(day, all_of(outcome)) %>%
#     pull(all_of(outcome))
#   tau_hat_lgbt <- actual_outcome_lgbt - synth_outcome_lgbt$synt_outcome
#
#   final_difference <- tau_hat_graves - tau_hat_lgbt
#
#   plot <- ggplot(data.frame() aes(x = day, y = .data[[outcome]], color = "Actual")) +
#     annotation_raster(rainbow, xmin = as.POSIXct(pride_month_2022_begin), xmax = as.POSIXct(pride_month_2022_end), ymin = -Inf, ymax = Inf) +
#     geom_line() +
#     geom_vline(xintercept = as.POSIXct(treatment_date), linetype = 4) +
#     xlab("") + ylab(y_label) + ggtitle(my_champion) +
#     scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
#           axis.text.x = element_text(angle = 45, hjust = 1)) +
#     geom_line(data = synth[[my_champion]], aes(y = synt_outcome, col = "Synthetic")) +
#     scale_color_manual(name = "Colors", values = c("Synthetic" = "#00BFC4", "Actual" = "tomato")) +
#     theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.11, 0.9), legend.title = element_blank(),
#           legend.direction = "vertical")
#
#
#   ## Output.
#   names(plots) <- champions
#   return(plots)
# }
