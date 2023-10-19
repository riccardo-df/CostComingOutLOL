## @author: riccardo-df
## University of Rome Tor Vergata
## 11\10\2023

## The Cost of Coming Out - Mechanisms (Graves' Performance).

## This script uses synthetic control methods to investigate whether Graves' performance changes at the treatment date.

# Preliminaries -----------------------------------------------------------
rm(list = ls())
set.seed(1986)

## Loading packages.
pkgs <- c("CostComingOutLOL", "dplyr", "Metrics", "stringr")
inst <- lapply(pkgs, library, character.only = TRUE)

# Settings --------------------------------------------
## Select champions.
champions <- "Graves"

## Select outcome series.
bandwidth <- 3

min_date <- as.POSIXct("2022-01-01", tryFormats = "%Y-%m-%d")
max_date <- as.POSIXct("2022-07-15", tryFormats = "%Y-%m-%d")

## Set SC estimator.
donors <- "all"
estimator <- "sc_reg"
treatment_date <- as.POSIXct("2022-06-01", tryFormats = "%Y-%m-%d")
inference <- TRUE
n_boot <- 200
backdate <- 10

covariates <- c()
# covariates <- c("ban_rate_pooled", "win_rate_pooled", "gold_pooled", "kills_pooled", "assists_pooled", "deaths_pooled")

# Estimation --------------------------------------------------------------
pooled_result_kd_ratio <- run_main_pooled(champions, "kd_ratio", donors, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth, covariate_colnames = covariates, max_date = max_date)
pooled_result_win_rate <- run_main_pooled(champions, "win_rate_pooled", donors, estimator, treatment_date, backdate, inference = inference, n_boot = n_boot, bandwidth = bandwidth, covariate_colnames = covariates, max_date = max_date)

# Plots -------------------------------------------------------------------
save_here <- "C:/Users/difra/Dropbox/University/Research/LoL/2_Data_Collection/CostComingOutLOL/Figures/3_Mechanisms/Graves_Performance"

produce_plots_pooled(pooled_result_kd_ratio, save_here)
produce_plots_pooled(pooled_result_win_rate, save_here)

# LATEX -------------------------------------------------------------------
n_col <- 3 # Number of outcomes + 1.

tau_hats <- c(pooled_result_kd_ratio$Graves$tau_hat, pooled_result_win_rate$Graves$tau_hat)
ses <- c(pooled_result_kd_ratio$Graves$se_tau_hat, pooled_result_win_rate$Graves$se_tau_hat)
cils <- trimws(format(round(tau_hats - 1.96 * ses, 3), nsmall = 3))
cius <- trimws(format(round(tau_hats + 1.96 * ses, 3), nsmall = 3))
tau_hats <- format(round(tau_hats, 3), nsmall = 3)

synth_outcomes_dta <- lapply(list(pooled_result_kd_ratio, pooled_result_win_rate), function(z) { lapply(z[!(names(z) %in% c("outcome_colname", "estimator", "donors", "treatment_date", "bandwidth"))], function(x) { construct_synth_outcome(x$tau_hat, x$dta, "champion", "smooth_outcome", "day") }) })
synth_outcomes <- lapply(synth_outcomes_dta, function(x) { x[["Graves"]]$synth_outcome %>% filter(day < treatment_date) })
n_donors <- sapply(synth_outcomes_dta, function(x) { x[["Graves"]]$weights %>% nrow()})

dta_kd_ratio <- pooled_result_kd_ratio$Graves$dta %>%
  filter(champion == "Graves" & day < treatment_date) %>%
  select(day, smooth_outcome)
dta_win_rate <- pooled_result_win_rate$Graves$dta %>%
  filter(champion == "Graves" & day < treatment_date) %>%
  select(day, smooth_outcome)

rmses_kd_ratio <- format(round(rmse(dta_kd_ratio$smooth_outcome, synth_outcomes[[1]]$synth_outcome), 3), nsmall = 3)
rmses_win_rate <- format(round(rmse(dta_win_rate$smooth_outcome, synth_outcomes[[2]]$synth_outcome), 3), nsmall = 3)

latex <- function() {
cat("\\begingroup
  \\setlength{\\tabcolsep}{8pt}
  \\renewcommand{\\arraystretch}{1.1}
  \\begin{table}[b!]
    \\centering
    \\begin{adjustbox}{width = 1\\textwidth}
    \\begin{tabular}{@{\\extracolsep{5pt}}l ", rep("c ", n_col), "}
      \\\\[-1.8ex]\\hline
      \\hline \\\\[-1.8ex]
      & ", str_sub(paste(paste(paste0("(", seq_len(2), ") &"), collapse = " "), collapse = " "), end = -3), " \\\\
      & ", str_sub(paste(paste(paste0(c("Kills-to-deaths", "Win rate"), " &"), collapse = " "), collapse = " "), end = -3), " \\\\
      \\addlinespace[2pt]
      \\hline \\\\[-1.8ex] \n\n", sep = "")

cat("      $\\hat{\\tau}$ & ", str_sub(paste(paste0(tau_hats, " &"), collapse = " "), end = -4), " \\\\
      $95\\%$ CI & ", str_sub(paste(paste0("[", cils, ", ", cius, "] &"), collapse = " "), end = -3), " \\\\ \n", sep = "")

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

latex()
