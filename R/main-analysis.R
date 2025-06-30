#' Cost of Coming Out - Main Analysis
#'
#' Runs the main analysis of the Cost of Coming Out paper using \code{\link{lol_champ_pool_dta}}.
#'
#' @param champions Character vector with the names of the champions of interest. The routine performs the analysis for all them separately.
#' @param outcome_colname Name of the column of \code{\link{lol_champ_pool_dta}} storing the outcome of interest.
#' @param donors Which units to include in the donor pool. See the details section below.
#' @param estimator Which estimator to use. Must be one of "sc" (standard synthetic control), "sc_reg" (sc plus a ridge penalty), "synthdid" (synthetic diff-in-diff).
#' @param treatment_date Object of class \code{POSIXct}. When the treatment took place.
#' @param backdate How many periods to backdate the treatment for a robustness check.
#' @param inference Logical, whether to estimate standard errors. If \code{TRUE}, the placebo method described in Section 5 of Arkhangelsky et al. is used.
#' @param n_boot Number of champions to be assigned the placebo treatment for standard error estimation. Ignored if \code{inference} is \code{FALSE}.
#' @param bandwidth Parameter controlling the amount of smoothing.
#' @param covariate_colnames Character vector with the names of the columns of \code{\link{lol_champ_pool_dta}} storing the time-varying covariates for which we want to adjust for.
#'
#' @details
#' For each champion in \code{champions}, \code{\link{run_main_pooled}} performs the following operations.
#'
#' \itemize{
#'  \item{First: }{constructs the treatment variable, which equals one for the champion under investigation starting from \code{treatment_date}.}
#'  \item{Second: }{subsets \code{\link{lol_champ_pool_dta}} to include only the champion under investigation and the desired units in the donor pool (as controlled by \code{donors}).}
#'  \item{Third: }{calls one of the \code{synthdid} estimation functions, as controlled by \code{estimator}. If \code{covariate_colnames} is non-empty, \code{\link{run_main_pooled}} regresses the outcomes
#'                on the covariates, on time fixed effects, and unit fixed effects, with the regression estimated without using observations in which the treatment takes place. Then, \code{\link{run_main_pooled}} uses
#'                the estimated coefficients to compute the residuals of the outcomes for all observations (also those in which the treatment takes place), and use these residuals rather than the original outcomes for
#'                estimation.}
#'  \item{Fourth: }{calls again one of the \code{synthdid} estimation functions, this time using the backdated treatment.}
#'  \item{Fifth: }{performs a leave-one-out exercise, where we repeatedly estimate the synthetic control series by excluding one champion with non-zero estimated weights at a time from the donor pool. This exercise
#'                is carried out only if the number of non-zero contributions is less than 10 to avoid a large running time.}
#' }
#'
#' \code{treatment_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' The outcome series is smoothed using a Nadaraya–Watson kernel regression before the covariate adjustment. The user can control the amount of smoothing by setting the \code{bandwidth} parameter. The larger parameter, the smoother the series.
#' An infinitesimal bandwidth amounts to no smoothing.\cr
#'
#' \code{donors} must be a character vector with one or more champions contained in \code{dta}. Alternatively, one of seven special strings can be used:
#'
#' \itemize{
#'    \item{"all" }{This includes all champions.}
#'    \item{"non_lgb" }{This includes all champions expect Graves, Twisted Fate, Diana, Leona, Nami, and Neeko.}
#'    \item{"jungle" }{This includes all champions whose main role is Jungle.}
#'    \item{"middle" }{This includes all champions whose main role is Middle.}
#'    \item{"top" }{This includes all champions whose main role is Top.}
#'    \item{"support" }{This includes all champions whose main role is Support.}
#'    \item{"adc" }{This includes all champions whose main role is Adc.}
#'    \item{"support_adc"}{This includes all champions whose main role is either Support or Adc.}
#'    \item{"top_jungle_middle"}{This includes all champions whose main role is either Top, Jungle, or Middle.}
#'    \item{"main_role"}{This includes all champions whose main role is the same as the champion of interest.}
#'    \item{"aux_role"}{This includes all champions whose auxiliary role is the same as the champion of interest.}
#' }
#'
#' Almost all of these choices (exceptions are \code{"all"} and \code{"non_lgb"}) can be combined with the prefix \code{"non"} to exclude champions from a particular role.
#' For instance, setting \code{donors} to \code{non_jungle} excludes from the donor pool all champions whose main role is jungle.
#'
#' It is possible to include \code{"LGB"} in \code{champions}. If so, \code{\link{run_main_pooled}} constructs a new unit by averaging the outcomes of Diana, Leona, Nami, and Neeko and
#' runs the analysis detailed above on this new unit. This is compatible only with \code{donors} set to \code{"non_lgb"}.\cr
#'
#' A special value for \code{outcome_colname} is \code{"kd_ratio"}. In this case, the routine computes the daily kills/deaths ratio of each champion and uses this an an outcome.
#'
#' @return
#' A list of the same length as \code{champions}. Each element stores a list with results for one of those champions (elements are named).
#' Each inner list stores five elements: the results of the main fit, the results of the backdating exercise, the results of the leave-one-out exercise, the pre-treatment average of \code{outcome_colname},
#' and the data used for estimation. The outer list stores five additional elements: the name of the outcome, the name of the estimator, the name of the donors, the treatment date, and the bandwidth used to smooth
#' the outcome series.
#'
#' @import dplyr fixest
#' @importFrom stats ksmooth
#' @importFrom stats time
#' @importFrom stats vcov
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{run_main_regional}}
#'
#' @export
run_main_pooled <- function(champions, outcome_colname, donors, estimator, treatment_date, backdate,
                            inference = FALSE, n_boot = 100, bandwidth = 0.01, covariate_colnames = c()) {
  ## Handling inputs and checks.
  kills_pooled <- NULL
  deaths_pooled <- NULL
  . <- NULL
  champion <- NULL
  selected_outcome <- NULL
  n_unique_outcome <- NULL
  day_no <- NULL
  lgb_selected_outcome <- NULL
  main_role <- NULL
  aux_role <- NULL
  treatment <- NULL
  smooth_outcome <- NULL

  if (!(outcome_colname %in% c("pick_level_sum", "pick_rate_pooled", "win_rate_pooled", "gold_pooled", "assists_pooled", "kd_ratio"))) stop("Invalid 'outcome'. This must be one of 'pick_level_sum', 'pick_rate_pooled', 'win_rate_pooled', 'gold_pooled', 'assists_pooled', 'kd_ratio'.", call. = FALSE)

  if (length(donors) == 1) {
    if (!(donors %in% c("all", "non_lgb", "main_role", "aux_role", "jungle", "middle", "top", "support", "adc", "support_adc", "top_jungle_middle", "non_main_role", "non_aux_role", "non_jungle", "non_middle", "non_top", "non_support", "non_adc"))) stop("Invalid 'donors'. Call 'help(run_main_pooled)' to check valid inputs.", call. = FALSE)
    if (any(champions == "LGB") & donors != "non_lgb") stop("We can run the analysis for the new LGB unit only if 'donors' is set to 'non_lgb'.", call. = FALSE)
  } else {
    if (sum(!(donors %in% unique(lol_champ_pool_dta$champion))) > 0) stop("Invalid 'donors'. One or more champions are not found in 'lol_champ_pool_dta'.", call. = FALSE)
  }

  if (!(estimator %in% c("sc", "sc_reg", "sdid"))) stop("Invalid 'estimator'. This must be one of 'sc', 'sc_reg', 'sdid'.", call. = FALSE)
  if (!inherits(treatment_date, "POSIXct")) stop("Invalid 'treatment_date'. This must of class 'POSIXct'.", call. = FALSE)
  if (backdate < 0 | backdate %% 1 != 0) stop("Invalid 'backdate'. This must be a positive integer.", call. = FALSE)
  if (!is.logical(inference)) stop("Invalid 'inference'. This must be either 'TRUE' or 'FALSE.", call. = FALSE)
  if (n_boot <= 1 | n_boot %% 1 != 0) stop("Invalid 'n_boot'. This must be an interger greater than or equal to 2.", call. = FALSE)
  if (bandwidth <= 0) stop("Invalid 'bandwidth'. This must be a positive number.", call. = FALSE)

  if (outcome_colname != "kd_ratio") {
    lol_champ_pool_dta$selected_outcome <- lol_champ_pool_dta[[outcome_colname]]
  } else {
    lol_champ_pool_dta <- lol_champ_pool_dta %>%
      dplyr::mutate(selected_outcome = kills_pooled / deaths_pooled) %>%
      replace(is.na(.), 0) %>%
      dplyr::group_by(champion) %>%
      dplyr::mutate(n_unique_outcome = length(unique(selected_outcome))) %>%
      dplyr::filter(n_unique_outcome == length(unique(lol_champ_pool_dta$day))) %>%
      dplyr::ungroup()
  }

  ## Construct synthdid object for each champion.
  output <- list()
  counter <- 1

  for (my_champion in champions) {
    ## 0.) Keep track of the loop.
    cat("Constructing synthetic control for ", my_champion, ": \n", sep = "")

    ## 1.) Generate treatment variable. Construct composite LGB if necessary. Smooth the series.
    cat("    1.) Generating treatment variable; \n")

    if (my_champion == "LGB") {
      lgb_avg_outcome <- lol_champ_pool_dta %>%
        dplyr::filter(champion %in% c("Diana", "Leona", "Nami", "Neeko")) %>%
        dplyr::group_by(day_no) %>%
        dplyr::mutate(lgb_selected_outcome = mean(selected_outcome)) %>%
        dplyr::distinct(day, .keep_all = TRUE) %>%
        dplyr::select(day, day_no, lgb_selected_outcome, all_of(covariate_colnames))

      colnames(lgb_avg_outcome)[3] <- "selected_outcome"
      lgb_avg_outcome$champion <- "LGB"

      temp_panel <- lol_champ_pool_dta %>%
        dplyr::select(day, day_no, champion, selected_outcome, all_of(covariate_colnames), main_role, aux_role) %>%
        dplyr::bind_rows(lgb_avg_outcome)
    } else {
      temp_panel <- lol_champ_pool_dta %>%
        dplyr::select(day, day_no, champion, selected_outcome, all_of(covariate_colnames), main_role, aux_role)
    }

    temp_panel$treatment <- as.logical(ifelse(temp_panel$champion == my_champion & temp_panel$day >= treatment_date + 1, 1, 0))

    temp_panel <- temp_panel %>%
      dplyr::group_by(champion) %>%
      dplyr::mutate(smooth_outcome = stats::ksmooth(stats::time(selected_outcome), selected_outcome, "normal", bandwidth = bandwidth)$y) %>%
      dplyr::select(day, day_no, champion, treatment, smooth_outcome, all_of(covariate_colnames), main_role, aux_role) %>%
      dplyr::ungroup()

    ## 2.) Subset according to donor pool. Do that again for backdate exercise and change treatment date.
    cat("    2.) Constructing donor pool; \n")
    estimation_dta <- construct_donor_pool(temp_panel, donors, my_champion)
    estimation_dta_back <- estimation_dta
    estimation_dta_back$treatment <- as.logical(ifelse(estimation_dta_back$champion == my_champion & estimation_dta_back$day >= as.Date(treatment_date) - backdate, 1, 0))

    ## 3.) Process data and estimation.
    cat("    3.) Constructing weights; \n")
    tau_hat <- call_synthdid(estimation_dta, "smooth_outcome", estimator, covariate_colnames)

    ## 4.) Estimate standard errors if necessary.
    cat("    4.) Estimating standard error; \n")
    if (inference) {
      se <- as.numeric(sqrt(stats::vcov(tau_hat, method = "placebo", replications = n_boot)))
    } else {
      cat("        Skipping. \n")
      se <- NULL
    }

    ## 5.) Backdate.
    cat("    5.) Backdating exercise; \n")
    tau_hat_back <- call_synthdid(estimation_dta_back, "smooth_outcome", estimator, covariate_colnames)

    ## 6.) Leave-one-out.
    cat("    6.) Leave-one-out exercise. \n")
    non_zero_champions <- rownames(summary(tau_hat)$controls)
    tau_hat_drop <- list()

    if (length(non_zero_champions) < 10) {
      counter_drop <- 1

      for (drop_champion in non_zero_champions) {
        ## Drop one champion from donor pool.
        estimation_dta_drop <- estimation_dta %>%
          dplyr::filter(champion != drop_champion)

        ## Construct synthetic control.
        temp_tau_hat_drop <- call_synthdid(estimation_dta_drop, "smooth_outcome", estimator, covariate_colnames)

        ## Save.
        tau_hat_drop[[counter_drop]] <- temp_tau_hat_drop
        counter_drop <- counter_drop + 1
      }

      names(tau_hat_drop) <- non_zero_champions
    } else {
      cat("        Skipping. \n")
    }

    cat("\n")

    ## 7.) Save results.
    temp_pre_avg <- temp_panel %>%
      dplyr::filter(champion == my_champion & day < treatment_date) %>%
      dplyr::pull(smooth_outcome) %>%
      mean()

    output[[counter]] <- list("tau_hat" =  tau_hat, "se_tau_hat" = se, "tau_hat_back" = tau_hat_back, "tau_hat_drop" = tau_hat_drop, "pre_treatment_avg" = temp_pre_avg, "dta" = temp_panel)
    counter <- counter + 1
  }

  ## Output.
  output[[counter]] <- outcome_colname
  output[[counter + 1]] <- estimator
  output[[counter + 2]] <- donors
  output[[counter + 3]] <- treatment_date
  output[[counter + 4]] <- bandwidth

  names(output) <- c(champions, "outcome_colname", "estimator", "donors", "treatment_date", "bandwidth")

  return(output)
}


#' Cost of Coming Out - Main Analysis by Region
#'
#' Runs the main analysis of the Cost of Coming Out paper using \code{\link{lol_champ_dta}}.
#'
#' @param champions Character vector with the names of the champions of interest. The routine performs the analysis for all them separately.
#' @param outcome_colname Name of the column of \code{\link{lol_champ_dta}} storing the outcome of interest.
#' @param donors Which units to include in the donor pool. See the details section below.
#' @param estimator Which estimator to use. Must be one of "sc" (standard synthetic control), "sc_reg" (sc plus a ridge penalty), "synthdid" (synthetic diff-in-diff).
#' @param treatment_date Object of class \code{POSIXct}. When the treatment took place.
#' @param backdate How many periods to backdate the treatment for a robustness check.
#' @param inference Logical, whether to estimate standard errors. If \code{TRUE}, the placebo method described in Section 5 of Arkhangelsky et al. is used.
#' @param n_boot Number of champions to be assigned the placebo treatment for standard error estimation. Ignored if \code{inference} is \code{FALSE}.
#' @param bandwidth Parameter controlling the amount of smoothing.
#' @param covariate_colnames Character vector with the names of the columns of \code{\link{lol_champ_dta}} storing the time-varying covariates for which we want to adjust for.
#' @param min_date Object of class \code{POSIXct}. When to start the series.
#' @param max_date Object of class \code{POSIXct}. When to end the series.
#'
#' @details
#' \code{\link{run_main_regional}} disaggregates \code{\link{lol_champ_dta}} by constructing four data sets, one for each region. Then, for each champion in \code{champions}, it performs
#' the following operations separately for each data set.\cr
#'
#' \itemize{
#'  \item{First: }{constructs the treatment variable, which equals one for the champion under investigation starting from \code{treatment_date}.}
#'  \item{Second: }{subsets each regional data set to include only the champion under investigation and the desired units in the donor pool (as controlled by \code{donors}).}
#'  \item{Third: }{calls one of the \code{synthdid} estimation functions, as controlled by \code{estimator}. If \code{covariate_colnames} is non-empty, \code{\link{run_main_pooled}} regresses the outcomes
#'                 on the covariates, on time fixed effects, and unit fixed effects, with the regression estimated without using observations in which the treatment takes place. Then, \code{\link{run_main_pooled}} uses
#'                 the estimated coefficients to compute the residuals of the outcomes for all observations (also those in which the treatment takes place), and use these residuals rather than the original outcomes for
#'                 estimation.}
#' }
#'
#' \code{treatment_date}, \code{min_date}, and \code{max_date} must be created by \code{as.POSIXct("YYYY-MM-DD", tryFormats = "\%Y-\%m-\%d")}.\cr
#'
#' The outcome series is smoothed using a Nadaraya–Watson kernel regression before the covariate adjustment. The user can control the amount of smoothing by setting the \code{bandwidth} parameter. The larger parameter, the smoother the series.
#' An infinitesimal bandwidth amounts to no smoothing.\cr
#'
#' \code{donors} must be a character vector with one or more champions contained in \code{dta}. Alternatively, one of seven special strings can be used:
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
#' It is possible to include \code{"LGB"} in \code{champions}. If so, \code{\link{run_main_pooled}} constructs a new unit by averaging the outcomes of Diana, Leona, Nami, and Neeko and
#' runs the analysis detailed above on this new unit. This is compatible only with \code{donor_pool} set to \code{"non_lgb"}.
#'
#' @return
#' A list of the same length as \code{champions}. Each element stores a list with results for one of those champions (elements are named).
#' Each inner list stores two elements: a list with the results of the main fit for each region, and a list of regional data sets used for estimation.
#' The outer list stores five additional elements: the name of the outcome, the name of the estimator, the name of the donors, the treatment date, and the bandwidth used to smooth the outcome series.
#'
#' @import dplyr fixest
#' @importFrom stats ksmooth
#' @importFrom stats time
#' @importFrom stats vcov
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{run_main_pooled}}
#'
#' @export
run_main_regional <- function(champions, outcome_colname, donors, estimator, treatment_date, backdate,
                              inference = FALSE, n_boot = 100, bandwidth = 0.01, covariate_colnames = c(), min_date = as.POSIXct("2022-01-01"), max_date = as.POSIXct("2023-09-12")) {
  ## Handling inputs and checks.
  lol_champ_pool_dta <- lol_champ_pool_dta
  region <- NULL
  champion <- NULL
  day_no <- NULL
  selected_outcome <- NULL
  n_unique_outcome <- NULL
  lgb_selected_outcome <- NULL
  main_role <- NULL
  treatment <- NULL
  smooth_outcome <- NULL

  if (!(outcome_colname %in% c("pick_level", "pick_rate"))) stop("Invalid 'outcome'. This must either 'pick_level' or 'pick_rate'.", call. = FALSE)

  if (length(donors) == 1) {
    if (!(donors %in% c("all", "non_lgb", "main_role", "aux_role", "jungle", "middle", "top", "support", "adc", "support_adc", "top_jungle_middle", "non_main_role", "non_aux_role", "non_jungle", "non_middle", "non_top", "non_support", "non_adc"))) stop("Invalid 'donors'. Call 'help(run_main_pooled)' to check valid inputs.", call. = FALSE)
    if (any(champions == "LGB") & donors != "non_lgb") stop("We can run the analysis for the new LGB unit only if 'donors' is set to 'non_lgb'.", call. = FALSE)
  } else {
    if (sum(!(donors %in% unique(lol_champ_pool_dta$champion))) > 0) stop("Invalid 'donors'. One or more champions are not found in 'lol_champ_pool_dta'.", call. = FALSE)
  }

  if (!(estimator %in% c("sc", "sc_reg", "sdid"))) stop("Invalid 'estimator'. This must be one of 'sc', 'sc_reg', 'sdid'.", call. = FALSE)
  if (!inherits(treatment_date, "POSIXct")) stop("Invalid 'treatment_date'. This must of class 'POSIXct'.", call. = FALSE)
  if (backdate < 0 | backdate %% 1 != 0) stop("Invalid 'backdate'. This must be a positive integer.", call. = FALSE)
  if (!is.logical(inference)) stop("Invalid 'inference'. This must be either 'TRUE' or 'FALSE.", call. = FALSE)
  if (n_boot <= 1 | n_boot %% 1 != 0) stop("Invalid 'n_boot'. This must be an interger greater than or equal to 2.", call. = FALSE)
  if (bandwidth <= 0) stop("Invalid 'bandwidth'. This must be a positive number.", call. = FALSE)

  lol_champ_dta <- lol_champ_dta %>%
    dplyr::filter(min_date < day & day < max_date)

  lol_champ_dta$selected_outcome <- lol_champ_dta[[outcome_colname]]

  ## Generate regional panels.
  regions <- unique(lol_champ_dta$region)
  regional_panels <- sapply(regions, function(x) { reg_panel <- lol_champ_dta %>% dplyr::filter(region == x) }, simplify = FALSE)

  ## Construct synthdid object for each champion.
  output <- list()
  counter <- 1

  for (my_champion in champions) {
    ## 0.) Keep track of the loop.
    cat("Constructing synthetic controls for ", my_champion, ": \n", sep = "")

    ## 1.) Generate treatment variable. Smooth the outcome series.
    cat("    1.) Generating treatment variable; \n")
    if (my_champion == "LGB") {
      lgb_avg_outcome <- lapply(regional_panels, function(x) {
        x %>%
          dplyr::filter(champion %in% c("Diana", "Leona", "Nami", "Neeko")) %>%
          dplyr::group_by(day_no) %>%
          dplyr::mutate(lgb_selected_outcome = mean(selected_outcome)) %>%
          dplyr::distinct(day, .keep_all = TRUE) %>%
          dplyr::select(region, day, day_no, lgb_selected_outcome, all_of(covariate_colnames), main_role, aux_role)
      })

      lgb_avg_outcome <- lapply(lgb_avg_outcome, function(x) {
        colnames(x)[4] <- "selected_outcome"
        x$champion <- "LGB"
        x
      })

      temp_regional_panels <- lapply(regional_panels, function(x) {
        x %>%
          dplyr::select(region, day, day_no, champion, selected_outcome, all_of(covariate_colnames), main_role, aux_role)
      })

      temp_regional_panels <- mapply(function(x, y) { bind_rows(x, y) }, x = temp_regional_panels, y = lgb_avg_outcome, SIMPLIFY = FALSE)
      temp_regional_panels <- lapply(temp_regional_panels, function(x) { x %>% dplyr::mutate(treatment = as.logical(ifelse(champion == my_champion & day >= treatment_date + 1, 1, 0))) })
    } else {
      temp_regional_panels <- lapply(regional_panels, function(x) { x %>% dplyr::mutate(treatment = as.logical(ifelse(champion == my_champion & day >= treatment_date + 1, 1, 0))) })
    }

    temp_regional_panels <- lapply(temp_regional_panels, function(x) { x %>% dplyr::group_by(champion) %>% dplyr::mutate(smooth_outcome = stats::ksmooth(stats::time(selected_outcome), selected_outcome, "normal", bandwidth = bandwidth)$y) %>% dplyr::select(region, day, day_no, champion, treatment, smooth_outcome, all_of(covariate_colnames), main_role, aux_role) %>% dplyr::ungroup()})

    ## 2.) Construct donor pools, the latter for the backdating exercise.
    cat("    2.) Constructing donor pools; \n")
    regional_donor_pools <- lapply(temp_regional_panels, function(x) { construct_donor_pool(x, donors, my_champion) })
    regional_donor_pools_back <- lapply(regional_donor_pools, function(x) { x %>% mutate(treatment = as.logical(ifelse(champion == my_champion & day >= as.Date(treatment_date) - backdate, 1, 0))) })

    ## 3.) Construct synthetic controls.
    cat("    3.) Constructing weights; \n")
    tau_hat <- lapply(regional_donor_pools, function(x) { call_synthdid(x, "smooth_outcome", estimator, covariate_colnames) })

    ## 4.) Estimate standard errors.
    cat("    4.) Estimating standard error; \n")
    if (inference) {
      ses <- lapply(tau_hat, function(x) { as.numeric(sqrt(stats::vcov(x, method = "placebo", replications = n_boot))) })
    } else {
      cat("        Skipping. \n")
      ses <- list()
    }

    ## 5.) Backdate.
    cat("    5.) Backdating exercise. \n")
    tau_hats_back <- lapply(regional_donor_pools_back, function(x) { call_synthdid(x, "smooth_outcome", estimator, covariate_colnames) })

    cat("\n")

    ## 6.) Save synthdid object.
    output[[counter]] <- list("tau_hats" = tau_hat, "ses" = ses, "tau_hats_back" = tau_hats_back, "dtas" = temp_regional_panels)
    counter <- counter + 1
  }

  ## Output.
  output[[counter]] <- outcome_colname
  output[[counter + 1]] <- estimator
  output[[counter + 2]] <- donors
  output[[counter + 3]] <- treatment_date
  output[[counter + 4]] <- bandwidth

  names(output) <- c(champions, "outcome_colname", "estimator", "donors", "treatment_date", "bandwidth")

  return(output)
}
