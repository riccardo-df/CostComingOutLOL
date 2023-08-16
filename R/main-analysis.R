#' Cost of Coming Out - Main Analysis
#'
#' Runs the main analysis of the Cost of Coming Out paper using \code{\link{lol_champ_pool_dta}}.
#'
#' @param dta Data set \code{\link{lol_champ_pool_dta}} bundled in the package.
#' @param champions Character vector with the names of the champions of interest. The routine performs the analysis for all them separately.
#' @param outcome_colname Name of the column of \code{lol_champ_pool_dta} storing the outcome of interest.
#' @param donor_pool Which units to include in the donor pool. Must be either "all" or "non_lgb". The latter excludes Graves, Nami, Leona, Diana, and Neeko.
#' @param estimator Which estimator to use. Must be one of "sc" (standard synthetic control), "sc_reg" (sc plus a ridge penalty), "synthdid" (synthetic diff-in-diff).
#' @param treatment_date When the treatment took place. Must be of class \code{as.POSIXct} with \code{tryFormats} set to "\%Y-\%m-\%d".
#' @param backdate How many periods to backdate the treatment for a robustness check.
#' @param covariate_colnames Character vector with the names of the columns of \code{lol_champ_pool_dta} storing the time-varying covariates for which we want to adjust for. If empty, no adjustment is performed. If non-empty, we adjust the outcome using the \code{xsynthdid} package.
#' @param max_date Object of class \code{POSIXct}. Where to cut the series.
#'
#' @details
#' For each champion in \code{champions}, \code{\link{run_main_pooled}} performs the following operations.
#'
#' First, it constructs the treatment variable, which equals one for the champion under investigation starting from \code{treatment_date}.\cr
#'
#' Second, it subsets \code{\link{lol_champ_pool_dta}} to include only the champion under investigation and the desired units in the donor pool (as controlled by \code{donor_pool}).\cr
#'
#' Third, it calls one of the \code{synthdid} estimation functions, as controlled by \code{estimator}. If \code{covariate_colnames} is non-empty, \code{\link{run_main_pooled}} regresses the outcomes
#' on the covariates, on time fixed effects, and unit fixed effects, with the regression estimated without using observations in which the treatment takes place. Then, \code{\link{run_main_pooled}} uses
#' the estimated coefficients to compute the residuals of the outcomes for all observations (also those in which the treatment takes place), and use these residuals rather than the original outcomes for
#' estimation.\cr
#'
#' Fourth, it calls again one of the \code{synthdid} estimation functions, this time using the backdated treatment.\cr
#'
#' Fifth, it performs a leave-one-out exercise, where we repeatedly estimate the synthetic control series by excluding one champion with non-zero estimated weights at a time from the donor pool. This exercise
#' is carried out only if the number of non-zero contributions is less than 10 to avoid a large running time.\cr
#'
#' It is possible to include \code{"LGB"} in \code{champions}. If so, \code{\link{run_main_pooled}} constructs a new unit by averaging \code{outcome_colname} of Nami, Leona, Diana, and Neeko and
#' runs the analysis detailed above on this new unit. This is compatible only with \code{donor_pool} set to \code{"non_lgb"}.
#'
#' @return
#' A list of the same length as \code{champions}. Each element stores a list with results for one of those champions (elements are named).
#' Each inner list stores three elements, one for the results of main fit, one for the results of the backdating exercise, and one for the results of the leave-one-out exercise.
#' The outer list stores two additional last elements, one storing \code{outcome_colname} and one storing \code{dta} up to \code{max_date}.
#'
#' @import dplyr
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{run_main_regional}}
#'
#' @export
run_main_pooled <- function(dta, champions, outcome_colname, donor_pool, estimator, treatment_date, backdate, covariate_colnames = c(), max_date = as.POSIXct("2022-07-13")) {
  ## Handling inputs and checks.
  if (!(outcome_colname %in% c("pick_level_sum", "pick_rate_pooled"))) stop("Invalid 'outcome'. This must be either 'pick_level_sum' or 'pick_rate_pooled'.", call. = FALSE)
  if (!(donor_pool %in% c("all", "non_lgb"))) stop("Invalid 'donor_pool'. This must be either 'all' or 'non_lgb'.", call. = FALSE)
  if (!(estimator %in% c("sc", "sc_reg", "sdid"))) stop("Invalid 'estimator'. This must be one of 'sc', 'sc_reg', 'sdid'.", call. = FALSE)
  if (!inherits(treatment_date, "POSIXct")) stop("Invalid 'treatment_date'. This must of class 'POSIXct'.", call. = FALSE)
  if (backdate < 0 | backdate %% 1 != 0) stop("Invalid 'backdate'. This must be a positive integer.", call. = FALSE)
  if (any(champions == "LGB") & donor_pool != "non_lgb") stop("We can run the analysis for the new LGB unit only if 'donor_pool' is set to 'non_lgb'.", call. = FALSE)
  if (max_date > max(dta$day)) stop("Invalid 'max_date'. It is larger than the most recent day in the data set.")

  dta <- dta %>%
    dplyr::filter(day < max_date)

  ## Construct synthdid object for each champion.
  output <- list()
  counter <- 1

  for (my_champion in champions) {
    ## 0.) Keep track of the loop.
    cat("Constructing synthetic control for ", my_champion, ": \n", sep = "")

    ## 1.) Generate treatment variable.
    cat("    1.) Generating treatment variable; \n")

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

      temp_panel <- dta %>%
        dplyr::select(day, day_no, pick_level_sum, pick_rate_pooled, champion) %>%
        dplyr::bind_rows(lgb_avg_outcome)
    } else {
      temp_panel <- dta
    }

    temp_panel$treatment <- as.logical(ifelse(temp_panel$champion == my_champion & temp_panel$day >= treatment_date + 1, 1, 0))

    ## 2.) Subset according to donor pool. Do that again for backdate exercise and change treatment date.
    cat("    2.) Constructing donor pool; \n")
    estimation_dta <- construct_donor_pool(temp_panel, donor_pool, my_champion)
    estimation_dta_back <- estimation_dta
    estimation_dta_back$treatment <- as.logical(ifelse(estimation_dta_back$champion == my_champion & estimation_dta_back$day >= as.Date(treatment_date) - backdate, 1, 0))

    ## 3.) Process data and estimation.
    cat("    3.) Constructing weights; \n")
    tau_hat <- call_synthdid(estimation_dta, outcome_colname, estimator, covariate_colnames)

    ## 4.) Backdate.
    cat("    4.) Backdating exercise; \n")
    tau_hat_back <- call_synthdid(estimation_dta_back, outcome_colname, estimator, covariate_colnames)

    ## 5.) Leave-one-out.
    cat("    5.) Leave-one-out exercise. \n")
    non_zero_champions <- rownames(summary(tau_hat)$controls)
    tau_hat_drop <- list()

    if (length(non_zero_champions) < 10) {
      counter_drop <- 1

      for (drop_champion in non_zero_champions) {
        ## Drop one champion from donor pool.
        estimation_dta_drop <- estimation_dta %>%
          dplyr::filter(champion != drop_champion)

        ## Construct synthetic control.
        temp_tau_hat_drop <- call_synthdid(estimation_dta_drop, outcome_colname, estimator, covariate_colnames)

        ## Save.
        tau_hat_drop[[counter_drop]] <- temp_tau_hat_drop
        counter_drop <- counter_drop + 1
      }

      names(tau_hat_drop) <- non_zero_champions
    } else {
      cat("        Skipping. \n")
    }

    cat("\n")

    ## 6.) Save results.
    output[[counter]] <- list("tau_hat" =  tau_hat, "tau_hat_back" = tau_hat_back, "tau_hat_drop" = tau_hat_drop)
    counter <- counter + 1
  }

  ## Output.
  output[[counter]] <- outcome_colname
  output[[counter + 1]] <- dta
  output[[counter + 2]] <- treatment_date
  names(output) <- c(champions, "outcome_colname", "dta", "treatment_date")
  return(output)
}


#' Cost of Coming Out - Main Analysis by Region
#'
#' Runs the main analysis of the Cost of Coming Out paper using \code{\link{lol_champ_dta}}.
#'
#' @param dta Data set \code{\link{lol_champ_dta}} bundled in the package.
#' @param champions Character vector with the names of the champions of interest. The routine performs the analysis for all them separately.
#' @param outcome_colname Name of the column of \code{lol_champ_pool_dta} storing the outcome of interest.
#' @param donor_pool Which units to include in the donor pool. Must be either "all" or "non_lgb". The latter excludes Graves, Nami, Leona, Diana, and Neeko.
#' @param estimator Which estimator to use. Must be one of "sc" (standard synthetic control), "sc_reg" (sc plus a ridge penalty), "synthdid" (synthetic diff-in-diff).
#' @param treatment_date When the treatment took place. Must be of class \code{as.POSIXct} with \code{tryFormats} set to "\%Y-\%m-\%d".
#' @param covariate_colnames Character vector with the names of the columns of \code{lol_champ_pool_dta} storing the time-varying covariates for which we want to adjust for. If empty, no adjustment is performed. If non-empty, we adjust the outcome using the \code{xsynthdid} package.
#' @param max_date Object of class \code{POSIXct}. Where to cut the series.
#'
#' @details
#' \code{\link{run_main_regional}} disaggregates \code{\link{lol_champ_dta}} by constructing four data sets, one for each region. Then, for each champion in \code{champions}, it performs
#' the following operations separately for each data set.\cr
#'
#' First, it constructs the treatment variable, which equals one for the champion under investigation starting from \code{treatment_date}.\cr
#'
#' Second, it subsets each regional data set to include only the champion under investigation and the desired units in the donor pool (as controlled by \code{donor_pool}).\cr
#'
#' Third, it calls one of the \code{synthdid} estimation functions, as controlled by \code{estimator}. If \code{covariate_colnames} is non-empty, \code{\link{run_main_pooled}} regresses the outcomes
#' on the covariates, on time fixed effects, and unit fixed effects, with the regression estimated without using observations in which the treatment takes place. Then, \code{\link{run_main_pooled}} uses
#' the estimated coefficients to compute the residuals of the outcomes for all observations (also those in which the treatment takes place), and use these residuals rather than the original outcomes for
#' estimation.\cr
#'
#' It is possible to include \code{"LGB"} in \code{champions}. If so, \code{\link{run_main_regional}} constructs a new unit by averaging \code{outcome_colname} of Nami, Leona, Diana, and Neeko and
#' runs the analysis detailed above on this new unit. This is compatible only with \code{donor_pool} set to \code{"non_lgb"}.
#'
#' @return
#' A list of the same length as \code{champions}. Each element stores a list with results for one of those champions (elements are named).
#' Each inner list stores three elements, one for the results of main fit, one for the results of the backdating exercise, and one for the results of the leave-one-out exercise.
#' The outer list stores an additional last element with \code{outcome_colname}.
#'
#' @import dplyr
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{run_main_pooled}}
#'
#' @export
run_main_regional <- function(dta, champions, outcome_colname, donor_pool, estimator, treatment_date, covariate_colnames = c(), max_date = as.POSIXct("2022-07-13")) {
  ## Handling inputs and checks.
  if (!(outcome_colname %in% c("pick_level", "pick_rate"))) stop("Invalid 'outcome'. This must be either 'pick_level' or 'pick_rate'.", call. = FALSE)
  if (!(donor_pool %in% c("all", "non_lgb"))) stop("Invalid 'donor_pool'. This must be either 'all' or 'non_lgb'.", call. = FALSE)
  if (!(estimator %in% c("sc", "sc_reg", "sdid"))) stop("Invalid 'estimator'. This must be one of 'sc', 'sc_reg', 'sdid'.", call. = FALSE)
  if (!inherits(treatment_date, "POSIXct")) stop("Invalid 'treatment_date'. This must of class 'POSIXct'.", call. = FALSE)
  if (any(champions == "LGB") & donor_pool != "non_lgb") stop("We can run the analysis for the new LGB unit only if 'donor_pool' is set to 'non_lgb'.", call. = FALSE)
  if (max_date > max(dta$day)) stop("Invalid 'max_date'. It is larger than the most recent day in the data set.")

  dta <- dta %>%
    dplyr::filter(day < max_date)

  ## Generate regional panels.
  regions <- unique(dta$region)
  regional_panels <- sapply(regions, function(x) {reg_panel <- dta %>% dplyr::filter(region == x)}, simplify = FALSE)

  ## Construct synthdid object for each champion.
  output <- list()
  counter <- 1

  for (my_champion in champions) {
    ## 0.) Keep track of the loop.
    cat("Constructing synthetic controls for ", my_champion, ": \n", sep = "")

    ## 2.) Generate treatment variable.
    cat("    1.) Generating treatment variable; \n")
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

      temp_regional_panels <- lapply(regional_panels, function(x) {
        x %>%
          select(region, day, day_no, pick_level, pick_rate, champion)
      })

      temp_regional_panels <- mapply(function(x, y) {bind_rows(x, y)}, x = temp_regional_panels, y = lgb_avg_outcome, SIMPLIFY = FALSE)
      temp_regional_panels <- lapply(temp_regional_panels, function(x) {x %>% mutate(treatment = as.logical(ifelse(champion == my_champion & day >= treatment_date + 1, 1, 0)))})
    } else {
      temp_regional_panels <- lapply(regional_panels, function(x) {x %>% mutate(treatment = as.logical(ifelse(champion == my_champion & day >= treatment_date + 1, 1, 0)))})
    }

    ## 2.) Construct donor pools.
    cat("    2.) Constructing donor pools; \n")
    regional_donor_pools <- lapply(temp_regional_panels, function(x) {construct_donor_pool(x, donor_pool, my_champion)})

    ## 3.) Construct synthetic controls.
    cat("    3.) Constructing weights. \n\n")
    tau_hat <- lapply(regional_donor_pools, function(x) {call_synthdid(x, outcome_colname, estimator, covariate_colnames)})

    ## 4.) Save synthdid object.
    output[[counter]] <- tau_hat
    counter <- counter + 1
  }

  ## Output.
  output[[counter]] <- outcome_colname
  output[[counter + 1]] <- dta
  output[[counter + 2]] <- treatment_date
  names(output) <- c(champions, "outcome_colname", "dta", "treatment_date")
  return(output)
}
