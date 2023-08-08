#' Waiting for Next Iteration
#'
#' It stops the execution of R for a random number of seconds. Useful to avoid hitting rate limits while web-scraping or pulling from APIs.
#'
#' @param min_sec Minimum amount of seconds to be waited.
#' @param max_sec Maximum amount of seconds to be waited.
#'
#' @author Riccardo Di Francesco
#'
#' @keywords internal
sleep <- function(min_sec = 0, max_sec = 0) {
  ## Handling inputs and checks.
  if (min_sec < 0) stop("Negative time does not make sense.", call. = FALSE)
  if (max_sec < 0) stop("Negative time does not make sense.", call. = FALSE)

  Sys.sleep(stats::runif(1, min = round(min_sec, 0), max = round(max_sec, 0)))
}
