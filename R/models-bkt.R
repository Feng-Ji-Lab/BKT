#' bkt(...)
#'
#' create bkt model object with initial parameters.
#'
#' @param parallel switch of parallel caculation.
#' @param seed seed for random number generate.
#' @param num_fits fits times
#' @return a bkt model object, used by other bkt functions.
#' @export
bkt <- function(...) {
  new("Model", ...)
}
