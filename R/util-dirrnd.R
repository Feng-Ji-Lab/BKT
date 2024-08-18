
#' dirrnd
#'
#' Generate random samples from a Dirichlet distribution.
#'
#' @keywords internal
dirrnd <- function(alphavec, rand = rgamma) {
  dimsum <- 1
  if (length(dim(alphavec)) == 3 || length(dim(alphavec)) == 2) {
    dimsum <- length(dim(alphavec)) - 2
  }
  a <- array(rand(alphavec, 1), dim = dim(alphavec))
  temp <- apply(a, dimsum, sum)
  a <- a / array(apply(a, dimsum, sum), dim = dim(a))
  return(a)
}
