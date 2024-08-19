
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
  rr = rand(n=length(alphavec), shape=alphavec, scale=1)
  a <- array(rr, dim(alphavec))
  temp <- apply(a, dimsum, sum)
  a <- a / array(apply(a, dimsum, sum), dim = dim(a))
  return(a)
}
