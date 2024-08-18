#' Say hello
#'
#' This function prints 'Hello, world!'.
#'
#' @export
hello <- function() {
  hello1();
  trans_prior <- array(t(matrix(c(20, 4, 1, 20), nrow = 2, byrow = TRUE)), dim = c(1, 2, 2))
  # Call the dirrnd function
  aa <- dirrnd(trans_prior);
  # Print the results
  print(trans_prior)
  print
}