test_that("hello works", {
  expect_output(hello(), "Hello, world!")
  # Define the trans_prior matrix
  trans_prior <- array(t(matrix(c(20, 4, 1, 20), nrow = 2, byrow = TRUE)), dim = c(1, 2, 2))
  # Call the dirrnd function
  aa <- dirrnd(trans_prior)
  # Print the results
  print(trans_prior)
  print(aa)
})