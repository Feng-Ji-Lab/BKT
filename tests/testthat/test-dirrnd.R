test_that("test-dirrnd.R", {
  target = c(1,10,100,1000)
  matrix <- reshape_python(target, dim = c(2,2))
  trans_prior <- reshape_python(matrix, dim = c(1, 2, 2))
  result <- dirrnd(trans_prior)
  result <- reshape_python(result, dim = c(4))
  result = result*1000 / target
  # check the results
  for (i in seq_along(result)) {
  expect_true(result[i] > 0.1 & result[i] < 10, 
              info = paste("ERROR:: test-dirrnd:: Element", i,"(", result[i], ") of result should be between 0.1 and 10."))
  }
})