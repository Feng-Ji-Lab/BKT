library(testthat)
test_that("test-usage_2", {
    # Verify the parallel configuration without opening PSOCK ports during
    # CRAN checks. Model fitting is covered by the simulation-data tests.
    model <- bkt(seed = 42, num_fits = 1, parallel = TRUE)

    expect_s4_class(model, "Model")
    expect_true(model@parallel)
})
