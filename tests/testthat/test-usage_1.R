library(testthat)
test_that("test-usage_1", {
    # parallel off
    data("simulation_data_50", package = "BKT")
    model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
    fitted_model <- fit(model, data = simulation_data_50)

    expect_s4_class(fitted_model, "Model")
    expect_equal(fitted_model@skills, "mathematic")
})
