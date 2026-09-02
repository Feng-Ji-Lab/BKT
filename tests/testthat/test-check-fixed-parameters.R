test_that("fixed prior remains unchanged during fitting", {
    model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
    model <- set_coef(model, list(mathematic = list(prior = 0.5)))
    fitted_model <- fit(
        model,
        data = simulation_test_data(),
        forgets = TRUE,
        skills = "mathematic",
        fixed = list(mathematic = list(prior = TRUE))
    )

    prior_values <- params(fitted_model)$value[params(fitted_model)$param == "prior"]
    expect_equal(as.numeric(prior_values), 0.5, tolerance = 1e-8)
})

test_that("fixed learning and forgetting rates remain unchanged", {
    model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
    model <- set_coef(model, list(mathematic = list(learns = 0.25, forgets = 0.25)))
    fitted_model <- fit(
        model,
        data = simulation_test_data(),
        forgets = TRUE,
        fixed = list(mathematic = list(learns = TRUE, forgets = TRUE))
    )
    fitted_params <- params(fitted_model)

    expect_equal(as.numeric(fitted_params$value[fitted_params$param == "learns"]), 0.25)
    expect_equal(as.numeric(fitted_params$value[fitted_params$param == "forgets"]), 0.25)
})

test_that("fixed guess and slip rates remain unchanged", {
    model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
    model <- set_coef(model, list(mathematic = list(guesses = 0.025, slips = 0.025)))
    fitted_model <- fit(
        model,
        data = simulation_test_data(),
        forgets = TRUE,
        fixed = list(mathematic = list(guesses = TRUE, slips = TRUE))
    )
    fitted_params <- params(fitted_model)

    expect_equal(as.numeric(fitted_params$value[fitted_params$param == "guesses"]), 0.025)
    expect_equal(as.numeric(fitted_params$value[fitted_params$param == "slips"]), 0.025)
})
