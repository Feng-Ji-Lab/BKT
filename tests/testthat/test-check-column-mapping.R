test_that("custom column mappings work with renamed simulation data", {
    custom_data <- simulation_test_data()
    names(custom_data) <- c("r1", "r2", "r3", "r4")
    model <- bkt(
        seed = 42,
        num_fits = 1,
        parallel = FALSE,
        defaults = list(
            order_id = "r1",
            correct = "r2",
            user_id = "r3",
            skill_name = "r4"
        )
    )

    fitted_model <- fit(model, data = custom_data)
    predictions <- predict_bkt(fitted_model, data = custom_data)

    expect_gt(nrow(params(fitted_model)), 0)
    expect_equal(nrow(predictions), nrow(custom_data))
    expect_true(all(c("correct_predictions", "state_predictions") %in% names(predictions)))
})
