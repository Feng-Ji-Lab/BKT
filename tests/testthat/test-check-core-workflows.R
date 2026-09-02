test_that("built-in simulation data has the expected structure", {
    simulation_data <- simulation_test_data()

    expect_s3_class(simulation_data, "data.frame")
    expect_named(
        simulation_data,
        c("order_id", "correct", "student_id", "skill_name")
    )
    expect_equal(length(unique(simulation_data$student_id)), 50L)
    expect_equal(unique(simulation_data$skill_name), "mathematic")
})

test_that("models fit simulation data and expose parameters", {
    fitted_model <- fit_simulation_model()
    fitted_params <- params(fitted_model)

    expect_s4_class(fitted_model, "Model")
    expect_equal(fitted_model@skills, "mathematic")
    expect_true(all(c("skill", "param", "class", "value") %in% names(fitted_params)))
    expect_true(all(c("learns", "guesses", "slips", "prior") %in% fitted_params$param))
})

test_that("models can be saved and loaded without changing parameters", {
    fitted_model <- fit_simulation_model()
    model_file <- tempfile(fileext = ".rds")
    on.exit(unlink(model_file), add = TRUE)

    save_model(fitted_model, model_file)
    loaded_model <- load_model(bkt(), model_file)

    expect_true(file.exists(model_file))
    expect_equal(params(loaded_model), params(fitted_model))
})

test_that("evaluation supports RMSE and a custom metric", {
    simulation_data <- simulation_test_data()
    fitted_model <- fit_simulation_model()
    mae <- function(true_vals, pred_vals) mean(abs(true_vals - pred_vals))

    rmse_value <- evaluate(fitted_model, data = simulation_data)
    mae_value <- evaluate(fitted_model, data = simulation_data, metric = mae)

    expect_true(is.finite(rmse_value))
    expect_true(is.finite(mae_value))
    expect_gte(rmse_value, 0)
    expect_gte(mae_value, 0)
})

test_that("prediction returns probabilities for every response", {
    simulation_data <- simulation_test_data()
    fitted_model <- fit_simulation_model(forgets = TRUE)
    predictions <- predict_bkt(fitted_model, data = simulation_data)

    expect_equal(nrow(predictions), nrow(simulation_data))
    expect_true(all(c("correct_predictions", "state_predictions") %in% names(predictions)))
    expect_true(all(predictions$correct_predictions >= 0 & predictions$correct_predictions <= 1))
    expect_true(all(predictions$state_predictions >= 0 & predictions$state_predictions <= 1))
})

test_that("cross-validation runs on simulation data", {
    model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
    cv_result <- crossvalidate(
        model,
        data = simulation_test_data(),
        folds = 2,
        parallel = FALSE
    )

    expect_s3_class(cv_result, "data.frame")
    expect_equal(cv_result$skill, "mathematic")
    expect_true(all(is.finite(unlist(cv_result[-1]))))
})

test_that("forgetting can be enabled or disabled", {
    params_with_forgetting <- params(fit_simulation_model(forgets = TRUE))
    params_without_forgetting <- params(fit_simulation_model(forgets = FALSE))

    expect_true("forgets" %in% params_with_forgetting$param)
    expect_false("forgets" %in% params_without_forgetting$param)
})
