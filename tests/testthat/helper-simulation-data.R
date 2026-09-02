simulation_test_data <- function(with_model_classes = FALSE) {
    data_env <- new.env(parent = emptyenv())
    utils::data("simulation_data_50", package = "BKT", envir = data_env)
    result <- data_env$simulation_data_50

    if (with_model_classes) {
        result$multilearn <- paste0("resource_", result$order_id %% 2 + 1)
        result$multigs <- paste0("subpart_", result$order_id %% 2 + 1)
        result$multipair <- paste0("resource_", result$order_id %% 3 + 1)
        result$multiprior <- paste0("group_", result$student_id %% 2 + 1)
    }

    result
}

fit_simulation_model <- function(forgets = FALSE) {
    model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
    fit(model, data = simulation_test_data(), forgets = forgets)
}
