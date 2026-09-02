variant_cases <- list(
    multilearn = c(multilearn = TRUE),
    multigs = c(multigs = TRUE),
    multilearn_multigs = c(multilearn = TRUE, multigs = TRUE),
    multipair = c(multipair = TRUE),
    multipair_multigs = c(multipair = TRUE, multigs = TRUE),
    multiprior = c(multiprior = TRUE),
    multiprior_multigs = c(multiprior = TRUE, multigs = TRUE)
)

for (case_name in names(variant_cases)) {
    test_that(paste("model variant", case_name, "fits simulation data"), {
        variant_args <- as.list(variant_cases[[case_name]])
        model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
        fitted_model <- do.call(
            fit,
            c(
                list(
                    object = model,
                    data = simulation_test_data(with_model_classes = TRUE),
                    forgets = TRUE,
                    skills = "mathematic"
                ),
                variant_args
            )
        )
        fitted_params <- params(fitted_model)

        expect_s4_class(fitted_model, "Model")
        expect_gt(nrow(fitted_params), 0)
        expect_true(all(is.finite(as.numeric(fitted_params$value))))
    })
}
