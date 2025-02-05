unloadNamespace("BKT")
rm(list = ls())
source("simulation/generator.R")

devtools::load_all("./")

# Define parameters
prior <- 0
guess <- 0.5
slip <- 0.05
learn <- 0.3
max_questions <- 10

# True parameters for comparison
true_params <- c(learns = learn, guesses = guess, slips = slip, prior = prior)

# Function to compute MSE and Bias
compute_metrics <- function(estimated, true_values) {
    estimated <- as.numeric(estimated)
    errors <- estimated - true_values
    mse <- errors^2
    bias <- errors
    return(list(MSE = mse, Bias = bias))
}

# Number of simulations
num_simulations <- 100

# Call simulate_bkt_data with different values of num_students
num_students_values <- c(50, 500, 2000)

# Store results
results_list <- list()

library(progress) # 加载进度条库

for (num_students in num_students_values) {
    print(num_students)
    output_file <- paste("simulation_data_", num_students, ".csv", sep = "")

    mse_values <- matrix(0, nrow = num_simulations, ncol = 4, dimnames = list(NULL, names(true_params)))
    bias_values <- matrix(0, nrow = num_simulations, ncol = 4, dimnames = list(NULL, names(true_params)))

    pb <- progress_bar$new(
        format = "  Processing [:bar] :percent in :elapsed",
        total = num_simulations, clear = FALSE, width = 60
    )

    for (i in 1:num_simulations) {
        pb$tick() # 更新进度条
        data <- simulate_bkt_data(prior, guess, slip, learn, num_students, max_questions, output_file = output_file)

        model <- bkt(num_fits = 5, parallel = FALSE, defaults = list("order_id" = "order_id", "user_id" = "student_id", "correct" = "correct", "skill_name" = "skill_name"))
        result <- fit(model, data = data)

        estimated_params <- setNames(params(result)$value, params(result)$param)
        metrics <- compute_metrics(estimated_params[names(true_params)], true_params)

        mse_values[i, ] <- metrics$MSE
        bias_values[i, ] <- metrics$Bias
    }

    avg_mse <- colMeans(mse_values)
    avg_bias <- colMeans(bias_values)

    results_list[[as.character(num_students)]] <- list(MSE = avg_mse, Bias = avg_bias)
    print(paste("MSE:", avg_mse, "Bias:", avg_bias))
}


# Print final results
print(results_list)

# # Define parameters
# prior <- 0
# guess <- 0.5
# slip <- 0.05
# learn <- 0.3
# max_questions <- 10
# # # Define parameters
# # prior <- 0.2
# # guess <- 0.03
# # slip <- 0.01
# # learn <- 0.2
# # max_questions <- 10
# # $`50`
# # $`50`$MSE
# #       learns      guesses        slips        prior
# # 0.0020732857 0.0003929945 0.0001042762 0.0038343926

# # $`50`$Bias
# #      learns     guesses       slips       prior
# #  0.00658904 -0.00064629 -0.00026916  0.00045293


# # $`500`
# # $`500`$MSE
# #       learns      guesses        slips        prior
# # 1.678112e-04 4.309942e-05 1.317629e-05 3.107675e-04

# # $`500`$Bias
# #      learns     guesses       slips       prior
# # -0.00010905  0.00052652  0.00063621 -0.00049037


# # $`2000`
# # $`2000`$MSE
# #       learns      guesses        slips        prior
# # 3.680554e-05 8.014726e-06 3.179281e-06 7.960864e-05

# # $`2000`$Bias
# #      learns     guesses       slips       prior
# #  0.00118115 -0.00024965 -0.00016040 -0.00011429


# prior <- 0
# guess <- 0.5
# slip <- 0.05
# learn <- 0.3
# max_questions <- 10

# $`50`
# $`50`$MSE
#      learns     guesses       slips       prior
# 0.010617775 0.009160709 0.001364555 0.020640199

# $`50`$Bias
#      learns     guesses       slips       prior
#  0.03544142 -0.05450892  0.01249428  0.09101379


# $`500`
# $`500`$MSE
#       learns      guesses        slips        prior
# 0.0007106359 0.0007432631 0.0001194657 0.0017977936

# $`500`$Bias
#      learns     guesses       slips       prior
#  0.00533499 -0.01243248  0.00206705  0.02922964


# $`2000`
# $`2000`$MSE
#       learns      guesses        slips        prior
# 1.744229e-04 1.950385e-04 3.645666e-05 6.758397e-04

# $`2000`$Bias
#      learns     guesses       slips       prior
# -0.00031973 -0.00864346  0.00147408  0.02054502
