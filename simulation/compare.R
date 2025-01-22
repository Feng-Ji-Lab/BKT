unloadNamespace("BKT")
rm(list = ls())
source("simulation/generator.R")

devtools::load_all("./")

# Define parameters
prior <- 0.2
guess <- 0.03
slip <- 0.01
learn <- 0.2
max_questions <- 10

# Call simulate_bkt_data with different values of num_students
num_students_values <- c(50, 500, 2000)

# Store results in a list
data_list <- list()

for (num_students in num_students_values) {
    print(num_students)
    output_file <- paste("simulation_data_", num_students, ".csv", sep = "")
    data <- simulate_bkt_data(prior, guess, slip, learn, num_students, max_questions, output_file = output_file)

    model <- bkt(num_fits = 5, parallel = FALSE, defaults = list("order_id" = "order_id", "user_id" = "student_id", "correct" = "correct", "skill_name" = "skill_name"))
    result <- fit(model, data = data)

    print(params(result))
}
