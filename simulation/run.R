unloadNamespace("BKT")
rm(list = ls())
# source("simulation/generator.R")

devtools::load_all("./")

prior <- 0.2
guess <- 0.1
slip <- 0.1
learn <- 0.3
num_students <- 4000
max_questions <- 100
num_fits <- 1
data <- simulate_bkt_data(prior, guess, slip, learn, num_students, max_questions)
# parallel
model <- bkt(
    num_fits = num_fits, parallel = TRUE,
    defaults = list(
        order_id = "order_id", user_id = "student_id",
        correct = "correct", skill_name = "skill_name"
    )
)
time_parallel <- system.time({
    result_parallel <- fit(model, data = data)
})
print(params(result_parallel))
cat("Elapsed (parallel = TRUE):", time_parallel["elapsed"], "seconds\n")
# unparallel

model <- bkt(num_fits = num_fits, parallel = FALSE, defaults = list("order_id" = "order_id", "user_id" = "student_id", "correct" = "correct", "skill_name" = "skill_name"))
time_serial <- system.time({
    result_serial <- fit(model, data = data)
})
print(params(result_serial))
cat("Elapsed (parallel = FALSE):", time_serial["elapsed"], "seconds\n\n")


#        skill   param   class    value
# 1 mathematic  learns default 0.298000
# 2 mathematic guesses default 0.052114
# 3 mathematic   slips default 0.048631
# 4 mathematic   prior default 0.208322
