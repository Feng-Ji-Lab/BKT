unloadNamespace("BKT")
rm(list = ls())
source("simulation/generator.R")

devtools::load_all("./")

prior <- 0.2
guess <- 0.1
slip <- 0.1
learn <- 0.3
num_students <- 500
max_questions <- 10
data <- simulate_bkt_data(prior, guess, slip, learn, num_students, max_questions)

model <- bkt(num_fits = 5, parallel = FALSE, defaults = list("order_id" = "order_id", "user_id" = "student_id", "correct" = "correct", "skill_name" = "skill_name"))
result <- fit(model, data = data)

print(params(result))
#        skill   param   class    value
# 1 mathematic  learns default 0.298000
# 2 mathematic guesses default 0.052114
# 3 mathematic   slips default 0.048631
# 4 mathematic   prior default 0.208322
