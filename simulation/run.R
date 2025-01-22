unloadNamespace("BKT")
rm(list = ls())
devtools::load_all("./")
# library(BKT)

# defaults
model <- bkt(num_fits = 1, parallel = FALSE, defaults = list("order_id" = "order_id", "user_id" = "student_id", "correct" = "correct", "skill_name" = "skill_name"))

result <- fit(model, data_path = "simulation_data.csv")
print(params(result))
