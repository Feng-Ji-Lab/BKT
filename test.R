unloadNamespace("BKT")
rm(list = ls())
devtools::load_all("./")
library("parallel")
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

# MARK: Fit
# result <- fit(model, data_path = "ct.csv", skills = ".*Plot.*")
# print(params(result))
# print("fit finished")

# MARK: Save & Load
# save(result, "model.pkl")
model2 <- bkt()
model2 <- load(model2, "model.pkl")
# print(params(model2))

# MARK: Evaluate
# mae <- function(true_vals, pred_vals) {
#     return(mean(abs(true_vals - pred_vals)))
# }
# result <- evaluate(result, data_path = "ct.csv", metric = mae)
# print(result)

# MARK: Predict
# model <- bkt(seed = 42, parallel = FALSE)
# fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/as.csv", ".")
# result <- fit(model, data_path = "as.csv", forgets = FALSE, skills = "Box and Whisker")
# print(params(result))
# preds_df <- predict(result, data_path = "as.csv")
# box_and_whisker_preds <- subset(preds_df, skill_name == "Box and Whisker",
#     select = c("user_id", "correct", "correct_predictions", "state_predictions")
# )
# print(box_and_whisker_preds)

# MARK:Crossvalidate
# start_time <- Sys.time()
# result <- crossvalidate(model2, data_path = "ct.csv", skills = "Plot non-terminating improper fraction", folds = 2, parallel = FALSE)
# print(result)
# end_time <- Sys.time()
# elapsed <- difftime(end_time, start_time, units = "secs")
# cat("Elapsed time:", round(elapsed, 2), "seconds\n")

# MARK: forgets
# result <- fit(model, data_path = "ct.csv", forgets = TRUE, parallel = FALSE, skills = "Plot non-terminating improper fraction")
# print(params(result))
# result <- fit(model, data_path = "ct.csv", forgets = FALSE, parallel = FALSE, skills = "Plot non-terminating improper fraction")
# print(params(result))

# MARK: fixed
# prior
# model <- set_coef(model, list("Plot non-terminating improper fraction" = list("prior" = 0.5)))
# result <- fit(model,
#     forgets = TRUE,
#     data_path = "ct.csv", skills = "Plot non-terminating improper fraction",
#     fixed = list("Plot non-terminating improper fraction" = list("prior" = TRUE))
# )
# print(params(result))
# learns and forgets
# model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# model <- set_coef(model, list("Plot non-terminating improper fraction" = list("learns" = c(0.25), "forgets" = c(0.25))))
# print(params(model))
# result <- fit(model,
#     data_path = "ct.csv", forgets = TRUE, skills = "Plot non-terminating improper fraction",
#     fixed = list("Plot non-terminating improper fraction" = list("learns" = TRUE, "forgets" = TRUE))
# )
# print(params(result))
# guesses and slips
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
model <- set_coef(model, list("Plot non-terminating improper fraction" = list("guesses" = c(0.025), "slips" = c(0.025))))
print(params(model))
result <- fit(model,
    data_path = "ct.csv", forgets = TRUE, skills = "Plot non-terminating improper fraction",
    fixed = list("Plot non-terminating improper fraction" = list("guesses" = TRUE, "slips" = TRUE))
)
print(params(result))

# MARK: BKT variants
# multilearn, multiprior, multipair, multigs
# multilearn
# model <- bkt(seed = 42)
# result <- fit(model, data_path = "ct.csv", multilearn = TRUE, forgets = TRUE, skills = "Plot non-terminating improper fraction")
# print(params(result))
