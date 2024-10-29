unloadNamespace("BKT")
rm(list = ls())
devtools::load_all("./")

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

# # MARK: Fit
# result <- fit(model, data_path = "ct.csv", skills = "Plot non-terminating improper fraction")
# print(params(result))

# # MARK: Save & Load
# save(result, "model.pkl")
# model2 <- bkt()
# model2 <- load(model2, "model.pkl")
# # print(params(model2))

# # MARK: Evaluate
# library(BKT)

# model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")
# result <- fit(model, data_path = "ct.csv", skills = "Plot non-terminating improper fraction")

# training_rmse <- evaluate(result, data_path = "ct.csv")
# print(training_rmse)

# We can define a custom metric as well
# mae <- function(true_vals, pred_vals) {
#     return(mean(abs(true_vals - pred_vals)))
# }
# training_mae <- evaluate(result, data_path = "ct.csv", metric = mae)
# print(training_mae)

# MARK: Predict
# model <- bkt(seed = 42, parallel = FALSE)
# fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/as.csv", ".")
# result <- fit(model, data_path = "as.csv", forgets = TRUE, skills = "Box and Whisker")
# print(params(result))
# save(result, "model.pkl")
# model2 <- bkt()
# model2 <- load(model2, "model.pkl")
# preds_df <- predict_bkt(model2, data_path = "as.csv")
# box_and_whisker_preds <- subset(preds_df, skill_name == "Box and Whisker",
#     select = c("user_id", "correct", "correct_predictions", "state_predictions")
# )
# print(head(box_and_whisker_preds))

# # MARK:Crossvalidate
# start_time <- Sys.time()
# result <- crossvalidate(model2, data_path = "ct.csv", skills = "Plot non-terminating improper fraction", folds = 2, parallel = FALSE)
# print(result)
# end_time <- Sys.time()
# elapsed <- difftime(end_time, start_time, units = "secs")
# cat("Elapsed time:", round(elapsed, 2), "seconds\n")

# # MARK: forgets
# result <- fit(model, data_path = "ct.csv", forgets = TRUE, parallel = FALSE, skills = "Plot non-terminating improper fraction")
# print(params(result))
# result <- fit(model, data_path = "ct.csv", forgets = FALSE, parallel = FALSE, skills = "Plot non-terminating improper fraction")
# print(params(result))

# # MARK: fixed
# # prior
# model <- set_coef(model, list("Plot non-terminating improper fraction" = list("prior" = 0.5)))
# result <- fit(model,
#     forgets = TRUE,
#     data_path = "ct.csv", skills = "Plot non-terminating improper fraction",
#     fixed = list("Plot non-terminating improper fraction" = list("prior" = TRUE))
# )
# print(params(result))
# # learns and forgets
# model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# model <- set_coef(model, list("Plot non-terminating improper fraction" = list("learns" = c(0.25), "forgets" = c(0.25))))
# print(params(model))
# result <- fit(model,
#     data_path = "ct.csv", forgets = TRUE, skills = "Plot non-terminating improper fraction",
#     fixed = list("Plot non-terminating improper fraction" = list("learns" = TRUE, "forgets" = TRUE))
# )
# print(params(result))
# # guesses and slips
# model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# model <- set_coef(model, list("Plot non-terminating improper fraction" = list("guesses" = c(0.025), "slips" = c(0.025))))
# print(params(model))
# result <- fit(model,
#     data_path = "ct.csv", forgets = TRUE, skills = "Plot non-terminating improper fraction",
#     fixed = list("Plot non-terminating improper fraction" = list("guesses" = TRUE, "slips" = TRUE))
# )
# print(params(result))

# MARK: BKT variants
# multilearn, multiprior, multipair, multigs
# multilearn
# model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# result <- fit(model, data_path = "ct.csv", multilearn = TRUE, forgets = TRUE, skills = "Plot non-terminating improper fraction")
# print(params(result))

# MARK: multigs
# result <- fit(model, data_path = "ct.csv", skills = c("Plot imperfect radical", "Plot pi"), multigs = TRUE)
# print(params(result))

# MARK: multigs and multilearn
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
result <- fit(model, data_path = "ct.csv", multilearn = TRUE, multigs = TRUE, forgets = TRUE, skills = "Plot non-terminating improper fraction")
print(params(result))

# library(BKT)

# model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

# # Fixes the prior rate to 0.5 for the 'Plot non-terminating improper fraction' skill, and trains the model given those fixed parameters
# model <- set_coef(model, list("Plot non-terminating improper fraction" = list("prior" = 0.5)))
# result <- fit(model,
#     forgets = TRUE,
#     data_path = "ct.csv", skills = "Plot non-terminating improper fraction",
#     fixed = list("Plot non-terminating improper fraction" = list("prior" = TRUE))
# )
# print(params(result))

# library(BKT)

# model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

# model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# model <- set_coef(model, list("Plot non-terminating improper fraction" = list("learns" = c(0.25), "forgets" = c(0.25))))
# print(params(model))
# result <- fit(model,
#     data_path = "ct.csv", forgets = TRUE, skills = "Plot non-terminating improper fraction",
#     fixed = list("Plot non-terminating improper fraction" = list("learns" = TRUE, "forgets" = TRUE))
# )
# print(params(result))
