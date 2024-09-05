unloadNamespace("BKT")
rm(list = ls())
devtools::load_all("./")

model <- bkt(seed = 2, num_fits = 1, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

# MARK: Fit
# result <- fit(model, data_path = "ct.csv", skills = ".*Plot.*")
# print(params(result))
# print("fit finished")

# MARK: Evaluate
# mae <- function(true_vals, pred_vals) {
#     return(mean(abs(true_vals - pred_vals)))
# }
# result <- evaluate(result, data_path = "ct.csv", metric = mae)
# print(result)

# MARK: Predict
model <- bkt(seed = 42, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/as.csv", ".")
result <- fit(model, data_path = "as.csv", forgets = FALSE, skills = "Box and Whisker")
print(params(result))
preds_df <- predict(result, data_path = "as.csv")
box_and_whisker_preds <- subset(preds_df, skill_name == "Box and Whisker",
    select = c("user_id", "correct", "correct_predictions", "state_predictions")
)
print(box_and_whisker_preds)

# MARK: Crossvalidate
# crossvalidate(result, folds = 3)
