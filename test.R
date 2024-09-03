unloadNamespace("BKT")
rm(list = ls())
devtools::load_all("./")

model <- bkt(seed = 3, num_fits = 5, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")
result <- fit(model, data_path = "ct.csv", skills = ".*Plot.*")
print(params(result))
print("fit finished")
mae <- function(true_vals, pred_vals) {
    return(mean(abs(true_vals - pred_vals)))
}

result <- evaluate(model, data_path = "ct.csv", metric = mae)

print(result)
