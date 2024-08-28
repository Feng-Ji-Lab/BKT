unloadNamespace("BKT");rm(list = ls());devtools::load_all("./");
library("parallel")

model = bkt(seed = 1, num_fits = 1, parallel = FALSE)
fetch_dataset(model, 'https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv', '.')
result = fit(model, data_path = 'ct.csv', skills = ".*Plot.*")

print(params(result))