test_that("test-usage_1", {
    model = bkt(seed = 42, num_fits = 1)
    fetch_dataset(model, 'https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv', '.')
    fit(model, data_path = 'ct.csv', skills = ".*Plot.*")
})