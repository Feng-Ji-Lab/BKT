test_that("test-fetch_dataset.R", {
    model <- Model()
    fetch_dataset(model, 'https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv', '.')
})