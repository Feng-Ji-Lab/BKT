test_that("test-fetch_dataset.R", {
    model <- bkt()
    fetch_dataset(model, 'https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv', '.')
})