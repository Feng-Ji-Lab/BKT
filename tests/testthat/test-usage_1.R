test_that("test-usage_1", {
    model = Model(seed = 42, num_fits = 1)
    fetch_dataset(model, 'https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv', '.')

})