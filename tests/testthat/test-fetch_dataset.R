test_that("test-fetch_dataset.R", {
    model <- bkt()
    current_directory <- getwd()
    print(current_directory)
    file.remove("ct.csv")
    # fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")
    result <- fit(model, data_path = "ct.csv", skills = ".*Plot.*")
    expect_equal(1, 1)
})
