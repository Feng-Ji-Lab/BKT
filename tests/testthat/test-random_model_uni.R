test_that("test-random_model_uni.R", {
    # 示例使用
    set.seed(123)
    # result <- random_model_uni()
    # print(result)
    num_resources = 3
    num_subparts = 3

    # dismiss handle 1
    trans_prior <- t(array(c(20, 4, 1, 20), dim = c(2, 2)))
    trans_prior <- reshape_python(rep(trans_prior, num_resources), dim = c(num_resources, 2, 2))
    target <- reshape_python(c(20, 1, 4, 20, 20, 1, 4, 20, 20, 1, 4, 20), dim = c(num_resources, 2, 2))
    # print_3_dim_matrix(trans_prior)
    # print_3_dim_matrix(target)
    expect_equal(trans_prior, target)

    # dismiss handle 2
    given_notknow_prior <- array(rep(c(5, 0.5), num_subparts), dim = c(2, num_subparts))
    # print(given_notknow_prior)
    expect_equal(given_notknow_prior, reshape_python(c(5,5,5,0.5,0.5,0.5), dim = c(2,3)))

    given_know_prior <- array(rep(c(0.5, 5), num_subparts), dim = c(2, num_subparts))
    
    pi_0_prior <- matrix(c(100, 1), nrow = 2, byrow = TRUE)

    # model parameter generate
    # As <- dirrnd(trans_prior)
    # given_notknow <- dirrnd(given_notknow_prior)
    # given_know <- dirrnd(given_know_prior)
    # pi_0 <- dirrnd(pi_0_prior)
    # print_3_dim_matrix(As)
})