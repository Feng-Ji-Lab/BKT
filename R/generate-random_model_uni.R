random_model_uni <- function(num_resources = 1, num_subparts = 1, 
                             trans_prior = NULL, given_notknow_prior = NULL, 
                             given_know_prior = NULL, pi_0_prior = NULL, rand = runif) {
  # dismiss handle
  if (is.null(trans_prior)) {
    trans_prior <- t(array(c(20, 4, 1, 20), dim = c(2, 2)))
    trans_prior <- reshape_python(rep(trans_prior, num_resources), dim = c(num_resources, 2, 2))
  }
  if (is.null(given_notknow_prior)) {
    given_notknow_prior <- array(rep(c(5, 0.5), num_subparts), dim = c(2, num_subparts))
  }
  if (is.null(given_know_prior)) {
    given_know_prior <- array(rep(c(0.5, 5), num_subparts), dim = c(2, num_subparts))
  }
  if (is.null(pi_0_prior)) {
    pi_0_prior <- matrix(c(100, 1), nrow = 2, byrow = TRUE)
  }

  # model parameter generate
  As <- dirrnd(trans_prior)
  given_notknow <- dirrnd(given_notknow_prior)
  given_know <- dirrnd(given_know_prior)
  pi_0 <- dirrnd(pi_0_prior)

  # 创建发射矩阵
  emissions <- array(0, dim = c(num_subparts, 2, 2))
  for (i in seq_len(num_subparts)) {
    emissions[i, , ] <- rbind(given_notknow[, i], given_know[, i])
  }

  # 初始化模型结构
  modelstruct <- list()
  modelstruct$prior <- runif(1)
  
  # 设置转移矩阵参数
  As[1, 2, ] <- rand(num_resources) * 0.40
  As[2, 2, ] <- 1 - As[1, 2, ]
  As[2, 1, ] <- 0
  As[1, 1, ] <- 1
  
  modelstruct$learns <- As[1, 2, ]
  modelstruct$forgets <- As[2, 1, ]
  
  # 设置猜测和滑动参数
  given_notknow[2, ] <- rand(num_subparts) * 0.40
  modelstruct$guesses <- given_notknow[2, ]
  
  given_know[1, ] <- rand(num_subparts) * 0.30
  modelstruct$slips <- given_know[1, ]
  
  modelstruct$As <- As
  modelstruct$emissions <- emissions
  modelstruct$pi_0 <- pi_0

  return(modelstruct)
}
