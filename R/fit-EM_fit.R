
#' @import parallel

EM_fit <- function(model, data, tol = 0.005, maxiter = 100, parallel = TRUE, fixed = list()) {
  
  check_data(data)
  num_subparts <- nrow(data$data)  # 第一维度对应于每个子部分
  num_resources <- length(model$learns)
  
  trans_softcounts <- array(0, dim = c(num_resources, 2, 2))
  emission_softcounts <- array(0, dim = c(num_subparts, 2, 2))
  init_softcounts <- matrix(0, nrow = 2, ncol = 1)
  log_likelihoods <- numeric(maxiter)
  
  result <- list(
    all_trans_softcounts = trans_softcounts,
    all_emission_softcounts = emission_softcounts,
    all_initial_softcounts = init_softcounts
  )
  
  for (i in seq_len(maxiter)) {
    result <- run(data, model, result$all_trans_softcounts, result$all_emission_softcounts, result$all_initial_softcounts, 1, parallel, fixed = fixed)
    
    stop("333")
    for (j in seq_len(num_resources)) {
      result$all_trans_softcounts[j,,] <- t(result$all_trans_softcounts[j,,])
    }
    
    for (j in seq_len(num_subparts)) {
      result$all_emission_softcounts[j,,] <- t(result$all_emission_softcounts[j,,])
    }
    
    log_likelihoods[i] <- result$total_loglike
    
    if (i > 1 && abs(log_likelihoods[i] - log_likelihoods[i - 1]) <= tol) {
      break
    }
    
    model <- M_step_run(model, result$all_trans_softcounts, result$all_emission_softcounts, result$all_initial_softcounts, fixed = fixed)
  }
  
  return(list(model = model, log_likelihoods = log_likelihoods[1:i]))
}

# MARK: run
run <- function(data, model, trans_softcounts, emission_softcounts, init_softcounts, num_outputs, parallel = TRUE, fixed = list()) {

  # Processed Parameters
  alldata <- data$data
  bigT <- ncol(alldata)
  num_subparts <- nrow(alldata)
  allresources <- data$resources
  starts <- data$starts
  learns <- model$learns
  forgets <- model$forgets
  guesses <- model$guesses
  slips <- model$slips
  lengths <- data$lengths

  prior <- model$prior
  num_sequences <- length(starts)
  num_resources <- length(learns)
  normalizeLengths <- FALSE
  if (!is.null(fixed$prior)) {
    prior <- fixed$prior
  }
  initial_distn <- numeric(2)
  initial_distn[1] <- 1 - prior
  initial_distn[2] <- prior
  if (!is.null(fixed$learns)) {
    learns <- learns * (fixed$learns < 0) + fixed$learns * (fixed$learns >= 0)
  }
  if (!is.null(fixed$forgets)) {
    forgets <- forgets * (fixed$forgets < 0) + fixed$forgets * (fixed$forgets >= 0)
  }
  As <- matrix(0, nrow = 2, ncol = 2 * num_resources)
  As[1,] <- c(1 - learns, forgets)
  As[2,] <- c(learns, 1 - forgets)

  if (!is.null(fixed$guesses)) {
    guesses <- guesses * (fixed$guesses < 0) + fixed$guesses * (fixed$guesses >= 0)
  }
  if (!is.null(fixed$slips)) {
    slips <- slips * (fixed$slips < 0) + fixed$slips * (fixed$slips >= 0)
  }
  Bn <- matrix(0, nrow = 2, ncol = 2 * num_subparts)
  Bn[1,] <- c(1 - guesses, guesses)
  Bn[2,] <- c(slips, 1 - slips)

  # Outputs
  all_trans_softcounts <- matrix(0, nrow = 2, ncol = 2 * num_resources)
  all_emission_softcounts <- matrix(0, nrow = 2, ncol = 2 * num_subparts)
  all_initial_softcounts <- matrix(0, nrow = 2, ncol = 1)

  alpha_out <- matrix(0, nrow = 2, ncol = bigT)

  total_loglike <- 0
  
  input <- list(
    As = As,
    Bn = Bn,
    initial_distn = initial_distn,
    allresources = allresources,
    starts = starts,
    lengths = lengths,
    num_resources = num_resources,
    num_subparts = num_subparts,
    alldata = alldata,
    normalizeLengths = normalizeLengths,
    alpha_out = alpha_out
  )

  num_threads <- if (parallel) parallel::detectCores() else 1
  thread_counts <- vector("list", num_threads)
  
  for (thread_num in seq_len(num_threads)) {
    blocklen <- 1 + ((num_sequences - 1) %/% num_threads)
    sequence_idx_start <- blocklen * (thread_num - 1)
    sequence_idx_end <- min(sequence_idx_start + blocklen, num_sequences)
    thread_counts[[thread_num]] <- c(
      list(sequence_idx_start = sequence_idx_start, sequence_idx_end = sequence_idx_end),
      input
    )
  }

  x <- list()
  if (parallel) {
    cl <- makeCluster(num_threads)
    x <- parLapply(cl, thread_counts, inner) 
    stopCluster(cl)
    print(x)
  } else {
    x <- lapply(thread_counts, inner)
  }
  stop("11")
  for (i in x) {
    total_loglike <- total_loglike + i[[4]]
    all_trans_softcounts <- all_trans_softcounts + i[[1]]
    all_emission_softcounts <- all_emission_softcounts + i[[2]]
    all_initial_softcounts <- all_initial_softcounts + i[[3]]
    for (alpha in i[[5]]) {
      sequence_start <- alpha[[1]]
      T <- alpha[[2]]
      alpha_out[, sequence_start:(sequence_start + T - 1)] <- alpha_out[, sequence_start:(sequence_start + T - 1)] + alpha[[3]]
    }
  }

  all_trans_softcounts <- as.vector(t(all_trans_softcounts))
  all_emission_softcounts <- as.vector(t(all_emission_softcounts))
  
  result <- list(
    total_loglike = total_loglike,
    all_trans_softcounts = array(all_trans_softcounts, dim = c(num_resources, 2, 2)),
    all_emission_softcounts = array(all_emission_softcounts, dim = c(num_subparts, 2, 2)),
    all_initial_softcounts = all_initial_softcounts,
    alpha_out = alpha_out
  )
  
  return(result)
}


# MARK: inner (R)
inner <- function(x) {
    print(Sys.getpid())  # 打印当前进程ID
    print("now")
    As <- x$As
    Bn <- x$Bn
    initial_distn <- x$initial_distn
    allresources <- x$allresources
    starts <- x$starts
    lengths <- x$lengths
    num_resources <- x$num_resources
    num_subparts <- x$num_subparts
    alldata <- x$alldata
    normalizeLengths <- x$normalizeLengths
    sequence_idx_start <- x$sequence_idx_start
    sequence_idx_end <- x$sequence_idx_end
    return(sequence_idx_start)

    print(c(sequence_idx_start, sequence_idx_end))
    N_R <- 2 * num_resources
    N_S <- 2 * num_subparts
    trans_softcounts_temp <- matrix(0, nrow = 2, ncol = N_R)
    emission_softcounts_temp <- matrix(0, nrow = 2, ncol = N_S)
    init_softcounts_temp <- matrix(0, nrow = 2, ncol = 1)
    loglike <- 0

    alphas <- list()

    for (sequence_index in sequence_idx_start:sequence_idx_end) {
        sequence_start <- starts[sequence_index] - 1
        T <- lengths[sequence_index]

        # 计算似然
        likelihoods <- matrix(1, nrow = 2, ncol = T)
        alpha <- matrix(NA, nrow = 2, ncol = T)
        for (t in 1:min(2, T)) {
            for (n in 1:num_subparts) {
                data_temp <- alldata[[n]][sequence_start + t]
                if (!is.na(data_temp)) {
                    sl <- Bn[, 2 * n + as.integer(data_temp == 2)]
                    likelihoods[, t] <- likelihoods[, t] * ifelse(sl == 0, 1, sl)
                }
            }
        }

        # 前向传播 alpha
        alpha[, 1] <- initial_distn * likelihoods[, 1]
        norm <- sum(alpha[, 1])
        alpha[, 1] <- alpha[, 1] / norm
        contribution <- log(norm) / (if (normalizeLengths) T else 1)
        loglike <- loglike + contribution

        # 结合 t = 2 的情况
        if (T >= 2) {
            resources_temp <- allresources[sequence_start + 1]
            k <- 2 * (resources_temp - 1)
            alpha[, 2] <- As[1:2, k:(k + 1)] %*% alpha[, 1] * likelihoods[, 2]
            norm <- sum(alpha[, 2])
            alpha[, 2] <- alpha[, 2] / norm
            contribution <- log(norm) / (if (normalizeLengths) T else 1)
            loglike <- loglike + contribution
        }

        # 处理一般情况下的 alpha 计算
        if (T > 2) {
            for (t in 3:T) {
                for (n in 1:num_subparts) {
                    data_temp <- alldata[[n]][sequence_start + t]
                    if (!is.na(data_temp)) {
                        sl <- Bn[, 2 * n + as.integer(data_temp == 2)]
                        likelihoods[, t] <- likelihoods[, t] * ifelse(sl == 0, 1, sl)
                    }
                }
                resources_temp <- allresources[sequence_start + t - 1]
                k <- 2 * (resources_temp - 1)
                alpha[, t] <- As[1:2, k:(k + 1)] %*% alpha[, t - 1] * likelihoods[, t]
                norm <- sum(alpha[, t])
                alpha[, t] <- alpha[, t] / norm
                loglike <- loglike + log(norm) / (if (normalizeLengths) T else 1)
            }
        }

        # 后向传递和统计计算
        gamma <- matrix(NA, nrow = 2, ncol = T)
        gamma[, T] <- alpha[, T]

        As_temp <- As
        first_pass <- TRUE
        for (t in (T - 1):1) {
            resources_temp <- allresources[sequence_start + t]
            k <- 2 * (resources_temp - 1)
            A <- As_temp[1:2, k:(k + 1)]
            pair <- A
            pair[1, ] <- pair[1, ] * alpha[, t]
            pair[2, ] <- pair[2, ] * alpha[, t]
            dotted <- A %*% alpha[, t]
            gamma_t <- gamma[, t + 1]
            pair[, 1] <- (pair[, 1] * gamma_t) / dotted
            pair[, 2] <- (pair[, 2] * gamma_t) / dotted
            pair[is.nan(pair)] <- 0
            trans_softcounts_temp[1:2, k:(k + 1)] <- trans_softcounts_temp[1:2, k:(k + 1)] + pair
            gamma[, t] <- colSums(pair)
            for (n in 1:num_subparts) {
                data_temp <- alldata[[n]][sequence_start + t]
                if (!is.na(data_temp)) {
                    emission_softcounts_temp[, (2 * n + as.integer(data_temp == 2))] <- emission_softcounts_temp[, (2 * n + as.integer(data_temp == 2))] + gamma[, t]
                }
                if (first_pass) {
                    data_temp_p <- alldata[[n]][sequence_start + (T - 1)]
                    if (!is.na(data_temp_p)) {
                        emission_softcounts_temp[, (2 * n + as.integer(data_temp_p == 2))] <- emission_softcounts_temp[, (2 * n + as.integer(data_temp_p == 2))] + gamma[, T]
                    }
                }
            }
            first_pass <- FALSE
        }
        init_softcounts_temp <- init_softcounts_temp + matrix(gamma[, 1], nrow = 2, ncol = 1)
        alphas[[length(alphas) + 1]] <- list(sequence_start = sequence_start, T = T, alpha = alpha)
    }

    return(list(trans_softcounts_temp = trans_softcounts_temp, emission_softcounts_temp = emission_softcounts_temp, init_softcounts_temp = init_softcounts_temp, loglike = loglike, alphas = alphas))
}
