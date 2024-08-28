M_step_run <- function(model, trans_softcounts, emission_softcounts, init_softcounts, fixed = list()) {
  z <- apply(trans_softcounts, c(1, 3), sum) == 0
  for (i in seq_len(dim(z)[1])) {
    for (j in seq_len(dim(z)[2])) {
      if (z[i, j]) {
        trans_softcounts[i, 1, j] <- 0
        trans_softcounts[i, 2, j] <- 1
      }
    }
  }
    emission_sums <- apply(emission_softcounts, c(1, 2), sum)

    zero_indices <- which(emission_sums == 0, arr.ind = TRUE)

    for (idx in seq_len(nrow(zero_indices))) {
        i <- zero_indices[idx, 1]
        j <- zero_indices[idx, 2]
        emission_softcounts[i, j, ] <- 1
    }

  stopifnot(dim(trans_softcounts)[2] == 2)
  stopifnot(dim(trans_softcounts)[3] == 2)
  temp <- apply(trans_softcounts, c(1,3), sum)
trans_sums <- apply(trans_softcounts, c(1,3), sum)
model$As <- divide_python(trans_softcounts, trans_sums)
  model$learns <- model$As[, 2, 1]

  if ("learns" %in% names(fixed)) {
    stop("not implemented")
    model$learns <- model$As[, 2, 1] * (fixed$learns < 0) + fixed$learns * (fixed$learns >= 0)
    for (i in seq_len(dim(model$As)[1])) {
      if (fixed$learns[i] >= 0) {
        model$As[i, 2, 1] <- fixed$learns[i]
        model$As[i, 1, 1] <- 1 - fixed$learns[i]
      }
    }
  }

  model$forgets <- model$As[, 1, 2]

  if ("forgets" %in% names(fixed)) {
    stop("not implemented")
    model$forgets <- model$As[, 1, 2] * (fixed$forgets < 0) + fixed$forgets * (fixed$forgets >= 0)
    for (i in seq_len(dim(model$As)[1])) {
      if (fixed$forgets[i] >= 0) {
        model$As[i, 1, 2] <- fixed$forgets[i]
        model$As[i, 2, 2] <- 1 - fixed$forgets[i]
      }
    }
  }

  temp <- apply(emission_softcounts, c(1,2), sum)
  model$emissions <- divide_python(emission_softcounts,temp, type = 2)
  model$guesses <- model$emissions[, 1, 2]

  if ("guesses" %in% names(fixed)) {
    stop("not implemented")
    model$guesses <- model$guesses * (fixed$guesses < 0) + fixed$guesses * (fixed$guesses >= 0)
  }

  model$slips <- model$emissions[, 2, 1]

  if ("slips" %in% names(fixed)) {
    stop("not implemented")
    model$slips <- model$slips * (fixed$slips < 0) + fixed$slips * (fixed$slips >= 0)
  }

  if ("prior" %in% names(fixed)) {
    stop("not implemented")
    model$pi_0 <- matrix(c(1 - fixed$prior, fixed$prior), ncol = 1)
    model$prior <- model$pi_0[2, 1]
  } else {
    temp <- sum(init_softcounts)
    model$pi_0 <- init_softcounts / temp
    model$prior <- model$pi_0[2, 1]
  }

  return(model)
}

# special handle python matrix diviations
divide_python <- function(m1,m2, type = 1) {
    if (all(dim(m1) == c(1, 2, 2)) && all(dim(m2) == c(1, 2))) {
        if(type == 1) {
            m2_rep <- rep(as.vector(m2), 2)
            m2_tr <- reshape_python(m2_rep, dim = dim(m1))
            return(m1 / m2_tr)
        } else {
            m2_rep <- rep(as.vector(m2), 2)
            m2_tr <- array(m2_rep, dim = dim(m1))
            return(m1 / m2_tr)
        }
    } else {
        stop("not implemented")
    }
}