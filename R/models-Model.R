#' @import RCurl

# MARK: setClass Model
setClass(
  "Model",
  slots = list(
    parallel = "logical",
    num_fits = "numeric",
    seed = "numeric",
    defaults = "ANY",
    model_type = "logical",
    keep = "list",
    fit_model = "list",
    manual_param_init = "logical",
    skills = "character",
    folds = "numeric",
    forgets = "logical",
    fixed = "ANY",
    MODELS_BKT = "character",
    MODEL_ARGS = "character",
    FIT_ARGS = "character",
    CV_ARGS = "character",
    DEFAULTS = "list",
    DEFAULTS_BKT = "list",
    INITIALIZABLE_PARAMS = "character"
  )
)

# MARK: Init Functions
setMethod("initialize", "Model", function(.Object, parallel = TRUE, num_fits = 5, folds = 5, seed = sample(1:1e8, 1), defaults = NULL, model_type = rep(FALSE, 4), ...) {
  .Object@parallel <- parallel
  .Object@num_fits <- num_fits
  .Object@seed <- seed
  .Object@defaults <- defaults
  .Object@model_type <- model_type
  .Object@fit_model <- list()
  .Object@manual_param_init <- FALSE
  .Object@skills <- ".*"
  .Object@folds <- folds
  .Object@forgets <- FALSE

  set.seed(.Object@seed)

  # MARK: static parameter handle
  .Object@MODELS_BKT <- c("multilearn", "multiprior", "multipair", "multigs")
  .Object@MODEL_ARGS <- c("parallel", "num_fits", "seed", "defaults", .Object@MODELS_BKT)
  .Object@FIT_ARGS <- c("skills", "num_fits", "defaults", "fixed", "parallel", "forgets", "preload", .Object@MODELS_BKT)
  .Object@CV_ARGS <- c(.Object@FIT_ARGS, "folds", "seed")

  .Object@DEFAULTS <- list(
    num_fits = 5,
    defaults = NULL,
    parallel = TRUE,
    skills = ".*",
    seed = sample(0:1e8, 1),
    folds = 5,
    forgets = FALSE,
    fixed = NULL,
    model_type = rep(FALSE, length(.Object@MODELS_BKT))
  )

  .Object@DEFAULTS_BKT <- list(
    order_id = "order_id",
    skill_name = "skill_name",
    correct = "correct",
    user_id = "user_id",
    multilearn = "template_id",
    multiprior = "correct",
    multipair = "problem_id",
    multigs = "template_id",
    folds = "template_id"
  )

  .Object@INITIALIZABLE_PARAMS <- c("prior", "learns", "guesses", "slips", "forgets")

  return(.Object)
})

# MARK: fit
#' fit(...)
#'
#' fit bkt model.
#'
#' @param data_path data file path.
#' @param data data filter.
#' @param forgets switch of using forget model.
#' @param skills skill name filter.
#' @return a bkt model object, used by other bkt functions.
#' @export
fit <- function(.Object, data_path = NULL, data = NULL, ...) {
  if (!.Object@manual_param_init) {
    .Object@fit_model <- list()
  }

  .Object <- partial_fit(.Object, data_path = data_path, data = data, ...)

  return(.Object)
}

# MARK: partial_fit
setGeneric("partial_fit", function(.Object, data_path = NULL, data = NULL, ...) {
  standardGeneric("partial_fit")
})

setMethod("partial_fit", "Model", function(.Object, data_path = NULL, data = NULL, ...) {
  .Object <- ._check_data(.Object, data_path, data)

  args <- list(...)

  .Object <- ._check_args(.Object, .Object@FIT_ARGS, args)
  .Object <- ._update_param(.Object, c("skills", "num_fits", "defaults", "fixed", "parallel", "forgets"), args)

  if (is.null(.Object@fit_model) || length(.Object@fit_model) == 0) {
    .Object@fit_model <- list()
  }

  if (length(.Object@fit_model) == 0 || (.Object@manual_param_init && length(.Object@fit_model) > 0)) {
    .Object <- ._update_param(.Object, "model_type", ._update_defaults(.Object, args))
  }

  .Object@manual_param_init <- TRUE

  all_data <- ._data_helper(.Object, data_path, data, .Object@defaults, .Object@skills, .Object@model_type)

  .Object <- ._update_param(.Object, "skills", list(skills = names(all_data)))

  for (skill in names(all_data)) {
    .Object@fit_model[[skill]] <- ._fit(.Object, all_data[[skill]], skill, .Object@forgets, preload = ifelse("preload" %in% names(args), args$preload, FALSE))
  }

  .Object@manual_param_init <- FALSE

  return(.Object)
})

# MARK: ._check_data
setGeneric("._check_data", function(.Object, data_path, data) {
  standardGeneric("._check_data")
})

setMethod("._check_data", "Model", function(.Object, data_path, data) {
  if (is.null(data_path) && is.null(data)) {
    stop("No data specified")
  } else if (!is.null(data_path) && !is.null(data)) {
    stop("Cannot specify both data location and data")
  } else if (!is.null(data_path) && !file.exists(data_path)) {
    stop("Data path is invalid or file not found")
  }
  return(.Object)
})

# MARK: ._check_args
setGeneric("._check_args", function(.Object, expected_args, args) {
  standardGeneric("._check_args")
})

setMethod("._check_args", "Model", function(.Object, expected_args, args) {
  for (arg in names(args)) {
    if (!(arg %in% expected_args)) {
      stop("Provided arguments are not recognized. They must be one or more of: ", paste(expected_args, collapse = ", "))
    }
  }
  return(.Object)
})

# MARK: ._update_param
setGeneric("._update_param", function(.Object, params, args, keep = FALSE) {
  standardGeneric("._update_param")
})

setMethod("._update_param", "Model", function(.Object, params, args, keep = FALSE) {
  if (is.list(args)) {
    for (param in params) {
      if (!param %in% names(args) && (!param %in% names(.Object@keep) || !.Object@keep[[param]])) {
        arg <- .Object@DEFAULTS[[param]]
        default_arg <- if (is.function(arg)) arg() else arg
        if (!is.null(slot(.Object, param))) {
          slot(.Object, param) <- default_arg
        }
      } else if (param %in% names(args)) {
        slot(.Object, param) <- args[[param]]
      }
      .Object@keep[[param]] <- keep
    }
  } else {
    slot(.Object, params) <- args
    .Object@keep[[params]] <- keep
  }

  if ("seed" %in% params) {
    set.seed(.Object@seed)
  }

  return(.Object)
})

# MARK:._update_defaults
setGeneric("._update_defaults", function(.Object, args) {
  standardGeneric("._update_defaults")
})

setMethod("._update_defaults", "Model", function(.Object, args) {
  # Update the default column names
  model_types <- rep(FALSE, 4)

  for (d in names(args)) {
    if (d %in% .Object@MODELS_BKT) {
      if (is.logical(args[[d]])) {
        model_types[which(.Object@MODELS_BKT == d)] <- args[[d]]
      } else if (is.character(args[[d]])) {
        if (is.null(.Object@defaults)) {
          .Object@defaults <- list()
        }
        .Object@defaults[[d]] <- args[[d]]
        model_types[which(.Object@MODELS_BKT == d)] <- TRUE
      } else {
        stop("model type must either be boolean for automatic column inference or string specifying column")
      }
    } else if (d %in% .Object@DEFAULTS_BKT) {
      if (is.null(.Object@defaults)) {
        .Object@defaults <- list()
      }
      .Object@defaults[[d]] <- args[[d]]
    }
  }

  return(model_types)
})

# MARK: fetch_dataset
setGeneric("fetch_dataset", function(object, link, loc) standardGeneric("fetch_dataset"))

setMethod("fetch_dataset", "Model", function(object, link, loc) {
  name <- basename(link)
  file_path <- file.path(loc, name)
  if (file.exists(file_path)) {
    message("File already exists: ", file_path)
  } else {
    file_data <- RCurl::getBinaryURL(link)
    writeBin(file_data, file_path)
    message("File downloaded to: ", file_path)
  }
})

# MARK: ._data_helper
setGeneric("._data_helper", function(.Object, data_path, data, defaults, skills, model_type, gs_ref = NULL, resource_ref = NULL, return_df = FALSE, folds = FALSE) {
  standardGeneric("._data_helper")
})

setMethod("._data_helper", "Model", function(.Object, data_path = NULL, data = NULL, defaults, skills, model_type, gs_ref = NULL, resource_ref = NULL, return_df = FALSE, folds = FALSE) {
  data_p <- NULL

  if (!is.null(data_path) && is.character(data_path)) {
    data_p <- convert_data(data_path, skills,
      defaults = defaults, model_type = model_type,
      gs_refs = gs_ref, resource_refs = resource_ref, return_df = return_df, folds = folds
    )
  }

  if (!return_df) {
    lapply(data_p, function(d) check_data(d))
  } else {
    lapply(data_p[[1]], function(d) check_data(d))
  }

  return(data_p)
})

# MARK: ._fit
setGeneric("._fit", function(object, data, skill, forgets, preload = FALSE) {
  standardGeneric("._fit")
})

setMethod(
  "._fit",
  signature(object = "Model"),
  function(object, data, skill, forgets, preload = FALSE) {
    num_learns <- length(data$resource_names)
    num_gs <- length(data$gs_names)
    check_manual_param_init(object, num_learns, num_gs, skill)
    if (!is.null(object@fixed)) {
      object <- ._check_fixed(object)
    }

    num_fit_initializations <- object@num_fits
    best_likelihood <- -Inf
    best_model <- NULL

    for (i in seq_len(num_fit_initializations)) {
      fitmodel <- random_model_uni(num_learns, num_gs)
      optional_args <- list(fixed = list())
      fitmodel$prior <- 0.3
      fitmodel$learns[1] <- 0.28
      fitmodel$forgets[1] <- 0
      fitmodel$guesses[1] <- 0.35
      fitmodel$slips[1] <- 0.27
      fitmodel$As[1, 2, 1] <- 0.28
      fitmodel$As[1, 2, 2] <- 0.72
      fitmodel$emissions[1, 1, 1] <- 0.78
      fitmodel$emissions[1, 1, 2] <- 0.21
      fitmodel$emissions[1, 2, 1] <- 0.16
      fitmodel$emissions[1, 2, 2] <- 0.84
      fitmodel$pi_0[1, 1] <- 0.95
      fitmodel$pi_0[2, 1] <- 0.33
      # print(fitmodel)
      if (forgets) {
        fitmodel$forgets <- runif(length(fitmodel$forgets))
      }

      # if (object@model_type[which(Model$MODELS_BKT == 'multiprior')]) {
      #   fitmodel$prior <- 0
      # }

      if (object@manual_param_init && skill %in% names(object@fit_model)) {
        for (var in names(object@fit_model[[skill]])) {
          if (!is.null(object@fixed) && skill %in% names(object@fixed) &&
            var %in% names(object@fixed[[skill]]) &&
            is.logical(object@fixed[[skill]][[var]]) && object@fixed[[skill]][[var]]) {
            optional_args$fixed[[var]] <- object@fit_model[[skill]][[var]]
          } else if (var %in% names(fitmodel)) {
            fitmodel[[var]] <- object@fit_model[[skill]][[var]]
          }
        }
      }

      if (!is.null(object@fixed) && skill %in% names(object@fixed)) {
        for (var in names(object@fixed[[skill]])) {
          if (!is.logical(object@fixed[[skill]][[var]])) {
            optional_args$fixed[[var]] <- object@fixed[[skill]][[var]]
          }
        }
      }

      if (!preload) {
        em_fit_result <- EM_fit(fitmodel, data, parallel = object@parallel, fixed = optional_args$fixed)
        fitmodel <- em_fit_result$model
        log_likelihoods <- em_fit_result$log_likelihoods
        if (log_likelihoods[length(log_likelihoods)] > best_likelihood) {
          best_likelihood <- log_likelihoods[length(log_likelihoods)]
          best_model <- fitmodel
        }
      } else {
        best_model <- fitmodel
      }
    }

    fit_model <- best_model
    fit_model$learns <- fit_model$As[, 2, 1]
    fit_model$forgets <- fit_model$As[, 1, 2]
    fit_model$prior <- fit_model$pi_0[2, 1]
    fit_model$resource_names <- data$resource_names
    fit_model$gs_names <- data$gs_names
    fit_model$likelihood <- best_likelihood

    return(fit_model)
  }
)

# MARK: ._check_fixed
._check_fixed <- function(object, fixed) {
  # Checks the fixed parameter
  if (is.null(object@fixed)) {
  } else if (is.logical(object@fixed) && object@fixed) {
    object@fixed <- object@fit_model
  } else if (is.list(object@fixed)) {
  } else {
    stop("fixed parameter incorrectly specified")
  }
  return(object)
}


# MARK: evaluate
setGeneric("evaluate", function(object, data = NULL, data_path = NULL, metric = NULL) {
  standardGeneric("evaluate")
})

rmse <- function(true_vals, pred_vals) {
  sqrt(mean((true_vals - pred_vals)^2))
}

setMethod(
  f = "evaluate",
  signature = c("Model"),
  definition = function(object, data = NULL, data_path = NULL, metric = rmse) {
    ._check_data(object, data_path, data)

    if (!is.list(metric) && !is.vector(metric)) {
      metric <- list(metric)
    }
    if (is.null(object@fit_model)) {
      stop("model has not been fitted yet")
    } else {
      for (i in seq_along(metric)) {
        m <- metric[[i]]
        if (is.character(m)) {
          stop("not implemented")
          if (!(m %in% metrics$SUPPORTED_METRICS)) {
            stop(paste("metric must be one of:", paste(metrics$SUPPORTED_METRICS, collapse = ", ")))
          }
          metric[[i]] <- metrics$SUPPORTED_METRICS[[m]]
        } else if (!is.function(m)) {
          stop("metric must either be a string, function, or list/vector of strings and functions")
        }
      }
    }
    all_data <- ._data_helper(object, data_path, data, object@defaults, object@skills, object@model_type,
      gs_ref = object@fit_model, resource_ref = object@fit_model
    )
    results <- ._evaluate(object, all_data, metric)
    return(if (length(results) == 1) results[[1]] else results)
  }
)

# MARK: ._evaluate
setGeneric("._evaluate", function(object, all_data, metric) {
  standardGeneric("._evaluate")
})

setMethod(
  f = "._evaluate",
  signature = c("Model"),
  definition = function(object, all_data, metric) {
    per_skill <- list()
    true <- c()
    pred <- c()

    for (skill in names(all_data)) {
      predictions <- ._predict(object@fit_model[[skill]], all_data[[skill]])
      correct_predictions <- predictions$correct_predictions
      state_predictions <- predictions$state_predictions
      real_data <- all_data[[skill]]$data
      true <- c(true, colSums(real_data))
      pred <- c(pred, correct_predictions)
    }

    true <- true - 1
    tryCatch(
      {
        res <- lapply(metric, function(m) m(true, pred))
      },
      error = function(e) {
        res <- lapply(metric, function(m) m(true, round(pred)))
      }
    )
    return(res)
  }
)

# MARK: ._predict
setGeneric("._predict", function(model, data) {
  standardGeneric("._predict")
})

setMethod(
  f = "._predict",
  signature = c("ANY", "ANY"),
  definition = function(model, data) {
    return(predict_onestep_run(model, data))
  }
)

# MARK: check_manual_param_init
setGeneric("check_manual_param_init", function(object, num_learns, num_gs, skill) {
  standardGeneric("check_manual_param_init")
})

setMethod("check_manual_param_init", signature(object = "Model"), function(object, num_learns, num_gs, skill) {
  if (!is.null(object@fit_model) && skill %in% names(object@fit_model)) {
    # Check for 'learns'
    if ("learns" %in% names(object@fit_model[[skill]]) &&
      length(object@fit_model[[skill]]$learns) != num_learns) {
      stop("invalid number of learns in initialization")
    }

    # Check for 'guesses'
    if ("guesses" %in% names(object@fit_model[[skill]]) &&
      length(object@fit_model[[skill]]$guesses) != num_gs) {
      stop("invalid number of guess classes in initialization")
    }

    # Check for 'slips'
    if ("slips" %in% names(object@fit_model[[skill]]) &&
      length(object@fit_model[[skill]]$slips) != num_gs) {
      stop("invalid number of slip classes in initialization")
    }
  }
})

# MARK: params
# Placeholder for format_param function, which should be defined elsewhere
setGeneric("params", function(object, skill, param, param_value) {
  standardGeneric("params")
})

setMethod(
  "params",
  "Model",
  function(object) {
    coefs <- coef_(object)
    formatted_coefs <- list()

    for (skill in names(coefs)) {
      for (param in names(coefs[[skill]])) {
        classes <- format_param(object, skill, param, coefs[[skill]][[param]])

        for (class_ in names(classes)) {
          formatted_coefs <- append(formatted_coefs, list(c(skill, param, class_, classes[[class_]])))
        }
      }
    }

    df <- as.data.frame(do.call(rbind, formatted_coefs), stringsAsFactors = FALSE)
    colnames(df) <- c("skill", "param", "class", "value")

    df <- transform(df, value = sprintf("%.6f", as.numeric(value)))

    return(df)
  }
)

# MARK: coef_
setGeneric("coef_", function(object) {
  standardGeneric("coef_")
})

setMethod(
  "coef_",
  "Model",
  function(object) {
    if (length(object@fit_model) == 0) {
      stop("model has not been trained or initialized")
    }

    initializable_params <- c("learns", "forgets", "guesses", "slips", "prior") # Equivalent to Model.INITIALIZABLE_PARAMS

    coefs <- list()

    for (skill in names(object@fit_model)) {
      params <- list()
      for (param in initializable_params) {
        if (!is.null(object@fit_model[[skill]][[param]])) {
          params[[param]] <- object@fit_model[[skill]][[param]]
        }
      }
      coefs[[skill]] <- params
    }

    return(coefs)
  }
)

# MARK: format_param
setGeneric("format_param", function(object, skill, param, value) {
  standardGeneric("format_param")
})

setMethod(
  "format_param",
  "Model",
  function(object, skill, param, value) {
    if (is.numeric(value) && length(value) > 1) {
      ptype <- if (param %in% c("learns", "forgets")) "resource_names" else "gs_names"

      if (!is.null(object@fit_model[[skill]][[ptype]])) {
        names <- as.character(object@fit_model[[skill]][[ptype]])
        return(setNames(as.list(value), names))
      } else {
        stop(paste("Parameter type", ptype, "not found for skill", skill))
      }
    } else {
      return(list("default" = value))
    }
  }
)

# MARK: crossvalidate
crossvalidate_single_skill <- function(data, skill, metrics) {
  lapply(metrics, function(metric) metric(rnorm(nrow(data)), data$truth))
}
setGeneric("crossvalidate", function(object, data = NULL, data_path = NULL, metric = rmse, ...) {
  standardGeneric("crossvalidate")
})

setMethod("crossvalidate", "Model", function(object, data = NULL, data_path = NULL, metric = rmse, ...) {
  metric_names <- c()

  if (!is.list(metric)) {
    metric <- list(metric)
  }

  if (is.null(data) && is.null(data_path)) {
    stop("no data specified")
  } else {
    for (i in seq_along(metric)) {
      m <- metric[[i]]
      if (is.character(m)) {
        if (!(m %in% metrics$SUPPORTED_METRICS)) {
          stop(paste("metric must be one of:", paste(metrics$SUPPORTED_METRICS, collapse = ", ")))
        }
        metric[[i]] <- metrics$SUPPORTED_METRICS[[m]]
        metric_names <- c(metric_names, m)
      } else if (is.function(m)) {
        metric_names <- c(metric_names, deparse(substitute(m)))
      } else {
        stop("metric must either be a string, function, or list/tuple of strings and functions")
      }
    }
  }

  ._check_args(object, object@CV_ARGS, list(...))
  object <- ._update_param(object, c("skills", "num_fits", "defaults", "parallel", "forgets", "seed", "folds"), list(...))
  object <- ._update_param(object, "model_type", ._update_defaults(object, list(...)))

  metric_vals <- list()

  if (!object@manual_param_init) {
    object@fit_model <- list()
  }

  if (is.character(object@folds)) {
    ._update_defaults(object, list(folds = object@folds))
  }

  all_data <- ._data_helper(object, data_path, data, object@defaults, object@skills, object@model_type, folds = is.character(object@folds))
  object <- ._update_param(object, "skills", list(skills = names(all_data)))

  for (skill in names(all_data)) {
    print(skill)
    metric_vals[[skill]] <- ._crossvalidate(object, all_data[[skill]], skill, metric)
  }
  object@manual_param_init <- FALSE
  object@fit_model <- list()
  df <- data.frame(skill = names(metric_vals), dummy = I(unname(metric_vals)))
  # df[metric_names] <- do.call(rbind, df$dummy)
  # df <- df[, !(names(df) %in% "dummy")]
  return(df)
})


# MARK: ._crossvalidate
setGeneric("._crossvalidate", function(model, data, skill, metric) {
  standardGeneric("._crossvalidate")
})

setMethod("._crossvalidate", "Model", function(model, data, skill, metric) {
  if (is.character(model@folds)) {
    return(crossvalidate_single_skill(model, data, skill, model@folds, metric, model@seed, use_folds = TRUE))
  } else {
    return(crossvalidate_single_skill(model, data, skill, model@folds, metric, model@seed))
  }
})

# MARK: predict
setGeneric("predict", function(model, data_path = NULL, data = NULL) {
  standardGeneric("predict")
})

setMethod("predict", "Model", function(model, data_path = NULL, data = NULL) {
  ._check_data(model, data_path, data)

  if (is.null(model@fit_model)) {
    stop("Model has not been fitted yet")
  }

  data_helper_result <- ._data_helper(
    model,
    data_path = data_path, data = data,
    defaults = model@defaults, skills = model@skills,
    model_type = model@model_type, gs_ref = model@fit_model,
    resource_ref = model@fit_model, return_df = TRUE
  )
  all_data <- data_helper_result[[1]]
  df <- data_helper_result[[2]]

  df$correct_predictions <- 0.5
  df$state_predictions <- 0.5

  for (skill in names(all_data)) {
    pred_result <- ._predict(model@fit_model[[skill]], all_data[[skill]])
    correct_predictions <- pred_result[[1]]
    state_predictions <- pred_result[[2]][[1]]

    if (!is.null(all_data[[skill]]$multiprior_index)) {
      correct_predictions <- correct_predictions[-all_data[[skill]]$multiprior_index]
      state_predictions <- state_predictions[-all_data[[skill]]$multiprior_index]
    }

    df[df$skill_name == skill, "correct_predictions"] <- correct_predictions
    df[df$skill_name == skill, "state_predictions"] <- state_predictions
  }

  return(df)
})

# MARK: set_coef (coef_)
setGeneric("set_coef", function(object, values, fixed = FALSE) standardGeneric("set_coef"))

setMethod("set_coef", "Model", function(object, values, fixed = FALSE) {
  # Sets or initializes parameters in the BKT model
  # Values must be organized by skill and BKT parameters

  object@fit_model <- list()

  for (skill in names(values)) {
    if (!skill %in% names(object@fit_model)) {
      object@fit_model[[skill]] <- list()
    }
    if (!._check_params(object, values[[skill]])) {
      stop("error in length, type or non-existent parameter")
    }
    for (param in names(values[[skill]])) {
      object@fit_model[[skill]][[param]] <- values[[skill]][[param]]
    }
  }

  object@manual_param_init <- TRUE
  return(object)
})

# ._check_params
._check_params <- function(object, params) {
  # Checks if BKT parameters are valid
  valid <- TRUE

  for (param in names(params)) {
    if (param == "prior") {
      valid <- valid && is.numeric(params[[param]])
    } else {
      valid <- valid && is(params[[param]], "numeric") && param %in% object@INITIALIZABLE_PARAMS
    }
  }

  if ("learns" %in% names(params) && "forgets" %in% names(params)) {
    valid <- valid && (length(params[["learns"]]) == length(params[["forgets"]]))
  }

  if ("guesses" %in% names(params) && "slips" %in% names(params)) {
    valid <- valid && (length(params[["slips"]]) == length(params[["guesses"]]))
  }

  return(valid)
}
