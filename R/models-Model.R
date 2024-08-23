#' Say hello
#'
#' This function prints 'Hello, world!'.
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
  # 设置默认值
  .Object@parallel <- parallel
  .Object@num_fits <- num_fits
  .Object@seed <- seed
  .Object@defaults <- defaults
  .Object@model_type <- model_type
  .Object@fit_model <- list()  # 初始化为空列表
  .Object@manual_param_init <- FALSE  # 初始化为 FALSE
  .Object@skills = ".*"
  .Object@folds = folds
  .Object@forgets = FALSE

  # 设置随机数生成器种子
  set.seed(.Object@seed)

  # MARK: static parameter handle
  # 初始化静态变量
  .Object@MODELS_BKT <- c('multilearn', 'multiprior', 'multipair', 'multigs')
  .Object@MODEL_ARGS <- c('parallel', 'num_fits', 'seed', 'defaults', .Object@MODELS_BKT)
  .Object@FIT_ARGS <- c('skills', 'num_fits', 'defaults', 'fixed', 'parallel', 'forgets', 'preload', .Object@MODELS_BKT)
  .Object@CV_ARGS <- c(.Object@FIT_ARGS, 'folds', 'seed')
  
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
    order_id = 'order_id',
    skill_name = 'skill_name',
    correct = 'correct',
    user_id = 'user_id',
    multilearn = 'template_id',
    multiprior = 'correct',
    multipair = 'problem_id',
    multigs = 'template_id',
    folds = 'template_id'
  )
  
  .Object@INITIALIZABLE_PARAMS <- c('prior', 'learns', 'guesses', 'slips', 'forgets')
  
  return(.Object)
})

# MARK: fit
setGeneric("fit", function(.Object, data_path = NULL, data = NULL, ...) {
  standardGeneric("fit")
})

setMethod("fit", "Model", function(.Object, data_path = NULL, data = NULL, ...) {
  # 如果没有手动初始化参数，重置 fit_model
  if (!.Object@manual_param_init) {
    .Object@fit_model <- list()
  }
  
  # 调用部分拟合方法
  .Object <- partial_fit(.Object, data_path = data_path, data = data, ...)
  
  return(.Object)
})

# MARK: partial_fit
setGeneric("partial_fit", function(.Object, data_path = NULL, data = NULL, ...) {
  standardGeneric("partial_fit")
})

setMethod("partial_fit", "Model", function(.Object, data_path = NULL, data = NULL, ...) {
  
  # 检查数据的合法性
  .Object <- ._check_data(.Object, data_path, data)
  
  # 获取额外参数
  args <- list(...)
  
  # 检查参数的合法性
  .Object <- ._check_args(.Object, .Object@FIT_ARGS, args)
  
  # 更新对象的参数
  .Object <- ._update_param(.Object, c("skills", "num_fits", "defaults", "fixed", "parallel", "forgets"), args)
  
  # 如果 fit_model 未初始化或为空，进行初始化
  if (is.null(.Object@fit_model) || length(.Object@fit_model) == 0) {
    .Object@fit_model <- list()
  }
  
  # 如果模型类型还未初始化或手动初始化后第一次调用
  if (length(.Object@fit_model) == 0 || (.Object@manual_param_init && length(.Object@fit_model) > 0)) {
    .Object <- ._update_param(.Object, "model_type", ._update_defaults(.Object, args))
  }
  
  # 标记为手动参数初始化
  .Object@manual_param_init <- TRUE
  
  # 处理数据并准备进行拟合
  all_data <- ._data_helper(.Object, data_path, data, .Object@defaults, .Object@skills, .Object@model_type)
  
  # 更新技能参数
  .Object <- ._update_param(.Object, "skills", list(skills = names(all_data)))
  
  # 对每个技能进行模型拟合
  for (skill in names(all_data)) {
    .Object@fit_model[[skill]] <- ._fit(.Object, all_data[[skill]], skill, .Object@forgets, preload = ifelse("preload" %in% names(args), args$preload, FALSE))
  }
  
  # 标记为非手动参数初始化
  .Object@manual_param_init <- FALSE
  
  return(.Object)
})

# MARK: _fit
setGeneric("_fit", function(.Object, data, skill, forgets, preload = FALSE) {
  standardGeneric("_fit")
})

setMethod("_fit", "Model", function(.Object, data, skill, forgets, preload = FALSE) {
  # TODO: 实现你的模型拟合逻辑
  return(list())  # 返回一个空列表作为占位符
})

# MARK: ._check_data
# 检查数据的合法性
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
        # 从 DEFAULTS 中获取参数的默认值
        arg <- .Object@DEFAULTS[[param]]
        # 如果默认值是函数，则调用它，否则直接赋值
        slot(.Object, param) <- if (is.function(arg)) arg() else arg
      } else if (param %in% names(args)) {
        # 如果参数在 args 中，则使用传递的参数值
        slot(.Object, param) <- args[[param]]
      }
      # 如果 keep 标志为 TRUE，则保存此参数
      .Object@keep[[param]] <- keep
    }
  } else {
    # 如果传递的不是列表，则直接设置参数
    slot(.Object, params) <- args
    .Object@keep[[params]] <- keep
  }

  # 如果 'seed' 在参数中，则设置随机数种子
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
  model_type_defaults <- .Object@DEFAULTS$model_type
  for (name in names(args)) {
    if (name %in% .Object@MODEL_ARGS) {
      model_type_defaults[which(.Object@MODEL_ARGS == name)] <- TRUE
    }
  }
  return(model_type_defaults)
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
  
  # 初始化一个空列表，用于存储处理后的数据
  data_p <- NULL

  # 处理数据路径
  if (!is.null(data_path) && is.character(data_path)) {
    data_p <- convert_data(data_path, skills, defaults = defaults, model_type = model_type, 
                           gs_refs = gs_ref, resource_refs = resource_ref, return_df = return_df, folds = folds)
  }

  # 检查数据的有效性
  if (!return_df) {
    lapply(data_p, function(d) check_data(d))
  } else {
    lapply(data_p[[1]], function(d) check_data(d))
  }
  
  return(data_p)
})
