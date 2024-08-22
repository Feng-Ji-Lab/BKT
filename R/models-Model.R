#' Say hello
#'
#' This function prints 'Hello, world!'.
#' @import RCurl
# MARK: setClass
setClass(
  "Model",
  slots = list(
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
#' @export
Model <- function() {
  new("Model",
      MODELS_BKT = c('multilearn', 'multiprior', 'multipair', 'multigs'),
      MODEL_ARGS = c('parallel', 'num_fits', 'seed', 'defaults', 'multilearn', 'multiprior', 'multipair', 'multigs'),
      FIT_ARGS = c('skills', 'num_fits', 'defaults', 'fixed', 'parallel', 'forgets', 'preload', 'multilearn', 'multiprior', 'multipair', 'multigs'),
      CV_ARGS = c('skills', 'num_fits', 'defaults', 'fixed', 'parallel', 'forgets', 'preload', 'multilearn', 'multiprior', 'multipair', 'multigs', 'folds', 'seed'),
      DEFAULTS = list(
        num_fits = 5,
        defaults = NULL,
        parallel = TRUE,
        skills = '.*',
        seed = function() sample(0:1e8, 1),
        folds = 5,
        forgets = FALSE,
        fixed = NULL,
        model_type = rep(FALSE, 4)
      ),
      DEFAULTS_BKT = list(
        order_id = 'order_id',
        skill_name = 'skill_name',
        correct = 'correct',
        user_id = 'user_id',
        multilearn = 'template_id',
        multiprior = 'correct',
        multipair = 'problem_id',
        multigs = 'template_id',
        folds = 'template_id'
      ),
      INITIALIZABLE_PARAMS = c('prior', 'learns', 'guesses', 'slips', 'forgets')
  )
}

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

