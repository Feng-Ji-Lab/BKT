#' Save
#'
#' Save a BKT model to a file.
#' This function saves a trained BKT model to a specified file location. The model is stored
#' as an RDS file, which can be loaded back into R using the `load_model()` function.
#'
#' @param model A trained BKT model object to be saved.
#' @param loc Character. The file path where the model will be saved, typically with an `.rds` extension.
#' @return None. The function saves the model to the specified location.
#' @examples
#' data("simulation_data_50", package = "BKT")
#' model <- bkt(seed = 42, parallel = FALSE, num_fits = 1)
#' fit_model <- fit(model, data = simulation_data_50)
#' model_file <- tempfile(fileext = ".rds")
#' save_model(fit_model, model_file)
#' unlink(model_file)
#' @export
save_model <- function(model, loc) {
    saveRDS(object = model, file = loc)
}

#' Load
#'
#' Load a BKT model from a file.
#' This function loads a previously saved BKT model from an RDS file. The model attributes
#' are restored into the provided model object, allowing it to be used for further analysis or predictions.
#'
#' @param model A BKT model object into which the saved model's attributes will be loaded.
#' @param loc Character. The file path from which the model will be loaded, typically an `.rds` file.
#' @return The updated BKT model object with the restored attributes from the saved model.
#' @examples
#' data("simulation_data_50", package = "BKT")
#' model <- bkt(seed = 42, parallel = FALSE, num_fits = 1)
#' fitted_model <- fit(model, data = simulation_data_50)
#' model_file <- tempfile(fileext = ".rds")
#' save_model(fitted_model, model_file)
#' loaded_model <- load_model(bkt(), model_file)
#' unlink(model_file)
#' @export
load_model <- function(model, loc) {
    orig_model <- readRDS(loc)

    for (attr in slotNames(orig_model)) {
        slot(model, attr) <- slot(orig_model, attr)
    }

    return(model)
}
