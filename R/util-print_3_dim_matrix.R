print_3_dim_matrix <- function(arr) {
  dims <- dim(arr)
  
  if (length(dims) != 3) {
    stop("This function only supports 3-dimensional arrays.")
  }
  
  cat("[\n")
  for (i in 1:dims[1]) {
    cat(" [\n")
    for (j in 1:dims[2]) {
      cat("  [", paste(arr[i, j, ], collapse = " "), "]", sep = "")
      if (j < dims[2]) cat(",\n")
    }
    cat("\n ]")
    if (i < dims[1]) cat(",\n")
  }
  cat("\n]\n")
}