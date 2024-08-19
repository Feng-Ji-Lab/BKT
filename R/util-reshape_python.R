reshape_python <- function(x, dims) {
  if (is.null(dim(x))) {
    # vector
    flattened <- as.vector(x)
  } else {
    rx = reverse_dimensions(x)
    flattened <- as.vector(rx)
  }
  reshaped <- array(flattened, dim = rev(dims))
  return(reverse_dimensions(reshaped))
}

reverse_dimensions <- function(x) {
  dims <- dim(x)
  if (is.null(dims)) {
    stop("Input must be a matrix or an array.")
  }
  reversed_array <- aperm(x, rev(seq_along(dims)))
  return(reversed_array)
}