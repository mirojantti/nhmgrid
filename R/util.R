orElse <- function(x, errorValue) {
  return(tryCatch({
    if (is.null(x)) {
      stop()
    }
    return(x)
  }, error = \(e) errorValue))
}
