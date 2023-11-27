orElse <- function(x, errorValue) {
  return(tryCatch({
    if (is.null(x)) {
      stop()
    }
    return(x)
  }, error = \(e) errorValue))
}

optional <- function(x) {
  return(orElse(x, NULL))
}
