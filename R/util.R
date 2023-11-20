orElse <- function(x, errorValue) {
  tryCatch(x, error = \(e) errorValue)
}
