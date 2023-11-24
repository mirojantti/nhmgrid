# "struct" is a temporary name for the structure that holds the necessary
# data to plot the transition probabilities

#' TODO document
#'
#' @param ... description
#'
#' @export
struct <- function(...) UseMethod("struct")

#' TODO document
#'
#' @param state description
#' @param x description
#' @param group description
#' @param prob description
#' @export
struct.manual <- function(state, x, group = NULL, prob) {
  n <- length(x)
  s <- list(state = state, x = x, group = group, prob = prob)
  class(s) <- "struct"
  return(s)
}
#' TODO document
#'
#' @param fit description
#' @param state_name description
#' @param x description
#' @param group description
#' @param fixed_predictors description
#' @param interval description
#'
#' @import data.table
#' @export
struct.multinom <- function(fit,
                            state_name,
                            x,
                            group = NULL,
                            fixed_predictors = NULL,
                            interval = NULL) {
  state <- list(name = state_name, values = fit$xlevels[[state_name]])

  new_data <- eval(parse(text = paste0(
    "data.table::CJ(",
    x$name, "=", expression(x$values),
    ", ", state_name, "=", expression(state$values),
    if (!is.null(group))
      paste0(", ", group$name, "=", expression(group$values)),
    if (!is.null(fixed_predictors))
      paste0(", ", paste0(
        names(fixed_predictors), "=",
        lapply(fixed_predictors, deparse), collapse = ", ")
      ),
    ")"
  )))

  p <- stats::predict(fit, type = "probs", newdata = new_data)

  try(new_data[, names(fixed_predictors) := NULL], silent = TRUE)

  prob <- new_data[rep(1:.N, each = ncol(p))]
  colnames(prob) <- c("x", "from", "group")
  prob[, c("to", "mean") :=
         list(rep(colnames(p), times = nrow(new_data)), c(t(p)))]

  return(struct.manual(
    state = state,
    x = x,
    group = group,
    prob = prob
  ))
}

#' TODO document
#'
#' @param fit description
#' @param state_name description
#' @param x_values description
#' @param group description
#' @param fixed_predictors description
#' @param interval description
#'
#' @import data.table
#' @export
struct.dynamitefit <- function(fit,
                               state_name,
                               x_values = NULL,
                               group = NULL,
                               fixed_predictors = NULL,
                               interval = NULL) {
  state <- list(name = state_name, values = levels(fit$data[[state_name]]))
  x <- list(name = fit$time_var, values = orElse(x_values, unique(fit$data[[fit$time_var]])))

  n_states <- length(state$values)

  prob <- data.table::CJ(
    x = x$values[-1],
    group = orElse(group$values, NA),
    from = state$values,
    to = state$values,
    mean = 0, lower = 0, upper = 0
  )

  if (is.null(group)) {
    prob[, group := NULL]
  }

  has_random = any(grepl("(\\s|~)+random\\s*\\(", fit$call))
  new_data <- unique(stats::na.omit(fit$data)[, c(fit$group_var, group$name), with = FALSE])
  if (!has_random) {
    if (!is.null(group)) {
      new_data <- unique(new_data, by = group$name)
    } else {
      new_data <- new_data[1]
    }
  }

  n_new_data <- nrow(new_data)
  new_data <- new_data[rep(1:.N, each = length(x$values))]
  new_data <- new_data[, x$name := rep(x$values, n_new_data)]

  try(new_data[, names(fixed_predictors) := fixed_predictors], silent = TRUE)

  for (s in state$values) {
    new_data[, state$name := list(s)]

    p <- stats::na.omit(stats::fitted(fit, newdata = new_data, df = FALSE))[
      ,
      as.list(unlist(lapply(.SD, \(q) list(
        mean = mean(q),
        quantile = stats::quantile(q, c(0.025, 0.975), names = FALSE)
      )))),
      .SDcols = 1:n_states,
      by = c(x$name, group$name)
    ]

    data.table::setnames(p, c(x$name, orElse(group$name, "")), c("x", "group"), skip_absent = TRUE)

    p <- data.table::melt(
      p,
      id = NULL,
      measure.vars = patterns(".+\\.mean$", ".+\\.quantile1$", ".+\\.quantile2$"),
      value.name = c("mean", "lower", "upper"),
      variable.name = "to"
      )[, to := state$values[to]]
    p[, from := list(s)]
    prob[
      p,
      c("mean", "lower", "upper") := list(i.mean, i.lower, i.upper),
      on = stats::na.omit(c("x", ifelse(!is.null(group), "group", NA), "from", "to"))
    ]
  }

  return(struct.manual(
    state = state,
    x = x,
    group = group,
    prob = prob
  ))
}
