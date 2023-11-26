# "struct" is a temporary name for the structure that holds the necessary
# data to plot the transition probabilities


#' TODO document
#'
#' @param state description
#' @param x description
#' @param group description
#' @param prob description
#' @param ... description
#'
#' @export
manual_struct <- function(state, x, group = NULL, prob, ...) {
  n <- length(x)
  s <- list(state = state, x = x, group = group, prob = prob)
  class(s) <- "struct"
  return(s)
}

#' A generic method to estimate transition probabilities.
#'
#' Estimates the transition probabilities within groups over time using
#' a Markov model.
#'
#' @param fit
#' The model object.
#' @param state \[`character(1) or list(name, values = NULL)`\]\cr
#' Either the name of the column that denotes the state of observations or
#' a named list of the column name and the desired state values.
#' See 'Details'.
#' @param x \[`character(1) or list(name, values = NULL)`\]\cr
#' Either the name of the column to be used as the x-axis or
#' a named list of the column name and the desired x values.
#' See 'Details'.
#' @param group \[`character(1) or list(name, values = NULL)`\]\cr
#' Either the name of the column to be used as the grouping variable or
#' a named list of the column name and the desired grouping values.
#' If `NULL`, the observations will not be grouped.
#' See 'Details'.
#' @param fixed_predictors \[`list()`\]\cr
#' A named list of parameters to control the other predictors used in the model.
#' @param interval \[`numeric(1)`\]\cr
#' The level of confidence/credible interval.
#' @param ...
#' Ignored.
#'
#' @details
#' Please view the 'Details' of a specific extended `struct` method.
#' All of the extension methods are listed in the 'See Also' section.
#'
#' @returns
#' A `struct` object which is a list containing the following components:
#' * `state`\cr A list of the state variable name and values.
#' * `x`\cr A list of the x-axis variable name and values.
#' * `group`\cr A list of the grouping variable name and values.
#' * `prob`\cr A `data.table` object of the transition probabilities between the states.
#'
#' @family struct
#' @export
struct <- function(fit,
                   state,
                   x,
                   group = NULL,
                   fixed_predictors = NULL,
                   interval = NULL,
                   ...) {
  UseMethod("struct")
}

#' Estimate transition probabilities using a `multinom` Markov model.
#'
#' @inherit struct description
#'
#' @inheritParams struct
#' @param fit \[`nnet::multinom`\]\cr
#' The model object. See [nnet::multinom()].
#' @param x \[`character(1) or list(name, values)`\]\cr
#' Either the name of the column to be used as the x-axis or
#' a named list of the column name and the desired x values.
#' See 'Details'.
#'
#' @details
#' Additional details...
#'
#' @inherit struct return
#' @family struct
#' @import data.table
#' @export
struct.multinom <- function(fit,
                            state,
                            x,
                            group = NULL,
                            fixed_predictors = NULL,
                            interval = NULL,
                            ...) {
  state <- list(name = state, values = fit$xlevels[[state]])

  new_data <- eval(parse(text = paste0(
    "data.table::CJ(",
    x$name, "=", expression(x$values),
    ", ", state$name, "=", expression(state$values),
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

  return(manual_struct(
    state = state,
    x = x,
    group = group,
    prob = prob
  ))
}

#' Estimate transition probabilities using a `dynamitefit` Markov model.
#'
#' @inherit struct description
#'
#' @inheritParams struct
#' @param fit \[`dynamite::dynamitefit`\]\cr
#' The model object. See [dynamite::dynamite()].
#' @param x \[`character(1) or list(name = NULL, values)`\]\cr
#' Either the name of the column to be used as the x-axis or
#' a named list of the column name and the desired x values.
#' See 'Details'.
#'
#' @details
#' Additional details...
#'
#' @inherit struct return
#' @family struct
#' @import data.table
#' @export
struct.dynamitefit <- function(fit,
                               state,
                               x = NULL,
                               group = NULL,
                               fixed_predictors = NULL,
                               interval = NULL,
                               ...) {
  state <- list(name = state, values = levels(fit$data[[state]]))
  x <- list(name = fit$time_var, values = orElse(x, unique(fit$data[[fit$time_var]])))

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

  has_random <- any(grepl("(\\s|~)+random\\s*\\(", fit$call))
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

  return(manual_struct(
    state = state,
    x = x,
    group = group,
    prob = prob
  ))
}
