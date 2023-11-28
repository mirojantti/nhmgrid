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
#' @param x See 'See also'.
#' @param state See 'See also'.
#' @param group \[`character(1) or list(name, values)`\]\cr
#' Either the name of the column to be used as the grouping variable or
#' a named list of the column name and the desired grouping values.
#' If `NULL`, the observations will not be grouped.
#' @param fixed_predictors \[`list()`\]\cr
#' A named list of parameters to control the other predictors used in the model.
#' @param interval \[`numeric(1)`\]\cr
#' The level of confidence/credible interval. Ignore at the moment.
#' @param ... Ignored.
#'
#' @details
#' Please view the 'Details' of a specific extended `struct` method in 'See also'.
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
                   x = NULL,
                   state = NULL,
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
#' @param x \[`list(name, values)`\]\cr
#' A named list of the column name and the desired values.
#' See 'Details'.
#' @param state \[`character()`\]\cr
#' A vector of the desired state values.
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
                            x,
                            state = NULL,
                            group = NULL,
                            fixed_predictors = NULL,
                            interval = NULL,
                            ...) {
  args <- struct_args(fit, state, x, group)

  new_data <- eval(parse(text = paste0(
    "data.table::CJ(",
    args$x$name, "=", expression(args$x$values),
    ", ", args$state$name, "=", expression(args$state$values),
    if (!is.null(args$group))
      paste0(", ", args$group$name, "=", expression(args$group$values)),
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
  prob[, c("to", "mean") := list(rep(colnames(p), times = nrow(new_data)), c(t(p)))]

  struct <- manual_struct(
    state = args$state,
    x = args$x,
    group = args$group,
    prob = prob
  )
  attr(struct, "fixed_predictors") <- fixed_predictors
  return(struct)
}

#' Estimate transition probabilities using a `dynamitefit` Markov model.
#'
#' @inherit struct description
#'
#' @inheritParams struct
#' @param fit \[`dynamite::dynamitefit`\]\cr
#' The model object. See [dynamite::dynamite()].
#' @param x \[`numeric()`\]\cr
#' A vector of the desired x-axis values. See 'Details'.
#' @param state \[`character()`\]\cr
#' A vector of the desired state values. See 'Details'.
#'
#' @details
#' Additional details...
#'
#' @inherit struct return
#' @family struct
#' @import data.table
#' @export
struct.dynamitefit <- function(fit,
                               x = NULL,
                               state = NULL,
                               group = NULL,
                               fixed_predictors = NULL,
                               interval = NULL,
                               ...) {
  args <- struct_args(fit, state, x, group)

  n_states <- length(args$state$values)

  prob <- data.table::CJ(
    x = args$x$values[-1],
    group = orElse(args$group$values, NA),
    from = args$state$values,
    to = args$state$values,
    mean = 0, lower = 0, upper = 0
  )

  if (is.null(args$group)) {
    prob[, group := NULL]
  }

  has_random <- any(grepl("(\\s|~)+random\\s*\\(", fit$call))
  new_data <- unique(stats::na.omit(fit$data)[, c(fit$group_var, args$group$name), with = FALSE])
  if (!has_random) {
    if (!is.null(args$group)) {
      new_data <- unique(new_data, by = args$group$name)
    } else {
      new_data <- new_data[1]
    }
  }

  n_new_data <- nrow(new_data)
  new_data <- new_data[rep(1:.N, each = length(args$x$values))]
  new_data <- new_data[, args$x$name := rep(args$x$values, n_new_data)]

  try(new_data[, names(fixed_predictors) := fixed_predictors], silent = TRUE)

  for (s in args$state$values) {
    new_data[, args$state$name := list(s)]

    p <- stats::na.omit(stats::fitted(fit, newdata = new_data, df = FALSE))[
      ,
      as.list(unlist(lapply(.SD, \(q) list(
        mean = mean(q),
        quantile = stats::quantile(q, c(0.025, 0.975), names = FALSE)
      )))),
      .SDcols = 1:n_states,
      by = c(args$x$name, args$group$name)
    ]

    data.table::setnames(p, c(args$x$name, orElse(args$group$name, "")), c("x", "group"), skip_absent = TRUE)

    p <- data.table::melt(
      p,
      id = NULL,
      measure.vars = patterns(".+\\.mean$", ".+\\.quantile1$", ".+\\.quantile2$"),
      value.name = c("mean", "lower", "upper"),
      variable.name = "to"
      )[, to := args$state$values[to]]
    p[, from := list(s)]
    prob[
      p,
      c("mean", "lower", "upper") := list(i.mean, i.lower, i.upper),
      on = stats::na.omit(c("x", ifelse(!is.null(args$group), "group", NA), "from", "to"))
    ]
  }

  struct <- manual_struct(
    state = args$state,
    x = args$x,
    group = args$group,
    prob = prob
  )
  attr(struct, "fixed_predictors") <- fixed_predictors
  return(struct)
}

struct_args <- function(fit, ...) {
  UseMethod("struct_args")
}

struct_args.multinom <- function(fit, state, x, group, ...) {
  args <- list(state = NULL, x = NULL, group = NULL)

  state_name <- names(which(sapply(fit$xlevels, \(x) all(x %in% fit$lab))))[1]
  state_values_allowed <- fit$xlevels[[state_name]]
  state_values <- optional(unlist(state$values, use.names = FALSE))
  if (is.null(state_values) && is.vector(state)) {
    state_values <- state[which(state %in% state_values_allowed)]
  }
  state_values <- orElse(state_values, state_values_allowed)

  if (length(state_values) == 0) {
    stop(paste0("Invalid state values! Should be a subset of (", paste(state_values_allowed, collapse = ", "), ")."))
  }
  args$state = list(name = state_name, values = state_values)

  if (!is.list(x) || !all(c("name", "values") %in% names(x))) {
    stop("Argument `x` should be a named list(name, values)!")
  }

  x_name_allowed <- attr(fit$terms, "term.labels")
  x_name <- x_name_allowed[which(x$name == x_name_allowed)][1]
  if (is.na(x_name)) {
    stop(paste0("Invalid x name! Should be one of (", paste(x_name_allowed, collapse = ", "), ")."))
  }
  x_values <- unlist(x$values, use.names = FALSE)
  if (!is.vector(x_values) || length(x_values) <= 1) {
    stop("Invalid x values! Should be a vector with length > 1.")
  }
  args$x = list(name = x_name, values = x_values)

  if (!is.null(group)) {
    if (is.list(group)) {
      if (!all(c("name", "values") %in% names(group))) {
        stop("Argument `group` should be a named list(name, values)!")
      }
      group_name <- group$name
      group_values <- unlist(group$values, use.names = FALSE)
    } else if (is.character(group) && length(group) == 1) {
      group_name <- group[1]
      group_values <- NULL
    } else {
        stop("Argument `group` should be a named list(name, values)!")
    }
    group_name_allowed <- names(fit$xlevels)[-which(args$state$name == names(fit$xlevels))]
    if (length(group_name_allowed) == 0) {
      stop("Grouping is not possible for this model!")
    }
    group_name <- group_name_allowed[which(group_name == group_name_allowed)][1]
    if (is.na(group_name)) {
      stop(paste0("Invalid group name! Should be one of (", paste(group_name_allowed, collapse = ", "), ")."))
    }
    group_values_allowed <- unlist(fit$xlevels[group_name], use.names = FALSE)
    if (!is.null(group_values)) {
      group_values <- group_values_allowed[which(group_values %in% group_values_allowed)]
    } else {
      group_values <- group_values_allowed
    }
    if (length(group_values) <= 1) {
      stop(paste0("Invalid group values! Should be a subset (length > 1) of (", paste(group_values_allowed, collapse = ", "), ")."))
    }
    args$group = list(name = group_name, values = group_values)
  }

  return(args)
}

struct_args.dynamitefit <- function(fit, state, x, group, ...) {
  args <- list(state = NULL, x = NULL, group = NULL)

  state_name <- fit$dformulas$all[[1]]$response
  state_values_allowed <- unique(levels(fit$data[[state_name]]))
  state_values <- optional(unlist(state$values, use.names = FALSE))
  if (is.null(state_values) && is.vector(state)) {
    state_values <- state[which(state %in% state_values_allowed)]
  }
  state_values <- orElse(state_values, state_values_allowed)

  if (length(state_values) == 0) {
    stop(paste0("Invalid state values! Should be a subset of (", paste(state_values_allowed, collapse = ", "), ")."))
  }
  args$state = list(name = state_name, values = state_values)

  x_name <- fit$time_var
  x_range_allowed <- range(fit$data[[x_name]])
  x_values <- optional(unlist(x$values, use.names = FALSE))
  if (is.null(x_values) && is.vector(x)) {
    x_values <- x[which(x_range_allowed[1] <= x & x <= x_range_allowed[2])]
  }
  x_values <- orElse(x_values, x_range_allowed[1]:x_range_allowed[2])
  if (length(x_values) == 0) {
    stop(paste0("Invalid x values! Should be within range (", paste(x_range_allowed, collapse = ", "), ")."))
  }
  args$x = list(name = x_name, values = x_values)

  if (!is.null(group)) {
    if (is.list(group)) {
      if (!all(c("name", "values") %in% names(group))) {
        stop("Argument `group` should be a named list(name, values)!")
      }
      group_name <- group$name
      group_values <- unlist(group$values, use.names = FALSE)
    } else if (is.character(group) && length(group) == 1) {
      group_name <- group[1]
      group_values <- NULL
    } else {
        stop("Argument `group` should be a named list(name, values)!")
    }
    data_cols <- colnames(fit$data)
    group_name_allowed <- data_cols[-which(
        grepl(paste0("^", args$state$name, "(_lag\\d+)?$"), data_cols) |
          fit$time_var == data_cols |
          fit$group_var == data_cols
    )]
    if (length(group_name_allowed) == 0) {
      stop("Grouping is not possible for this model!")
    }
    group_name <- group_name_allowed[which(group_name == group_name_allowed)][1]
    if (is.na(group_name)) {
      stop(paste0("Invalid group name! Should be one of (", paste(group_name_allowed, collapse = ", "), ")."))
    }
    group_values_allowed <- stats::na.omit(unique(fit$data[[group_name]]))
    if (!is.null(group_values)) {
      group_values <- group_values_allowed[which(group_values %in% group_values_allowed)]
    } else {
      group_values <- group_values_allowed
    }
    if (length(group_values) <= 1) {
      stop(paste0("Invalid group values! Should be a subset (length > 1) of (", paste(group_values_allowed, collapse = ", "), ")."))
    }
    args$group = list(name = group_name, values = group_values)
  }

  return(args)
}
