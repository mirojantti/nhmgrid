# "struct" is a temporary name for the structure that holds the necessary
# data to plot the transition probabilities

format_transition <- Vectorize(function(group, from, to) {
  if (is.null(group)) {
    return(paste0(from, "->", to))
  }
  return(paste0(group, ": ", from, "->", to))
}, vectorize.args = "to", USE.NAMES = FALSE)

#' @importFrom data.table as.data.table
#' @export
prob_template <- function(states, x, group) {
  nx <- length(x$values)
  nstates <- length(states)
  if (is.null(group)) {
    return(setnames(
      matrix(nrow = nx, ncol = nstates * nstates) |>
        data.table::as.data.table(),
      expand.grid(states, states) |> # TODO CJ?
        apply(MARGIN = 1, FUN = \(row) format_transition(NULL, row[2], row[1]))
    ))
  } else {
    ngroups <- length(group$values)
    return(setnames(
      matrix(nrow = nx, ncol = nstates * nstates * ngroups) |>
        data.table::as.data.table(),
      expand.grid(group$values, states, states) |>
        apply(MARGIN = 1, FUN = \(row) format_transition(row[1], row[3], row[2]))
    ))
  }
}

#' @export
struct <- function(...) UseMethod("struct")

#' @export
struct.manual <- function(state, x, group = NULL, prob) {
  n <- length(x)
  s <- list(state = state, x = x, group = group, prob = prob)
  class(s) <- "struct"
  return(s)
}

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

  p <- predict(fit, type = "probs", newdata = new_data)

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
  new_data <- unique(na.omit(fit$data)[, c(fit$group_var, group$name), with = FALSE])
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

    p <- na.omit(fitted(fit, newdata = new_data, df = FALSE))[
      ,
      as.list(unlist(lapply(.SD, \(q) list(
        mean = mean(q),
        quantile = quantile(q, c(0.025, 0.975), names = FALSE)
      )))),
      .SDcols = 1:n_states,
      by = c(x$name, group$name)
    ]

    setnames(p, c(x$name, orElse(group$name, "")), c("x", "group"), skip_absent = TRUE)

    p <- melt(p,
              id = NULL,
              measure.vars = patterns(".+\\.mean$", ".+\\.quantile1$", ".+\\.quantile2$"),
              value.name = c("mean", "lower", "upper"),
              variable.name = "to")[, to := state$values[to]]
    p[, from := list(s)]
    prob[
      p,
      c("mean", "lower", "upper") := list(i.mean, i.lower, i.upper),
      on = na.omit(c("x", ifelse(!is.null(group), "group", NA), "from", "to"))
    ]
  }

  return(struct.manual(
    state = state,
    x = x,
    group = group,
    prob = prob
  ))
}
