#' State Transition Probabilities
#'
#' Estimate state transition probabilities for given Markov model.
#'
#' @param model The model object.
#' @param x \[`character(1)`\]\cr
#' The predictor name in `model` which represents time.
#' @param group \[`character(1)`\]\cr
#' The predictor name in `model` which to group the observations by. If `NULL`,
#' the observations will not be grouped.
#' @param variables \[`list()`\]\cr
#' A named list containing desired predictor values to use when estimating the
#' probabilities. See 'Details'.
#' @param lag_state \[`character(1)`\]\cr
#' The predictor name in `model` which represents lagged states. This should
#' only be supplied if automatic detection fails.
#' @param interval \[`numeric(1)`\]\cr
#' The confidence/credibility interval.
#'
#' @details
#' By default, all unique predictor values are marginalized. However, unique
#' numeric predictor values are used in marginalization only if the length of
#' unique values are no more than 10. In case there are more than 10 unique
#' numeric values, only the mean of the original values are used in the
#' estimation. This limitation can be circumvented by supplying the desired
#' values using the `variables` argument.
#'
#' @returns
#' A `stprob` object containing the estimated state transition probabilities.
#'
#' @examplesIf FALSE
#' # Fit a multinomial logistic regression model
#' fit <- nnet::multinom(state ~ lagstate + age + sex, nhmgrid::health)
#'
#' # Plot the transition probabilities separately for male and female
#' probs <- nhmgrid::stprobs(fit, x = "age", group = "sex")
#' plot(probs)
#'
#' # Plot the transition probabilities for males aged 15-40 years
#' probs <- nhmgrid::stprobs(fit, x = "age", variables = list(sex = "male", age = 15:40))
#' plot(probs, subtitle = "sex=male")
#'
#' @importFrom nnet multinom
#' @import data.table
#' @export
stprobs <- function(model,
                    x = NULL,
                    group = NULL,
                    variables = NULL,
                    lag_state = NULL,
                    interval = 0.95) {
  if (!is.list(variables)) {
    if (!is.null(variables)) {
      stop("Argument `variables` value is not a list!")
    }
    variables <- list()
  }
  if (!is.null(interval)) {
    interval <- onlyIf(is.numeric(interval) && 0 <= interval && interval <= 1, interval)
    if (is.null(interval)) {
      warning("Argument `interval` should be a value between 0 and 1!")
    }
  }

  fit_data <- find_data(model)
  response <- find_response(model)

  if (!is_dynamitefit(model)) {
    lag_state_allowed <- unlist(fit_data[, lapply(.SD, \(c) all(response$values %in% unique(c)))])
    lag_state_allowed <- names(lag_state_allowed[which(lag_state_allowed)])
    if (length(lag_state_allowed) == 0) {
      stop("None of the model predictors appear to represent the lagged state!")
    }
    if (is.null(lag_state)) {
      lag_state <- lag_state_allowed[1]
    } else if (!(lag_state %in% lag_state_allowed)) {
      stop(paste0("Argument `lag_state` value should probably be one of (",
                  paste0(lag_state_allowed, collapse = ", "),
                  ")!"))
    }
  }

  datagrid_args <- list(
    newdata = fit_data,
    FUN_character = unique,
    FUN_factor = unique,
    FUN_logical = unique,
    FUN_numeric = \(q) unique_or_mean(q, fit_data, x),
    FUN_integer = \(q) unique_or_mean(q, fit_data, x),
    FUN_other = unique
  )
  if (is_dynamitefit(model)) {
    variables[[model$group_var]] <- fit_data[[model$group_var]][1]
  }
  datagrid_args <- c(datagrid_args, variables)

  new_data <- do.call(marginaleffects::datagrid, args = datagrid_args) |>
    data.table::as.data.table()
  if (is_dynamitefit(model)) {
    new_data <- new_data[order(new_data[[x]])]
    n <- table(new_data[[x]])[1]
    new_data[, c(model$group_var) := as.factor(rep(-(1:n), l = .N))]
  }

  prob <- estimate_probs(model, new_data, x, group, lag_state, interval)

  stprob <- manual_stprob(
    state_name = response$name,
    state_values = response$values,
    x_name = x,
    group_name = group,
    prob = prob
  )
  attr(stprob, "proportions") <- FALSE
  return(stprob)

}

#' State Transition Proportions
#'
#' Calculate state transition proportions for given data.
#'
#' @param data The data object.
#' @param id \[`character(1)`\]\cr
#' The column name in `data` which represents individual identifiers.
#' @param state \[`character(1)`\]\cr
#' The column name in `data` which represents states.
#' @param x \[`character(1)`\]\cr
#' The column name in `data` which represents time.
#' @param group \[`character(1)`\]\cr
#' The column name in `data` which to group the observations by. If `NULL`, the
#' observations will not be grouped.
#'
#' @returns
#' A `stprob` object containing the calculated state transition proportions.
#'
#' @examplesIf FALSE
#' # Data structure
#' str(nhmgrid::health)
#'
#' # Calculate and plot empirical transition probabilities
#' props <- nhmgrid::stprops(nhmgrid::health, "id", "state", "age")
#' plot(props)
#'
#' # Group by sex
#' props <- nhmgrid::stprops(nhmgrid::health, "id", "state", "age", "sex")
#' plot(props)
#'
#' @import data.table
#' @export
stprops <- function(data, id, state, x, group = NULL) {
  if (is.data.table(data)) {
    data <- data.table::copy(data)
  } else {
    data <- data.table::as.data.table(data)
  }

  if (is.factor(data[[state]])) {
    state_values <- levels(data[[state]])
  } else {
    state_values <- sort(unique(data[[state]]))
  }

  x_values <- unique(data[[x]])
  if (!is.factor(data[[x]])) {
    x_values <- sort(x_values)
  }

  prob <- data.table::CJ(
    from = state_values,
    to = state_values,
    x = x_values,
    group = orElse(unique(data[[group]]), NA),
    mean = 0
  )

  data[, "$lagstate$" := shift(c(state)), by = c(id)]
  data[, "$group$" := orElse(data[[group]], list(NA))]
  setnames(data, old = c(state, x), new = c("$state$", "$x$"))

  g <- c("$x$", "$lagstate$")
  if (!is.null(group)) {
    g <- c(g, "$group$")
  }
  f <- data[!is.na(`$lagstate$`), .(`$n_from$` = .N), by = g]
  t <- data[f, .(`$n_to$` = .N), by = c("$state$", g), on = g]
  prob_sub <- f[t, .(from = `$lagstate$`,
                     to = `$state$`,
                     x = `$x$`,
                     group = orElse(`$group$`, NA),
                     mean = `$n_to$` / `$n_from$`),
                on = g]
  prob[prob_sub, mean := i.mean, on = .(from, to, x, group)]

  m <- prob[, .(`$missing$` = all(mean == 0)), by = .(from, x, group)][(`$missing$`)]
  prob[m, mean := NA, on = .(from, x, group)]

  prob <- prob[x != x_values[1], ]

  stprob <- manual_stprob(
    state_name = state,
    state_values = state_values,
    x_name = "x",
    group_name = group,
    prob = prob
  )
  attr(stprob, "proportions") <- TRUE
  return(stprob)
}

#' Coerce an object to a `stprob` object
#'
#' Currently only works with objects obtained from [TraMineR::seqtrate].
#'
#' @param x The object.
#' @param ... Ignored.
#'
#' @returns
#' A `stprob` object containing the state transition probabilities.
#'
#' @export
as.stprob <- function(x, ...) {
  UseMethod("as.stprob")
}

#' @describeIn as.stprob Convert the result of [TraMineR::seqtrate] to a
#' `stprob` object.
#' @import data.table
#' @export
as.stprob.array <- function(x, ...) {
  d <- dim(x)
  if (length(d) != 3 || diff(d[1:2]) != 0) {
    stop("`as.stprob.array` works only for objects created with TraMineR::seqtrate function with argument `time.varying` set to true!")
  }

  prob <- data.table::as.data.table(x, sorted = FALSE)
  setnames(prob, c("from", "to", "x", "mean"))
  prob[, c("from", "to") := list(
    factor(stringi::stri_replace_all(from, "", regex = "(^\\[| ->\\]$)")),
    factor(stringi::stri_replace_all(to, "", regex = "(^\\[-> |\\]$)"))
  )]
  if (!is.numeric(prob$x)) {
    prob[, x := factor(x, levels = unique(x))]
  }
  state_values <- unique(prob$from)
  stprob <- manual_stprob(
    state_name = "state",
    state_values = state_values,
    x_name = "x",
    group_name = NULL,
    prob = prob
  )
  return(stprob)
}

#' Manual creation of a `stprob` object
#'
#' @param state_name \[`character(1)`\]\cr
#' The name of the variable that denotes the state of observations.
#' @param state_values \[`character() or integer()`\]\cr
#' A vector of possible state values.
#' @param x_name \[`character(1)`\]\cr
#' The name of the variable that is used for the x-axis.
#' @param group_name \[`character(1)`\]\cr
#' The name of the variable that is used for grouping.
#' @param prob \[`data.table or data.frame`\]\cr
#' A data table/frame containing the state transition probabilities.
#' See 'Details'.
#'
#' @export
manual_stprob <- function(state_name,
                          state_values,
                          x_name,
                          group_name = NULL,
                          prob) {
  stopifnot(all(c("x", "from", "to", "mean") %in% colnames(prob)))
  s <- list(state_name = state_name,
            state_values = state_values,
            x_name = x_name,
            group_name = group_name,
            prob = prob)
  class(s) <- "stprob"
  return(s)
}

is_dynamitefit <- function(model) {
  return("dynamitefit" %in% class(model))
}

find_data <- function(model) {
  if (is_dynamitefit(model)) {
    data <- model$data
    data <- data[, !(grepl("^.+_lag\\d+$", colnames(data))), with = FALSE]
    data <- data.table::copy(data)
    response <- find_response(model)
    data[, c(response$name) := list(response$values[1])]
    return(data)
  }
  predictors <- unlist(insight::find_predictors(model), use.names = FALSE)
  data <- insight::get_data(model)
  if (is.data.table(data)) {
    data <- data[, predictors, with = FALSE] |> data.table::copy()
  } else {
    data <- data[, predictors] |> data.table::as.data.table()
  }
  return(data)
}

find_response <- function(model) {
  if (is_dynamitefit(model)) {
    name <- model$dformulas$all[[1]]$response
    values <- levels(model$data[[name]])
    levels <- orElse(levels(values), unique(values))
    return(list(name = name, values = levels))
  }
  name <- insight::find_response(model)[1]
  values <- insight::get_data(model)[[name]]
  levels <- orElse(levels(values), unique(values))
  return(list(name = name, values = levels))
}

find_column_name <- function(data, values) {
  values <- unique(values)
  values <- values[order(values)]
  for (col in colnames(data)) {
    col_values <- unique(data[[col]])
    col_values <- col_values[order(col_values)]
    if (length(values) == length(col_values)) {
      if (all(values == col_values)) {
        return(col)
      }
    }
  }
  stop("Couldn't find a matching column!")
}

estimate_probs <- function(model, new_data, x, group, lag_state, interval) {
  if (is_dynamitefit(model)) {
    return(estimate_probs.dynamitefit(model, new_data, x, group, interval))
  }

  p <- marginaleffects::avg_predictions(
    model = model,
    newdata = new_data,
    by = c(x, group, lag_state),
    conf_level = interval,
  ) |> data.table::as.data.table()

  p[, c("std.error", "statistic", "p.value", "s.value") := NULL]

  setnames(p,
           old = c("group", x, orElse(group, ""), lag_state, "estimate", "conf.low", "conf.high"),
           new = c("to", "x", "group", "from", "mean", "lower", "upper"),
           skip_absent = TRUE)

  if (is.null(group)) {
    p[, "group" := list(NA)]
  }

  p <- p[, .(x, group, from, to, mean, lower, upper)]
  return(p)
}

estimate_probs.dynamitefit <- function(model, new_data, x, group, interval) {
  alpha <- 1 - interval
  response <- find_response(model)
  prob <- NULL
  for (s in response$values) {
    new_data[, response$name := list(s)]

    p <- stats::na.omit(stats::fitted(model, newdata = new_data, df = FALSE))[
      ,
      as.list(unlist(lapply(.SD, \(q) list(
        mean = mean(q),
        quantile = stats::quantile(q, c(alpha / 2, 1 - (alpha / 2)), names = FALSE)
      )))),
      .SDcols = 1:length(response$values),
      by = c(x, group)
    ]

    data.table::setnames(
      p,
      old = c(x, orElse(group, "")),
      new = c("x", "group"),
      skip_absent = TRUE
    )

    p <- data.table::melt(
      p,
      id = NULL,
      measure.vars = patterns(".+\\.mean$", ".+\\.quantile1$", ".+\\.quantile2$"),
      value.name = c("mean", "lower", "upper"),
      variable.name = "to"
      )[, to := response$values[to]]
    p[, from := list(s)]
    if (is.null(prob)) {
      prob <- p
    } else {
      prob <- rbind(prob, p)
    }
  }
  return(prob)
}

unique_or_mean <- function(q, data, x) {
  u <- unique(q)
  l <- length(u)
  if (l > 10) {
    col <- find_column_name(data, q)
    if (col != x) {
      warning(paste0("Column `", col, "` has many unique values (", l, " > 10)! Defaulting to mean. Use argument `variables` to define specific values."))
      return(mean(q, na.rm = TRUE))
    }
  }
  return(u)
}
