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

#' @export
state_probs <- function(model,
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
    state = response,
    x = list(name = x, values = c()),
    group = onlyIf(!is.null(group), list(name = group, values = c())),
    prob = prob
  )
  return(stprob)

}

#' @export
state_props <- function(data, id, state, x, group = NULL) {
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
  setnames( data, old = c(state, x), new = c("$state$", "$x$"))

  g <- c("$x$", "$lagstate$")
  if (!is.null(group)) {
    g <- c(g, "$group$")
  }
  f <- data[!is.na(`$lagstate$`), .(`$n_from$` = .N), by = g]
  t <- data[f, .(`$n_to$` = .N), by = c("$state$", g), on = g]
  prob_sub <- f[t, .(from = `$lagstate$`, to = `$state$`, x = `$x$`, group = orElse(`$group$`, NA), mean = `$n_to$` / `$n_from$`), on = g]
  prob[prob_sub, mean := i.mean, on = .(from, to, x, group)]

  prob <- prob[x != x_values[1], ]

  stprob <- manual_stprob(
    state = list(name = state, values = state_values),
    x = list(name = "x", values = x_values),
    group = onlyIf(!is.null(group), list(name = group, values = unique(data[[group]]))),
    prob = prob
  )
  return(stprob)
}

#' @export
as.stprob <- function(x, ...) {
  UseMethod("as.stprob")
}

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
  x_values <- unique(prob$x)
  if (is.numeric(prob$x)) {
    x_values <- sort(x_values)
  }
  stprob <- manual_stprob(
    state = list(name = "state", values = state_values),
    x = list(name = "x", values = x_values),
    group = NULL,
    prob = prob
  )
  return(stprob)
}

#' TODO document
#'
#' @param state description
#' @param x description
#' @param group description
#' @param prob description
#' @param ... description
#'
#' @export
manual_stprob <- function(state, x, group = NULL, prob, ...) {
  n <- length(x)
  s <- list(state = state, x = x, group = group, prob = prob)
  class(s) <- "stprob"
  return(s)
}
