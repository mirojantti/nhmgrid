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
#' @param lag_state \[`character(1)`\]\cr
#' The predictor name in `model` which represents lagged states. This should
#' only be supplied if automatic detection fails.
#' @param interval \[`numeric(1)`\]\cr
#' The confidence/credibility interval.
#'
#' @details
#' The transition probabilities are estimated using
#' [marginaleffects::avg_predictions]. In case of a [dynamite::dynamite] model, a custom
#' implementation of predicting the probabilities is used.
#'
#' @returns
#' A `stprob` object containing the estimated state transition probabilities.
#'
#' @importFrom nnet multinom
#' @examplesIf FALSE
#' # Fit a multinomial logistic regression model
#' fit <- nnet::multinom(state ~ lagstate + age + sex, nhmgrid::health)
#'
#' # Estimate and plot the transition probabilities
#' probs <- nhmgrid::stprobs(fit, x = "age")
#' plot(probs)
#'
#' # Estimate and plot the transition probabilities separately for men and women
#' probs <- nhmgrid::stprobs(fit, x = "age", group = "sex")
#' plot(probs) +
#'   ggplot2::labs(title = "Men and women separately")
#'
#' @export
stprobs <- function(model,
                    x = NULL,
                    group = NULL,
                    lag_state = NULL,
                    interval = 0.95) {
  if (!is.null(interval)) {
    interval <- onlyIf(is.numeric(interval) && 0 <= interval && interval <= 1, interval)
    if (is.null(interval)) {
      warning("Argument `interval` should be a value between 0 and 1!")
    }
  }
  if (!is_dynamitefit(model) && missing(x)) {
    stop("Argument `x` not supplied!")
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

  remove_cols <- "rowidcf"
  datagrid_args <- list(
    newdata = fit_data,
    grid_type = "counterfactual"
  )
  if (is_dynamitefit(model)) {
    datagrid_args[[response$name]] <- response$values[1]
    remove_cols <- c(remove_cols, model$group_var)
  } else {
    datagrid_args[[lag_state]] <- response$values
  }
  new_data <- do.call(marginaleffects::datagrid, args = datagrid_args)
  new_data <- new_data[stats::complete.cases(new_data), ]
  new_data <- data.table::as.data.table(new_data)

  new_data[, c(remove_cols) := list(NULL)]
  new_data <- new_data[, .(`$weight$` = .N), by = names(new_data)]
  if (is_dynamitefit(model)) {
    new_data <- new_data[order(new_data[[x]])]
    n <- table(new_data[[x]])[1]
    new_data[, c(model$group_var) := as.factor(rep(-(1:n), l = .N))]
  }

  if (is_dynamitefit(model)) {
    prob <- estimate_probs.dynamitefit(model, new_data, x, group, interval)
  } else {
    prob <- estimate_probs(model, new_data, x, group, lag_state, interval)
  }
  prob$from = factor(prob$from, levels = response$values)
  prob$to = factor(prob$to, levels = response$values)

  stprob <- manual_stprob(prob)
  attr(stprob, "x_name") <- x
  attr(stprob, "group") <- group
  return(stprob)
}

estimate_probs <- function(model, new_data, x, group, lag_state, interval) {
  p <- marginaleffects::avg_predictions(
    model = model,
    newdata = new_data,
    wts = new_data[["$weight$"]],
    by = c(x, group, lag_state),
    conf_level = interval,
  ) |> data.table::as.data.table()

  cols <- c("group", x, orElse(group, ""), lag_state, "estimate", "conf.low", "conf.high")
  pick_cols <- intersect(cols, names(p))

  p <- p[, ..pick_cols]
  setnames(p,
           old = cols,
           new = c("to", "x", "group", "from", "mean", "lower", "upper"),
           skip_absent = TRUE)

  if (is.null(group)) {
    p[, "group" := list(NA)]
  }

  p <- p[, .(x, group, from, to, mean, lower, upper)]
  return(p)
}

estimate_probs.dynamitefit <- function(model, new_data, x, group, interval) {
  n_draws <- getOption("stprobs_n_draws")
  alpha <- 1 - interval
  response <- find_response(model)
  prob <- NULL
  for (s in response$values) {
    new_data[, response$name := list(s)]
    p <- stats::na.omit(stats::fitted(model, newdata = new_data, df = FALSE, n_draws = n_draws))
    p <- p[unique(new_data[, .(id, `$weight$`)]), on = .(id)][
      ,
      as.list(unlist(lapply(.SD, \(q) list(
        mean = Hmisc::wtd.mean(q, weights = `$weight$`),
        quantile = {
          wq <- Hmisc::wtd.quantile(q, weights = `$weight$`, prob = c(alpha / 2, 1 - (alpha / 2)))
          names(wq) <- 1:2
          wq
        }
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
      measure.vars = patterns(".+\\.mean$", ".+\\.quantile\\.1$", ".+\\.quantile\\.2$"),
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
    from = factor(state_values, levels = state_values),
    to = factor(state_values, levels = state_values),
    x = x_values,
    group = orElse(unique(data[[group]]), NA),
    mean = 0
  )

  setnames(data, old = c(state, x), new = c("$state$", "$x$"))
  data[, "$lagstate$" := shift(`$state$`), by = c(id)]
  data[, "$group$" := orElse(data[[group]], list(NA))]

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
  prob <- prob[order(x, match(from, state_values), match(to, state_values))]

  stprob <- manual_stprob(prob)
  attr(stprob, "pro(b|p)") <- "p"
  attr(stprob, "x_name") <- x
  attr(stprob, "group") <- group
  return(stprob)
}

#' Coerce an object to a `stprob` object
#'
#' Currently only works with objects obtained from [TraMineR::seqtrate] with
#' parameter `time.varying` defined as `TRUE`.
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
  stprob <- manual_stprob(prob)
  attr(stprob, "pro(b|p)") <- "p"
  attr(stprob, "x_name") <- "x"
  return(stprob)
}

#' Manual creation of a `stprob` object
#'
#' Create a `stprob` object manually. This is useful if the model used to estimate
#' the transition probabilities is not supported by this package.
#'
#' @param prob \[`data.table or data.frame`\]\cr
#' A data table/frame containing the state transition probabilities.
#' See 'Details'.
#'
#' @details
#' The argument passed to this function must be a [data.frame] with a specific
#' structure. The table must contain the transition probabilities `mean` between
#' factored states `from` and `to` at every time point `x`. If the transition
#' probabilities are grouped, the group indicator must be defined in `group`.
#' The lower and upper bounds of confidence/credibility intervals are to be set
#' in `lower` and `upper`. The columns marked with a star may be omitted.
#'
#' Below is an example of the structure of the table. The values are from the
#' [nhmgrid::health] dataset.
#'
#' |x|from|to|group*|mean|lower*|upper*|
#' |-|-|-|-|-|-|-|
#' |4|healthy|healthy|male|0.86|0.79|0.92|
#' |4|healthy|healthy|female|0.89|0.84|0.95|
#' |4|healthy|sick|male|0.14|0.07|0.20|
#' |4|healthy|sick|female|0.11|0.05|0.16|
#' |4|healthy|deceased|male|0.00|0.00|0.01|
#' |4|healthy|deceased|female|0.00|0.00|0.00|
#' |4|sick|healthy|male|0.84|0.74|0.93|
#' |...|...|...|...|...|...|...|
#' |5|healthy|healthy|male|0.85|0.78|0.92|
#' |...|...|...|...|...|...|...|
#'
#' @export
manual_stprob <- function(prob) {
  stopifnot(
    is.data.frame(prob),
    all(c("x", "from", "to", "mean") %in% colnames(prob)),
    is.factor(prob$from),
    is.factor(prob$to),
    is.numeric(prob$mean),
    !("lower" %in% colnames(prob)) || is.numeric(prob$lower),
    !("upper" %in% colnames(prob)) || is.numeric(prob$upper),
    sum(c("lower", "upper") %in% colnames(prob)) %% 2 == 0
  )

  small <- data.table::as.data.table(prob)
  if (!("group" %in% colnames(small))) {
    small$group <- NA
  }
  big <- CJ(x = unique(small$x),
            from = unique(small$from),
            to = unique(small$to),
            group = unique(small$group))
  stprob <- small[big, on = .(x, group, from, to)]
  attr(stprob, "pro(b|p)") <- "b"
  class(stprob) <- c("stprob", class(stprob))
  return(stprob)
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
