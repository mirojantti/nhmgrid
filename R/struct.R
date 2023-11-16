# "struct" is a temporary name for the structure that holds the necessary
# data to plot the transition probabilities

# Idea:
#
# Calling
#
# struct(
#   fit,
#   x = list(name = "age", values = 0:100),
#   group = list(name = "sex", values = c("male", "female")),
#     or
#   fixed_predictors = list(sex = "male"))
#
# returns
#
# list(
#   x = list(name = "age", values = 0:100),
#   group = list(name = "sex", values = c("male", "female")),
#   prob = matrix(ncol = ngroup * nstate * nstate),
#   l_prob = matrix(ncol = ngroup * nstate * nstate),
#   u_prob = matrix(ncol = ngroup * nstate * nstate)
# )

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
    template <- matrix(nrow = nx, ncol = nstates * nstates) |> as.data.table()
    colnames(template) <- expand.grid(states, states) |>
      apply(MARGIN = 1, FUN = \(row) format_transition(NULL, row[2], row[1]))
    return(template)
  } else {
    ngroups <- length(group$values)
    template <- matrix(nrow = nx, ncol = nstates * nstates * ngroups) |> as.data.table()
    colnames(template) <- expand.grid(group$values, states, states) |>
      apply(MARGIN = 1, FUN = \(row) format_transition(row[1], row[3], row[2]))
    return(template)
  }
}

#' @export
struct <- function(...) UseMethod("struct")

#' @export
struct.manual <- function(states, group = NULL, x, prob, l_prob = NULL, u_prob = NULL) {
  n <- length(x)
  s <- list(states = states, group = group, x = x, prob = prob)
  class(s) <- "struct"
  s[["l_prob"]] <- l_prob
  s[["u_prob"]] <- u_prob
  return(s)
}

#' @importFrom data.table as.data.table
#' @importFrom data.table data.table
#' @importFrom data.table `:=`
#' @export
struct.multinom <- function(fit,
                            state_name,
                            x,
                            group = NULL,
                            fixed_predictors = NULL,
                            interval = NULL) {
  states <- fit$xlevels[[state_name]]

  n <- length(x$values)
  m <- ifelse(is.null(group), 1, length(group$values))
  prob <- prob_template(states, x, group)

  new_data <- data.table()
  new_data[, x$name] <- x$values
  sapply(names(fixed_predictors), \(predictor) {
    new_data[, predictor] <<- fixed_predictors[[predictor]]
  })

  ngroup <- ifelse(is.null(group), 0, length(group$values))
  for (gi in 0:ngroup) {
    if (gi == 0 && ngroup > 0) {
      next()
    }
    g <- if (gi == 0) NULL else group$values[gi]
    if (!is.null(g)) {
      new_data[, group$name] <- g
    }
    for (state in states) {
      new_data[, state_name] <- state
      p <- predict(fit, type = "probs", newdata = new_data) |> as.data.table()
      prob[, format_transition(g, state, colnames(p)) := p]
    }
  }

  return(struct.manual(
    states = states,
    group = group,
    x = x,
    prob = prob
  ))
}

#' @export
struct.dynamitefit <- function(fit) {
  return(struct.manual(
    time = c(1, 2),
    prob = c(1, 1)
  ))
}
