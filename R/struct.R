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
    return(setnames(
      matrix(nrow = nx, ncol = nstates * nstates) |>
        as.data.table(),
      expand.grid(states, states) |>
        apply(MARGIN = 1, FUN = \(row) format_transition(NULL, row[2], row[1]))
    ))
  } else {
    ngroups <- length(group$values)
    return(setnames(
      matrix(nrow = nx, ncol = nstates * nstates * ngroups) |>
        as.data.table(),
      expand.grid(group$values, states, states) |>
        apply(MARGIN = 1, FUN = \(row) format_transition(row[1], row[3], row[2]))
    ))
  }
}

#' @export
struct <- function(...) UseMethod("struct")

#' @export
struct.manual <- function(state, x, group = NULL, prob, l_prob = NULL, u_prob = NULL) {
  n <- length(x)
  s <- list(state = state, x = x, group = group, prob = prob)
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

  prob <- new_data[rep(1:nrow(new_data), each = ncol(p))]
  prob[, paste0("trans_", c("dest", "prob")) :=
         list(rep(colnames(p), times = nrow(new_data)), c(t(p)))]

  return(struct.manual(
    state = state,
    x = x,
    group = group,
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
