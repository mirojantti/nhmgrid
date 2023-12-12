utils::globalVariables(c(
  "x", "group", "from", "to", "mean", "lower", "upper", "i.mean", "i.lower",
  "i.upper", "patterns", "$group$", "$lagstate$", "$n_from$", "$n_to$",
  "$state$", "$x$", ".", "x_name", "group_name", "state_name", "state_values"
))


#' The `gradu` package.
#'
#' An user friendly interface to estimating state transition probabilities
#' using a Markov model. Includes plotting tools to visualize the probabilities
#' over time between groups.
#'
#' # See also
#' * [stprobs] for information on estimating state transition probabilities.
#' * [stprops] for information on calculating state transition proportions.
#' * [plot.stprob] for information on plotting the probabilities.
#' * \href{https://github.com/joakim219/gradu/issues/}{Submit a bug report or a feature request}
#'
#' @docType package
#' @name gradu
NULL

#' Simulated health state dataset
#'
#' This panel data contains simulated health state measurements from multiple
#' individuals over several years.
#'
#' @format ## `health`
#' A data frame with 500 rows and 4 columns:
#' \describe{
#'   \item{id}{Person identification number}
#'   \item{age}{Person age in years}
#'   \item{state}{Measured health state: healthy, sick or deceased}
#'   \item{lagstate}{Previous measured health state}
#' }
"health"
