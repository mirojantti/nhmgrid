utils::globalVariables(c(
  "x", "group", "from", "to", "mean", "lower", "upper", "i.mean", "i.lower",
  "i.upper", "patterns", "$group$", "$lagstate$", "$n_from$", "$n_to$",
  "$state$", "$x$", ".", "x_name", "group_name", "state_name", "state_values",
  "default_geoms", "%->%", "..pick_cols", "$missing$", "$weight$", "id"
))


#' The `nhmgrid` package.
#'
#' A user friendly interface to estimating state transition probabilities
#' using a Markov model. Includes plotting tools to visualize the probabilities
#' over time between groups.
#'
#' # See also
#' * [nhmgrid::health] for information on the data set used in examples.
#' * [nhmgrid::stprobs] for information on estimating state transition probabilities.
#' * [nhmgrid::stprops] for information on calculating state transition proportions.
#' * [nhmgrid::plot.stprob] for information on plotting the probabilities.
#' * [nhmgrid::manual_stprob] for information on creating a `stprob` object manually.
#' * \href{https://github.com/joakim219/nhmgrid/issues/}{Submit a bug report or a feature request}
#'
#' @docType package
#' @name nhmgrid
#' @importFrom data.table :=
NULL

#' Simulated health state dataset
#'
#' This panel data contains simulated health state measurements from 100
#' individuals over 10 years. This data set is used in the examples of the
#' [nhmgrid] package.
#'
#' @format ## `health`
#' A data frame with 1000 rows and 4 columns:
#' \describe{
#'   \item{id}{Person identification number}
#'   \item{age}{Person age in years}
#'   \item{state}{Measured health state: healthy, sick or deceased}
#'   \item{lagstate}{Previous measured health state}
#' }
"health"
