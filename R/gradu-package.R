utils::globalVariables(c(
  "x", "group", "from", "to", "mean", "lower", "upper", "i.mean", "i.lower",
  "i.upper", "patterns"
))

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
