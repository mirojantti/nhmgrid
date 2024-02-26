#' Plot State Transition Probabilities
#'
#' Plot state transition probabilities as a matrix of figures.
#'
#' @param x \[`nhmgrid::stprob`\]\cr
#' The `stprob` object containing the transition probabilities.
#' @param default_geoms \[`logical(1)`\]\cr
#' Inclusion of default geometric layers. See 'Details' and 'Examples'.
#' @param ... Ignored.
#'
#' @details
#' The resulting plot is created with [ggplot2]. The matrix-like structure is
#' constructed with [ggplot2::facet_grid]. By default, certain geometric layers
#' are added in the plot to visualize the data. In order to replace the default
#' geometric components one has to define `default_geoms` as `FALSE` and add the
#' new layers manually.
#'
#' When plotting the result of [nhmgrid::stprobs], the default geometric components
#' are [ggplot2::geom_line] and [ggplot2::geom_ribbon] for the estimates and their
#' corresponding confidence/credibility intervals. When plotting the result of
#' [nhmgrid::stprops], the default geometric components for the resulting
#' lollipop plot are [ggplot2::geom_point] and [ggplot2::geom_segment].
#'
#' One can add any [ggplot2] objects to the result of this function. See the
#' 'Examples' section.
#'
#' @returns A [ggplot2] figure.
#'
#' @examplesIf FALSE
#' # Estimate state transition probabilities in the health data and plot them
#' fit <- nnet::multinom(state ~ lagstate + age, nhmgrid::health)
#' probs <- nhmgrid::stprobs(fit, x = "age")
#' plot(probs)
#'
#' # Calculate state transition proportions and plot them with a custom title
#' props <- nhmgrid::stprops(nhmgrid::health, "id", "state", "age")
#' plot(props) +
#'   ggplot2::labs(title = "Custom title")
#'
#' # The parameter `default_geoms` can be used to hide/replace the default
#' # geometric components
#'
#' # Defining it as FALSE will result in a matrix of empty plots
#' plot(probs, default_geoms = FALSE)
#'
#' # Combining this with specific components will replace the default components
#' plot(probs, default_geoms = FALSE) +
#'   ggplot2::geom_area()
#'
#' # One could plot the probabilities and proportions together
#' plot(probs) +
#'   ggplot2::geom_point(data = props) +
#'   ggplot2::geom_smooth(data = props, se = FALSE)
#'
#' @export
plot.stprob <- function(x, default_geoms = TRUE, ...) {
  if (!("stprob" %in% class(x))) {
    stop("The object to plot is not a `stprob` object!")
  }

  group_optional <- optional(x$group)
  if (any(is.na(group_optional))) {
    group_optional <- NULL
  }

  add_interval <- all(c("lower", "upper") %in% names(x))
  y_label <- paste("transition", ifelse(attr(x, "pro(b|p)") == "b", "probability", "proportion"))

  ggplot2::ggplot(data = x,
                  mapping = ggplot2::aes(
                    x = x,
                    y = mean,
                    color = group_optional,
                    fill = group_optional,
                    linetype = group_optional
                  )) +
    onlyIf(default_geoms, list(
      onlyIf(attr(x, "pro(b|p)") == "b", list(
        onlyIf(add_interval, list(
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.1),
          ggplot2::geom_line(ggplot2::aes(y = lower), alpha = 0.1),
          ggplot2::geom_line(ggplot2::aes(y = upper), alpha = 0.1)
        )),
        ggplot2::geom_line(linewidth = 1)
      )),
      onlyIf(attr(x, "pro(b|p)") == "p", list(
        ggplot2::geom_point(),
        ggplot2::geom_segment(ggplot2::aes(xend = x, y = 0, yend = mean))
      ))
    ))+
    ggplot2::facet_grid(from ~ to,
                        switch = "y",
                        labeller = ggplot2::label_bquote(
                          rows = .(as.character(from)) %->% "",
                          cols = "" %->% .(as.character(to)))) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.placement = "outside") +
    ggplot2::labs(x = orElse(attr(x, "x_name"), "x"),
                  y = y_label,
                  color = orElse(attr(x, "group"), "group"),
                  fill = orElse(attr(x, "group"), "group"),
                  linetype = orElse(attr(x, "group"), "group")) +
    ggplot2::coord_cartesian(ylim = 0:1)
}
