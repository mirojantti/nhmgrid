#' Plot State Transition Probabilities
#'
#' @param x \[`nhmgrid::stprob`\]\cr
#' The `stprob` object containing the transition probabilities.
#' @param default_geoms \[`logical(1)`\]\cr
#' See 'Details' and 'Examples'.
#' @param ... Ignored.
#'
#' @details
#' The function defined in `fun_cell` is applied to all cells of the plot matrix
#' individually. The function should be defined with 1 or 2 arguments and it
#' should return the modified plot. The first argument is assigned the original
#' plot object obtained with [ggplot2::ggplot]. If the second argument is
#' defined, it contains the index of the cell in the plot matrix. See 'Examples'.
#'
#' @examplesIf FALSE
#' # Estimate state transition probabilities in the health data and plot them
#' fit <- nnet::multinom(state ~ lagstate + age, nhmgrid::health)
#' probs <- nhmgrid::stprobs(fit, x = "age")
#' plot(probs)
#'
#' # Calculate state transition proportions
#' props <- nhmgrid::stprops(nhmgrid::health, "id", "state", "age")
#' plot(props)
#'
#' # The argument `fun_cell` can be used to customize the individual cells # TODO
#'
#' # One could add a smoothing lines in all of the plots
#' plot(props, fun_cell = \(cell) cell + ggplot2::geom_smooth())
#'
#' # or only in a specific cell by using a 2 argument function
#' plot(props, fun_cell = \(cell, index) {
#'   if (index == 5) {
#'     return(cell + ggplot2::geom_smooth())
#'   }
#'   return(cell)
#' })
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
    ggplot2::scale_y_continuous(limits = c(-0.01, 1.01))
}
