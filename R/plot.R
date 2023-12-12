#' Plot State Transition Probabilities
#'
#' @param x \[`gradu::stprob`\]\cr
#' The `stprob` object containing the transition probabilities.
#' @param fun_cell \[`function(cell, index)`\]\cr
#' The function to apply to the individual plots. By default, [identity] is
#' used. See 'Details' and 'Examples'.
#' @param title \[`character(1)`\]\cr
#' Title for the plot. If explicitly set to `NULL`, no title is used.
#' @param subtitle \[`character(1)`\]\cr
#' Subtitle for the plot.
#' @param x_label \[`character(1)`\]\cr
#' Label for the x-axis. If explicitly set to `NULL`, no label is used.
#' @param y_label \[`character(1)`\]\cr
#' Label for the y-axis. If explicitly set to `NULL`, no label is used.
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
#' fit <- nnet::multinom(state ~ lagstate + age, gradu::health)
#' probs <- gradu::stprobs(fit, x = "age")
#' plot(probs)
#'
#' # Calculate state transition proportions
#' props <- gradu::stprops(gradu::health, "id", "state", "age")
#' plot(props)
#'
#' # The argument `fun_cell` can be used to customize the individual cells
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
#' @importFrom methods formalArgs
#' @import data.table
#' @import ggplot2
#' @import patchwork
#' @importFrom nnet multinom
#' @export
plot.stprob <- function(x,
                        fun_cell = identity,
                        title = NULL,
                        subtitle = NULL,
                        x_label = NULL,
                        y_label = NULL,
                        ...) {
  n_states <- length(x$state_values)
  n_cases <- n_states * n_states
  #n_groups <- orElse(length(x$group_name), 0)

  pw <- Reduce(`+`, lapply(1:n_cases, \(i) {
    gx <- ifelse(i %% n_states != 0, i %% n_states, n_states)
    gy <- floor((i - 1) / n_states) + 1
    cell <- plot_cell(x, gx, gy)
    if (!is.function(fun_cell)) {
      warning("Argument `fun_cell` isn't a function! Ignoring `fun_cell`.")
      return(cell)
    }
    attr(cell, "is_gradu_cell") <- TRUE
    n_args <- length(methods::formalArgs(fun_cell))
    if (n_args == 0) {
      warning("Method `fun_cell` needs at least one argument for the cell! Ignoring `fun_cell`.")
      return(cell)
    }
    args <- list(cell)
    if (n_args > 1) {
      args[2] <- i
      if (n_args > 2) {
        warning("Method `fun_cell` has more than 2 arguments! Ignoring the extra.")
        args[3:n_args] <- NA
      }
    }
    cell_modified <- do.call(fun_cell, args = args)
    if (!("is_gradu_cell" %in% names(attributes(cell_modified)))) {
      warning("Method `fun_cell` doesn't return the modified cell! Ignoring `fun_cell`.")
      return(cell)
    }
    return(cell_modified)
  })) +
    patchwork::plot_layout(
      guides = "collect",
      ncol = n_states,
      nrow = n_states)

  prob_or_prop <- ifelse(isTRUE(attr(x, "proportions")), "proportion", "probability")
  if (missing(y_label)) {
    y_label <- orElse(y_label, paste("transition", prob_or_prop))
  }
  if (missing(x_label)) {
    x_label <- orElse(x_label, x$x_name)
  }
  if (missing(title)) {
    title <- orElse(title, paste("State transition", prob_or_prop, "matrix"))
  }

  y_axis <- do.call("fake_axis", args = list(y = y_label))
  x_axis <- do.call("fake_axis", args = list(x = x_label))

  layout <- "
    BA
    CC
  "
  result <- patchwork::wrap_elements(full = pw) + y_axis + x_axis +
    patchwork::plot_layout(design = layout,
                           widths = c(0, 1),
                           heights = c(1, 0)) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle
    )
  return(result)
}

fake_axis <- function(...) {
  return(ggplot2::ggplot() +
           ggplot2::labs(...) +
           patchwork::plot_layout(widths = 0, heights = 0))
}

plot_cell <- function(stprob, gx, gy) {
  state_from <- stprob$state_values[gy]
  state_to <- stprob$state_values[gx]
  settings <- list(
    ggplot2::labs(
      x = NULL,
      y = if(gx == 1) state_from else NULL,
      subtitle = if(gy == 1) state_to else NULL
    ),
    ggplot2::scale_y_continuous(limits = c(0, 1)),
    ggplot2::scale_color_discrete(name = stprob$group_name),
    ggplot2::scale_fill_discrete(name = stprob$group_name),
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  )
  if(gy != length(stprob$state_values)) {
    settings[[length(settings) + 1]] <- ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  }
  if(gx != 1) {
    settings[[length(settings) + 1]] <- ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  }

  cell_data <- stprob$prob[to == state_to & stprob$prob$from == state_from]

  draw_interval <- all(c("lower", "upper") %in% colnames(cell_data))

  group_optional <- onlyIf(!is.null(stprob$group), "group")

  ggplot2::ggplot(
    data = cell_data,
    mapping = ggplot2::aes(y = mean, x = cell_data$x, group = orElse(cell_data$group, 1))
  ) +
    onlyIf(draw_interval,
           ggplot2::geom_ribbon(ggplot2::aes(
             group = NULL,
             ymin = lower,
             ymax = upper,
             fill = optional(.data[[group_optional]])),
             alpha = 0.25
           )) +
    ggplot2::geom_line(ggplot2::aes(color = optional(.data[[group_optional]]))) +
    settings
}
