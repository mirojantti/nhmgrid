#' TODO document
#'
#' @param x description
#' @param ... description
#'
#' @export
plot.struct <- function(x, ...) {
  n_states <- length(x$state$values)
  n_cases <- n_states * n_states
  n_groups <- orElse(length(x$group$values), 0)
  fixed_predictors <- attr(x, "fixed_predictors")
  if (is.list(fixed_predictors)) {
    subtitle <- paste(lapply(seq_len(length(fixed_predictors)), \(i) {
      paste(names(fixed_predictors)[i], "=", fixed_predictors[i])
    }), collapse = ", ")
  } else {
    subtitle <- NULL
  }

  pw <- Reduce(`+`, lapply(1:n_cases, \(i) {
    gx <- ifelse(i %% n_states != 0, i %% n_states, n_states)
    gy <- floor((i - 1) / n_states) + 1
    plot_cell(x, gx, gy)
  })) +
    patchwork::plot_layout(
      guides = "collect",
      ncol = n_states,
      nrow = n_states)

  y_axis <- ggplot2::ggplot() +
      ggplot2::ylab("transition probability") +
    patchwork::plot_layout(widths = 0, heights = 0)
  x_axis <- ggplot2::ggplot() +
    ggplot2::xlab(x$x$name) +
    patchwork::plot_layout(widths = 0, heights = 0)

  (((y_axis | pw) + patchwork::plot_layout(widths = c(0, 1))) / x_axis) +
    patchwork::plot_layout(heights = c(1, 0)) +
    patchwork::plot_annotation(
      title = "State transition probability matrix",
      subtitle = subtitle
    )

}

#' TODO document
#'
#' @param struct description
#' @param gx description
#' @param gy description
#'
plot_cell <- function(struct, gx, gy) {
  state_from <- struct$state$values[gy]
  state_to <- struct$state$values[gx]
  settings <- list(
    ggplot2::labs(
      x = NULL,
      y = if(gx == 1) state_from else NULL,
      subtitle = if(gy == 1) state_to else NULL
    ),
    ggplot2::scale_y_continuous(limits = c(0, 1)),
    ggplot2::scale_color_discrete(name = struct$group$name),
    ggplot2::scale_fill_discrete(name = struct$group$name),
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  )
  if(gy != length(struct$state$values)) {
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


  cell_data <- struct$prob[to == state_to & struct$prob$from == state_from]
  ggplot2::ggplot(
    data = cell_data,
    mapping = ggplot2::aes(x = cell_data$x)
  ) +
    (if (all(c("lower", "upper") %in% colnames(cell_data))) {
      if (!is.null(struct$group)) {
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = group), alpha = 0.25)
      } else {
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.25)
      }
    }) +
    (if (!is.null(struct$group)) {
      ggplot2::geom_line(ggplot2::aes(y = mean, color = cell_data$group))
    } else {
      ggplot2::geom_line(ggplot2::aes(y = mean))
    }) +
    settings
}
