#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_y_continuous
#' @export
plot.struct <- function(struct) {
  n_states <- length(struct$states)
  n_cases <- n_states * n_states
  n_groups <- if (is.null(struct$group)) 0 else length(struct$group$values)

  pw <- lapply(1:n_cases, \(i) {
    x <- ifelse(i %% n_states != 0, i %% n_states, n_states)
    y <- floor((i - 1) / n_states) + 1
    trans <- paste0(struct$states[y], "->", struct$states[x])
    ci <- (i - 1) * max(1, n_groups) + 1
    cj <- ifelse(n_groups != 0, ci + n_groups - 1, ci)
    plot_cell(
      grid_coords = list(x = x, y = y),
      states = struct$states,
      x = struct$x,
      prob = struct$prob[, ci:cj],
      group = struct$group,
      trans = trans
    )
  }) |> (\(x) Reduce(`+`, x))() +
  patchwork::plot_layout(
    guides = "collect",
    ncol = n_states,
    nrow = n_states)

  y_axis <- ggplot() +
      ggplot2::ylab("transition probability") +
    patchwork::plot_layout(widths = 0, heights = 0)
  x_axis <- ggplot() +
    ggplot2::xlab(struct$x$name) +
    patchwork::plot_layout(widths = 0, heights = 0)

  (((y_axis | pw) + patchwork::plot_layout(widths = c(0, 1))) / x_axis) +
    patchwork::plot_layout(heights = c(1, 0))

}

#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 scale_y_continuous
plot_cell <- function(grid_coords, states, x, prob, group, trans) {
  settings <- list(
    labs(
      x = NULL,
      y = if(grid_coords$x == 1) states[grid_coords$y] else NULL,
      subtitle = if(grid_coords$y == 1) states[grid_coords$x] else NULL
    ),
    scale_y_continuous(limits = c(0, 1)),
    theme(
      plot.subtitle = element_text(hjust = 0.5)
    )
  )
  if(grid_coords$y != length(states)) {
    settings[[length(settings) + 1]] <- theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
  if(grid_coords$x != 1) {
    settings[[length(settings) + 1]] <- theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }

  ggplot() +
    (if (!is.null(group)) {
      lapply(group$values, \(g) {
        geom_line(aes(
          x = x$values,
          y = prob[, get(paste0(g, ": ", trans))],
          color = g))
      })
    } else {
      geom_line(aes(
        x = x$values,
        y = prob[, get(trans)]))
    }) +
    settings
}
