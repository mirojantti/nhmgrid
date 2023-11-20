#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_y_continuous
#' @export
plot.struct <- function(struct) {
  n_states <- length(struct$state$values)
  n_cases <- n_states * n_states
  n_groups <- orElse(length(struct$group$values), 0)

  pw <- Reduce(`+`, lapply(1:n_cases, \(i) {
    x <- ifelse(i %% n_states != 0, i %% n_states, n_states)
    y <- floor((i - 1) / n_states) + 1
    trans <- paste0(struct$states[y], "->", struct$states[x])
    ci <- (i - 1) * max(1, n_groups) + 1
    cj <- ifelse(n_groups != 0, ci + n_groups - 1, ci)
    plot_cell(struct, x, y)
  })) +
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
#' @importFrom ggplot2 scale_color_discrete
plot_cell <- function(struct, gx, gy) {
  state_from <- struct$state$values[gy]
  state_to <- struct$state$values[gx]
  settings <- list(
    labs(
      x = NULL,
      y = if(gx == 1) state_from else NULL,
      subtitle = if(gy == 1) state_to else NULL
    ),
    scale_y_continuous(limits = c(0, 1)),
    scale_color_discrete(name = struct$group$name),
    theme(
      plot.subtitle = element_text(hjust = 0.5)
    )
  )
  if(gy != length(struct$state$values)) {
    settings[[length(settings) + 1]] <- theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
  if(gx != 1) {
    settings[[length(settings) + 1]] <- theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }


  cell_data <- struct$prob[
    trans_dest == state_to & struct$prob[[struct$state$name]] == state_from]
  ggplot(
    data = cell_data,
    mapping = aes(x = cell_data[[struct$x$name]])
  ) +
    (if (!is.null(struct$group)) {
      geom_line(aes(y = trans_prob, color = cell_data[[struct$group$name]]))
    } else {
      geom_line(aes(y = trans_prob))
    }) +
    settings
}
