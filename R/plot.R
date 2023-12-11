#' TODO document
#'
#' @param x description
#' @param fun_cell description
#' @param ... description
#'
#' @importFrom methods formalArgs
#' @import data.table
#' @export
plot.stprob <- function(x, fun_cell = identity, ...) {
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

  y_axis <- do.call("fake_axis", args = list(y = "transition probability"))
  x_axis <- do.call("fake_axis", args = list(x = x$x$name))

  layout <- "
    BA
    CC
  "
  result <- patchwork::wrap_elements(full = pw) + y_axis + x_axis +
    patchwork::plot_layout(design = layout,
                           widths = c(0, 1),
                           heights = c(1, 0)) +
    patchwork::plot_annotation(
      title = "State transition probability matrix",
      subtitle = subtitle
    )
  return(result)
}

fake_axis <- function(...) {
  return(ggplot2::ggplot() +
           ggplot2::labs(...) +
           patchwork::plot_layout(widths = 0, heights = 0))
}

#' TODO document
#'
#' @param stprob description
#' @param gx description
#' @param gy description
#'
plot_cell <- function(stprob, gx, gy) {
  state_from <- stprob$state$values[gy]
  state_to <- stprob$state$values[gx]
  settings <- list(
    ggplot2::labs(
      x = NULL,
      y = if(gx == 1) state_from else NULL,
      subtitle = if(gy == 1) state_to else NULL
    ),
    ggplot2::scale_y_continuous(limits = c(0, 1)),
    ggplot2::scale_color_discrete(name = stprob$group$name),
    ggplot2::scale_fill_discrete(name = stprob$group$name),
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  )
  if(gy != length(stprob$state$values)) {
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
