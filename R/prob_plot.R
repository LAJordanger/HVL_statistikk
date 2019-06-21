#' Visualisation of a probability density function.
#'
#' A helper function to add a graph and mark an area corresponding to
#' some probability.
#'
#' @param x A vector with the values to be used along the first axis.
#'
#' @param y A vector with the values along the second axis.
#'
#' @param .plot The plot upon which additional information should be
#'     added.  A new plot will be created if the default value
#'     \code{NULL} is used.
#'
#' @param .lower Default value \code{NULL}. To be used when a lower
#'     end of the area is desired.  If this argument is \code{NULL}
#'     and an upper end is specified in \code{.upper}, then this
#'     argument will be reset to the lowest value given in \code{x}.
#' 
#' @param .upper Default value \code{NULL}. To be used when an upper
#'     end of the area is desired.  If this argument is \code{NULL}
#'     and a lower end is specified in \code{.lower}, then this
#'     argument will be reset to the highest value given in \code{x}.
#'
#' @param .colour The \code{colour}-value to be used for the graph,
#'     default value \code{"black"}.
#'
#' @param .lty The \code{lty}-value to be used for the graph, default
#'     value \code{1}.
#'
#' @param .size The \code{size}-value to be used for the graph,
#'     default value \code{0.7}.
#'
#' @param .alpha The \code{alpha}-value to be used for the graph,
#'     default value \code{0.7}.
#' 
#' @param ... Other arguments passed on to the plot of the area, like
#'     \code{fill = "red"}, \code{colour = "red"}, \code{alpha = 0.6}
#'     or \code{size = 0.1}.
#'
#' @return A plot showing a graph (given by \code{x} and \code{y}).
#'     If at least one of \code{.lower} and \code{.upper} are
#'     different from \code{NULL}, then a shaded area representing a
#'     probability will be added too.  Additional graphs/areas can be
#'     added iteratively by the help of the \code{.plot}-argument.
#'
#' @export

prob_plot <- function(x,
                      y,
                      .plot = NULL,
                      .colour = "black",
                      .lty = 1,
                      .size = 0.7,
                      .alpha = 0.7,
                      .lower = NULL,
                      .upper = NULL,
                      ...) {
    require(ggplot2)
    ##  Check if it is necessary to create a new plot
    if (is.null(.plot))
        .plot <- ggplot() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank())
    ##  Check if an area should be added.
    .area <- any(!is.null(.lower),
                 !is.null(.upper))
    ##  If an area is needed, check if one of the bounds should be
    ##  adjusted, and identify the parts of 'x' and 'y' that is needed
    ##  for the plotting of the area.
    if (.area) {
        if (is.null(.lower))
            .lower <- min(x)
        if (is.null(.upper))
            .upper <- max(x)
        .area_part <- as.logical({x >= .lower} * {x <= .upper})
        .x_area <- x[.area_part]
        .y_area <- y[.area_part]
    }
    ##  Return a plot with graph and area added to it.
    .plot +
        annotate(geom = "path",
             x = x,
             y= y,
             colour = .colour,
             lty = .lty,
             size = .size,
             alpha = .alpha) +
        if (.area) {
            annotate(geom = "ribbon",
                     x= .x_area,
                     ymin = 0,
                     ymax = .y_area,
                     ...)
        }
}
