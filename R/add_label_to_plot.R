#' Add information about critical values to a plot.
#'
#' This function collects some code that helps place information about
#' critical values to a density plot.
#'
#' @param .plot The plot upon which the information should be added.
#'
#' @param x A number that gives the first coordinate of the point
#'     where information in \code{.label} should be added.
#'
#' @param y A number (default value \code{0}) that gives the second
#'     axis coordinate of the point where the information in
#'     \code{.label} should be added.
#' 
#' @param add_tick Logical value, default value \code{FALSE}, that can
#'     be used to add a "tick" to the plot at the coordinate given by
#'     \code{x} and \code{y}.
#'
#' @param label The text to be used as a label, based at the position
#'     given by \code{x} and \code{y}.
#'
#' @param parse Logical value, default value \code{TRUE}, which
#'     decides if the text given in \code{label} should be parsed.
#'
#' @param vjust A number (default value \code{2}) that governs the
#'     vertical adjustment of \code{label} relative the value
#'     \code{y}. Note that this argument is relative to the
#'     \code{size}-value, and an increase in \code{size} might thus
#'     require a decrease in \code{vjust} in order to get the desired
#'     position.
#'
#' @param size The value that governs the size of the text, default
#'     value \code{5}.
#'
#' @param colour The colour, default value \code{"black"}, that will
#'     be used for the added details.
#' 
#' @return An updated version of \code{.plot}, where information has
#'     been added.
#'
#' @keywords internal

add_label_to_plot <- function(.plot, x, y, add_tick = FALSE, label,
                              parse = TRUE, vjust = 2, size = 5,
                              colour = "black") {
    ##  Return an updated plot.
    .plot +
        annotate(
            geom = "text",
            label = label,
            x = x,
            y = y,
            vjust = vjust,
            size = size,
            colour = colour,
            parse = parse) +
        if (add_tick) {
            ## Compute the vertical length of the added tick by the
            ## help of the following crappy approach: Extract a list
            ## of data-frames from '.plot', find the maximum of y for
            ## each of the available dataframes (for the present cases
            ## this should either be a column named "y" or "ymax")
            .ref <- max(vapply(
                X = ggplot_build(.plot)$data,
                FUN = function(.t) 
                    max(.t$y, .t$ymax),
                FUN.VALUE = numeric(1)))
            annotate(geom = "path",
                     x = c(x,x),
                     y= 0.005*c(-1, 1)*.ref,
                     colour = colour,
                     lty = 1,
                     size = 0.5,
                     alpha = 0.8)
        }
}
