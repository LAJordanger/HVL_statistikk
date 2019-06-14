#' A plot to visualise the law of large numbers
#'
#' @param .plot The plot upon which additional information should be
#'     added.  A new plot will be created if the default value
#'     \code{NULL} is used.
#'
#' @param .true_value The true value of the event of interest.  The
#'     default value \code{NULL} can be used to create a plot also in
#'     the case this value is unknown.
#'
#' @param .indicator_values A vector with the result of an
#'     indicator-function that tells us whether the desired event was
#'     observed in the different cases of interest. This could e.g. be
#'     related to the result of a coin-toss being head.
#'
#' @param .m_of_n An integer that shows how many of the
#'     indicator-values that should be used. This is included in order
#'     to enable an animation of the development. The default value
#'     \code{NULL} will set this argument to be the length of the
#'     vector given in \code{.indicator_values}.
#' 
#' @param .colours A character-vector, default value \code{c("red",
#'     "black")}, that gives the \code{colour}-arguments to be used
#'     for the lines showing \code{.indicator_values} and
#'     \code{.true_value}.  The second part will be ignored when
#'     \code{.true_value=NULL}.
#'
#' @param .ltys An integer-valued vector, default value \code{c(1,2)},
#'     that gives the \code{lty}-arguments to be used for the lines
#'     showing \code{.indicator_values} and \code{.true_value}.  The
#'     second part will be ignored when \code{.true_value=NULL}.
#'
#' @param .show_text Logical value, default \code{TRUE} that will add
#'     the true value given in \code{.true_value} (when known) and the
#'     estimated value based on \code{.m_of_n} and
#'     \code{indicator_values}.
#'
#' @param ... Additional arguments to be used in the
#'     \code{xy_sprintf}-function.
#'
#' @return A plot to visualise the law of large numbers.
#'
#' @export


LLN_plot <- function(.plot = NULL,
                     .true_value = NULL,
                     .indicator_values,
                     .m_of_n = NULL,
                     .colours = c("red", "black"),
                     .ltys = c(1,2),
                     .show_text = TRUE,
                     ...) {
    require(ggplot2)
    ##  Check if it is necessary to create a new plot, and if so
    ##  create one with the desired limits on the scales.
    if (is.null(.plot))
        .plot <- ggplot() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank()) +
            ylim(0, 1) +
            xlim(0, length(.indicator_values))
    ##  Compute the desired values to be used when plotting the
    ##  step-function representing the estimated values.
    .x <- seq_along(.indicator_values)
    .y <- cumsum(.indicator_values)/.x
    ##  Decide if only a part should be plotted.
    if (!is.null(.m_of_n)) {
        .x <- .x[seq_len(.m_of_n)]
        .y <- .y[seq_len(.m_of_n)]
    }
    ## Create a list with the content to be added to the plot.
    ## Reminder: This is done in order to avoid the annoying error
    ## "Cannot add ggproto objects together.", that alas occurs when
    ## several 'if'-conditions occur in the setup.
    .add_list <- list(
        ##  Add true line, when required.
        true_line =
            if (!is.null(.true_value)) {
                geom_hline(
                    yintercept = .true_value,
                    colour = .colours[2],
                    lty = .ltys[2]) },
        ##  Add text with the true value when required.
        true_text =
            if (all(.show_text, !is.null(.true_value))) {
                annotate(geom = "text",
                         x = length(.indicator_values)*0.75,
                         y = 1,
                         alpha = 0.8,
                         vjust = "outward",
                         hjust = "outward",
                         label = xy_sprintf(
                             .txt = "~p",
                             x = .true_value,
                             ...),
                             ## digits = 3),
                         colour = .colours[2],
                         size = 10,
                         parse = TRUE) },
        ##  Add the step-function representing the estimates.
        step = annotate(geom = "step",
                        x = .x,
                        y = .y,
                        alpha = 0.7,
                        lty = .ltys[1],
                        colour = .colours[1],
                        lwd = 0.7),
        ##  Add information about the estimated value, when required.
        text_p_hat =
            if (all(.show_text)) {
                annotate(geom = "text",
                         x = length(.indicator_values)*0.75,
                         y = 1,
                         alpha = 0.8,
                         vjust = "inward",
                         hjust = "outward",
                         label = xy_sprintf(
                             .txt = "~widehat(p)",
                             x = tail(.y, n = 1),
                             ...),
                             ## digits = 3),
                         colour = .colours[1],
                         size = 10,
                         parse = TRUE) },
        ##  Add information about the sample-size, when required.
        text_sample_size =
            if (all(.show_text)) {
                annotate(geom = "text",
                         x = length(.indicator_values)*0.75,
                         y = 1,
                         alpha = 0.8,
                         vjust = "inward",
                         hjust = "inward",
                         label = xy_sprintf(
                             .txt = "phantom(widehat(p))*n",
                             x = .m_of_n,
                             digits = 0),
                         colour = .colours[1],
                         size = 10,
                         parse = TRUE) })
    ##  Loop over the list in order to add everything to '.plot'
    for (i in seq_along(.add_list))
        .plot <- .plot + .add_list[[i]]
    ##  Return the result to the workflow.
    .plot
}
