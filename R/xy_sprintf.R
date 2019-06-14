#' Slightly adjusted version of sprintf
#'
#' This is a wrapper for \code{sprintf}, which had to be written in
#' order to circumvent a problem that occurred when it turned out that
#' an attempt at selecting either \code{==} or \code{%%~~%%} based on
#' a logical test made some \code{ggplot}-code return an error when
#' the latter alternative where selected.
#'
#' @param .txt A textstring to be included at the left side of the
#'     equality (or approximation) sign.
#'
#' @param x A number which will be included at the right side of the
#'     equality (or approximation) sign.  Note that \code{x} will be
#'     converted to a text-string by the help of the function
#'     \code{xy_formatC}.
#' 
#' @param ... Arguments to be given to \code{xy_formatC}, which will
#'     govern how \code{x} is transformed into the text to be included
#'     at the right side of the equality (or approximation) sign.
#'
#' @return This function will return a character-string that either
#'     states that \code{.txt} is equal to \code{x}, or that
#'     \code{.txt} is approximately equal to \code{x}.  Depending on
#'     the arguments in \code{...}, this text can then either be used
#'     to present the result in a \code{ggplot} or it can be included
#'     in the describing text.
#'
#' @keywords internal

xy_sprintf <- function(.txt, x, ...) {
    ##  Compute the desired rounded number
    .result <- xy_formatC(x, ...)
    if (attributes(.result)$equal) {
        sprintf("%s == %s",
                .txt,
                .result)
    } else {
        sprintf("%s %%~~%% %s",
                .txt,
                .result)
    }
}
