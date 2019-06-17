#' An indicator function to reveal if a given observation belong to a
#' specified event.
#'
#' @param x A vector of observations from some distribution.
#'
#' @param .type Either \code{"c"} for continuous (default) or
#'     \code{"d"} for discrete.  If the value is \code{"c"}, then
#'     \code{event} must be a numeric vector of length 2, otherwise it
#'     can be a vector of any length.
#' 
#' @param event A numeric vector.  The vector must have length 2 when
#'     \code{.type="c"}, since the values then will be used as the
#'     lower and upper limits of an interval.  The argument
#'     \code{border_criteria} endpoints of the intervals are included
#'     in the event.
#'
#' @param border_criteria A vector of length two, that must be one the
#'     following four: \code{c("<=", "<=")} (default), \code{c("<",
#'     "<=")}, \code{c("<", "<")}, \code{c("<", "<=")}.  This is
#'     needed when \code{.type="c"}, since it will govern whether or
#'     not the endpoints of the interval should be included in event.
#'     This argument is ignored when \code{.type="d"}.
#'
#' @param .res_type Either \code{"L"} for logical (default) or
#'     \code{"I"} for integer.  Governs the format of the resulting
#'     vector.
#' 
#' @return This function will return vector of the same length as
#'     \code{x}, which depending on \code{.res_type} either will be of
#'     type logical or of type integer.  The values will be used to
#'     indicate whether or not the observations in \code{x} was
#'     contained in the set defined by \code{event}.
#' 
#' @keywords internal

Ind <- function(x, .type = "c", event,
                border_criteria = c("<=", "<="),
                .res_type = "L") {
    if (.type == "c") {
        ##  Test the sanity of the 'event'-argument.
        if (length(event)!=2)
            stop("'event' must have length 2 when '.type' is equal to 'd'.")
        ##  Test the sanity of the 'border_criteria'-argument.
        .valid_border <- list(
            c("<=", "<="),
            c("<", "<="),
            c("<", "<"),
            c("<", "<="))
        .test <- vapply(
            X = .valid_border,
            FUN = function(.vb) identical(.vb, border_criteria),
            FUN.VALUE = logical(1))
        if (!any(.test))
            stop("Erroneous argument for 'border_criteria'.  See documtation for details.")
        ##  Check if the observations are included in the continuous interval.
        .result  <- vapply(
            X = x,
            FUN = function(.x) {
                all(ifelse(
                    test = {border_criteria[1] == "<="},
                    yes  = min(event) <= .x,
                    no   = min(event) <  .x),
                    ifelse(
                    test = {border_criteria[2] == "<="},
                    yes  = .x <= max(event),
                    no   = .x <  max(event)))
            },
            FUN.VALUE = logical(1))
    } else {
        ##  Check if the observations are included in the discrete event.
        .result <- x %in% event
    }
    ##  Return the result in the format described by '.res_type'
    if (.res_type!="L") {
        as.integer(.result)
    } else
        .result
}
