#' A wrapper for \code{formatC}
#'
#' This wrapper for \code{formatC} includes code that allows for some
#' additional tweaking to be taken care of when a number is converted
#' to a text-string.  It was primarily created in order to resolve an
#' issue related to the disappearance of trailing zeros when adding
#' numbers to a \code{ggplot}. (Reminder: The problem occurred when
#' \code{parse=TRUE} was used when adding text/labels.)
#'
#' @param x The number that should be converted to text.
#'
#' @param digits A number, default value \code{3}, to be given to the
#'     \code{digits}-argument of \code{round}.
#'
#' @param not_IEC60599 A logical value, default \code{TRUE}, which can
#'     be used to round \code{x} in the same manner a human normally
#'     would do, e.g. that "2.5" rounds to "3" instead of "2".
#'     Reminder: This is included to avoid that students must be
#'     explained why the rounding used in the text is different from
#'     what they themselves have computed.
#'
#' @param .plotmath A logical value, default \code{TRUE}, which will
#'     ensure that the format of the returned character-string works
#'     as desired when included as a part of some text or label in a
#'     \code{ggplot}.  In particular, this ensures that any trailing
#'     zeros are properly preserved when \code{parse=TRUE} is used
#'     when adding the text.
#'
#' @param .decimal A character, default value \code{.}, which is to be
#'     used for the decimal sign for the converted number.  This
#'     enables the decimal sign to be replaced with \code{,}, which is
#'     of interest for those countries where that is the norm.
#'
#' @return A character-string based on \code{x}, modified according to
#'     the description given in the arguments \code{digits},
#'     \code{not_IEC60599}, \code{.plotmath} and \code{.decimal}.
#'     Reminder: The result will have an attribute named \code{equal},
#'     which is included sine the intended use of this function is
#'     that it will be called from \code{xy_sprintf}, which then will
#'     either add an equality sign or an approximation sign based on
#'     the value of the \code{equal}-attribute.
#'
#' @keywords internal

xy_formatC <- function(x,
                       digits = 3,
                       not_IEC60559=TRUE,
                       .plotmath=TRUE,
                       .decimal = ".") {
    ##  Create a copy of 'x' that depending on 'not_IEC60559' might be
    ##  a slightly perturbed version of 'x'.  Reminder: This is done
    ##  in order to imitate the human way of rounding numbers.
    .x  <- x + ifelse(
                   test = not_IEC60559,
                   yes  = sign(x) * .Machine$double.eps,
                   no   = 0)
    .result <- formatC(x = .x, digits = digits, format = "f")
    ##  If this is to be used inside a plot, then it might be
    ##  necessary to replace each trailing zero with "*0". This can be
    ##  done by comparing the result when trailing zeros are ignored.
    ##  A decimal sign will also be added when required.
    if (.plotmath) {
        .temp_plot <- as.character(x = round(.x, digits = digits))
        ##  Add decimal sign when required, taking into account the
        ##  desired sign to use.
        .need_decimal_sign <- all(
            round(x, digits = digits) == round(x, digits = 0),
            digits > 0)
        .tz <- nchar(.result) - nchar(.temp_plot) -
            as.integer(.need_decimal_sign)
        ##  Adjust or add decimal-sign
        if (.tz < digits) {
            .temp_plot <- gsub(
                x = .temp_plot,
                pattern = "\\.",
                replacement = sprintf(
                    "*'%s'*",
                    .decimal))
        } else {
            if (digits > 0)
                .temp_plot <- sprintf(
                    "%s*'%s'",
                    .temp_plot,
                    .decimal)
        }
        ##  Update '.result'
        .result <- paste(
            c(.temp_plot,
              rep(x="0", times = .tz)),
            collapse = "*")
        ##  When required, do a replacement of the decimal-sign.
        if (.decimal!=".") {
            .result <- gsub(
                x = .result,
                pattern = "\\.",
                replacement = sprintf("*'%s'%s",
                                      .decimal,
                                      ifelse(
                                          test = {.tz < digits},
                                          yes  = "*",
                                          no   = "")))
        }
    } else {
        ##  When required, do a replacement of the decimal-sign.
        if (.decimal!=".") {
            .result <- gsub(
                x = .result,
                pattern = "\\.",
                replacement = .decimal)
        }
    }
    ##  Add an attribute to '.result', to show if its value is equal
    ##  to the original 'x', so other functions can select the correct
    ##  symbol when displaying formulas.
    attr(x = .result, which = "equal") <- x == round(x, digits = digits)
    ##  Return the result to the workflow.
    .result
}
