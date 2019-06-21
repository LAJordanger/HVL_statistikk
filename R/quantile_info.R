#' Create a list with quantile-information.
#'
#' The purpose of this function is to create a list with information
#' related to different quantiles, in order for this to be used as
#' arguments in \code{add_label_to_plot}.
#'
#'
#' @param .type The distribution to be used, which should be one of
#'     \code{c("chisq", "norm", "t")}.
#'
#' @param .alpha The number to be used for the alpha value when
#'     computing the quantile, which as usual refers to the upper tail
#'     of the distribution.  The default value for this argument is
#'     \code{0.05}.
#'
#' @param .df The degrees of freedom, default \code{NULL}, to be used
#'     for those quantiles that requires this argument.
#'
#' @param .df_txt The text to be used for the specification of the
#'     df-value.  The default value is \code{"nu"}, but it might in
#'     some cases be of interest to use e.g. \code{"n-1"} or
#'     \code{"n-2"}.
#' 
#' @param digits An integer, default value \code{2}, which will decide
#'     the number of digits to use when rounding the quantile values.
#'     Note: The rounding will 
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
#' @keywords internal

quantile_info <- function(.type = c("chisq", "norm", "t"),
                          .alpha = 0.05,
                          .df = NULL,
                          .df_txt = "nu",
                          digits = 2,
                          not_IEC60559 = TRUE,
                          .plotmath = TRUE,
                          .decimal = ".") {
    ##  Check that '.df' is included when it is needed.
    if (.type != "norm")
        if (is.null(.df))
            stop("The '.df'-argument must be included")
    ##  Compute all the normally occuring quantiles for the
    ##  density-function specified in '.type'.
    .alpha_values <- c(1-.alpha/2, 1-.alpha, .alpha, .alpha/2 )
    .quantile_values <- vapply(
        X = .alpha_values,
        FUN = function(..alpha)
            switch(
                EXPR = .type,
                chisq = qchisq(
                    p = ..alpha,
                    df = .df,
                    lower.tail = FALSE),
                norm = qnorm(
                    p = ..alpha,
                    lower.tail = FALSE),
                t = qt(
                    p = ..alpha,
                    df = .df,
                    lower.tail = FALSE)),
        FUN.VALUE = numeric(1))
    ##  Create the text to be used in the subsetting.  A minor check
    ##  is included with regard to the number of digits in order avoid
    ##  ugly representations of values like '0.10' and '0.05'.  The
    ##  tweaking for the t- and z-quantiles is needed in order to get
    ##  the required subsetting.
    .alpha_values_txt <- lapply(
        X  = if (.type == "chisq") {
                 .alpha_values
             } else
                 .alpha * c(1/2, 1, 1, 1/2),
        FUN = function(..alpha) {
            ..digits <- nchar(strsplit(
                x = as.character(..alpha),
                split = "\\.")[[1]][2])
            if (..digits < 2)
                ..digits <- 2
            xy_formatC(..alpha,
                       digits = ..digits,
                       not_IEC60559 = FALSE)
        })
    ##  When relevant: Add information about the degrees of freedom.
    if (.type %in% c("chisq", "t"))
        .alpha_values_txt <-
            vapply(
                X = .alpha_values_txt,
                FUN = function(x)
                    sprintf("%s%s%s",
                            x,
                            ifelse(
                                test = .plotmath,
                                    yes  = "*','*",
                                no   = ","),
                            .df),
                FUN.VALUE = character(1))
    ##  Create the text that gives the symbolic presentation to be
    ##  used when subsetting.
    .alpha_symbols <- local({
        ..alpha_part <-
            if (.type == "chisq") {
                c("1-alpha/2", "1-alpha", "alpha", "alpha/2")
            } else
                c("alpha/2", "alpha", "alpha", "alpha/2")
        if (.type %in% c("chisq", "t"))
            ..alpha_part <-
                vapply(
                    X = ..alpha_part,
                    FUN = function(x)
                        sprintf("%s%s%s",
                                x,
                                ifelse(
                                    test = .plotmath,
                                    yes  = "*','*",
                                    no   = ","),
                                .df_txt),
                    FUN.VALUE = character(1))
        ..alpha_part
    })
    ##  Create a text-string to help adding the desired sign.
    ..sign_adjustment <-
        if (.type == "chisq") {
            rep("", times = 4)
        } else
            c("-","-","","")
    ##  Create the text to be used for the only-symbols version of the
    ##  quantiles.
    .quantile_only_symbols <- vapply(
        X = seq_along(.alpha_symbols),
        FUN = function(i)
            sprintf("%s%s[%s]",
                    ..sign_adjustment[i],
                    switch(
                        EXPR = .type,
                        chisq = "chi",
                        norm  = "z",
                        t     = "t"),
                    .alpha_symbols[i]),
        FUN.VALUE = character(1))
    ##  Create the text to be used for the with-values version of the
    ##  quantiles.
    .quantile_with_values <- vapply(
        X = seq_along(.alpha_values_txt),
        FUN = function(i)
            sprintf("%s%s[%s]",
                    ..sign_adjustment[i],
                    switch(
                        EXPR = .type,
                        chisq = "chi",
                        norm  = "z",
                        t     = "t"),
                    .alpha_values_txt[i]),
        FUN.VALUE = character(1))
    ##  Create a list with all the options that could be of interest
    ##  to include.  Reminder: Two steps needed since the last
    ##  component is a function of two other components.
    .result <- list(
        Qs = .quantile_only_symbols,
        Qv = .quantile_with_values,
        QV = .quantile_values,
        ##  Create the rounded text-version of the quantile values.
        Qt =     vapply(
            X = .quantile_values,
            FUN = function(..q) 
                xy_formatC(x=..q,
                           digits = digits,
                           not_IEC60559 = not_IEC60559,
                           .plotmath = .plotmath,
                           .decimal = .decimal),
            FUN.VALUE = character(1)),
        ##  Create the basic presentation of quantile and its
        ##  (approximate) value.
        Qvt = vapply(
            X = seq_along(.quantile_values),
            FUN = function(i)
                xy_sprintf(
                    .txt = .quantile_with_values[i],
                    x = .quantile_values[i],
                    digits = digits,
                    not_IEC60559 = not_IEC60559,
                    .plotmath = .plotmath,
                    .decimal = .decimal),
            FUN.VALUE = character(1)))
    ##  Add the last component, where the whole length of it is
    ##  included.
    .result$Qsvt  <- vapply(
        X = seq_along(.quantile_values),
        FUN = function(i)
            sprintf("%s%s%s",
                    .result$Qs[i],
                    ifelse(test = .plotmath,
                           yes  = "==",
                           no   = "="),
                    .result$Qvt[i]),
        FUN.VALUE = character(1))
    ##  Return the result to the workflow
    .result
}
