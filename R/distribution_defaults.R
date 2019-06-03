#' Distribution defaults
#'
#' A list that describes the default values to be used for the
#' distributions.  Functions with a \code{distribution}-argument will
#' have this tested against the details given in this list.  Note that
#' the \code{type}-part of the \code{distribution}-argument must be
#' one of the top-level names of the list \code{distribution_details},
#' and that the parameters must be within the acceptable range.


distribution_details <- list(
    binomial = list(
        par = list(
            n = 1:100,
            p = seq(from = 0.05,
                    to = 0.95,
                    by = 0.05)),
        par_default = list(
            n = 10,
            p = 0.5),
        par_range = list(
            n = "(1,Inf)",
            p = "(0,1)"),
        type = "discrete",
        sample = list(.fun = "rbinom",
                      .n = "n")),
    normal = list(
        par = list(
            mean = -1:1,
            sd = c(.25, 1, 4)),
        par_default = list(
            mean = 0,
            sd = 1),
        par_range = list(
            mean = "(-Inf,Inf)",
            sd = "[0,Inf)"),
        type = "continuous",
        sample = list(.fun = "rnorm",
                      .n = "n")),
    chi.squared = list(
        par = list(
            df = c(0.25, 1, 2, 4, 8, 16, 30),
            ncp = c(0, 0.25, 0.5, 1, 2, 4)),
        par_default = list(
            df = 2,
            ncp = 0),
        par_range = list(
            df = "[0,Inf)",
            ncp = "[0,Inf)"),
        type = "continuous",
        sample = list(.fun = "rchisq",
                      .n = "n")),
    t = list(
        par = list(
            df = c(0.25, 1, 2, 4, 8, 16, 30),
            ncp = c(0, 0.25, 0.5, 1, 2, 4)),
        par_default = list(
            df = 2,
            ncp = 0),
        par_range = list(
            df = "(0,Inf]",
            ncp = "[0,Inf)"),
        type = "continuous"),
    sample = list(.fun = "rt",
                  .n = "n"))

##  Only these for the moment, should include other options later on,
##  and allow for "kji.kvadrat" to be used as an alternative option.
