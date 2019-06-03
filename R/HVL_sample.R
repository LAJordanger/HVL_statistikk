#' Simulate samples to be used in \code{HVL_shiny},
#'
#' This function will be called from \code{HVL_shiny}, and its purpose
#' is to create the samples needed for some of the interactive
#' investigations.
#'
#' @param dist A character vector that must be one of the implemented
#'     distributions.
#'
#' @param par A list with the arguments to be used when sampling from
#'     the distribution given in \code{dist}.
#'
#' @param n An integer, default value \code{100}, that specifies the
#'     number of observations in each individual sample.
#'
#' @param M An integer, default value \code{100}, that specifies the
#'     number of replicates of samples of size \code{n} from the
#'     distribution specified in \code{distribution}.
#'
#' @param seed An integer that can be used to specify a seed-value to
#'     be used when sampling.  The default value \code{NULL} turns of
#'     the part of the code that sets the seed.
#'
#' @return The \code{M} samples of size \code{n} will be returned as
#'     an array.  
#'
#' @export

HVL_sample <- function(dist,
                       par,
                       n = 100,
                       M = 100,
                       seed = NULL) {
    ##  Set the seed-value when required
    if (!is.null(seed))
        set.seed(seed)
    ##  Create a copy of the 'par'-list, and add the value that
    ##  decides how many samples that should be created altogether.
    .par <- par
    .par[[distribution_details[[dist]]$sample$.n]] <- n*M
    ##  Create an array with the desired samples, structured with one
    ##  sample-sequence of size n for each column.  Add
    ##  dimension-names as a reminder, and include attributes to be
    ##  used in computations/visualisations later on.
    .sample <- structure(
        .Data = do.call(
            what = distribution_details[[dist]]$sample$.fun,
            args = .par),
        .Dim = c(n,M),
        .Dimnames = list(
            i = sprintf("i%s",1:n),
            M = sprintf("M%s", 1:M)),
        details = list(
            dist = dist,
            n = n,
            par = par,
            type = distribution_details[[dist]]$type))
    ##  Add an attribute with information about the means and standard
    ##  deviations of the 'M' different samples.
    attr(x = .sample, which = "m_sd") <- apply(
        X = .sample,
        MARGIN = 2,
        FUN = function(x) {
            c(mean = mean(x), sd = sd(x))})
    ##  Return the result to the workflow.
    .sample
}
