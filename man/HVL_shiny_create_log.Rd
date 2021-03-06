% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HVL_shiny_create_log.R
\name{HVL_shiny_create_log}
\alias{HVL_shiny_create_log}
\title{Create \code{HVL_logging}-object for the dynamic
\code{HVL_shiny}-interface.}
\usage{
HVL_shiny_create_log(.env)
}
\arguments{
\item{.env}{The environment where the original arguments given to
\code{HVL_shiny} lives.  The logging-object will be added to
this environment.}
}
\value{
This function adds the \code{HVL_logging}-object into the
    environment \code{.env}.  This object will log the input-values in
    order to avoid problems when switching between different
    branches, i.e. it prevents residual input-values to trigger
    subsetting problems.  Moreover, the last selected input-values
    for an old branch will be stored when there is a switch to a
    new branch, and these will then be loaded the next time the old
    branch is visited.  The function does also add the initial
    \code{NA}-values to the reactive \code{input}-object in the
    \code{shiny}-application started by \code{HVL_shiny}, and this
    is done in order for some of the logical tests to become
    simpler during the initiation phase.
}
\description{
This function initiates the \code{HVL_logging}-object needed in
order for the dynamic structure of the \code{HVL_shiny}-application
to work without any glitches.  Moreover, it does also adds a bunch
of values to the reactive \code{input}-object in the
\code{shiny}-application started by \code{HVL_shiny}.
}
\keyword{internal}
