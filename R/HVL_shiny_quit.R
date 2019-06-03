#' Create the button to manually quit \code{HVL_shiny}.
#'
#'
#' @param .env The environment where the original argument to
#'     \code{HVL_shiny} lives, i.e. the argument
#'     \code{initial_values}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This function decides if the insist-quit button should be
#'     present or not, and it also takes care of the language to be
#'     used on this button.
#'
#' @keywords internal


HVL_shiny_quit <- function(.env, .env2) {
    ###-------------------------------------------------------------------
    ##  Detect the default language to be used for the interface, and
    ##  add the  "I want to quit"-button.
    .lang <- .env$HVL_logging$select_language[1]
    eval(bquote({
        output$insist_quit <- renderUI(
            if (.(isTRUE(.env$input$quit))) {
                br()
                actionButton("insist_quit",
                             label = .(HVL_defaults$insist_quit_label[[.lang]]))
                             ## label = "Yes, I DO want to quit!")
            } else {
                return()
            })
    }),
    envir = .env2)    
}
