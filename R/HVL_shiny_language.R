#' The language-interface for \code{HVL_shiny}-interface.
#'
#' This function takes care of the 
#'
#' @param .env The environment where the original argument to
#'     \code{HVL_shiny} lives, i.e. the argument
#'     \code{initial_values}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This function updates the language of the
#'     \code{HVL_shiny}-interface.
#'
#' @keywords internal


HVL_shiny_language <- function(.env, .env2) {
    ###-------------------------------------------------------------------
    ##  TODO: Include code to adjust the default values in
    ##  '.env$HVL_logging$select_language' based on the
    ##  browser-language captured in '.env$input$browser_language'
    if (is.null(.env$input$select_language)) {
        ##  Code to update default-values.
    }
    ###-------------------------------------------------------------------
    ##  Detect the default language to be used for the interface.
    .lang <- .env$HVL_logging$select_language[1]
    ###-------------------------------------------------------------------
    ##  Update the title:
    eval(bquote({
        jscode <- sprintf(
        "document.title = \"%s\";",
        HVL_defaults$document.title[[.(.lang)]])
        shinyjs::runjs(jscode)
    }),
    envir = .env2)
    ###-------------------------------------------------------------------
    ##  Update 'select_language_intro'.
    eval(bquote({
        output$select_language_intro <-
            renderPrint(cat(.(HVL_defaults$select_language_intro[[.lang]])))
    }),
    envir = .env2)
    ###-------------------------------------------------------------------
    ##  Create radioButtons to be used when the user want to change
    ##  the interface-language.
    .label <- HVL_defaults$select_language$label[[.lang]]
    .choices <- structure(
        .Data = names(HVL_defaults$select_language$choices),
        .Names = HVL_defaults$select_language$choices)
    eval(bquote(
        output$select_language  <- renderUI({
            radioButtons(inputId = "select_language",
                         label = .(.label),
                         choices = .(.choices),
                         selected = .(.lang))
        })),
        envir = .env2)
    kill(.label, .choices)
    ###-------------------------------------------------------------------
    ##  Update the titles of the tab-panels.  Reminder: The output
    ##  should be following the rule of ".name"_title
    for (.name in names(HVL_defaults$tabPanel$values)) {
        .title <- sprintf("%s_title", .name) 
        eval(bquote({
            output[[.(.title)]] <- renderPrint(
                cat(.(HVL_defaults$tabPanel$values[[c(.name,.lang)]])))
        }),
        envir = .env2)
    }
    kill(.name, .title)
    ###-------------------------------------------------------------------
    ##  Update the text on the 'quit'-button.  Reminder: The 'isTRUE'
    ##  is needed in the initiatio, since 'NULL' is the value stored
    ##  at that time.
    eval(bquote({
        output$quit <- renderUI(
            checkboxInput(inputId = "quit",
                          label = .(HVL_defaults$quit_label[[.lang]]),
                          value = .(isTRUE(.env$input$quit))))
    }),
    envir = .env2)
    ###-------------------------------------------------------------------
    ##  Ensure that other functions also are called, since details
    ##  there also might need to be translated.  This is obtained by
    ##  simply setting all values in the 'xyz'-list to 'TRUE'
    .env$xyz[] <- TRUE
    
    ## if (!is.null(.env$xyz))
    ##     capture_env() 

    ## str(HVL_defaults)
    

}
