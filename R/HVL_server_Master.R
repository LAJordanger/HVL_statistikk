#' Master for the \code{server}-part of the \code{HVL_shiny}-interface.
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
#' @return This function is the master function that takes of the
#'     dynamic part of the interface for the
#'     \code{HVL_shiny}-function.  (The resulting updates are stored
#'     in the environments \code{.env} and \code{.env2}.)
#'
#' @keywords internal

HVL_server_Master <- function(.env, .env2) {


    ##  Create an updated nonreactive version of the 'input'-values
    ##  (to simplfy the development).
    .env$input <- reactiveValuesToList(x = .env2$input,
                                       all.names = TRUE)
    ##  Initiation: Create a counter and detect the browser language.
    if (!exists(x = "counter", envir = .env, inherits = FALSE)) {
        .env$counter <- 0
        ##  Store browser language as 'input$browser_language'.
        eval(bquote({
            jscode <- "var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('browser_language', language); console.log(language);"
            shinyjs::runjs(jscode)
            output$browser_language <- renderPrint(input$browser_language)
        },
where = .env2),
envir = .env2)


    ################################################################################
    
    ##  A test to see how much might be created here instead of in the
    ##  server, i.e. can entire panels an and other stuff be taken
    ##  care of here...

        parameters <- function(.model, .par, .label = .par) {
        ## Sanity-check of the arguments.
        if (! .model %in% names(distribution_details))
            stop(sprintf("The model '%s' has not been implemented...", .model))
        if (! .par %in% names(distribution_details[[c(.model, "par")]]))
            stop(sprintf("No argument '%s' in the distribution '%s'.", .par, .model))
        ##  Extract the vector of parameters.
        .parameters <- distribution_details[[c(.model, "par", .par)]]
        ##  Find the position of the default value in '.parameters'
        .default <- which(
            .parameters == distribution_details[[c(.model, "par_default", .par)]])
        ##  Create the desired interface.
        .first <- sliderInput(
            inputId = .par,
            label = .label,
            min = 1,
            max = length(.parameters),
            value = .default,
            step = 1,
            animate = animationOptions(
                interval = 333,
                loop = FALSE,
                playButton = NULL,
                pauseButton = NULL))
    }

    ##  Create a list with the interface to be used later on.
    .interface <- list(
        plot_type = radioButtons(
            inputId = "plot_type",
            label = "Select the desired plot-type.",
            choices = c("pdf", "cdf"),
            selected = "pdf",
            inline = TRUE),
        normal_approximation = radioButtons(
            inputId = "normal_approximation",
            label = "Show normal approximation.",
            choices = c("on", "off"),
            selected = "off",
            inline = TRUE),
        mean_pm_sd = radioButtons(
            inputId = "mean_pm_sd",
            label = "Show mean with standard deviations.",
            choices = c("on", "off"),
            selected = "off",
            inline = TRUE),
        show_intervals_areals = radioButtons(
            inputId = "show_intervals_areals",
            label = "Show intervals and areals.",
            choices = c("on", "off"),
            selected = "off",
            inline = TRUE),
        alpha_quantile = radioButtons(
            inputId = "alpha_quantile",
            label = "Select a quantile.",
            choices = local({
                .alternatives <- c(0.1, 0.05, 0.025, 0.010, 0.005, 0.001)
                structure(
                    .Data = .alternatives,
                    .Names = .alternatives)
            }),
            selected = "0.05",
            inline = TRUE),
        side_type = radioButtons(
            inputId = "side_type",
            label = "Select type of interval/areal.",
            choices = structure(
                .Data = c("lower", "two_sided", "upper"),
                .Names = c("Lower", "Two sided", "Upper")),
            selected = "two_sided",
            inline = TRUE),
        scaling = selectInput(
              inputId = "scaling",
              label = "Select the desired scaling",
              choices = c("none", "only x", "only y", "both x and y"),
            selected = "none"))
    
    eval(bquote({
        output$wellpanel_test <-
            renderUI(wellPanel(
        uiOutput("distribution"),
        .(.interface$scaling),
        .(.interface$plot_type)
        ))
    }),
    envir = .env2)


    ## uiOutput("wellpanel_test")

        
        return()
    } else
        .env$counter <- .env$counter + 1
    ###-------------------------------------------------------------------
    ##  After the initial detection of the browser language, it is
    ##  time to create the desired interface.
    if (.env$counter == 1) {
        HVL_shiny_language(.env, .env2)
        return()
    }
    ###-------------------------------------------------------------------
    ##  Create a list that detects the differences between stored and
    ##  logged input-values, i.e. return 'TRUE' for differences.
    .env$xyz <- list()
    for (.name in .env$HVL_logging$input_triggers) {
        .env$xyz[[.name]] <- ! identical(
            x = .env$input[[.name]],
            y = .env$HVL_logging[[.name]][1])
        ##  Update 'HVL_logging' when required.
        if (.env$xyz[[.name]]) {
            .env$HVL_logging[[.name]] <-
                c(.env$input[[.name]],
                  .env$HVL_logging[[.name]][1])
        }
    }
    kill(.name)
    ##  Terminate when no changes are detected.
    if (! any(unlist(.env$xyz))) {
        return()
    }
    ###-------------------------------------------------------------------
    ##  When required: Update the interface language.
    if (.env$xyz$select_language) {
        HVL_shiny_language(.env, .env2)
    }
    ###-------------------------------------------------------------------
    ##  When required: Create or hide the 'insist-quit' button.
    if (.env$xyz$quit) {
        HVL_shiny_quit(.env, .env2)
    }

    
    print(sprintf("Counter is %s", .env$counter))


}

## ## {

    

    
## ##     .env$output_nodes <- list(
## ##         TS_info = c("TS_key", "TS", "Approx", "Boot_Approx"),
## ##         TCS = "TCS_type",
## ##         CS = c("var_local", "p_diag_bw",  "levels"),
## ##         S = c("spectrum_type", "spectrum_arguments"),
## ##         ass = c("graphs", "graphs_call", "Explain_Plot"))
## ##     ###-------------------------------------------------------------------
## ##     ##  The first time this function is called, it workers will create
## ##     ##  the object 'HVL_logging' (in '.env').  This object is central
## ##     ##  for the dynamic interface to work properly, since residual
## ##     ##  'input'-values can wreak havoc if there is no bookkeeping to
## ##     ##  help things stay on track when different branches are
## ##     ##  investigated.  Note that this initial test also contains an
## ##     ##  escape-test in order to avoid problems if there is nothing
## ##     ##  there to investigate.
## ##     if (!exists(x="HVL_logging", envir = .env, inherits = FALSE)) {
## ##         ##  Check if something is available for inspection.
## ##         if (length(.env$TS_content) == 0) {
## ##             ##  Return a message if there is nothing to see.
## ##             .Hoodwinked_text <- renderText(sprintf(
## ##                 fmt="You have been hoodwinked! %sAn empty list was found in the file '%s'.%s",
## ##                 "<br>",
## ##                 paste(c(.env$main_dir,
## ##                         LG_default$content_file_name),
## ##                       collapse = .Platform$file.sep),
## ##                 "<br>"))
## ##             eval(expr = bquote(output$Hoodwinked <- .(.Hoodwinked_text)),
## ##                  envir = .env2)
## ##             return(NULL)
## ##             #####  REMINDER: Need to tweak this Hoodwinked-text later
## ##             #####  on if it is to look decent when printed.
## ##         } else {
## ##             ##  Create the 'HVL_logging' object, and add the initial
## ##             ##  values to the 'input'-object in order for the logical
## ##             ##  tests to become simpler during the initiation phase.
## ##             LG_shiny_interface_0_create_log(.env, .env2)
## ##             .env$counter <- 1
## ##         }
## ##     } else {
## ##         .env$counter <- .env$counter + 1
## ##         ###-------------------------------------------------------------------
## ##         print(sprintf("Updating worker-status at counter %s", .env$counter))
## ##         ##  Create a list that detects the differences between stored
## ##         ##  and logged input-parameters.  Reminder: The 'xyz'-list is
## ##         ##  used in some of the functions in order to keep track of
## ##         ##  switching between sub-branches.
## ##         .env$xyz <- list()
## ##         for (.name in names(.env$HVL_logging$update$input_triggers)) {
## ##             .triggers <- .env$HVL_logging$update$input_triggers[[.name]]
## ##             .env$xyz[[.name]] <- vapply(
## ##                 X = names(.triggers),
## ##                 FUN = function(x)  {
## ##                     ##  Reminder: The test for the numerical values
## ##                     ##  was selected due getting a quick fix to the
## ##                     ##  issue that the numerical values might be
## ##                     ##  stored as numeric and integer at different
## ##                     ##  places in the code.  The 'any'-part is
## ##                     ##  included since some of the stored numerical
## ##                     ##  values have length larger than 1.
## ##                     if (is.numeric(.triggers[[x]])) {
## ##                         any(.triggers[[x]] != .env$input[[x]])
## ##                     } else
## ##                         ! identical(x = unclass(.triggers[[x]]),
## ##                                     y = unclass(.env$input[[x]]))
## ##                 },
## ##                 FUN.VALUE = logical(1))
## ##             .env$HVL_logging$update$worker[[.name]] <- any(.env$xyz[[.name]])
## ##             ##  Another ad hoc solution, to ensure that the updating
## ##             ##  of the triggers for the "plots"-case is taken care.
## ##             if (.env$HVL_logging$update$worker$plots) {
## ##                 ##  Identify the part to update, and update it.
## ##                 .update_this <- names(.env$xyz$plots)[.env$xyz$plots]
## ##                 .env$HVL_logging$update$input_triggers$plots[[.update_this]] <- .env$input[[.update_this]]
## ##                 kill(.update_this)
## ##             }
## ##         }
## ##         kill(.name, .triggers)
## ##         ##  Reminder: The iterative nature of the shiny-application,
## ##         ##  perhaps triggered by the 'input'-update performed by some
## ##         ##  of the worker-functions, does alas trigger this function
## ##         ##  to be called immediately after an update.  In these cases
## ##         ##  none of the workers should be called, and the following
## ##         ##  termination procedure is thus included.
## ##         if (!any(unlist(.env$HVL_logging$update$worker))) {
## ##             print("**************************************************")
## ##             return(NULL)
## ##         }
## ##         ##  Reminder: If the test above did not terminate this
## ##         ##  function, then the relevant worker-functions will now be
## ##         ##  called.  Each function will update the 'worker'-node
## ##         ##  status according to the 'input'-parameters and the
## ##         ##  available information in 'HVL_logging'.  This implies that
## ##         ##  the workers will hand the task of creating the
## ##         ##  interface/plots onwards as far as possible.
## ##     }
## ##     ###-------------------------------------------------------------------
## ##     ##  Reorder the content of the input-copy in order to make it
## ##     ##  easier to inspect during development/updates.
## ##     .names_from_input_triggers <- 
## ##         unlist(lapply(X = .env$HVL_logging$update$sort_input,
## ##                       FUN = function(x)
## ##                           names(.env$HVL_logging$update$input_triggers[[x]])))
## ##     .other_names <- setdiff(x = names(.env$input),
## ##                             y = .names_from_input_triggers)
## ##     .env$input <- c(.env$input[.names_from_input_triggers],
## ##                     .env$input[.other_names])
## ##     kill(.names_from_input_triggers, .other_names) 
## ##     ###-------------------------------------------------------------------
## ##     ##  When required call the 'TS_info'-worker.  This function
## ##     ##  creates the interface for 'TS_key', 'TS', 'Approx', and
## ##     ##  'Boot_Approx'.  If sufficient information is available, then
## ##     ##  it will also create the 'TCS_type'-output and trigger the
## ##     ##  'TCS_type'-worker to perform further updates of the interface.
## ##     if (.env$HVL_logging$update$worker$TS_info) {
## ##         print(sprintf("TS_info-worker at counter %s", .env$counter))
## ##         LG_shiny_interface_1_TS_info(.env, .env2)
## ##     }
## ##     ###-------------------------------------------------------------------
## ##     ##  When required call the 'TCS_type'-worker.  This function
## ##     ##  creates the buttons and sliders that describes the plot to be
## ##     ##  investigated.
## ##     if (.env$HVL_logging$update$worker$TCS_type) {
## ##         print(sprintf("TCS_input-worker at counter %s", .env$counter))
## ##         LG_shiny_TCS_input(.env, .env2)
## ##     }
## ##     if (.env$HVL_logging$update$worker$var_local) {
## ##         print(sprintf("CS-worker with 'p_diag_bw', at counter %s", .env$counter))
## ##         LG_shiny_CS_input(.env, .env2, .part = "p_diag_bw")
## ##     }
## ##     if (.env$HVL_logging$update$worker$p_diag_bw) {
## ##         print(sprintf("CS-worker with 'levels', at counter %s", .env$counter))
## ##         LG_shiny_CS_input(.env, .env2, .part = "levels")
## ##     }
## ##     ###-------------------------------------------------------------------
## ##     ##  When required call the 'S_update'-worker, which takes care of
## ##     ##  the effects due to the actionButtons that selects the type of
## ##     ##  spectrum to investigate.
## ##     if (.env$HVL_logging$update$worker$spectrum_type) {
## ##         print(sprintf("S_update at counter %s", .env$counter))
## ##         LG_shiny_S_update(.env, .env2)
## ##     }
## ##     ###-------------------------------------------------------------------
## ##     ##  When required call the 'plots'-worker.  This function creates
## ##     ##  the plot (or the code needed to re-create the )
## ##     if (.env$HVL_logging$update$worker$plots) {
## ##         print(sprintf("plot-worker at counter %s", .env$counter))
## ##         LG_shiny_interface_plots(.env, .env2)
## ##     }
## ##     ###-------------------------------------------------------------------
## ##     ##  When required call the 'explanation'-worker.
## ##     if (.env$input$explain_interface) {
## ##         print(sprintf("explain-interface-worker at counter %s", .env$counter))
## ##         LG_shiny_interface_explanations(.env, .env2, .explain="interface")
## ##     }
## ##     if (.env$input$explain_plot) {
## ##         print(sprintf("explain-plot-worker at counter %s", .env$counter))
## ##         LG_shiny_interface_explanations(.env, .env2, .explain="plot")
## ##     }
## ##     if (isTRUE(.env$input$show_shiny)) 
## ##         LG_shiny_interface_explanations(.env, .env2, .explain="show_shiny")
## ## }
