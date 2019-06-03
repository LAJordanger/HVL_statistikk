#' Shiny app for HVLstatistikk
#'
#' An interactive interface to some simple statistical concepts.
#'
#' @param initial_values A list that can be used to specify the
#'     default values when the application starts.
#'
#' @return An interactive shiny application will be loaded.
#'
#' @export


HVL_shiny <- function(initial_values = list(selected_tab = "info",
                                            par_list = NULL)) {
    ##  Create the 'HVL_logging' object based on the content of
    ##  'initial_values'.  This will be used to keep track of
    ##  different branches of the investigation.
    HVL_shiny_create_log(.env = pryr::where("initial_values"))

    ################################################################################
    ##  Some stuff imported from test_shiny, to be adjusted later on.

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

    ################################################################################
    

    
    ###-------------------------------------------------------------------
    ##  Define the 'ui'-argument to be delivered to 'shinyApp'.
    ui <- shinyUI(
        navbarPage(
            shinyjs::useShinyjs(), 
            title = uiOutput("tst"),
            id = "navbarPage_id",
            collapsible = TRUE,
            windowTitle = "Updated later on by 'HVL_shiny_language'",
            selected = HVL_logging$tabPanel$selected_tab[1],
            tabPanel(title = uiOutput("info_title"),
                     value = "info",
                     uiOutput("select_language_intro"),
                     uiOutput("select_language")),
            tabPanel(title = uiOutput("parametric_title"),
                     value = "parametric",
                     fluidPage(
                         ##     "This is your browser language",
                         textOutput("browser_language"),
                         
                         ## Application title
                         titlePanel("TEST"),
                         ## Define the side-panel
                         fluidRow(
                             column(
                                 width = 3,
                                 conditionalPanel(
                                     condition = "(input.switch1 + input.switch2) % 2 == 0",
                                     wellPanel(
                                         ##  Switch to hide path-controls.
                                         actionButton("switch1",
                                                      label = "Hide path-controls"),
                                         br()
                                     )
                                 ),
                                 conditionalPanel(
                                     condition = "(input.switch2 + input.switch1) % 2 == 1",
                                     wellPanel(
                                         ##  Switch to show path-controls.
                                         actionButton("switch2",
                                                      label = "Show path-controls"))
                                 )
                                 ##  A panel for the control of the graphical details,
                                 ##  i.e. the actionButtons, radioButtons and
                                 ##  sliderInputs needed for the fine tuning of the
                                 ##  graphical controls.
                                 ## wellPanel(
                                 ##     "Select the data to inspect...",
                                 ##     ##  Add actionButtons for 'TS_graphic',
                                 ##     ##  'Approx_graphic' and 'Spectra_graphic.
                                 ##     uiOutput("TCS_type"),
                                 ##     ##  Add actionButtons to select the type of spectrum
                                 ##     ##  to investigate.
                                 ##     uiOutput("spectrum_type"),
                                 ##     ##  Add radioButtons for selection of variables 'Vi'
                                 ##     ##  and 'Vj' (for multivariate time series) and
                                 ##     ##  "local versus global" data.
                                 ##     uiOutput("var_local"),
                                 ##     ##  Add radioButtons for number of parameters 'p' in
                                 ##     ##  the Local Gaussian approximations, point type
                                 ##     ##  (on or off diagonal) and selection of
                                 ##     ##  bandwidths.
                                 ##     uiOutput("p_diag_bw"),
                                 ##     ##  Add the 'sliderInput'-interface to be used when
                                 ##     ##  selecting the levels/points to be investigated,
                                 ##     uiOutput("levels"),
                                 ##     ##  Add interface for the selection of the arguments
                                 ##     ##  needed in order to compute the different
                                 ##     ##  spectral densities.
                                 ##     uiOutput("spectrum_arguments"),
                                 ##     uiOutput("second_graphical")
                                 ## ),
                             )## ,
                             ## column(
                             ##     width = 9,
                             ##     conditionalPanel(
                             ##         condition = "input.get_code == false",
                             ##         plotOutput("graphs")
                             ##     ),
                             ##     conditionalPanel(
                             ##         condition = "input.get_code == true",
                             ##         "The following code can be used to create the plot in e.g. an article/paper:",
                             ##         verbatimTextOutput("graphs_call")
                             ##     ),
                             ##     conditionalPanel(
                             ##         condition = "input.explain_interface == true",
                             ##         htmlOutput("Explain_Interface")
                             ##     ),
                             ##     conditionalPanel(
                             ##         condition = "input.explain_plot == true",
                             ##         htmlOutput("Explain_Plot")
                             ##     ),
                             ##     conditionalPanel(
                             ##         condition = "input.show_shiny == true",
                             ##         verbatimTextOutput("internal_status")
                             ##     )
                             ## )
                         ))
                     ),
            tabPanel(title = uiOutput("lin.reg_title"),

################################################################################
                     ## Application title
                     titlePanel("Under development - HVL statistkk"),

        ##     ## Define the side-panel
        fluidRow(
            column(
                width = 3,
                ##  A panel for the control of the graphical details.
                uiOutput("wellpanel_test"),
                ## ## wellPanel(
                ## ##     uiOutput("distribution"),
                ## ##     .interface$scaling,
                ## ##     .interface$plot_type
                ## ## ),
                conditionalPanel(
                    condition = "input.distribution == 'binomial'",
                    wellPanel(
                        ##  Add the required input interface for the parameters
                        parameters(.model = "binomial", .par = "n"),
                        parameters(.model = "binomial", .par = "p"),
                        .interface$normal_approximation,
                        .interface$mean_pm_sd
                    )
                ),
                conditionalPanel(
                    condition = "input.distribution == 'chi.squared'",
                    "The chi-squared case",
                    wellPanel(
                        ##  Switch to show path-controls.
                        actionButton("switch2",
                                     label = "Show path-controls"))
                    ## wellPanel(
                    ##     renderText("The chi-squared case"),
                    ##     renderPrint("The chi-squared case"))
                ),
                conditionalPanel(
                    condition = "input.distribution == 'normal'",
                    wellPanel(
                        ##  Add the required input interface for the parameters
                        parameters(.model = "normal", .par = "mean"),
                        parameters(.model = "normal", .par = "sd"),
                        .interface$show_intervals_areals,
                        .interface$side_type,
                        .interface$alpha_quantile
                    )
                ),
                conditionalPanel(
                    condition = "input.distribution == 't'",
                    "The t case"
                ),
                ###-------------------------------------------------------------------
                ##  The interface that describes what to plot.
                wellPanel(
                    .interface$plot_type,
                    .interface$normal_approximation,
                    .interface$mean_pm_sd
                ),
                ###-------------------------------------------------------------------
                ##  Additional buttons.
                wellPanel(
                    ##  Explain details about the interface.
                    checkboxInput(inputId = "explain_interface",
                                  label = "Explanation interface",
                                  value = FALSE),
                    ##  Explain details about the plots.
                    checkboxInput(inputId = "explain_plot",
                                  label = "Explanation plot",
                                  value = TRUE),
                    ##  Get the code for the plots.
                    checkboxInput(inputId = "get_code",
                                  label = " Show code for plot",
                                  value = FALSE))
            ),
            column(
                width = 9,
                conditionalPanel(
                    condition = "input.get_code == false",
                    ##  The next part will depend on the selected data.
                    plotOutput("graphs")
                ),
                conditionalPanel(
                    condition = "input.get_code == true",
                    "The following code can be used to create the plot in e.g. an article/paper:",
                    verbatimTextOutput("graphs_call")
                ),
                conditionalPanel(
                    condition = "input.explain_interface == true",
                    ##  Present information about the interface
                    htmlOutput("Explain_Interface")
                    ## verbatimTextOutput("Explain_Interface")
                ),
                conditionalPanel(
                    condition = "input.explain_plot == true",
                    ##  Present information about the plot.
                    htmlOutput("Explain_Plot")
                    ## verbatimTextOutput("Explain_Interface")
                )
            )
        ),
        ################################################################################
                     
                     value = "lin.reg"),
            footer = wellPanel(
                ##  Interface needed to manually quit the application.
                uiOutput("quit"),
                uiOutput("insist_quit"))))
    ## This concludes the definition of 'ui'.
    ###-------------------------------------------------------------------
    ##  Define the 'server'-argument to be delivered to 'shinyApp'.
    server <- shinyServer(function(input, output, session) {
        ##  Code to ensure that the app is terminated if the
        ##  web-page is closed without using the stop-button.
        session$onSessionEnded(stopApp)
        ##  Perform manual termination when required
        observe({
            if(isTRUE(input$insist_quit > 0)) 
                stopApp()
        })


        ################################################################################

         
          ##  Create a solution for the selection of the distribution,
          ##  based on the values found in the list
          ##  'distribution_details' (internal to the package).

        distribution =
                           list(type = "normal",
                              mean = -2:2,
                              sd = c(.1, .25, .5, 1, 2, 4, 8))
        
          output$distribution <- renderUI(selectInput(
              inputId = "distribution",
              label = "Select the desired distribution",
              choices = sort(names(distribution_details)),
              selected = distribution$type))          
          ## ## ## selectInput(inputId, label, choices, selected = NULL, multiple = FALSE,
          ## ## ##             selectize = TRUE, width = NULL, size = NULL)          
          

        

          
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The aim of this function is to make this interactive application
###  able to deal with all the cases of interest, which implies that
###  the user interface must be completely dynamic.  In order to make
###  the code a tad bit more transparent, the naming conventions will
###  be to mirror 'input' and 'output' to a high degree.  Moreover, to
###  avoid a lot of "select the only available option"-steps in the
###  interface, the aim is to auto-select when no alternatives are
###  available.
#############---------------------------------------------------------
###-------------------------------------------------------------------            


          output$graphs <- renderPlot({
              ##  NB: This part must be updated when changes are
              ##  performed in the selection menus.
              {   input$distribution
                  input$scaling
                  ##  Details for the binomial distribution.
                  input$n
                  input$p
                  input$plot_type
                  input$normal_approximation
                  input$mean_pm_sd
                  ##  Details for the normal distribution.
                  input$mean
                  input$sd
                  input$show_intervals_areals
                  input$alpha_quantile
                  input$side_type
              }

              ##  Create a noninteractive copy of 'input' for the
              ##  debugging strategy to work properly.
              .input <- vector(mode = "list", length = length(input))
              for (.name in isolate(names(input))) {
                  .input[[.name]] <- isolate(input[[.name]])
              }

              ##  Create a call to be used for the 
              the_plot_call <- leanRcoding::create_call(
                  .cc_fun = the_plot,
                  .input = .input)

              ## capture_env() 
              
              
              ##  Create the plot.
              the_plot(.input = input)
          })


        ################################################################################

        
        ##  Let the interface-master take care of all the other
        ##  details related to the updating of the interface.
        observe({
            input$browser_language
            .Env <- pryr::where("initial_values")
            .Env2 <- pryr::where("output")
            HVL_server_Master(
                .env = .Env,
                .env2 = .Env2)
        })
    })
    ## This concludes the definition of 'server'.
    ###-------------------------------------------------------------------
    ##  Run 'shinyApp'
    shinyApp(ui = ui,
             server = server)
}
