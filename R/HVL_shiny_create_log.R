#' Create \code{HVL_logging}-object for the dynamic
#' \code{HVL_shiny}-interface.
#'
#' This function initiates the \code{HVL_logging}-object needed in
#' order for the dynamic structure of the \code{HVL_shiny}-application
#' to work without any glitches.  Moreover, it does also adds a bunch
#' of values to the reactive \code{input}-object in the
#' \code{shiny}-application started by \code{HVL_shiny}.
#'
#' @param .env The environment where the original arguments given to
#'     \code{HVL_shiny} lives.  The logging-object will be added to
#'     this environment.
#' 
#' @return This function adds the \code{HVL_logging}-object into the
#'     environment \code{.env}.  This object will log the input-values in
#'     order to avoid problems when switching between different
#'     branches, i.e. it prevents residual input-values to trigger
#'     subsetting problems.  Moreover, the last selected input-values
#'     for an old branch will be stored when there is a switch to a
#'     new branch, and these will then be loaded the next time the old
#'     branch is visited.  The function does also add the initial
#'     \code{NA}-values to the reactive \code{input}-object in the
#'     \code{shiny}-application started by \code{HVL_shiny}, and this
#'     is done in order for some of the logical tests to become
#'     simpler during the initiation phase.
#'
#' @keywords internal

HVL_shiny_create_log <- function(.env) {
    ##  Create 'HVL_logging' based on the values in 'HVL_defaults' and
    ##  those given in '.env$initial_values'.
    HVL_logging <- list()
    ##  Register the 'tabPanel'-details.
    HVL_logging$tabPanel$selected_tab <- c(
        .env$initial_values$selected_tab,
        HVL_defaults$tabPanel$selected_tab)
    ##  Create a list of the input-triggers to keep track of.  The
    ##  idea is that two values should be stored.  The values from the
    ##  'input'-list will be checked against the first of the stored
    ##  values, and if they differ a helper function will be called.
    HVL_logging$input_triggers <- c(
        "select_language",
        "quit")
    ##  Register the default language
    HVL_logging$select_language <- c(
        HVL_defaults$select_language$selected,
        HVL_defaults$select_language$selected)
    ##  Register default for 'quit'-button
    HVL_logging$quit <- c(FALSE, FALSE)

    ##  Insert the log-object into '.env'
    .env$HVL_logging <- HVL_logging
}

##     with(data = .env,
##          expr = {


##              ##  DEVELOPMENT.  NEED TO USE A COMBINATION OF STORED
##              ##  DEFAULT VALUES AND ARGUMENTS SPECIFIED WHEN THE
##              ##  FUNCTION IS CALLED.
             
##              ##  The core skeleton based on 'TS_content'.
##              HVL_logging <- structure(
##                  .Data = lapply(
##                      X = seq_along(TS_content),
##                      FUN = function(i) {
##                          structure(
##                              .Data = lapply(
##                                  X = names(TS_content[[i]]),
##                                  FUN = function(x)
##                                      list(last = NA_character_,
##                                           names = NA_character_,
##                                           header = character(0),
##                                           label = NA_character_,
##                                           visited = FALSE)),
##                              .Names = names(TS_content[[i]]))
##                      }),
##                  .Names = names(TS_content))
## ###-------------------------------------------------------------------
##              ##  Add the nodes to the first level too, in order to
##              ##  simplify the code later on.
##              HVL_logging$names <- sort(names(HVL_logging))
##              HVL_logging$label <- ifelse(
##                  test = length(HVL_logging$names) == 1,
##                  yes = "Auto-selected the only available group",
##                  no = sprintf("%s groups available",
##                               length(HVL_logging$names)))
##              if (length(HVL_logging$names) > 1) {
##                  HVL_logging$header <- "Select a group"
##              } else {
##                  HVL_logging$header <- character(0)
##              }
                 
##             if (length(HVL_logging$names) == 1) {
##                 HVL_logging$last <- HVL_logging$names
##             } else {
##                 HVL_logging$last <- HVL_logging$header
##             }
##              ##  Override the previous values when no data are available.
##              if (length(HVL_logging$names) == 0) {
##                  HVL_logging$last <- "Nothing here to select"
##                  HVL_logging$label <- "No groups detected"
##              }
##              ##  Update the information on the second level too.
##              for (.name in HVL_logging$names) {
##                  HVL_logging[[.name]]$names <- sort(names(HVL_logging[[.name]]))
##                  HVL_logging[[.name]]$label <- ifelse(
##                      test = length(HVL_logging[[.name]]$names) == 1,
##                      yes = "Auto-selected the only available time series",
##                      no = sprintf("%s time series available",
##                                   length(HVL_logging[[.name]]$names)))
##                  if (length(HVL_logging[[.name]]$names) > 1) {
##                      HVL_logging[[.name]]$header <- "Select a time series"
##                  }
##                  HVL_logging[[.name]]$last <- ifelse(
##                      test = {length(HVL_logging[[.name]]$names) == 1},
##                      yes  = HVL_logging[[.name]]$names,
##                      no   = HVL_logging[[.name]]$header)
##                  ##  Override the previous values when no data are available.
##                  if (length(HVL_logging[[.name]]$names) == 0) {
##                      HVL_logging[[.name]]$last <- "Nothing here to select"
##                      HVL_logging[[.name]]$label <- "No time series detected"
##                  }
##              }
##              kill(.name)
## ###-------------------------------------------------------------------
##              ##  Add a top level 'update'-node, with the information
##              ##  required in order for the master function to know
##              ##  when different workers are required.  Reminder: The
##              ##  basic idea in this setup is that 'update' will be a
##              ##  list with three nodes, all having names in accordance
##              ##  with the interface-workers.  Reminder: The
##              ##  'input_triggers'-part of 'update' is a list that will
##              ##  collect all the last set values from the time the
##              ##  interface was last created, and these are structured
##              ##  in such a manner that it is easy to decide where/if a
##              ##  new value have been given.  The second component is a
##              ##  logical list that will be used to tell the master a
##              ##  'TRUE'/'FALSE' with regard to calling the different
##              ##  workers.  The third component is a logical list that
##              ##  simply states whether or not the desired update is to
##              ##  hide something, and this is included to avoid
##              ##  duplicating tests in successive functions.  Depending
##              ##  on the available data, the different workers will
##              ##  decide the status of the next worker(s) in line.
##              .zero <- LG_default$spectrum_type_ID
##              HVL_logging$update <- list(
##                  ##  Reminder: The 'input_triggers' is converted to an
##                  ##  environment.  It is thus necessary with some
##                  ##  additional information in order to sort the input
##                  ##  list in a way that makes it easier to work with
##                  ##  during development and updates.  The first node
##                  ##  is included in order to deal with this.
##                  sort_input = c("TS_info", "var_local", "p_diag_bw",
##                                 "derived_graphical", "plots",
##                                 "TCS_type", "spectrum_type",
##                                 "second_graphical", "explanations"),
##                  input_triggers = as.environment(list(
##                      TS_info = list(TS_key = NA_character_,
##                                     TS = NA_character_,
##                                     Approx = NA_character_,
##                                     Boot_Approx = NA_character_),
##                      TCS_type = list(TS_graphic = .zero,
##                                      Approx_graphic = .zero,
##                                      Spectra_graphic = .zero),
##                      var_local = list(Vi = NA_character_,
##                                       Vj = NA_character_,
##                                       global_local = NA_character_),
##                      p_diag_bw = list(type = NA_character_,
##                                       point_type = NA_character_,
##                                       bw_points = NA_character_),
##                      ##  Selectors for spectrum inspection.
##                      spectrum_type = structure(
##                          .Data = lapply(
##                              X = LG_default$spectrum_type_ID,
##                              FUN = function(x) LG_default$spectrum_type_zero),
##                          .Names = LG_default$spectrum_type_ID),
##                      ##  Selectors for time series inspection
##                      second_graphical = list(TS_plot = .zero,
##                                              TS_acf = .zero,
##                                              TS_pacf = .zero,
##                                              TS_spec.pgram = .zero,
##                                              TS_lags = .zero),
##                      plots = list(get_code = FALSE,
##                                   window = NA_character_,
##                                   cut = NA_character_,
##                                   confidence_interval = NA_character_,
##                                   frequency_range = NA_character_,
##                                   levels_Diagonal = NA_character_,
##                                   levels_Line = NA_character_,
##                                   levels_Horizontal = NA_character_,
##                                   levels_Vertical = NA_character_),
##                      explanations = list(explain_interface = FALSE,
##                                          explain_plot = TRUE,
##                                          show_shiny = TRUE),
##                      ## Reminder: This last part is not a part of the
##                      ## interface as such. This is used in order to
##                      ## simplify some parts of the code that deals
##                      ## with values derived from the 'TCS_type'
##                      ## and 'second_graphical' parts of this list.
##                      derived_graphical = list(TCS_type = NA_character_,
##                                               sub_graph_type = NA_character_,
##                                               S_type = NA_character_))),
##                  worker = list(TS_info = FALSE,
##                                TCS_type = FALSE,
##                                var_local = FALSE,
##                                p_diag_bw = FALSE,
##                                second_graphical = FALSE,
##                                spectrum_type = FALSE,
##                                plots = FALSE,
##                                explanations = FALSE))
##              ##  Add details to simplify some of the code later on,
##              ##  i.e. only look up stuff once.
##              HVL_logging$update$select_names  <-  list(
##                  TS_key = HVL_logging$names,
##                  TS = NA_character_,
##                  Approx = NA_character_,
##                  Boot_Approx = NA_character_)
##              #####  ## The stuff above will hopefully soon be
##              #####  ## removed...
##              HVL_logging$update$select_Input  <-  structure(
##                  .Data = lapply(
##                      X = names(HVL_logging$update$input_triggers$TS_info),
##                      FUN = function(x)
##                          list(label = NA_character_,
##                               choices = NA_character_,
##                               selected = NA_character_)),
##                  .Names = names(HVL_logging$update$input_triggers$TS_info))
##              HVL_logging$update$is_bootstrap = FALSE
## ###-------------------------------------------------------------------
##              ##  Figure out if any pre-selected values are present
##              ##  in 'data_dir'
##              pre_selected <- structure(
##                  .Data = as.list(data_dir),
##                  .Names = LG_default$folder_defaults[names(data_dir)])
##              ##  Add 'TS_key' (when possible).
##              if (! is.null(data_dir)) {
##                  pre_selected$TS_key <- local(expr = {
##                      .match <- vapply(
##                          X = HVL_logging,
##                          FUN = function(x) 
##                              pre_selected$TS %in% names(x),
##                          FUN.VALUE = logical(1))
##                      names(.match)[.match]
##                  })
##              }
##              ##  If not present, add 'Boot_Approx'-node too.
##              if (is.null(pre_selected$Boot_Approx))
##                  pre_selected$Boot_Approx <- NA_character_
##              ##  If possible, update the 'TS_key' and 'TS' part of
##              ##  HVL_logging' based on 'pre_selected', and update the
##              ##  'input_triggers' too.  'pre_selected' can of course
##              ##  contain more information that this, but those updates
##              ##  can not be performed before the info-object has been
##              ##  loaded.
##              if (! is.null(pre_selected$TS_key)) {
##                  HVL_logging$last <- pre_selected$TS_key
##                  ## ## ## HVL_logging$update$input_triggers$TS_info["TS_key"] <-
##                  HVL_logging$update$input_triggers$TS_info$TS_key <-
##                      HVL_logging$last
##                  ##  Update the values to be used in 'select_Input'
##                  HVL_logging$update$select_Input$TS_key$label <- HVL_logging$label
##                  HVL_logging$update$select_Input$TS_key$selected <- HVL_logging$last
##                  HVL_logging$update$select_Input$TS_key$choices <- c(HVL_logging$header,
##                                                                     HVL_logging$names)
##              }
##              if (!is.null(pre_selected$TS)) {
##                  HVL_logging[[pre_selected$TS_key]]$last <- pre_selected$TS
##                  ## ## ## HVL_logging$update$input_triggers$TS_info["TS"] <-
##                  HVL_logging$update$input_triggers$TS_info$TS <-
##                      HVL_logging[[pre_selected$TS_key]]$last
##                  HVL_logging$update$select_names[["TS"]] <-
##                      HVL_logging[[pre_selected$TS_key]]$names
##                  ## ## I think the select_names/select_Input stuff
##                  ## ## mentioned here in fact should be superfluous,
##                  ## ## given the updates encountered later on.
##                  HVL_logging$update$select_Input$TS$label <-
##                      HVL_logging[[pre_selected$TS_key]]$label
##                  HVL_logging$update$select_Input$TS$selected <-
##                      HVL_logging[[pre_selected$TS_key]]$last
##                  HVL_logging$update$select_Input$TS$choices <-
##                      c(HVL_logging[[pre_selected$TS_key]]$header,
##                        HVL_logging[[pre_selected$TS_key]]$names)
##              }
##              ##  Create a logical value '.first_time' to be used later
##              ##  on in the code in order for pre-selected values to be
##              ##  added the first time an info object is loaded.
##              .first_time <- TRUE
##          })
## ###-------------------------------------------------------------------
##     ## Add the initial values to the 'input'-object in order for the
##     ## logical tests to become simpler during the initiation phase.
##     HVL_shiny_set_values(.env, .env2)
## ###-------------------------------------------------------------------    
##     ##  Instruct the first worker-function that it is time to act.
##     .env$HVL_logging$update$worker$TS_info <- TRUE
## }
