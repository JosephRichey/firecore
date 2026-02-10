#' Identify Firefighter Modal (UI)
#'
#' Displays a modal dialog for a firefighter to "sign in" by selecting their name
#' from a dropdown and entering their unique PIN. This function is intended to be
#' called from within the IdentifyFirefighterServer module to ensure proper
#' namespace consistency.
#'
#' @param ns The namespace function from a Shiny module session (`session$ns`)
#' @return Displays a modal dialog with firefighter selection and PIN input.
#' @note This function is typically not called directly by users. It is called
#'   internally by IdentifyFirefighterServer when show_on_load = TRUE.
#' @examples
#' \dontrun{
#' # Called from inside a module server
#' IdentifyFirefighterModal(session$ns)
#' }
IdentifyFirefighterModal <- function(ns) {
  .CheckPackageEnv() # Ensure package environment is loaded
  app_data <- .pkg_env$app_data

  shiny::showModal(
    shiny::modalDialog(
      # Firefighter dropdown
      shiny::selectInput(
        ns('identify_firefighter'),
        label = '',
        choices = BuildNamedVector(
          df = app_data$Firefighter,
          name = full_name,
          value = id,
          filterExpr = is_active == TRUE # Only show active firefighters
        ),
        selected = NULL,
        multiple = FALSE,
        selectize = FALSE
      ),
      # PIN input field
      shiny::passwordInput(
        ns('input_pin'),
        label = "",
        placeholder = 'Pin'
      ),
      title = "Sign In",
      # Submit button
      footer = shiny::tagList(
        shiny::actionButton(ns("submit_id_pin"), "Submit")
      )
    )
  )
}


#' Identify Firefighter Server (Server Logic)
#'
#' A Shiny module that handles firefighter authentication via PIN validation.
#' This module displays a modal dialog for firefighters to sign in and validates
#' their credentials. The modal can be shown automatically on module load or
#' triggered manually.
#'
#' When the submit button is pressed, this module:
#'   1. Checks the entered PIN against the stored firefighter PIN in the database.
#'   2. If correct, sets the reactive `current_user` to the firefighter's name
#'      and shows a success notification.
#'   3. If incorrect, shows a warning alert allowing retry.
#'
#' @param id The namespace ID for the Shiny module (e.g., "identify_firefighter")
#' @param current_user A reactiveVal containing the current user's name. Should
#'   be created in the parent scope and passed to this module. The module will
#'   update this value upon successful authentication.
#' @param show_on_load Logical. If TRUE (default), the identification modal will
#'   be displayed automatically when the module loads and current_user is NULL.
#'   Set to FALSE if you want to trigger the modal manually.
#'
#' @return None; side effects include updating the reactive `current_user` value
#'   and displaying notifications/modals.
#'
#' @examples
#' \dontrun{
#' # In your main server function
#' server <- function(input, output, session) {
#'   # Create reactive to store current user
#'   current_user <- reactiveVal(NULL)
#'
#'   # Initialize the identification module
#'   # Modal shows automatically on app load
#'   IdentifyFirefighterServer("identify_firefighter", current_user)
#'
#'   # Use current_user elsewhere in your app
#'   observe({
#'     req(current_user())
#'     print(paste("Current user:", current_user()))
#'   })
#' }
#'
#' # To trigger modal manually (show_on_load = FALSE)
#' IdentifyFirefighterServer("identify_firefighter", current_user, show_on_load = FALSE)
#' # Then trigger with: IdentifyFirefighterModal(session$ns) when needed
#' }
#'
#' @export
IdentifyFirefighterServer <- function(id, current_user, show_on_load = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    # Show modal on module load if requested
    if (show_on_load) {
      shiny::observe({
        shiny::req(is.null(current_user()))
        IdentifyFirefighterModal(session$ns)
      })
    }

    # Handle submission
    shiny::observe({
      .CheckPackageEnv()
      app_data <- .pkg_env$app_data

      true_pin <- app_data$Firefighter |>
        dplyr::left_join(
          app_data$Firefighter_Pin,
          by = c('id' = 'firefighter_id')
        ) |>
        dplyr::filter(id == input$identify_firefighter) |>
        dplyr::pull(firefighter_pin)

      if (length(true_pin) > 0 && true_pin == input$input_pin) {
        firefighter_name <- IdToString(
          app_data$Firefighter,
          full_name,
          input$identify_firefighter
        )

        current_user(firefighter_name)

        shiny::showNotification(
          paste(firefighter_name, "is signed in"),
          duration = 5
        )

        shiny::removeModal()
      } else {
        shinyalert::shinyalert(
          title = "Incorrect Pin",
          type = 'warning',
          text = "The pin is incorrect. Please try again."
        )
      }
    }) |>
      shiny::bindEvent(input$submit_id_pin)
  })
}
