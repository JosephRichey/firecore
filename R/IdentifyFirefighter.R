#' Identify Firefighter Modal (UI)
#'
#' Displays a modal dialog for a firefighter to "sign in" by selecting their name
#' from a dropdown and entering their unique PIN. Intended to be used within a
#' Shiny module system; the input IDs are namespaced via `ns`.
#'
#' @param ns The namespace function from a Shiny module session (`session$ns`)
#' @return Displays a modal dialog with firefighter selection and PIN input.
#' @examples
#' \dontrun{
#' # inside a module server
#' IdentifyFirefighterModal(session$ns)
#' }
#' @export
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
#' Handles the PIN validation logic for a firefighter logging in. When the
#' submit button is pressed, this module:
#'   1. Checks the entered PIN against the stored firefighter PIN.
#'   2. If correct, sets the reactive `currentUser` to the firefighter's info
#'      and shows a success notification.
#'   3. If incorrect, shows a warning alert.
#'
#' @param id The namespace ID for the Shiny module
#' @param currentUser A reactiveVal or reactiveValues object to store the
#'   current user. Should be passed from parent module/app.
#' @return None; updates reactive `currentUser` and displays notifications
#' @examples
#' \dontrun{
#' # inside a module server
#' currentUser <- reactiveVal(NULL)
#' IdentifyFirefighterServer(input, output, session, currentUser)
#' }
#' @export
IdentifyFirefighterServer <- function(id, currentUser) {
  moduleServer(id, function(input, output, session) {
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

        currentUser(firefighter_name)

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
