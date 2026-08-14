#' Identify Firefighter Modal (UI)
#'
#' Displays a modal dialog for a firefighter to "sign in" by selecting their name
#' from a dropdown and entering their unique PIN. This function is intended to be
#' called from within the IdentifyFirefighterServer module to ensure proper
#' namespace consistency.
#'
#' @param ns The namespace function from a Shiny module session (`session$ns`)
#' @param officer_filter Logical. If TRUE, only officers will be shown in the
#'   firefighter dropdown. If FALSE (default), all active firefighters are shown.
#' @param use_pin Logical. If TRUE (default), display and validate a PIN. If
#'   FALSE, use the dropdown only as a firefighter-selection confirmation.
#' @return Displays a modal dialog with firefighter selection and PIN input.
#' @note This function is typically not called directly by users. It is called
#'   internally by IdentifyFirefighterServer when show_on_load = TRUE.
#' @examples
#' \dontrun{
#' # Called from inside a module server
#' IdentifyFirefighterModal(session$ns, officer_filter = FALSE)
#' }
IdentifyFirefighterModal <- function(ns, officer_filter, use_pin = TRUE) {
  .CheckPackageEnv() # Ensure package environment is loaded
  app_data <- .pkg_env$app_data

  # Should you filter to only officers?
  if (officer_filter) {
    Firefighter <- app_data$Firefighter |>
      dplyr::filter(officer == 1)
  } else {
    Firefighter <- app_data$Firefighter
  }

  choices <- BuildNamedVector(
    df = Firefighter,
    name = full_name,
    value = id,
    filterExpr = is_active == TRUE
  )

  body <- if (use_pin) {
    shiny::tagList(
      shiny::selectInput(ns('identify_firefighter'), '', choices = choices, selected = NULL, selectize = FALSE),
      shiny::passwordInput(ns('input_pin'), label = "", placeholder = 'Pin')
    )
  } else {
    shiny::selectInput(ns('identify_firefighter'), '', choices = c("Select Firefighter" = "", choices), selected = "", selectize = FALSE)
  }

  shiny::showModal(
    shiny::modalDialog(
      body,
      title = if (use_pin) "Sign In" else "Firefighter Performing Check",
      # Submit button
      footer = shiny::tagList(
        shiny::actionButton(
          ns("submit_id_pin"),
          "Submit",
          class = 'btn btn-primary'
        )
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
#' @param officer_filter Logical. If TRUE, only officers will be shown in the
#'   firefighter dropdown. If FALSE (default), all active firefighters are shown.
#' @param use_pin Logical. If TRUE (default), display and validate a PIN. If
#'   FALSE, use the dropdown only as a firefighter-selection confirmation.
#'   This is useful for restricting certain actions to department leadership.
#' @param use_pin Logical. If TRUE (default), require a PIN. If FALSE, the
#'   modal is a firefighter selection confirmation.
#' @param cookie_config A configuration created by [NewAuthCookieConfig()]. When
#'   supplied, the browser cookie is checked before the sign-in modal is shown.
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
#'   # Restrict to officers only
#'   IdentifyFirefighterServer("officer_action", current_user, officer_filter = TRUE)
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
#' # Then trigger with: IdentifyFirefighterModal(session$ns, officer_filter = FALSE) when needed
#' }
#'
#' @export
IdentifyFirefighterServer <- function(
  id,
  current_user,
  show_on_load = TRUE,
  officer_filter = FALSE,
  use_pin = TRUE,
  cookie_config = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    cookie_checked <- shiny::reactiveVal(is.null(cookie_config))

    if (!is.null(cookie_config)) {
      session$onFlushed(
        function() {
          session$sendCustomMessage(
            "firecore-read-auth-cookie",
            list(
              cookie_name = cookie_config$cookie_name,
              input_id = session$ns("auth_cookie")
            )
          )
        },
        once = TRUE
      )

      shiny::observeEvent(input$auth_cookie, {
        user_id <- ValidateAuthCookie(input$auth_cookie, cookie_config)

        if (!is.null(user_id)) {
          .CheckPackageEnv()
          firefighter <- .pkg_env$app_data$Firefighter
          matching_firefighter <- firefighter[
            as.character(firefighter$id) == user_id &
              firefighter$is_active == TRUE,
            ,
            drop = FALSE
          ]

          if (nrow(matching_firefighter) == 1) {
            current_user(matching_firefighter$full_name[[1]])
            WriteFirefighterCookie(session, user_id, cookie_config)
            NotifySignedInFirefighter(current_user())
          }
        }

        cookie_checked(TRUE)
      }, once = TRUE)
    }

    # Show modal on module load if requested
    if (show_on_load) {
      shiny::observe({
        shiny::req(cookie_checked())
        shiny::req(is.null(current_user()))
        IdentifyFirefighterModal(session$ns, officer_filter, use_pin)
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

      firefighter_name <- IdToString(
        app_data$Firefighter,
        full_name,
        input$identify_firefighter
      )

      valid_login <- !is.null(input$identify_firefighter) && nzchar(input$identify_firefighter) && (!use_pin || (length(true_pin) > 0 && true_pin == input$input_pin))

      if (valid_login) {
        current_user(firefighter_name)
        if (!is.null(cookie_config)) {
          WriteFirefighterCookie(session, input$identify_firefighter, cookie_config)
        }
        logger::log_success(
          glue::glue("{firefighter_name} logged in"),
          namespace = "IdentifyFirefighterServer"
        )

        shiny::showNotification(
          paste(firefighter_name, "is signed in"),
          duration = 5
        )

        shiny::removeModal()
      } else if (use_pin) {
        logger::log_warn(
          glue::glue(
            "Invalid sign in attempt. Name: {firefighter_name}, Pin: {input$input_pin}"
          ),
          namespace = "IdentifyFirefighterServer"
        )
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

#' Signed-in firefighter status UI
#' @param id Module ID shared with [SignedInFirefighterServer()].
#' @export
SignedInFirefighterUI <- function(id) shiny::uiOutput(shiny::NS(id)("firefighter_checking"))

#' Server for a signed-in firefighter status element
#' @param id Module ID shared with [SignedInFirefighterUI()].
#' @param current_user A `reactiveVal` containing the signed-in firefighter's
#'   display name.
#' @export
SignedInFirefighterServer <- function(id, current_user) {
  shiny::moduleServer(id, function(input, output, session) {
    output$firefighter_checking <- shiny::renderUI({
      firefighter <- current_user()
      if (is.null(firefighter) || !nzchar(firefighter)) return(NULL)
      shiny::tags$span(style = "font-size: 1.2em; font-weight: bold; color: #2b8764;", firefighter, " is currently performing checks")
    })
  })
}

#' Show a five-second signed-in notification
#' @param firefighter Display name of the signed-in firefighter.
#' @export
NotifySignedInFirefighter <- function(firefighter) {
  shiny::showNotification(paste(firefighter, "is signed in"), duration = 5)
}
