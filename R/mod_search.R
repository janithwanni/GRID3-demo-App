
searchUI <- function(id, state_vec, hcf_category_vec, hcf_func_status) {
  ns <- NS(id)
  tagList(
    fluidRow(
      br(),
      textAreaInput(inputId = ns("incidentAddress"),
                    label = "Incident Address: ",
                    value = ""),
      selectInput(inputId = ns("incidentState"),
                  label = "Select State:",
                  choices = c(Choose='', state_vec)),
      
      selectInput(inputId = ns('facilityCategory'), 
                  label = 'Select Facility Category:',
                  choices = c(Choose='', hcf_category_vec), 
                  selectize=TRUE),
      selectInput(inputId = ns('functional_status'), 
                  label = 'Select Facility Status:',
                  choices = c(Choose='', hcf_func_status), 
                  selectize=TRUE),
      actionButton(inputId = ns("erSubmitButton"),
                   label = "Submit"),
      br()
    )
  )
}

searchServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      shinyjs::click(id = "erSubmitButton")
      
      address      <- reactive(input$incidentAddress)
      state        <- reactive(input$incidentState)
      fac_category <- reactive(input$facilityCategory)
      action_btn   <- reactive(input$erSubmitButton)
      
      return(
        list(
          address,
          state,
          fac_category,
          action_btn
        )
      )
    }
  )
}

