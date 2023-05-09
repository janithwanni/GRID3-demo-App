
uploadUI <- function(id, state_vec, hcf_category_vec) {
  ns <- NS(id)
  tagList(
      fluidRow(
               fileInput("upload", "Upload Reference geodata file"),
               hr(),
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
               actionButton(inputId = ns("erSubmitButton"),
                            label = "Submit"),
               br()
      )
  )
}

uploadServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      sf_data <- reactive({
        req(input$upload)
        sf_tbl = sf::st_read(input$upload$datapath)
        # sf_tbl = sf::st_read("data/national-health-care-facilities/health-care-facilities-primary-secondary-and-tertiary.geojson")
      })
      
      return(sf_data)
      shinyjs::click(id = "erSubmitButton")
      
      return(
        list(
          df           = sf_data,
          address      = shiny::reactive(input$incidentAddress),
          state        = shiny::reactive(input$incidentState),
          fac_category = shiny::reactive(input$facilityCategory),
          action_btn   = shiny::reactive(input$erSubmitButton)
        )
      )
    }
  )
}