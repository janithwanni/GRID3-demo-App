
library(shiny)
library(shinyjs)

library(osmdata)    # Open Street Map Overpass API
library(osrm)       # Open Street Map Routing API

library(sf)         # Simple Features
library(nngeo)      # Nearest Neighbors
library(mapview)    # Interactive Maps
library(leaflet)
library(tidygeocoder) # Used for geocoding
library(tidyverse)  # Core tidy libs


# selectIput data
selectInput_data <- readRDS(file = "data/select_item_data.rds")
#sf_tbl = sf::st_read("data/health-care-facilities-primary-secondary-and-tertiary.geojson")

ui <- shiny::fluidPage(
  
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Working with GRID3 Geospatial Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        fileInput("upload", "Upload Reference geodata file"),
        hr(),
        br(),
        textAreaInput(inputId = "incidentAddress",
                      label = "Incident Address: ",
                      value = ""),
        selectInput(inputId = "incidentState",
                    label = "Select State:",
                    choices = c(Choose='', selectInput_data$state_values)),
        
        selectInput(inputId = 'facilityCategory', 
                    label = 'Select Facility Category:',
                    choices = c(Choose='', selectInput_data$category_values), 
                    selectize=TRUE),
        actionButton(inputId = "erSubmitButton",
                     label = "Submit"),
        br()
      )
      
    ),
    
    mainPanel(
      fluidRow(h3("Determine the Closest Health Facilities"),
               leafletOutput(outputId = "nnHCF"))
    )
  )
)


server <- function(input, output, session) {
  
  options(shiny.maxRequestSize= 100*1024^2)
  
  sf_data <- reactive({
    req(input$upload)
    sf_tbl = sf::st_read(input$upload$datapath)
  })
  
  m <- eventReactive(input$erSubmitButton, {
    #req(input$upload)
    national_hcf_filtered <- sf_data() |>  #sf_tbl |> 
      filter(state_name == input$incidentState, 
             category == input$facilityCategory, 
             functional_status == "Functional") |> 
      select(latitude, longitude,name) |> 
      rowid_to_column(var = "hcf_id")
    
    
    # * Geocoding: Address -> Lat Long ----
    
    # convert address to geometry
    inc_locations_latlon_tbl_sf <- geo(input$incidentAddress, 
                                       method = "arcgis") |> 
      st_as_sf(
        coords = c("long", "lat"),
        crs    = 4326
      ) |> 
      left_join(geo(input$incidentAddress, 
                    method = "arcgis")) |> 
      rowid_to_column(var = "inc_id")
    
    
    
    # 3.0 NEAREST NEIGHBORS ----
    # * Alternatively we can use sfnetworks
    # * I'm going to use nngeo
    
    # * Getting Nearest Neighbors with nngeo ----
    
    if(nrow(national_hcf_filtered)<=3){
      network_ids <- st_nn(
        x = inc_locations_latlon_tbl_sf,
        y = national_hcf_filtered, 
        k = nrow(national_hcf_filtered),
        progress = T
      )
    } else{
      network_ids <- st_nn(
        x = inc_locations_latlon_tbl_sf, 
        y = national_hcf_filtered, 
        k = 5,
        #k = nrow(national_hcf_filtered),
        progress = T
      )
    }
    
    network_lines_sf <-st_connect(
      x = inc_locations_latlon_tbl_sf, 
      y = national_hcf_filtered, 
      ids = network_ids
    )
    
    m <- mapview(
      national_hcf_filtered[unlist(network_ids),], 
      col.region = "cyan",
      color      = "white",
      layer.name = "National Hospitals",
      cex        = 12
    )+
      mapview(
        inc_locations_latlon_tbl_sf, 
        col.region = "magenta",
        color      = "white",
        layer.name = "Incident location",
        cex        = 20
      )+
      mapview(
        network_lines_sf,
        color      = "yellow"
      )
    m
  })
  
  output$nnHCF <- renderLeaflet({
    m()@map
  })
  
}


shinyApp(ui = ui, server = server)




