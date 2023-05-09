
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

ui <- shiny::fluidPage(
  
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Modules Tutorial"),
  
  sidebarLayout(
    sidebarPanel(
      uploadUI("upload_ui",state_vec=selectInput_data$state_values, 
            hcf_category_vec=selectInput_data$category_values)
      
    ),
    
    mainPanel(
      mapUI("er_map")
    )
  )
)


server <- function(input, output, session) {
  options(shiny.maxRequestSize = 50*1024^2)
  
  queryVals <- uploadServer(id = "upload_ui")
  
  mapServer(id = "er_map", 
            dataset = queryVals()$df, 
            state   = queryVals()$state, 
            address = queryVals()$address, 
            facility_category = queryVals()$fac_category, 
            action_btn = queryVals()$action_btn)
  
}


shinyApp(ui = ui, server = server)




