
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
hcf_f_status <- readRDS("data/func_status.rds")
sf_tbl = sf::st_read("data/health-care-facilities-primary-secondary-and-tertiary.geojson")

ui <- shiny::fluidPage(
  
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Working with GRID3 Geospatial Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      uploadUI("upload"),
      searchUI("search",
               state_vec = selectInput_data$state_values,
               hcf_category_vec=selectInput_data$category_values,
               hcf_func_status = hcf_f_status)
    ),
    
    mainPanel(
      
      mapUI("er_map")
    )
  )
)


server <- function(input, output, session) {
  options(shiny.maxRequestSize = 50*1024^2)
  
  df_data <- uploadServer(id = "upload")
  
  searchVals <- searchServer(id = "search")

  # address,state,fac_category,func_status,action_btn
  # dataset, address, state, hcf_category, hcf_func_status, action_btn
  mapServer(id = "er_map", 
            dataset = df_data,
            searchVals = searchVals
            )
  
}


shinyApp(ui = ui, server = server)




