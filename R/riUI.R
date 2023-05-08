
# rm(list = ls())
# select_data_list <- readRDS("data/select_data.rds")
# hcf_state_names <- select_data_list$hcf_state_names
# hcf_category <- select_data_list$hcf_category

#source("data_scripts/data_prep.R")

ri_UI <- function(id, state_vec, hcf_category_vec) {
  ns <- NS(id)
  tagList(fluidPage(
    fluidRow(
      column(3,
             selectInput(ns("riStates"), 
                         label = "Select State:",
                         choices = c(Choose='', national_hcf_state_names), 
                         selectize=TRUE),
             radioButtons(ns("rdRIStep"), 
                          h3("Select Routine Immunization Plan:"),
                          choices = list("Pharmacy to Primary Health Care" = 1, 
                                         "Primary Health Care to Patients" = 2),selected = 1),
             
             actionButton(inputId = "riSubmitButton",
                          label = "Submit"),
             hr()
      ),
      column(9,
             leafletOutput(outputId = "riMapRoute")
      )
    )
  )
  )
}

ri_Server <- function(id, hcf_df, edu_df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      m <- eventReactive(input$erSubmitButton, {
        
        if(input$rdRIStep == 1){
          national_pharmacy <- hcf_df |> 
            filter(state_name == input$incidentState, 
                   category == "Pharmacy", 
                   functional_status == "Functional") |> 
            select(latitude,longitude,name) |> 
            rowid_to_column(var = "hcf_id")
          
          national_phc <- hcf_df |> 
            filter(state_name == input$incidentState, 
                   category == "Primary Health Care", 
                   functional_status == "Functional") |> 
            select(latitude,longitude,name) |> 
            rowid_to_column(var = "hcf_id")
          
        } else{
          national_phc <- dataset |> 
            filter(state_name == input$incidentState, 
                   category == "Primary Health Care", 
                   functional_status == "Functional") |> 
            select(latitude,longitude, name, state_code) |> 
            rowid_to_column(var = "hcf_id")
          
          nga_education <- edu_df |> 
            filter(state_name == input$incidentState, 
                   category == "Primary Health Care", 
                   functional_status == "Functional") |> 
            select(latitude,longitude,name) |> 
            rowid_to_column(var = "hcf_id")
        }
        
        
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
            k = 3,
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
  )
}

