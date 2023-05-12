
mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Determine the Closest Health Facilities"),
    leafletOutput(outputId = ns("nnHCF"))
  )
}


mapServer <- function(id, dataset, searchVals) {
  
  moduleServer(
    id,
    function(input, output, session) {

      output$nnHCF <- renderLeaflet({
        req(!is.null(dataset()), !is.null(searchVals()))

        state   = searchVals()$state
        address = searchVals()$address
        hcf_category = searchVals()$fac_category
        hcf_func_status = searchVals()$func_status

        national_hcf_filtered <- dataset() |> 
            filter(state_name == state, 
                   category == hcf_category, 
                   functional_status == hcf_func_status) |> 
            select(latitude, longitude,name) |> 
            rowid_to_column(var = "hcf_id")
          

        # * Geocoding: Address -> Lat Long ----
        
        # convert address to geometry
        inc_locations_latlon_tbl_sf <- geo(address, 
                                           method = "arcgis") |> 
          st_as_sf(
            coords = c("long", "lat"),
            crs    = 4326
          ) |> 
          left_join(geo(address, 
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
        m()@map
      })
    }
  )
}

