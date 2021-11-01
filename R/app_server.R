#####################################!
#### Alpine salamander Shiny app ####

########## Server part ####

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    data_points[data_points$category == input$taxon, ]
  })
 
  # Downloadable csv of selected dataset ----
  observe(
    {
      if(input$download_format == "GeoPackage")
      {
        output$downloadData <- downloadHandler(
          filename = function() {
            paste(input$taxon, ".gpkg", sep = "")
          },
          content = function(file) {
            sf::st_write(filteredData(), file)
          }
        )
      }
      
      ####
      if(input$download_format == "Shapefile")
      {
      output$downloadData <- downloadHandler(
        
        filename = function() {
          paste(input$taxon, ".zip", sep = "")
        },
        content = function(file) {
          withProgress(message = "Exporting Data", {
            
            incProgress(0.5)
            tmp.path <- dirname(file)
            
            name.base <- file.path(tmp.path, input$taxon)
            name.glob <- paste0(name.base, ".*")
            name.shp  <- paste0(name.base, ".shp")
            name.zip  <- paste0(name.base, ".zip")
            
            if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
            sf::st_write(filteredData(), dsn = name.shp, ## layer = "shpExport",
                         driver = "ESRI Shapefile", quiet = TRUE)
            
            zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
            req(file.copy(name.zip, file))
            
            if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
            
            incProgress(0.5)
          })
        }  
      )
      }
    }
  )
  
  ## Panel 1 stuff ####
  
  data_panel1 <- reactive({
    # Get raster path
    if (input$period_panel1 == "Future")
    {
      x <- make_bivar(
        x = bivariate_names,
        period = input$period_panel1,
        taxon = input$taxon_panel1,
        scenario = input$scenario_panel1,
        folder = here("data", "bivariate")
      )
      return(x)
    }
    if (input$period_panel1 == "Current")
    {
      x <- make_bivar(
        x = bivariate_names,
        period = input$period_panel1,
        taxon = input$taxon_panel1,
        folder = here("data", "bivariate")
      )
      return(x)
    }
  })
  
  output$map_panel1 <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)", layerId = "tile1") %>% 
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain", layerId = "tile2") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Sattelite", layerId = "tile3") %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c("OSM (default)", "Terrain", "Sattelite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addOpacityControls(layerId = c("Layer opacity"),
                         collapsed = FALSE, position = "topleft",
                         renderOnLayerAdd = TRUE
      ) %>% 
      fitBounds(7, 42, 20, 47)
    
  })
  
  observe({
  
    # P1 download ####
    observe(
      {
        output$downloadBivariateRasterData <- downloadHandler(
          filename = function() {
            if (input$period_panel1 == "Future")
            {
              str_glue("BIVARIATE_{input$period_panel1}-{input$taxon_panel1}-{input$scenario_panel1}.tif")
            }
            if (input$period_panel1 == "Current")
            {
              str_glue("BIVARIATE_{input$period_panel1}-{input$taxon_panel1}.tif")
            }
            
          },
          content = function(file) {
            raster::writeRaster(raster(data_panel1()), file)
          }
        )
      }
    )
    
    #### P1 render ####
    # Observe Render map button event. 
    # BUG there's a bag where button value should be reset once it's activated. 
    # BUG For example, if current period map is rendered, and then future is selected without scenario, this will crash the app since it doesn't have value for {scenario}
    if (input$plot_panel1)
    {
      ## Some bug cathing console outputs
      # print(str_glue("
      #                {input$period_panel1}
      #                {input$taxon_panel1}
      #                {input$scenario_panel1}
      #                "))
      # print(data_panel1())
      
      # if (length(data_panel2()) < 1)
      # {
      #   stop("ERROR")
      # } else {
      
      ####
      r_bivar <- raster(data_panel1())
      
      # Reproject variables to leaflet-friendly projection if needed.
      # TODO find a way to plot these maps, but provide WGS84 ones if downloading
      if (!str_detect(st_crs(r_bivar)$input, "Pseudo-Mercator"))
      {
        r_bivar <- leaflet::projectRasterForLeaflet(r_bivar, method = "ngb")
      }
      
      #### Code for subsetting raster categories
      # This comes from categories_panel1 UI element
      if (length(input$categories_panel1) > 0)
      {
        
        my_categories <- mydf %>% 
          filter(category != "00") %>% 
          filter(!category %in% input$categories_panel1) %>%
          pull(ID)
        
        r_bivar_vals <- getValues(r_bivar)
        r_bivar_vals[r_bivar_vals %in% my_categories] <- NA
        r_bivar <- setValues(r_bivar, r_bivar_vals)
        
        #### Set categorical value
        r_bivar <- ratify(r_bivar)
        rat <- levels(r_bivar)[[1]]
        ####
        # Convert values for legend plotting
        my_category_lvel <- mydf %>% 
          filter(category != "00") %>% 
          filter(ID %in% rat$ID) %>% 
          filter(category %in% input$categories_panel1) %>%
          inner_join(
            tibble::tribble(
              ~value, ~category,
              "Low Suit - Low Cert", "11",
              "Low Suit - Med Cert", "12",
              "Low Suit - High Cert", "13",
              "Med Suit - Low Cert", "21",
              "Med Suit - Med Cert", "22",
              "Med Suit - High Cert", "23",
              "High Suit - Low Cert", "31",
              "High Suit - Med Cert", "32",
              "High Suit - High Cert", "33"
            ), by = "category"
          ) %>% 
          pull(value) %>% 
          as.character()
        
        
        rat$lvl <- my_category_lvel
        levels(r_bivar) <- rat
        
      } else 
      {
        r_bivar <- ratify(r_bivar)
        rat <- levels(r_bivar)[[1]]
        ####
        # Convert values for legend plotting 
        my_category_lvel <- mydf %>% 
          filter(category != "00") %>% 
          filter(ID %in% rat$ID) %>%
          inner_join(
            tibble::tribble(
              ~value, ~category,
              "Low Suit - Low Cert", "11",
              "Low Suit - Med Cert", "12",
              "Low Suit - High Cert", "13",
              "Med Suit - Low Cert", "21",
              "Med Suit - Med Cert", "22",
              "Med Suit - High Cert", "23",
              "High Suit - Low Cert", "31",
              "High Suit - Med Cert", "32",
              "High Suit - High Cert", "33"
            ), by = "category"
          ) %>% 
          pull(value) %>% 
          as.character()
        
        rat$lvl <- my_category_lvel
        levels(r_bivar) <- rat
        
      }
      
      #### Set raster color scales
      # continuous numeric for individal projections panel
      pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r_bivar),
                          na.color = "transparent")
      #  factor for bivariate projections panel
      pal_cat <- colorFactor(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r_bivar),
                             na.color = "transparent")
      
      #### Set title according to timeperiod
      if (input$period_panel1 == "Future")
      {
        plot_title <-  str_glue("{str_to_title(input$period_panel2)} projection\n {input$taxon_panel2} {input$scenario_panel2}")
      }
      if (input$period_panel1 == "Current")
      {
        plot_title <-  str_glue("{str_to_title(input$period_panel1)} projection\n{input$taxon_panel2}")
      }
      
      # P1 Proxy ####
      withProgress(message = "Calculating bivariate projection", value = 20,
                   {
                     leafletProxy("map_panel1") %>%
                       clearImages() %>%
                       clearControls() %>%
                       addProviderTiles(providers$OpenStreetMap, group = "OSM (default)", layerId = "tile1") %>% 
                       addProviderTiles(providers$Stamen.Terrain, group = "Terrain", layerId = "tile2") %>%
                       addProviderTiles(providers$Esri.WorldImagery, group = "Sattelite", layerId = "tile3") %>%
                       addRasterImage(r_bivar,
                                      colors = pal, project = FALSE,
                                      layerId = "Layer opacity") %>%
                       addLegend(pal = pal_cat, 
                                 values = values(r_bivar),
                                 title = plot_title,
                                 position = "bottomright",
                                 labFormat  = labelFormat(
                                   transform = function(x) {
                                     levels(r_bivar)[[1]]$lvl[which(levels(r_bivar)[[1]]$ID == x)]
                                     
                                   }
                                 )
                       )
                   }
      )
      
    }
  })
  
  # Panel 1 - Freeze scenario pane (wait for Future period input)
  observe({
    if(input$period_panel1 == "Current"){
      toggleState(id = "variable")
    }
    
    else if(input$period_panel1 == "Future"){
      toggleState(id = "variable")
      updateSelectInput(session = session,
                        inputId = "scenario_panel1",
                        choices = c("RCP2.6" = "rcp26", "RCP8.5" = "rcp85")
                          # c("rcp26", "rcp85")
      )
    }
  })
  # Panel 2 - Freeze scenario pane 
  observe({
    if(input$period_panel2 == "Current"){
      toggleState(id = "variable")
    }
    
    else if(input$period_panel2 == "Future"){
      toggleState(id = "variable")
      updateSelectInput(session = session,
                        inputId = "scenario_panel2",
                        choices = c("RCP2.6" = "rcp26", "RCP8.5" = "rcp85")
                        # c("rcp26", "rcp85")
      )
    }
  })
  
  ## Panel 2 stuff ##########################################################
  
    output$map_panel2 <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)", layerId = "tile1") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain", layerId = "tile2") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Sattelite", layerId = "tile3") %>%
      addLayersControl(
        position = "topleft",
        baseGroups = c("OSM (default)", "Terrain", "Sattelite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addOpacityControls(layerId = c("Layer opacity"),
                         collapsed = FALSE, position = "topleft",
                         renderOnLayerAdd = TRUE
                         # title = "Layer opacity"
      ) %>%
      fitBounds(7, 42, 20, 47)
    
  })
  
  ### Panel 2
  
  data_panel2 <- reactive({
    
    if (input$period_panel2 == "Future")
    {
      files_future <- list.files(here::here("data", "projections", "future"), full.names = TRUE)
      
      x <- files_future %>% 
        str_subset(input$algorithm_panel2) %>% 
        str_subset(input$scenario_panel2) %>% 
        str_subset(input$taxon_panel2) %>% 
        str_subset(input$gcm_panel2)
      
      return(x)
    }
    if (input$period_panel2 == "Current")
    {
      files_current <- list.files(here::here("data", "projections", "current"), full.names = TRUE)
      
      x <- files_current %>% 
        str_subset(input$algorithm_panel2) %>%
        str_subset(input$taxon_panel2)
      
      return(x)
      
    }
    
    
  })
  
  observe({
  
      # P2 download ####
    observe(
      {
        output$downloadRasterData <- downloadHandler(
          filename = function() {
            if (input$period_panel2 == "Future")
            {
              str_glue("{input$period_panel2}-{input$taxon_panel2}-{input$scenario_panel2}-{input$algorithm_panel2}.tif")  
            }
            if (input$period_panel2 == "Current")
            {
              str_glue("{input$period_panel2}-{input$taxon_panel2}-{input$algorithm_panel2}.tif")  
            }
            
          },
          content = function(file) {
            raster::writeRaster(raster(data_panel2()) / 1000, file)
          }
        )    
      }
    )
    
    #### P2 render ####
    # Observe Render map button event. 
    if (input$plot_panel2)
    {
      ## Some bug cathing console outputs
      
      # print(str_glue("
      #                {input$period_panel2}
      #                {input$taxon_panel2}
      #                {input$scenario_panel2}
      #                {input$algorithm_panel2}
      #                "))
      # print(data_panel2())
      
      # if (length(data_panel2()) < 1)
      # {
      #   stop("ERROR")
      # } else {
      
      ####
      r_suitability <- raster(data_panel2()) / 1000
    
      # Reproject to leaflet if needed
      if (!str_detect(st_crs(r_suitability)$input, "Pseudo-Mercator"))
      {
        r_suitability <- leaflet::projectRasterForLeaflet(r_suitability, method = "ngb")
      }
      
      # Subset raster values with a slider {range}
      r_suitability_vals <- getValues(r_suitability)

      r_suitability_vals[r_suitability_vals <= input$range[1]] <- NA
      r_suitability_vals[r_suitability_vals >= input$range[2]] <- NA
      r_suitability <- setValues(r_suitability, r_suitability_vals)
      
      pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r_suitability),
                          na.color = "transparent")
      
      #### Set title according to timeperiod
      if (input$period_panel2 == "Future")
      {
        plot_title <-  str_glue("Climate suitability for {input$taxon_panel2}")
      }
      if (input$period_panel2 == "Current")
      {
        plot_title <-  str_glue("Climate suitability {input$taxon_panel2}")
      }
      # P2 Proxy ####
      leafletProxy("map_panel2") %>%
        clearImages() %>% 
        addRasterImage(r_suitability,
                       colors = pal,
                       project = FALSE,
                       layerId = "Layer opacity") %>%
        clearControls() %>% 
        addLegend(pal = pal, values = values(r_suitability),
                  position = "bottomright",
                  title = plot_title)
    }
  })
  
  ################## Psp Species data panel ####
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Sattelite") %>% 
      addLayersControl(
        position = "topleft",
        baseGroups = c("OSM (default)", "Terrain", "Sattelite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      fitBounds(7, 42, 20, 47)
  })
  
  observe({
    # Add points to the map
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        radius = 5,
        weight = 1, 
        color = "#777777",
        stroke = TRUE,
        # fillColor = ~pal(combined_num),
        fillColor = "#db1616",
        fillOpacity = 0.9, 
        # popup = ~paste(species, category, locality)
        popup = ~paste("<div>",
                       "<h4>",
                       species, subspecies,
                       "</h4>",
                       "ID: ", category,
                       "<br>", "<br>",
                       "Locality: ", locality,
                       "<br>",
                       "Source: ", database,
                       "</div>")
      )
  })
  # species   category subspecies                                 locality    database database_c
  
  # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = data_points)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     # pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         # pal = pal,
  #                         values = ~combined_num
  #     )
  #   }
  # })
}
