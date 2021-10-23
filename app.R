library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinythemes)
library(here)
library(sf)
library(dplyr)
library(stringr)
library(tictoc)
library(shinycssloaders)
library(leaflet.opacity)

# if (Sys.info()[["user"]] == "mirza")
# {
#     source(here::here("R", "main_setup.R"))
#     
# } else {
#     # Load setup script from path on cluster; local path 
#     source(file = "/vol/milkunB/mcengic/Projects/atra_climate_model/R/main_setup.R")
# }


# Script setup ------------------------------------------------------------


source(here::here("app_functions.R"))


projections_datavizz_folder <- "Projects/atra_climate_model/runs/climate_impact/Datavizz" %>% 
    Rahat::milkunize2("archive")

#### Output data is on milkun archive
bivariate_names <- projections_datavizz_folder %>% 
    list.files(full.names = TRUE)


data_points <- st_read(here::here("test_data", "points_rcp85.gpkg"))

update = Rahat::today(format = "human")

#### UI part ####
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Alpine salamander: Supplementary Info</a>'), id="nav",
               windowTitle = "Atra SI",
               tabPanel("Main page",
                        tags$div(
                            tags$h1("Welcome to the main page"),
                            "There are several tabs you can use to navigate:",
                            tags$h3("Species data"),
                            "- Contains species data",
                            tags$h3("Future projections"),
                            tags$h3("Future projections extent"),
                            
                            tags$br(),tags$br(),tags$h4("Background"), 
                            "Some text on the project. Maybe even an abstract or something.",
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and input data are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Authors"),
                            "Mirza Čengić", tags$a(href="mirzaceng@gmail.cpm", "email") ,tags$br(),
                            "Emina Šunje", tags$a(href="sunje.emina@gmail.cpm", "email"),tags$br(),
                            # tags$br(),tags$br(),tags$h4("Contact"),
                            # "mirzaceng@gmail.com", tags$a(href="mirzaceng@gmail.cpm", "email"), tags$br(),tags$br(),
                            # tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                        )   
               ),
               #### Species data panel
               tabPanel("Species data",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            # leafletOutput("mymap", width="100%", height="100%")
                            leafletOutput("map", width = "100%", height = "100%"),
                            absolutePanel(top = 10, right = 10,
                                          # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                                          #             value = range(quakes$mag), step = 0.1
                                          # ),
                                          # selectInput("colors", "Color Scheme",
                                          #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                          # ),
                                          selectizeInput("taxon", "Select taxon:",
                                                         # c("RCP85", "RCP26")
                                                         unique(data_points$subspecies),
                                                         multiple = FALSE,
                                                         options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                         )
                                          ),
                                          # checkboxInput("legend", "Show legend", FALSE),
                                          # Button
                                          downloadButton("downloadData", "Download data")
                            )
                        )
               ),
               ##############################
               ##### Panel 1 bivariate projections ####
               tabPanel("Bivariate projections",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("map_panel1", width = "100%", height = "100%"),
                            absolutePanel(top = 10, right = 10,
                                          selectizeInput("period_panel1", "Select time period:",
                                                         # c("RCP85", "RCP26")
                                                         c("Current", "Future"),
                                                         options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                         )
                                          ),
                                          selectizeInput("taxon_panel1", "Select taxon:",
                                                         # c("RCP85", "RCP26")
                                                         unique(data_points$subspecies),
                                                         options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                         )
                                          ),
                                          selectizeInput("scenario_panel1", "Select RCP scenario:",
                                                         c("rcp26", "rcp85"),
                                                         options = list(
                                                             placeholder = 'Relevant only for future time period',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                         )
                                                         # unique(data_points$subspecies)
                                          ),
                                          downloadButton("downloadBivariateRasterData", "Download bivar data")
                                          
                            )
                            
                        )
               ),
               ###################################
               ##### Panel 2 individual projections
               tabPanel("Individual projections",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            # leafletOutput("mymap", width="100%", height="100%")
                            leafletOutput("map_panel2", width = "100%", height = "100%"),
                            div(class = "selectt",
                                tags$head(includeCSS("styles.css")),
                                
                                absolutePanel(top = 10, right = 10, 
                                              # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                                              #             value = range(quakes$mag), step = 0.1
                                              # ),
                                              # selectInput("colors", "Color Scheme",
                                              #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                              # ),
                                              selectizeInput("period_panel2", "Select time period:",
                                                             # c("RCP85", "RCP26")
                                                             c("Current", "Future"),
                                                             options = list(
                                                                 placeholder = 'Please select an option below',
                                                                 onInitialize = I('function() { this.setValue(""); }')
                                                             )
                                              ),
                                              selectizeInput("taxon_panel2", "Select taxon:",
                                                             # c("RCP85", "RCP26")
                                                             unique(data_points$subspecies),
                                                             options = list(
                                                                 placeholder = 'Please select an option below',
                                                                 onInitialize = I('function() { this.setValue(""); }')
                                                             )
                                              ),
                                              selectizeInput("scenario_panel2", "Select RCP scenario:",
                                                             c("rcp26", "rcp85"),
                                                             options = list(
                                                                 placeholder = 'Relevant only for future time period',
                                                                 onInitialize = I('function() { this.setValue(""); }')
                                                             )
                                                             # unique(data_points$subspecies)
                                              ),
                                              selectizeInput("algorithm_panel2", "Select algorithm:",
                                                             c("GBM", "GLM", "ensemble"),
                                                             options = list(
                                                                 placeholder = 'Please select an option below',
                                                                 onInitialize = I('function() { this.setValue(""); }')
                                                             )
                                                             # unique(data_points$subspecies)
                                              ),
                                              downloadButton("downloadRasterData", "Download data")
                                              
                                )
                            )
                        )
               ),
               # tabPanel("Future projections",
               #          div(class="outer",
               #              tags$head(includeCSS("styles.css")),
               #              # leafletOutput("mymap", width="100%", height="100%")
               #              fluidRow(
               #                column(8, wellPanel(
               #                  verbatimTextOutput("urlText")
               #                ))
               #              ),
               #              absolutePanel(top = 10, right = 10,
               #                            selectInput("taxon_panel2", "Select taxon:",
               #                                        # c("RCP85", "RCP26")
               #                                        unique(data_points$subspecies)
               #                            ),
               #                            selectInput("scenario_panel2", "Select scenario:",
               #                                        c("rcp26", "rcp85")
               #                                        # unique(data_points$subspecies)
               #                            ),
               #                            selectInput("algorithm_panel2", "Select algorithm:",
               #                                        c("GBM", "GLM", "ensemble")
               #                                        # unique(data_points$subspecies)
               #                            )
               #              )
               #          )
               # ),
               ####### Panel about
               
               tabPanel("About this site",
                        tags$div(
                            tags$h4("Last update"), 
                            h6(paste0(update)),
                            tags$h2("Alpine salamander modeling"),
                            tags$br(),tags$br(),tags$h4("Background"), 
                            "Some text on the project. Maybe even an abstract or something.",
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and input data are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Authors"),
                            "Mirza Čengić", tags$a(href="mirzaceng@gmail.cpm", "email") ,tags$br(),
                            "Emina Šunje", tags$a(href="sunje.emina@gmail.cpm", "email"),tags$br(),
                            # tags$br(),tags$br(),tags$h4("Contact"),
                            # "mirzaceng@gmail.com", tags$a(href="mirzaceng@gmail.cpm", "email"), tags$br(),tags$br(),
                            # tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                        )
               )
               
    )
)


# server <- function(input, output, session) {
#   
#   # Reactive expression for the data subsetted to what the user selected
#   filteredData <- reactive({
#     quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
#   })
#   
#   # This reactive expression represents the palette function,
#   # which changes as the user makes selections in UI.
#   colorpal <- reactive({
#     colorNumeric(input$colors, quakes$mag)
#   })
#   
#   output$map <- renderLeaflet({
#     # Use leaflet() here, and only include aspects of the map that
#     # won't need to change dynamically (at least, not unless the
#     # entire map is being torn down and recreated).
#     leaflet(quakes) %>% addTiles() %>%
#       fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
#   })
#   
#   # Incremental changes to the map (in this case, replacing the
#   # circles when a new color is chosen) should be performed in
#   # an observer. Each independent set of things that can change
#   # should be managed in its own observer.
#   observe({
#     pal <- colorpal()
#     
#     leafletProxy("map", data = filteredData()) %>%
#       clearShapes() %>%
#       addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
#                  fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
#       )
#   })
#   
#   # Use a separate observer to recreate the legend as needed.
#   observe({
#     proxy <- leafletProxy("map", data = quakes)
#     
#     # Remove any existing legend, and only if the legend is
#     # enabled, create a new one.
#     proxy %>% clearControls()
#     if (input$legend) {
#       pal <- colorpal()
#       proxy %>% addLegend(position = "bottomright",
#                           pal = pal, values = ~mag
#       )
#     }
#   })
# }

#### Server part ####

server_new <- function(input, output, session) {
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        data_points[data_points$subspecies == input$taxon, ]
    })
    
    # Downloadable csv of selected dataset ----
    observe(
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
    )
    
    #### Panel 1 stuff ####
    
    
    
    
    output$map_panel1 <- renderLeaflet({
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
    
    data_panel1 <- reactive({
        
        if (input$period_panel1 == "Future")
        {
            x <- make_bivar(
                x = bivariate_names,
                period = input$period_panel1,
                taxon = input$taxon_panel1,
                scenario = input$scenario_panel1)
            
            return(x)
        }
        if (input$period_panel1 == "Current")
        {
            x <- make_bivar(
                x = bivariate_names,
                period = input$period_panel1,
                taxon = input$taxon_panel1
            )
            
            return(x)
            
        }
        
        
    })
    
    output$urlText <- renderText({
        as.character(input$my_url)
        data_panel2()
    })
    
    observe({
        # pal <- colorpal()
        # file_name_future <- files_future %>%
        #   str_subset(input$algorithm_panel2) %>%
        #   str_subset(input$scenario_panel2)
        
        
        # Downloadable raster of selected dataset ----
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
        
        if (nchar(input$period_panel1) > 1 & nchar(input$taxon_panel1) > 1)
        {
            print(str_glue("
                     {input$period_panel1}
                     {input$taxon_panel1}
                     {input$scenario_panel1}
                     "))
            print(data_panel1())
            
            # if (length(data_panel2()) < 1)
            # {
            #   stop("ERROR")
            # } else {
            r_bivar <- raster(data_panel1())
            r_bivar <- leaflet::projectRasterForLeaflet(r_bivar, method = "ngb")
            
            # }
            
            
            
            pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r_bivar),
                                na.color = "transparent")
            
            #### Set title according to timeperiod
            if (input$period_panel1 == "Future")
            {
                plot_title <-  str_glue("{str_to_title(input$period_panel2)} bivariate projection for {input$taxon_panel2} {input$scenario_panel2}")
            }
            if (input$period_panel1 == "Current")
            {
                plot_title <-  str_glue("{str_to_title(input$period_panel1)} bivariate projection for {input$taxon_panel2}")
            }
            
            withProgress(message = "Calculating bivariate projection", value = 20,
                         {
                             leafletProxy("map_panel1") %>%
                                 clearImages() %>%
                                 clearControls() %>%
                                 addRasterImage(r_bivar,
                                                colors = pal, project = FALSE,
                                                layerId = "raster") %>%
                                 addOpacitySlider(layerId = "raster") %>% 
                                 addLegend(pal = pal, values = values(r_bivar),
                                           position = "bottomright",
                                           title = plot_title)
                         }
            )
            
        }
    })
    
    #### Panel 2 stuff ####
    
    
    
    
    output$map_panel2 <- renderLeaflet({
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
    
    data_panel2 <- reactive({
        
        if (input$period_panel2 == "Future")
        {
            files_future <- list.files(folder_path_projections_future, full.names = TRUE)
            
            x <- files_future %>% 
                str_subset(input$algorithm_panel2) %>% 
                str_subset(input$scenario_panel2) %>% 
                str_subset(input$taxon_panel2)
            
            return(x)
        }
        if (input$period_panel2 == "Current")
        {
            files_current <- list.files(folder_path_projections_current, full.names = TRUE)
            
            x <- files_current %>% 
                str_subset(input$algorithm_panel2) %>%
                str_subset(input$taxon_panel2)
            
            return(x)
            
        }
        
        
    })
    
    output$urlText <- renderText({
        as.character(input$my_url)
        data_panel2()
    })
    
    observe({
        # pal <- colorpal()
        # file_name_future <- files_future %>%
        #   str_subset(input$algorithm_panel2) %>%
        #   str_subset(input$scenario_panel2)
        
        
        # a <- "aaa"
        # b <- "baa"
        # c <- "caa"
        
        # Downloadable raster of selected dataset ----
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
        
        if (nchar(input$algorithm_panel2) > 1 & nchar(input$period_panel2) > 1 & nchar(input$taxon_panel2) > 1)
        {
            print(str_glue("
                     {input$period_panel2}
                     {input$taxon_panel2}
                     {input$scenario_panel2}
                     {input$algorithm_panel2}
                     "))
            print(data_panel2())
            
            # if (length(data_panel2()) < 1)
            # {
            #   stop("ERROR")
            # } else {
            r_future <- raster(data_panel2()) / 1000
            # }
            
            
            
            pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r_future),
                                na.color = "transparent")
            
            #### Set title according to timeperiod
            if (input$period_panel2 == "Future")
            {
                plot_title <-  str_glue("{str_to_title(input$period_panel2)} climate suitability for {input$taxon_panel2} {input$algorithm_panel2} {input$scenario_panel2}")
            }
            if (input$period_panel2 == "Current")
            {
                plot_title <-  str_glue("{str_to_title(input$period_panel2)} climate suitability for {input$taxon_panel2} {input$algorithm_panel2}")
            }
            
            leafletProxy("map_panel2") %>%
                clearImages() %>% 
                addRasterImage(r_future,
                               colors = pal,
                               opacity = 0.8) %>%
                clearControls() %>% 
                addLegend(pal = pal, values = values(r_future),
                          position = "bottomright",
                          title = plot_title)
        }
    })
    
    ################## Panel XX species
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    # colorpal <- reactive({
    #   colorNumeric(input$colors, quakes$mag)
    # })
    
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
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        # pal <- colorpal()
        
        leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>%
            addCircleMarkers(
                # radius = ~10^combined_num/10,
                radius = 5,
                weight = 1, 
                color = "#777777",
                stroke = TRUE,
                # fillColor = ~pal(combined_num),
                fillColor = "#db1616",
                fillOpacity = 0.9, popup = ~paste(combined_num)
            )
    })
    
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



# runApp(shinyApp(ui, server_new), launch.browser = T)
# shinyApp(ui, server)
shinyApp(ui, server_new)

######################