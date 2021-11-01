#####################################!
#### Alpine salamander Shiny app ####

## User Interface for the Atra app ##

# UI part ####
ui <- bootstrapPage(
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Alpine salamander: Supplementary Info</a>'), id="nav",
             windowTitle = "Atra SI",
             #### Pmain Main page ####
             tabPanel("Main page",
                      tags$div(
                        tags$h1("Main page"),
                        tags$b("Main page still under construction"), tags$br(), 
                        "This is the digital Supplementary Information document accompanying Salamandra atra climate impact paper.",
                        tags$br(),
                        "There are several tabs you can use to navigate:",
                        tags$h3("Species data tab"),
                        "Visualize and download occurence records. Download species data in ", tags$a(href="https://www.geopackage.org/", "GeoPackage", target = "_blank"), " or in ", tags$a(href="https://desktop.arcgis.com/en/arcmap/latest/manage-data/shapefiles/what-is-a-shapefile.htm", "Shapefile", target = "_blank"), " format. If Shapefile option is selected, files will be provided in zipped archive.",
                        tags$h3("Bivariate projections"),
                        "Bivariate projections combine climate suitability data with model agreement data, indicating prediction certainty. Both suitability and certainty are reclassified into low-medium-high categories. There are 9 possible combinations of two variables:",
                        tags$br(),
                        tags$h4("Table values"),
                        tags$b("1:"), "Low Suitability - Low Certainty", tags$br(),
                        tags$b("2:"), "Low Suitability - Medium Certainty", tags$br(),
                        tags$b("3:"), "Low Suitability - High Certainty", tags$br(),
                        tags$b("4:"), "Medium Suitability - Low Certainty", tags$br(),
                        tags$b("5:"), "Medium Suitability - Medium Certainty", tags$br(),
                        tags$b("6:"), "Medium Suitability - High Certainty", tags$br(),
                        tags$b("7:"), "High Suitability - Low Certainty", tags$br(),
                        tags$b("8:"), "High Suitability - Medium Certainty", tags$br(),
                        tags$b("9:"), "High Suitability - High Certainty", tags$br(),
                        tags$h4("Raster values [TODO]"),tags$br(),
                        tags$h3("Individual projections"),
                        "Some description", 
                        tags$br(),tags$br(),tags$h4("Background"), 
                        "Some text on the project. Maybe even an abstract or something.",
                        tags$br(),tags$br(),tags$h4("Code"),
                        "Code and input data are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                        tags$br(),
                        "Workflow for the GitHub repo.", 
                        tags$br(), tags$br(), tags$h4("Referencing"),
                        "To cite this work, please use XXXXXX", tags$br(), tags$br(),
                        tags$br(),tags$br(),tags$h4("Authors"),
                        "Mirza Čengić", tags$a(href="mirzaceng@gmail.cpm", "email") ,tags$br(),
                        "Emina Šunje", tags$a(href="sunje.emina@gmail.cpm", "email"),tags$br(),
                        # tags$br(),tags$br(),tags$h4("Contact"),
                        # "mirzaceng@gmail.com", tags$a(href="mirzaceng@gmail.cpm", "email"), tags$br(),tags$br(),
                        # tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                      )  
                      
             ),
             #### Psp species data panel ####
             tabPanel("Species data",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("map", width = "100%", height = "100%"),
                          absolutePanel(top = 10, right = 10,
                                           selectizeInput("taxon", "Select taxon:",
                                                       # c("RCP85", "RCP26")
                                                       c(
                                                         "<i>Salamandra atra</i>"  = "points_all",
                                                         "<i>Salamandra atra atra</i>"  = "points_atra",
                                                         "<i>Salamandra atra prenjensis</i>"  = "points_prenjensis"
                                                       ),
                                                       # selected = "<i>Salamandra atra</i>",
                                                       # unique(data_points$subspecies),
                                                       multiple = FALSE,
                                                       options = list(
                                                         placeholder = 'Please select an option below',
                                                         onInitialize = I('function() { this.setValue(""); }'),
                                                         # Text in italics
                                                         render = I(
                                                           '{
                           item: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           },
                           option: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           }
  }'
                                                         )
                                                       )
                                        ),
                                        # checkboxInput("legend", "Show legend", FALSE),
                                        # Download geopackage
  radioButtons("download_format", "File format", 
               choices = c("GeoPackage",
                           "Shapefile"),
               inline = TRUE),
                                        # downloadButton("downloadData", "Download data as GeoPackage", width = "50"),
                                        # Download shapefile

                                        downloadButton("downloadData", "Download data")
                          )
                      )
             ),
             ############################# #
             ## P1 bivariate projections ####
             tabPanel("Bivariate projections",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("map_panel1", width = "100%", height = "100%"),
                          absolutePanel(top = 10, right = 10,
                                        selectizeInput("period_panel1", "Select time period:",
                                                       # c("Current"),
                                                       c("Current", "Future"),
                                                       options = list(
                                                         placeholder = 'Please select an option below',
                                                         onInitialize = I('function() { this.setValue(""); }')
                                                       )
                                        ),
                                        selectizeInput("taxon_panel1", "Select taxon:",
                                                       # c("RCP85", "RCP26")
                                                       c(
                                                         "<i>Salamandra atra</i>" = "points_all",
                                                         "<i>Salamandra atra atra</i>" = "points_atra",
                                                         "<i>Salamandra atra prenjensis</i>" = "points_prenjensis"
                                                       ),
                                                       # unique(data_points$subspecies),
                                                       options = list(
                                                         placeholder = 'Please select an option below',
                                                         onInitialize = I('function() { this.setValue(""); }'),
                                                         # Text in italics
                                                         render = I(
                                                           '{
                           item: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           },
                           option: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           }
  }'
                                                         )
                                                       )
                                        ),
                                        selectizeInput("scenario_panel1", "Select RCP scenario (future only):",
                                                       c(
                                                         "RCP2.6" = "rcp26",
                                                         "RCP8.5" = "rcp85"), selected = "rcp26",
                                                       # NULL,
                                                       options = list(
                                                         placeholder = 'For future time period only'
                                                       #   onInitialize = I('function() { this.setValue(""); }')
                                                       )
                                        ),
                                        selectizeInput("categories_panel1", "Subset bivariate categories:",
                                                       # c("rcp26", "rcp85"),
                                                       c(
                                                         "Low Suit - Low Cert" = "11",
                                                         "Low Suit - Med Cert" = "12",
                                                         "Low Suit - High Cert" = "13",
                                                         "Med Suit - Low Cert" = "21",
                                                         "Med Suit - Med Cert" = "22",
                                                         "Med Suit - High Cert" = "23",
                                                         "High Suit - Low Cert" = "31",
                                                         "High Suit - Med Cert" = "32",
                                                         "High Suit - High Cert" = "33"
                                                       ),
                                                       multiple = TRUE,
                                                       options = list(
                                                         placeholder = 'Filter categories',
                                                         onInitialize = I('function() { this.setValue(""); }')
                                                       )
                                        ),
                                        actionButton(
                                          "plot_panel1", "Render map"
                                        ),
                                        downloadButton("downloadBivariateRasterData", "Download data")
                                        
                          )
                          
                      )
             ),
             ######################################!
             #### P2 individual projections ####
             tabPanel("Individual projections",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          # leafletOutput("mymap", width="100%", height="100%")
                          leafletOutput("map_panel2", width = "100%", height = "100%"),
                          div(class = "selectt",
                              tags$head(includeCSS("styles.css")),
                              
                              absolutePanel(top = 10, right = 10, 
                                                selectizeInput("period_panel2", "Select time period:",
                                                           # "Current",
                                                           c("Current", "Future"),
                                                           options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            ),
                                            selectizeInput("taxon_panel2", "Select taxon:",
                                                           # c("RCP85", "RCP26")
                                                           # unique(data_points$subspecies),
                                                           c(
                                                             "<i>Salamandra atra</i>" = "points_all",
                                                             "<i>Salamandra atra atra</i>" = "points_atra",
                                                             "<i>Salamandra atra prenjensis</i>" = "points_prenjensis"
                                                           ),
                                                           options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }'),
                                                             # Text in italics
                                                             render = I(
                                                               '{
                           item: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           },
                           option: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           }
  }'
                                                             )
                                                           )
                                            ),
                                            selectizeInput("algorithm_panel2", "Select algorithm:",
                                                           c("GBM", "GLM", "GAM", "ANN", "CTA", "MARS", "RF", "MAXENT", "Ensemble" = "ensemble"),
                                                           options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            ),
                                            selectizeInput("scenario_panel2", "Select RCP scenario (future only):",
                                                           # c("rcp26", "rcp85"),
                                                           c(
                                                             "RCP2.6" = "rcp26",
                                                             "RCP8.5" = "rcp85"), selected = "rcp26"
                                                           # NULL,
                                                           # options = list(
                                                           #   placeholder = 'Relevant only for future time period',
                                                           #   onInitialize = I('function() { this.setValue(""); }')
                                                           # )
                                            ),
                                            selectizeInput("gcm_panel2", "Select GCM (no-choice currently):",
                                                           "CCSM4",
                                                           # c("rcp26", "rcp85"),
                                                           # options = list(
                                                           #   placeholder = 'Relevant only for future time period',
                                                           #   onInitialize = I('function() { this.setValue(""); }')
                                                           # )
                                                           # unique(data_points$subspecies)
                                            ),
                                            sliderInput("range", "Cell value range",
                                                        min = 0, max = 1,
                                                        value = c(0.01,1)
                                                        ),
                                            actionButton(
                                              "plot_panel2", "Render map"
                                            ),
                                            downloadButton("downloadRasterData", "Download data")
                                            
                              )
                          )
                      )
             )
            
             ####### Panel about; leave for now. Maybe add atra github repo workflow, makefile etc.
             
             # tabPanel("About this site",
             #          tags$div(
             #            tags$h4("Last update"), 
             #            h6(paste0(update)),
             #            tags$h2("Alpine salamander modeling"),
             #            tags$br(),tags$br(),tags$h4("Background"), 
             #            "Some text on the project. Maybe even an abstract or something.",
             #            tags$br(),tags$br(),tags$h4("Code"),
             #            "Code and input data are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
             #            tags$br(),tags$br(),tags$h4("Authors"),
             #            "Mirza Čengić", tags$a(href="mirzaceng@gmail.cpm", "email") ,tags$br(),
             #            "Emina Šunje", tags$a(href="sunje.emina@gmail.cpm", "email"),tags$br(),
             #            # tags$br(),tags$br(),tags$h4("Contact"),
             #            # "mirzaceng@gmail.com", tags$a(href="mirzaceng@gmail.cpm", "email"), tags$br(),tags$br(),
             #            # tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
             #          )
             # )
             
  )
)

