#### Atra Shiny app - Main script ####


## app_setup loads packages and loads data
source(here::here("R", "app_setup.R"))
## load server and ui part
source(here::here("R", "app_server.R"))
source(here::here("R", "app_ui.R"))

# Run app -----------------------------------------------------------------

## Launch in browser
# runApp(shinyApp(ui, server), launch.browser = TRUE)

## Launch via RStudio
shinyApp(ui, server)
# shinyApp(ui, server_new)
