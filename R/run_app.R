#### Atra Shiny app setup ####

# app setup loads packages and loads data
# server and ui might be a bit outdated; most recent code is in the app liteS
source(here::here("R", "app_setup.R"))
source(here::here("R", "app_server.R"))
source(here::here("R", "app_ui.R"))





# Run app -----------------------------------------------------------------

runApp(shinyApp(ui, server), launch.browser = T)
# shinyApp(ui, server)
# shinyApp(ui, server_new)
