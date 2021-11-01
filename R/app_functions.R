
# period <- "Future"
# taxon <- "points_all"
# scenario <- "rcp26"


make_bivar <- function(x, period, taxon, scenario, mode = "local", folder)
{
  
  mydf <- data.frame(
    ID = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    category = c("00", "11", "12", "13","21", "22", "23", "31", "32", "33")
  )
  
  if (period == "Current")
  {
    # output_filename <- str_glue("{folder_path_projections_bivariate}{taxon}_{period}_bivariate.tif")
    output_filename <- str_glue("{folder}/{taxon}_{period}_bivariate_leaflet_proj.tif")
  }
  if (period == "Future")
  {
    # output_filename <- str_glue("{folder_path_projections_bivariate}{taxon}_{period}_{scenario}_bivariate.tif")
    output_filename <- str_glue("{folder}/{taxon}_{period}_{scenario}_bivariate_leaflet_proj.tif")
  }
  
  if (mode == "local")
  {
    return(output_filename)
  }
  
  if (!file.exists(output_filename))
  {
    
    tic()  
    if (period == "Future")
    {
      my_stack <- x %>% 
        str_subset(taxon) %>% 
        str_subset(scenario) %>% 
        str_subset(period) %>% 
        stack()
      # return(nostack)
      
    }
    if (period == "Current")
    {
      my_stack <- x %>% 
        str_subset(taxon) %>% 
        str_subset(period) %>% 
        stack()
      # return(nostack)
    }
    
    
    my_valz <- getValues(my_stack) %>% 
      as.data.frame() %>% 
      rename(
        committee = 1,
        suitability = 2
      )
    
    bivar_values <- my_valz %>% 
      mutate(
        hr = suitability,
        pr = committee,
        hry = ifelse(hr< 0.33, 1, ifelse(hr< 0.66, 2, 3)),
        prx = ifelse(pr< 0.33 ,1, ifelse(pr< 0.66, 2, 3)),
        combined = str_c(hry, prx),
        # Subjective threshold of 0.1; FEATURE set as argument
        combined = ifelse(suitability < 0.1, "00", combined)
        # bivar = atan(hry/prx)
        # hry = ifelse(hr<h.v[1],1, ifelse(hr<h.v[2],2,3)),
        # prx = ifelse(pr<p.v[1],1, ifelse(pr<p.v[2],2,3))
      ) 
    my_values <- bivar_values %>% 
      left_join(mydf, by = c("combined" = "category")) %>% 
      pull(ID)    
    ####
    raster_bivar <- setValues(my_stack[[1]], my_values)
    raster_bivar[raster_bivar == 0] <- NA
    
    toc()
    
    writeRaster(raster_bivar, output_filename)
    
  }
  return(output_filename)
}



# Date function -----------------------------------------------------------

today <- function (format = "computer", sep = "_") 
{
  stopifnot(format %in% c("computer", "human"))
  if (format == "computer") {
    today_date <- format(Sys.Date(), paste0("%d", sep, "%m", 
                                            sep, "%Y"))
    return(today_date)
  }
  if (format == "human") {
    today_date <- format(Sys.time(), "%B %d, %Y.")
    return(today_date)
  }
}

