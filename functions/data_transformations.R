transform_metadata_to_df <- function(data){
  df <- data[[1]] %>% 
  map(as_tibble) %>% 
  list_rbind() %>% 
  mutate(latestData = map_chr(latestData, 1, .default=NA_character_)) %>% 
  mutate(latestData = as_datetime(latestData, tz="UTC")) %>% 
  unnest_wider(location) %>% 
  unnest_wider(latLon)
}

to_iso8601 <- function(dt, offset){
  return(paste0(iso8601(anytime(dt)+days(offset)), "Z"))
}

transform_volumes <-function(data){
  # Create an empty df
  df <- data_frame(
    from = as_datetime(character(0), tz = "UTC"),
    to = as_datetime(character(0), tz = "UTC"),
    volume = integer(0)
  )
  # Locate the relevant information
  edge_nodes <- data$trafficData$volume$byHour$edges
  # repeat for every row in the data
  for (i in edge_nodes) {
    x <- i %>% 
      # clean the data for one row
      map(as_tibble) %>% 
      list_rbind() %>% 
      mutate(from = map_chr(from,1, .default = NA_character_)) %>%
      mutate(from = as_datetime(from, tz = "UTC")) %>% 
      mutate(to = map_chr(to,1, .default = NA_character_)) %>%
      mutate(to = as_datetime(to, tz = "UTC")) %>% 
      unnest_wider(total)
    
    # Append the results
    df <- rbind(df, x)
  }
  return(df)
}

