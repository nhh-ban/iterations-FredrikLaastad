library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)

GQL <- function(query,
                ...,
                .token = NULL,
                .variables = NULL,
                .operationName = NULL,
                .url = url) {
  pbody <-
    list(query = query,
         variables = .variables,
         operationName = .operationName)
  if (is.null(.token)) {
    res <- POST(.url, body = pbody, encode = "json", ...)
  } else {
    auth_header <- paste("bearer", .token)
    res <-
      POST(
        .url,
        body = pbody,
        encode = "json",
        add_headers(Authorization = auth_header),
        ...
      )
  }
  res <- content(res, as = "parsed", encoding = "UTF-8")
  if (!is.null(res$errors)) {
    warning(toJSON(res$errors))
  }
  res$data
}

# The URL we will use is stored below: 
url <- "https://www.vegvesen.no/trafikkdata/api/"


# Let's figure out which sensor stations that are operable. 
# The query below extracts all the stations, with a date for 
# when the station was in operation as well as a long/latitude. 
qry <-
  '
{
    trafficRegistrationPoints {
        id
        name
        latestData {
            volumeByDay
        }
        location {
            coordinates {
                latLon {
                    lat
                    lon
                }
            }
        }
    }
}
'

# Allright - let's try submitting the query: 
stations <-GQL(qry)


#Exercise

stasjoner <- stations[[1]] %>% 
  map(as_tibble) %>% 
  list_rbind() %>% 
  mutate(latestData = map_chr(latestData, 1, .default=NA_character_)) %>% 
  mutate(latestData = as_datetime(latestData, tz="UTC")) %>% 
  unnest_wider(location) %>% 
  unnest_wider(latLon)

