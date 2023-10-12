library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)
library(glue)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)

#### 4: Query test
source("gql-queries/vol_qry.r")
GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

### 5: Final volume query: 

source("gql-queries/vol_qry.r")

stations_metadata_df %>%
  filter(latestData > Sys.Date() - days(7)) %>%
  sample_n(1) %$% {
    vol_qry(
      id = id,
      from = to_iso8601(.$latestData, -4),
      to = to_iso8601(.$latestData, 0)
    ) %>% 
      GQL(., .url = configs$vegvesen_url) %>%
      transform_volumes() %>%
      ggplot(aes(x = from, y = volume)) +
      geom_line() +
      theme_classic() +
      ggtitle("Name:", name)+
      labs(x = "Date", y = "Traffic")+
      geom_point(aes(color=volume))+
      theme(minimal,legend.position = "none")
  }
