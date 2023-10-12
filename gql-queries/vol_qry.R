vol_qry <- function(id, from, to) {
  query <- glue('
    {{
      trafficData(trafficRegistrationPointId: "{id}") {{
        volume {{
          byHour(from: "{from}", to: "{to}") {{
            edges {{
              node {{
                from
                to
                total {{
                  volumeNumbers {{
                    volume
                  }}
                }}
              }}
            }}
          }}
        }}
      }}
    }}',
                id = id,
                from = from,
                to = to
  )
  
  return(query)
}