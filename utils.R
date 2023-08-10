library(httr)

apikey <- "INSERT_HERE"

get_partisanship <- function(username) {
  
  screen_name = username
  
  url <- paste("https://mescalc.p.rapidapi.com/account/",tolower(screen_name),sep='')
  
  response <- VERB("GET", url, add_headers('X-RapidAPI-Host' = 'mescalc.p.rapidapi.com', 'X-RapidAPI-Key' = apikey), content_type("application/octet-stream"))
  
  json_response <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  
  score <- json_response[["partisan_score"]]
  
  df <- data.frame (username  = screen_name,
                    ideo_score = score)
  
  return(df)
  
}