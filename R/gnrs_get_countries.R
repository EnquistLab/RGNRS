#'Get metadata on countries
#'
#'Return metadata about countries used by the GNRS
#' @return Dataframe containing information on countries (e.g. iso code, fips code, continent, standardized name)
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' countries <- GNRS_get_countries()
#' }
#' 
GNRS_get_countries <- function(){
  
  # api url
  url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # production
  #url = "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_api.php" # development
  
  # All we need to do is reset option mode.
  # all other options will be ignored
  mode <- "countrylist"		

  # Re-form the options json again
  # Note that only 'mode' is needed
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  opts_json <- jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Input json requires only the option
  input_json <- paste0('{"opts":', opts_json, '}' )
  
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  
  # Send the request again
  results_json <- postForm(url, .opts=list(postfields= input_json, httpheader=headers))
  
  # Display the results
  results <- jsonlite::fromJSON(results_json)

  return(results)
  
} #GNRS version
