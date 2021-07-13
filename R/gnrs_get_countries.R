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
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
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
  
  
  # Send the request in a "graceful failure" wrapper for CRAN compliance
  tryCatch(expr = results_json <-  postForm(url, .opts=list(postfields= input_json, httpheader=headers)),
           error = function(e) {
             message("There appears to be a problem reaching the API.")
           })
  
  #Return NULL if API isn't working
  if(!exists("results_json")){return(invisible(NULL))}
  
  
  # Display the results
  results <- jsonlite::fromJSON(results_json)

  return(results)
  
} #GNRS version
