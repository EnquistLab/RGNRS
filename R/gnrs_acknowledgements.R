#'Get acknowledgment information
#'
#'Return information needed to acknowledge GNRS contributors
#' @return Dataframe containing acknowledgments
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' GNRS_acknowledgments_metadata <- GNRS_acknowledgments()
#' }
#' 
GNRS_acknowledgments <- function(){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }

  # api url
  url = "https://gnrsapi.xyz/gnrs_api.php" # public stable version
  #url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # public development production
  #url = "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_api.php" #bleeding edge development
  
  # set option mode.
  mode <- "collaborators"		
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  # Re-form the options json again
  # Note that only 'mode' is needed
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  opts_json <- jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  
  # Make the options
  # No data needed
  input_json <- paste0('{"opts":', opts_json, '}' )
  
  # Send the request in a "graceful failure" wrapper for CRAN compliance
  tryCatch(expr = results_json <- POST(url = url,
                                  add_headers('Content-Type' = 'application/json'),
                                  add_headers('Accept' = 'application/json'),
                                  add_headers('charset' = 'UTF-8'),
                                  body = input_json,
                                  encode = "json"
             )
           ,
           error = function(e) {
             message("There appears to be a problem reaching the API.")
           })
  
  #Return NULL if API isn't working
  if(!exists("results_json")){return(invisible(NULL))}
  
  # Format the results
  results <- as.data.frame( fromJSON( rawToChar( results_json$content ) ) )
  
  
  return(results)
  
}#GNRS sources
