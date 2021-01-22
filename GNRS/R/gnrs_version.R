#'Get metadata on current GNRS version
#'
#'Return metadata about the current GNRS version
#' @return Dataframe containing current GNRS version number, build date, and code version.
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples{
#' GNRS_version_metadata <- GNRS_version()
#' }
#' 
GNRS_version <- function(){
  
  # api url
  url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # production: /var/www/gnrs
  
  # set option mode.
  mode <- "meta"		
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  # Re-form the options json again
  # Note that only 'mode' is needed
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  opts_json <- toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Make the options
  # No data needed
  input_json <- paste0('{"opts":', opts_json, '}' )
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  # Send the request again
  results_json <- postForm(url, .opts=list(postfields= input_json, httpheader=headers))
  
  # Format the results
  results <- fromJSON(results_json)
  
  return(results)
  
}#TNRS version
