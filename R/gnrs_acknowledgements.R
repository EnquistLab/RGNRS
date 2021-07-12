#'Get acknowledgment information
#'
#'Return information needed to acknowledge GNRS contributors
#' @return Dataframe containing acknowledgments
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' GNRS_acknowledgments_metadata <- GNRS_acknowledgments()
#' }
#' 
GNRS_acknowledgments <- function(){
  
  # Check for internet access
  if (!is.character(getURL("www.google.com"))) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  # api url
  #url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # production
  url = "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_api.php" # development
  
  # set option mode.
  mode <- "collaborators"		
  
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
  
  # Send the request in a "graceful failure" wrapper for CRAN compliance
  tryCatch(expr =results_json <-  postForm(url, .opts=list(postfields= input_json, httpheader=headers)),
           error = function(e) {
             message("There appears to be a problem reaching the API.") 
             return(NULL)
           })
  
  #Return NULL if API isn't working
  if(is.null(results_json)){return(invisible(NULL))}
  
  
  # Format the results
  results <- fromJSON(results_json)
  
  return(results)
  
}#GNRS sources
