#'Standardize political division names
#'
#'GNRS returns standardized political division names (according to geonames.org).
#' @param political_division_dataframe A properly formatted dataframe, see http://bien.nceas.ucsb.edu/bien/tools/gnrs/gnrs-api/
#' @return Dataframe containing GNRS results.
#' @import RCurl
#' @importFrom jsonlite fromJSON
#' @importFrom rjson toJSON
#' @export
#' @examples {
#' gnrs_testfile <- 
#' read.csv(system.file("extdata", "gnrs_testfile.csv", package = "GNRS", mustWork = TRUE),
#' stringsAsFactors = FALSE)
#'
#' results <- GNRS(political_division_dataframe = gnrs_testfile)
#'   
#' 
#' }
GNRS <- function(political_division_dataframe){
  
  ###############################################
  # Example use of GNRS API
  # By: Brad Boyle (bboyle@email.arizona.edu)
  # Date: Feb. 19, 2019
  ###############################################
  
  # URL for GNRS API
  url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_ws.php"
  #url = "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_ws.php" #development
  
  # Read in example file of political division names
  # See the BIEN website for details on how to organize a GNRS Batch Mode input file:
  # http://bien.nceas.ucsb.edu/bien/tools/gnrs/gnrs-input-format/
  
  
  # Inspect the input
  #head(political_division_dataframe ,10)
  
  # Convert to JSON
  obs_json <- rjson::toJSON(unname(split(political_division_dataframe, 1:nrow(political_division_dataframe))))
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  # Send the API request
  results_json <- postForm(url, .opts=list(postfields=obs_json, httpheader=headers))
  
  #str(results_json)
  
  
  
  # Convert JSON file to a data frame
  # This takes a bit of work
    #results <- fromJSON(results_json)
  results <- jsonlite::fromJSON(results_json)
    #results <- as.data.frame(do.call(rbind,results))
    #rownames(results) <- NULL	# Reset row numbers
  
  return(results)
  
}


