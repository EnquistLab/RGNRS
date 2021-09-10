#'Standardize political division names
#'
#'GNRS returns standardized political division names (according to geonames.org).
#' @param political_division_dataframe A properly formatted dataframe, see http://bien.nceas.ucsb.edu/bien/tools/gnrs/gnrs-api/
#' @param batches NULL or Numeric.  Optional number of batches to divide the request into for parallel processing.
#' @return Dataframe containing GNRS results.
#' @note To create an empty and properly formatted dataframe, use GNRS_template()
#' @note The fields the GNRS takes as input are titled "country", "state_province", and "county_parish" for simplicity, but these field actually refer to 0th-, 1st-, and 2nd-order political division, respectively. In the case of some exceptions (e.g. the UK) this distinction becomes important (e.g. Ireland is a 1st-order political division and should be treated as a "state_province" and cannot be matched as a country.)
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples\dontrun{
#' results <- GNRS(political_division_dataframe = gnrs_testfile)
#'   
#' 
#' }
GNRS <- function(political_division_dataframe, batches = NULL){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  

  # api url
  url = "https://gnrsapi.xyz/gnrs_api.php" # public stable version
  #url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # public development production
  #url = "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_api.php" #bleeding edge development
  
  #check that input is a data.frame
  if(!inherits(political_division_dataframe,"data.frame")){
    stop("political_division_dataframe should be a data.frame")
  }
  
  
  #Check that user_id is populated properly, and populate if not

  if(all(is.na(political_division_dataframe$user_id))){political_division_dataframe$user_id <- 1:nrow(political_division_dataframe)}
  if(any(duplicated(political_division_dataframe$user_id))){stop("user_id should be either null or populated by unique values")}         
  
  #check that batches makes sense
  
  #check that batches is either NULL or numeric
  if(!is.null(batches) & !is.numeric(batches)){stop("Argument 'batches' must be either NULL or a positive integer. ")}
  
  #check that it is a whole number greater than zero
  if(is.numeric(batches)){ 
    
    
    if(batches<=0 | batches%%1!=0){stop("Argument 'batches' must be either NULL or a positive integer.")}
    
    }
  
  # Convert the data to JSON
  data_json <- toJSON(unname(political_division_dataframe))
  
  #################################
  # Example 1: Resolve
  #################################
  
  # Set API options
  mode <- "resolve"			# Processing mode
  
  # Convert the options to data frame and then JSON
  opts <- data.frame( c(mode) )
  names(opts) <- c("mode")
  if ( exists("batches") ) opts$batches <- batches
  
  opts_json <-  jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Combine the options and data into single JSON object
  input_json <- paste0('{"opts":', opts_json, ',"data":', data_json, '}' )
  
  # Send the API request
  
  
  # Send the request in a "graceful failure" wrapper for CRAN compliance
  tryCatch(expr = results_json <- POST(url = url,
                                       add_headers('Content-Type' = 'application/json'),
                                       add_headers('Accept' = 'application/json'),
                                       add_headers('charset' = 'UTF-8'),
                                       body = input_json,
                                       encode = "json"
                                       ),
           error = function(e) {
             message("There appears to be a problem reaching the API.")
           })
  
  #Return NULL if API isn't working
  if(!exists("results_json")){return(invisible(NULL))}

  # Convert JSON results to a data frame
  results_raw <- fromJSON(rawToChar(results_json$content)) 
  results <- as.data.frame(results_raw)
  
  #Re-order results to match original data
  results <- results[match(table = results$user_id,
                           x = political_division_dataframe$user_id),]
  
  #reset the row numbers
  rownames(results) <- NULL 
  
  
  return(results)
  
}


