#'Standardize political division names
#'
#'GNRS returns standardized political division names (according to geonames.org).
#' @param political_division_dataframe A properly formatted dataframe, see http://bien.nceas.ucsb.edu/bien/tools/gnrs/gnrs-api/
#' @param batches NULL or Numeric.  Optional number of batches to divide the request into for parallel processing.
#' @return Dataframe containing GNRS results.
#' @note To create an empty and properly formatted dataframe, use GNRS_template()
#' @note The fields the GNRS takes as input are titled "country", "state_province", and "county_parish" for simplicity, but these field actually refer to 0th-, 1st-, and 2nd-order political division, respectively. In the case of some exceptions (e.g. the UK) this distinction becomes important (e.g. Ireland is a 1st-order political division and should be treated as a "state_province" and cannot be matched as a country.)
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON
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
GNRS <- function(political_division_dataframe, batches = NULL){
  

  # api url
  url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # production
  #url = "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_api.php" # development
  
  
  #Check that user_id is populated properly, and populate if not

  if(all(is.na(political_division_dataframe$user_id))){political_division_dataframe$user_id <- 1:nrow(political_division_dataframe)}
  if(any(duplicated(political_division_dataframe$user_id))){stop("user_id should be either null or populated by unique values")}         
  
  #check that batches makes sense
  
  #check that it its either NULL or numeric
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
  
  
  if ( !is.null(batches) ){ opts$batches <- batches }
  
  opts_json <-  toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Combine the options and data into single JSON object
  input_json <- paste0('{"opts":', opts_json, ',"data":', data_json, '}' )
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  # Send the API request
  results_json <- postForm(url, .opts=list(postfields= input_json, httpheader=headers))
  
  # Convert JSON results to a data frame
  results <-  fromJSON(results_json)
  
  #Re-order results to match original data
  results <- results[match(table = results$user_id,x = political_division_dataframe$user_id),]
  
  #reset the row numbers
  rownames(results) <- NULL 
  
  
  return(results)
  
}


