#'Standardize political division names
#'
#'GNRS_super_simple returns standardized political division names (according to geonames.org).
#' @param country A single country or a vector of countries.  If a vector, length must equal length of species vector.
#' @param state_province A single state/province or a vector of states.  If a vector, length must equal length of species vector.
#' @param county_parish A single county/parish or a vector of counties.  If a vector, length must equal length of species vector.
#' @param user_id A single user id to be appended to results (optional).
#' @return Dataframe containing GNRS results.
#' @import RCurl  rjson
#' @export
#' @examples \dontrun{
#'
#'  results <- GNRS_super_simple(country = "United States of America")
#'  results <- GNRS_super_simple(
#'              country = "United States",
#'              state_province = "Arizona",
#'              county_parish = "Pima County")
#' 
#' }
GNRS_super_simple <- function(country=NULL, state_province=NULL,county_parish=NULL,user_id=NULL){
  
  
  #Check input for odd stuff
  
  if(!is.null(state_province) & !is.null(country)){
    if(length(country) != length(state_province)){stop("Country and state vectors should be the same length.")}
  }
  
  
  if(!is.null(state_province) & !is.null(county_parish)){
    if(length(county_parish) != length(state_province)){stop("State and county should be the same length.")}  
    
  }
  
  if(!is.null(county_parish) & !is.null((country))){
    if(length(country) != length(county_parish)){stop("Country and county vectors should be the same length.")}  
  }
  
  
  #Make template
  template<-GNRS_template(nrow = max(length(country),length(state_province),length(county_parish)))
  
  
  #Throw an error if user_id doesn't make sense
  if((nrow(template)!=length(user_id)) & (length(user_id)!=1) & (!is.null(user_id)) ){stop(" user_id should be either i) null, ii) of length one, or iii) have the same length as your country/state/county vectors.")}
  
  
  #Populate fields as needed
  
  
  #Country
  if(!is.null(country)){
    template$country <- country  
  }
  
  #State
  if(!is.null(state_province)){
    template$state_province <- state_province  
  }
  
  #County
  if(!is.null(county_parish)){
    template$county_parish <- county_parish  
  }
  
  #user_id
  if(!is.null(user_id)){
    template$user_id <- user_id  
  }
  
  
  
  
  return(GNRS(template))
  
  
}
############
