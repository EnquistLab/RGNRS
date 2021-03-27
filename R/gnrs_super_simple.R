#'Standardize political division names
#'
#'GNRS_super_simple returns standardized political division names (according to geonames.org).
#' @param country A single country or a vector of countries.  If a vector, length must equal length of species vector.
#' @param state_province A single state/province or a vector of states.  If a vector, length must equal length of species vector.
#' @param county_parish A single county/parish or a vector of counties.  If a vector, length must equal length of species vector.
#' @param user_id A single identifier or vector of identifiers.  This field is assigned if not provided and is used to maintain record order.
#' @note The fields the GNRS takes as input are titled "country", "state_province", and "county_parish" for simplicity, but these field actually refer to 0th-, 1st-, and 2nd-order political division, respectively. In the case of some exceptions (e.g. the UK) this distinction becomes important (e.g. Ireland is a 1st-order political division and should be treated as a "state_province" and cannot be matched as a country.)
#' @return Dataframe containing GNRS results.
#' @export
#' @examples  \dontrun{
#'
#'  results <- GNRS_super_simple(country = "United States of America")
#'  results <- GNRS_super_simple(
#'              country = "United States",
#'              state_province = "Arizona",
#'              county_parish = "Pima County")
#' 
#' }
GNRS_super_simple <- function(country = NULL, state_province = NULL, county_parish = NULL, user_id = NULL ){
  
  
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
  template <- GNRS_template(nrow = max(length(country),length(state_province),length(county_parish)))
  
  
  #Throw an error if user_id doesn't make sense
  if((nrow(template)!=length(user_id)) &  (!is.null(user_id)) ){stop(" user_id should be either i) null or ii) have the same length as your country/state/county vectors.")}
  if(!is.null(user_id)){  if(any(duplicated(user_id))){stop("user_id should be either null or populated by unique values")}         }
  
  
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
  }else{template$user_id <- 1:nrow(template)}

  
  #Get GNRS output
  GNRS_out <- GNRS(template)
  
  
  #Re-order output to match what was submitted #no longer needed since this is done by the GNRS function
  #GNRS_out <- GNRS_out[match(table = GNRS_out$user_id,x = template$user_id),]
  
  #Return output
  return(GNRS_out)
  
}
############
