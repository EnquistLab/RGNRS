#'Make a template for a GNRS query
#'
#'GNRS_template builds a template that can be populated to submit a GNRS query.
#' @param nrow The number of rows to include in the template
#' @return Template data.frame that can be populated and then used in GNRS queries.
#' @export
#' @examples \dontrun{
#' 
#' template<-GNRS_template(nrow = 2)
#' template$country<-c("United Stapes","Mexico")
#' template$state_province<-c("Arizona","Sinalo")
#' GNRS(political_division_dataframe = template)  
#' 
#' }
GNRS_template <- function(nrow=1){
  
  
  template<-matrix(nrow = nrow, ncol= 4)
  template<-as.data.frame(template)
  colnames(template)<-c("user_id", "country", "state_province", "county_parish" )      
  return(template)  
  
}

