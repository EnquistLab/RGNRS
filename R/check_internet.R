#'Check whether the internet is on
#'
#'Check for internet
#' @return TRUE if internet connection is detected, FALSE otherwise.
#' @import RCurl
#' @keywords internal
check_internet <- function(){

    tryCatch(is.character(getURL("www.google.com")),
             error = function(e) {
               return(FALSE)
             })
  
}

