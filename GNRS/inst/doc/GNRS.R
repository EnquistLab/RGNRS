## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  
#  library(devtools)
#  install_github("EnquistLab/RGNRS/GNRS")
#  
#  

## ------------------------------------------------------------------------
library(GNRS)
GNRS_super_simple(country =  "United States", 
                 state_province = "Arizona",
                 county_parish = "Pima County")



## ------------------------------------------------------------------------

#First, we'll grab some occurrence records from BIEN
library(BIEN)
xs <- BIEN_occurrence_species(species = "Xanthium strumarium", political.boundaries = T)


#The GNRS function expects 4 columns as input, but all are optional.
#If you ever forget, you can use the function GNRS_template as a quick lookup, or to populate.

xs_query<-GNRS_template(nrow = nrow(xs))

xs_query$country <- xs$country
xs_query$state_province <- xs$state_province
xs_query$county_parish <- xs$county


xs_gnrs_results <- GNRS(xs_query)

head(xs_gnrs_results)


