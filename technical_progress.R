source("code/feebate.R"); 
source("code/CO2_emissions.R") ;
#source("code/norm.R");


library(tidyverse)
library(readxl)
library(janitor) #For clean_names



# Getting started -----------------------------------------------------------

products <- readRDS("outputs/products.RDS") 
products_sans_2020 <- readRDS("outputs/products_sans_2020.RDS")

products_2019 <- readRDS("outputs/products_2019_norm.RDS")





# TECHNICAL PROGRESS COEFFICIENTS  -----------------------------------------------------------------

get_tech_coeff <-function(year, motorization_technos){
  
  technical_progress <- read_excel("inputs/technical_progress.xlsx") %>% clean_names() %>% dplyr::select(-annual_technical_coefficient_change_2)
  
  starting_year <- 2019
  
  coef <- tibble(motorization = motorization_technos) %>% 
    left_join(., technical_progress, by = "motorization") %>% 
    mutate(total_change = (1+annual_technical_coefficient_change)^ifelse(year <= starting_year, 0, year - starting_year))
  
  return(coef$total_change)
  
}

get_tech_coeff(2020, "Electrique")


get_tech_coeff_advanced <-function(year, motorization_technos){
  
  technical_progress <- read_excel("inputs/technical_progress.xlsx") %>% clean_names() %>% dplyr::select(-annual_technical_coefficient_change)
  
  starting_year <- 2019
  
  coef <- tibble(motorization = motorization_technos) %>% 
    left_join(., technical_progress, by = "motorization") %>% 
    mutate(total_change = (1+annual_technical_coefficient_change_2)^ifelse(year <= starting_year, 0, year - starting_year))
  
  return(coef$total_change)
  
}

get_tech_coeff_advanced(2020, "Electrique")


# Market shares and Technical Progress -------------------------------------

get_average_price <- function(annee_filtre, motorization_filtre){
   data_temp <- products %>% 
     filter(annee == annee_filtre,
            motorization == motorization_filtre) 
   average_price <- weighted.mean(data_temp$prix, data_temp$volume)
   average_price
}

get_average_price(2020, "Hybride_rechargeable")
get_average_price(2019, "Hybride_rechargeable")
get_average_price(2018, "Hybride_rechargeable")
get_average_price(2017, "Hybride_rechargeable")
get_average_price(2016, "Hybride_rechargeable")
get_average_price(2015, "Hybride_rechargeable")




