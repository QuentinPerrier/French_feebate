source("code/feebate.R"); 


library(tidyverse)

setwd("d:/morvillier/Desktop/REVISION_BONUS_MALUS/I4CE/Bonus malus/3_Méthodologie/")


products <- readRDS("outputs/products.RDS")
products_2019 <- readRDS("outputs/products_2019.RDS")
# CO2 emission evolution -------------------------------------------------


#driving emission : 

get_real_driving_emissions<-function(motorization, NEDC_emissions){
  #' Emissions in gCO2/km with corrected driving emissions        
  
  HR_utility_factor <- 0.3  

  
  global_NEDC_gap <- 1.39   #Discrepancy between NEDC and real emissions. Source : ICCT "ON THE WAY TO “REAL-WORLD CO2 VALUES"
  hybrid_NEDC_gap <- (1 + HR_utility_factor)*global_NEDC_gap #Utility factor for hybrids = % of kilometers driven on electricity. Cf ICCT report "Real-world usage of plug-in hybrid"
  hybrid_NR_NEDC_gap <- global_NEDC_gap #Utility factor for non rechargeable hybrids = % of kilometers driven on electricity.
  
  driving_emissions <- case_when(
    motorization == "Hybride_rechargeable" ~ NEDC_emissions * hybrid_NEDC_gap,
    motorization == "Hybride_non_rechargeable" ~ NEDC_emissions * hybrid_NR_NEDC_gap,
    motorization == "Electrique" ~ 0,
    TRUE ~ NEDC_emissions * global_NEDC_gap,
  )
  
  driving_emissions
  
}  


products_2019$driving_real_emissions <- get_real_driving_emissions(products_2019$motorization, products_2019$co2)
  



get_real_emissions_LCA <- function(motorization, NEDC_emissions) {
  #' Emissions in gCO2/km with LCA. 
  
  car_production <- 25     #Emissions for car production, except for batteries. Cf T&E report, "How clean are electric cars?"
  
  driving_emissions <- get_real_driving_emissions(motorization, NEDC_emissions)
 
  battery_production <- 20 #Cf T&E report, "How clean are electric cars?"
  
  battery_emissions <- case_when(
    motorization == "Electrique" ~ battery_production,
    motorization == "Hybride_rechargeable" ~ battery_production * (1/3),        #HR batterie =1/3 VE batterie 
    motorization == "Hybride_non_rechargeable" ~ battery_production * (1/6),    #HNR batterie =1/2 HR batterie     
    TRUE ~ 0
  )
  
  real_emissions_LCA <- driving_emissions + battery_emissions + car_production  
  real_emissions_LCA
  
}

products_2019$LCA_real_emissions <- get_real_emissions_LCA(products_2019$motorization, products_2019$co2)

products_2019_co2 <- products_2019
saveRDS(products_2019_co2, "outputs/products_2019_co2.RDS")

#Graphic : Real emissions vs NEDC
products_2019 %>% 
  dplyr::select(product_id, motorization, co2, driving_real_emissions, LCA_real_emissions, volume) %>% 
  pivot_longer(cols = c(co2, driving_real_emissions, LCA_real_emissions), names_to = "measure_type", values_to = "emission_rate") %>% 
  group_by(motorization, measure_type) %>% 
  summarise(emission_rate = weighted.mean(emission_rate, volume)) %>% 
  mutate(motorization = str_replace_all(motorization, "_", " \n"),
         measure_type = case_when(
           measure_type == "co2" ~ "NEDC emissions",
           measure_type == "driving_real_emissions" ~ "real driving emissions",
           measure_type == "LCA_real_emissions" ~ "LCA emissions"
         )) %>%
  mutate(measure_type = factor(measure_type, levels = c("NEDC emissions", "real driving emissions", "LCA emissions")))%>%
  ggplot(aes(x = motorization, y = emission_rate, fill = measure_type)) + geom_bar(stat = "identity", position=position_dodge()) +
  labs(x = "Motorisation", y = "Taux d'émissions (gCO2/km)", fill = "Type de mesure") + theme_bw()
ggsave("outputs/plots/real_emissions.jpg", w = 8, h = 4)           



get_CO2 <- function(market_shares, emission_rates) {
  average_CO2_NEDC <- sum(market_shares * emission_rates) / sum(market_shares)
  return(average_CO2_NEDC)
}



bareme_2021 <- products_2019$bareme_2021
market_shares_2021 <- shares_evolution_with_feebate(bareme_2021)
get_CO2(products_2019$market_share_s1, products_2019$co2)
get_CO2(products_2019$shares_with_BM2019, products_2019$co2)

get_CO2(products_2019$shares_with_BM2019, products_2019$driving_real_emissions)




# Compute NEDC and real emissions for estimated shares --------------------


average_CO2_NEDC <- rep(0, length(2019:2023))
average_CO2_real_driving_emissions <- rep(0, length(2019:2023))
average_CO2_real_LCA_emissions <- rep(0, length(2019:2023))
for (i in 1:ncol(estimated_shares_with_feebate)) {
  average_CO2_NEDC[i] <- get_CO2(estimated_shares_with_feebate[[i]], products_2019$co2)
  average_CO2_real_driving_emissions[i] <- get_CO2(estimated_shares_with_feebate[[i]], products_2019$driving_real_emissions)
  average_CO2_real_LCA_emissions[i] <- get_CO2(estimated_shares_with_feebate[[i]], products_2019$LCA_real_emissions)
}


CO2_average_rates <- tibble(
  year = 2019:2023,
  NEDC = average_CO2_NEDC,
  Driving = average_CO2_real_driving_emissions,
  LCA = average_CO2_real_LCA_emissions
)


CO2_average_rates %>%
  pivot_longer(-year, names_to = "emission_types", values_to = "emissions") %>% 
  ggplot(aes(x = year, y = emissions, colour = emission_types)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Evolution of the Average Emissions Rate with current feebate")
ggsave("outputs/plots/Evolution of the Average Emissions Rate with current feebate.png", w = , h = 4)


