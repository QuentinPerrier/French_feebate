source("code/feebate.R"); 
source("code/CO2_emissions.R") ;
#source("code/norm.R");
source("code/technical_progress.R");


library(tidyverse)

# Getting started
products <- readRDS("outputs/products.RDS") 
products_sans_2020 <- readRDS("outputs/products_sans_2020.RDS")

products_2019 <- readRDS("outputs/products_2019_norm.RDS")

# Weight tax  -------------------------------------------------------------
get_bareme_malus_poids <- function(annee){
  data_temp <- products_2019 %>%
    mutate(poids_en_ordre_de_marche = poids + 75,
           bareme = 0) 
  if(annee == 2019){
    data_temp$bareme <- data_temp$bareme_2019
  } else if(annee == 2020){
    data_temp$bareme <- data_temp$bareme_2020
  } else if(annee == 2021){
    data_temp$bareme <- data_temp$bareme_2021
  } else if(annee == 2022){
    data_temp$bareme <- data_temp$bareme_2022
  } else if(annee == 2023){
    data_temp$bareme <- data_temp$bareme_2023
  } else {
    data_temp$bareme <- data_temp$bareme
  }
  
  data_temp <- data_temp %>%
    mutate(bareme_malus_poids = case_when(
      (bas_carbone != 1 & poids_en_ordre_de_marche > 1799 & (bareme + (poids_en_ordre_de_marche - 1799)*10) < max(bareme) & (poids_en_ordre_de_marche - 1799)*10 < 10000) ~ (poids_en_ordre_de_marche - 1799)*10,
      (bas_carbone != 1 & poids_en_ordre_de_marche > 1799 & (bareme + (poids_en_ordre_de_marche - 1799)*10) < max(bareme) & (poids_en_ordre_de_marche - 1799)*10 >= 10000) ~ 10000,
      (bas_carbone != 1 & poids_en_ordre_de_marche > 1799 & bareme == max(bareme)) ~ 0,
      (bas_carbone != 1 & poids_en_ordre_de_marche > 1799 & (bareme + (poids_en_ordre_de_marche - 1799)*10) > max(bareme)) & (poids_en_ordre_de_marche - 1799)*10 < 10000 ~ bareme - (poids_en_ordre_de_marche - 1799)*10,
      (bas_carbone != 1 & poids_en_ordre_de_marche > 1799 & (bareme + (poids_en_ordre_de_marche - 1799)*10) > max(bareme)) & (poids_en_ordre_de_marche - 1799)*10 > 10000 ~ 10000,
      TRUE ~ 0
    ))
  
  return(data_temp$bareme_malus_poids)
}



products_2019$weight_tax_2019 <- rep(0, length(products_2019$bareme_2019))  # the weight tax takes effect from 2022
products_2019$weight_tax_2020 <- rep(0, length(products_2019$bareme_2020))  # "                                                   "
products_2019$weight_tax_2021 <- rep(0, length(products_2019$bareme_2021))  # "                                                   " 
products_2019$weight_tax_2022 <- get_bareme_malus_poids(2022)
products_2019$weight_tax_2023 <- get_bareme_malus_poids(2023)
products_2019$weight_tax_2024 <- get_bareme_malus_poids(2024)               # the government did not gave feebates for the years 2024 and 2025
products_2019$weight_tax_2025 <- get_bareme_malus_poids(2025)               # 

shares_evolution_with_feebate_and_weight_tax <- function(feebate, weight_tax){
  #' returns market shares according to a feebate rate and a weight tax applied to the park of vehicle of 2019
  data <- products_2019 %>% 
    mutate(price_with_new_feebate = (prix_final + feebate + weight_tax - yearly_adjusted_bareme) / 10000)
  
  delta_with_residuals <- products_2019 %>% .$delta_with_resids
  
  out <- share_nested(data$price_with_new_feebate, delta_with_residuals)
  
  return(out)
}

products_2019$shares_weight_and_feebate2019 <- shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2019, products_2019$weight_tax_2019)
products_2019$shares_weight_and_feebate2020 <- shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2020, products_2019$weight_tax_2020)
products_2019$shares_weight_and_feebate2021 <- shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2021, products_2019$weight_tax_2021)
products_2019$shares_weight_and_feebate2022 <- shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2022, products_2019$weight_tax_2022)
products_2019$shares_weight_and_feebate2023 <- shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2023, products_2019$weight_tax_2023)


products_2019_weight <- products_2019
saveRDS(products_2019_weight, "outputs/products_2019_weight.RDS")




#  number of vehicles concerned by the weight tax in 2019: 68 
products_2019 %>%
  mutate(temp = case_when(
    weight_tax_2019 !=0 ~ 1, 
    TRUE ~ 0)) %>%
  summarise(nombre_vehicules_taxes = sum(temp)) 


# number of vehicles sales concerned by the weight tax in 2019: 51246
products_2019 %>%
  mutate(temp = case_when(
    weight_tax_2019 !=0 ~ 1, 
    TRUE ~ 0)) %>%
  summarise(nombre_ventes_vehicules_taxes = sum(temp * volume))

## market shares 
products_2019 %>%
  mutate(temp = case_when(
    weight_tax_2019 !=0 ~ 1, 
    TRUE ~ 0)) %>%
  summarise(nombre_vehicules_taxes_BM_and_weight = sum(shares_weight_and_feebate2019*temp))

sum(products_2019$volume)




### Generated revenues 

products_2019 %>%
  mutate(temp = weight_tax_2019 * volume) %>%
  summarise(revenu_malus = sum(temp))



### Market shares of thermic vehicles below the threshold 


fuel_engines_shares <- function(seuil, market_share){
  data <- products_2019 %>%
    mutate(market_share_chosen = market_share) %>%
    filter(motorization != "Electrique", motorization != "Hybride_non_rechargeable", motorization != "Hybride_rechargeable") %>%
    mutate(poids_en_ordre_de_marche = poids + 75) %>%
    filter(poids_en_ordre_de_marche < seuil)
  fuel_engines <- sum(data$market_share_chosen)
  return(fuel_engines)
}


fuel_engines_shares(1799, products_2019$shares_with_BM2019)
fuel_engines_shares(1399, products_2019$shares_with_BM2019)
fuel_engines_shares(1199, products_2019$shares_with_BM2019)



# Graphics ----------------------------------------------------
# graphic NEDC emissions for thermic vehicles according to the weight 



products_2019 %>%
  filter(motorization != "Electrique", motorization != "Hybride_rechargeable", motorization != "Hybride_non_rechargeable", motorization != "Autres") %>%
  ggplot(aes(x = poids , y = co2)) + geom_point() + labs(x = "Poids en kg", y = "Emissions en gCO2/km", colour = "Motorisation", title = "Poids des véhicules thermiques et émissions NEDC") + 
  theme_bw() + ylim(50, 700) + geom_smooth(method = 'lm', se = FALSE)
ggsave("outputs/plots/Répartition du poids des véhicules thermiques par émissions NEDC de CO2.png", w= 5, h = 5)


# Graphic : Real emissions for thermic vehicles in function of the weight 
products_2019 %>%
  filter(motorization != "Electrique", motorization != "Hybride_rechargeable", motorization != "Hybride_non_rechargeable", motorization != "Autres") %>%
  ggplot(aes(x = poids, y = real_emissions)) + geom_point(color='darkblue') + labs(x = "Poids en kg", y = "Emissions en gCO2/km", title = "Poids des véhicules thermiques et émissions réelles") + 
  theme_bw() + ylim(50, 700) + geom_smooth(method = 'lm', se = FALSE)
ggsave("outputs/plots/Répartition du poids des véhicules thermiques par émissions réelles de CO2.png", w= 5, h = 5)



# Graphic representing the evolution of market shares with feebates and weigh tax
products_2019 %>% 
  dplyr::select(product_id, motorization, shares_weight_and_feebate2019, shares_weight_and_feebate2020, shares_weight_and_feebate2021, shares_weight_and_feebate2022, shares_weight_and_feebate2023) %>% 
  pivot_longer(cols = -c(product_id, motorization),
               names_to = "year",
               values_to = "market_shares") %>% 
  mutate(year = ifelse(year == "market_share_s1", 2019, year),
         year = str_remove_all(year, "shares_weight_and_feebate"),
         year = as.integer(year)) %>% 
  group_by(year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Evolution des parts de marché avec les barèmes prévus (sans norme)") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)
ggsave("outputs/plots/market_shares_evolution_BM_and_weight_by_motorization.png", w = , h = 4)



# graphique representant l'évolution des parts avec le malus poids uniquement 

shares_evolution_weight_tax <- function(weight_tax){
  #' returns market shares according to a feebate rate and a weight tax applied to the park of vehicle of 2019
  data <- products_2019 %>% 
    mutate(price_with_new_weight_tax = (prix_final + weight_tax - yearly_adjusted_bareme) / 10000)
  
  delta_with_residuals <- products_2019 %>% .$delta_with_resids
  
  out <- share_nested(data$price_with_new_weight_tax, delta_with_residuals)
  
  return(out)
}

products_2019$shares_weight2019 <- shares_evolution_weight_tax(products_2019$weight_tax_2019)
products_2019$shares_weight2020 <- shares_evolution_weight_tax(products_2019$weight_tax_2020)
products_2019$shares_weight2021 <- shares_evolution_weight_tax(products_2019$weight_tax_2021)
products_2019$shares_weight2022 <- shares_evolution_weight_tax(products_2019$weight_tax_2022)
products_2019$shares_weight2023 <- shares_evolution_weight_tax(products_2019$weight_tax_2023)


products_2019 %>% 
  dplyr::select(product_id, motorization, shares_weight2019, shares_weight2020, shares_weight2021, shares_weight2022, shares_weight2023) %>% 
  pivot_longer(cols = -c(product_id, motorization),
               names_to = "year",
               values_to = "market_shares") %>% 
  mutate(year = ifelse(year == "market_share_s1", 2019, year),
         year = str_remove_all(year, "shares_weight"),
         year = as.integer(year)) %>% 
  group_by(year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Impact du malus poids sur l'évolution des parts de marché") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)
ggsave("outputs/plots/market_shares_evolution_weight_by_motorization.png", w = , h = 4)


# Graphic of the evoluation of market with a weight tax declenching to a weight of 1400 kgs

get_bareme_malus_poids_kilo <- function(annee, seuil){
  data_temp <- products_2019 %>%
    mutate(poids_en_ordre_de_marche = poids + 75,
           bareme = 0) 
  if(annee == 2019){
    data_temp$bareme <- data_temp$bareme_2019
  } else if(annee == 2020){
    data_temp$bareme <- data_temp$bareme_2020
  } else if(annee == 2021){
    data_temp$bareme <- data_temp$bareme_2021
  } else if(annee == 2022){
    data_temp$bareme <- data_temp$bareme_2022
  } else if(annee == 2023){
    data_temp$bareme <- data_temp$bareme_2023
  } else {
    data_temp$bareme <- data_temp$bareme
  }
  
  data_temp <- data_temp %>%
    mutate(bareme_malus_poids = case_when(
      (bas_carbone != 1 & poids_en_ordre_de_marche > seuil & (bareme + (poids_en_ordre_de_marche - seuil)*10) < max(bareme) & (poids_en_ordre_de_marche - seuil)*10 < 10000) ~ (poids_en_ordre_de_marche - seuil)*10,
      (bas_carbone != 1 & poids_en_ordre_de_marche > seuil & (bareme + (poids_en_ordre_de_marche - seuil)*10) < max(bareme) & (poids_en_ordre_de_marche - seuil)*10 >= 10000) ~ 10000,
      (bas_carbone != 1 & poids_en_ordre_de_marche > seuil & bareme == max(bareme)) ~ 0,
      (bas_carbone != 1 & poids_en_ordre_de_marche > seuil & (bareme + (poids_en_ordre_de_marche - seuil)*10) > max(bareme)) & (poids_en_ordre_de_marche - seuil)*10 < 10000 ~ bareme - (poids_en_ordre_de_marche - seuil)*10,
      (bas_carbone != 1 & poids_en_ordre_de_marche > seuil & (bareme + (poids_en_ordre_de_marche - seuil)*10) > max(bareme)) & (poids_en_ordre_de_marche - seuil)*10 > 10000 ~ 10000,
      TRUE ~ 0
    ))
  
  return(data_temp$bareme_malus_poids)
}


products_2019 %>%
  mutate(sw2019 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2019, 1400)),
         sw2020 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2020, 1400)),
         sw2021 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2021, 1400)),
         sw2022 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2022, 1400)),
         sw2023 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2023, 1400))) %>%
  dplyr::select(product_id, motorization, sw2019, sw2020, sw2021, sw2022, sw2023) %>% 
  pivot_longer(cols = -c(product_id, motorization),
               names_to = "year",
               values_to = "market_shares") %>% 
  mutate(year = ifelse(year == "market_share_s1", 2019, year),
         year = str_remove_all(year, "sw"),
         year = as.integer(year)) %>% 
  group_by(year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Impact du malus poids 1400kg sur l'évolution des parts de marché") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)
ggsave("outputs/plots/market_shares_evolution_weight_tax_1400_by_motorization.png", w = , h = 4)



# Graphic of the evoluation of market with a weight tax declenching for a weight of 1200 kgs

products_2019 %>%
  mutate(sw2019 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2019, 1199)),
         sw2020 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2020, 1199)),
         sw2021 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2021, 1199)),
         sw2022 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2022, 1199)),
         sw2023 = shares_evolution_weight_tax(get_bareme_malus_poids_kilo(2023, 1199))) %>%
  dplyr::select(product_id, motorization, sw2019, sw2020, sw2021, sw2022, sw2023) %>% 
  pivot_longer(cols = -c(product_id, motorization),
               names_to = "year",
               values_to = "market_shares") %>% 
  mutate(year = ifelse(year == "market_share_s1", 2019, year),
         year = str_remove_all(year, "sw"),
         year = as.integer(year)) %>% 
  group_by(year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Impact du malus poids 1200kg sur l'évolution des parts de marché") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)
ggsave("outputs/plots/market_shares_evolution_weight_tax_1400_by_motorization.png", w = , h = 4)



# Graphic of the evoluation of market with a weight tax declenching for a weight of 1400 kgs

products_2019 %>%
  mutate(shares_weight_1400_and_bm2019 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2019, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2019, 1399)),
         shares_weight_1400_and_bm2020 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2020, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2020, 1399)),
         shares_weight_1400_and_bm2021 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2021, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2021, 1399)),
         shares_weight_1400_and_bm2022 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2022, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2022, 1399)),
         shares_weight_1400_and_bm2023 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2023, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2023, 1399))) %>%
  dplyr::select(product_id, motorization, shares_weight_1400_and_bm2019, shares_weight_1400_and_bm2020, shares_weight_1400_and_bm2021, shares_weight_1400_and_bm2022, shares_weight_1400_and_bm2023)%>% 
  pivot_longer(cols = -c(product_id, motorization),
               names_to = "year",
               values_to = "market_shares") %>% 
  mutate(year = ifelse(year == "market_share_s1", 2019, year),
         year = str_remove_all(year, "shares_weight_1400_and_bm"),
         year = as.integer(year)) %>% 
  group_by(year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Evolution des parts de marché avec les barèmes prévus (sans norme)") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)
ggsave("outputs/plots/market_shares_evolution_BM_and_weight_1400_by_motorization.png", w = , h = 4)

# Graphic of the evoluation of market with a weight tax declenching for a weight of 1200 kgs

products_2019 %>%
  mutate(shares_weight_1200_and_bm2019 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2019, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2019, 1199)),
         shares_weight_1200_and_bm2020 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2020, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2020, 1199)),
         shares_weight_1200_and_bm2021 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2021, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2021, 1199)),
         shares_weight_1200_and_bm2022 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2022, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2022, 1199)),
         shares_weight_1200_and_bm2023 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2023, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2023, 1199))) %>%
  dplyr::select(product_id, motorization, shares_weight_1200_and_bm2019, shares_weight_1200_and_bm2020, shares_weight_1200_and_bm2021, shares_weight_1200_and_bm2022, shares_weight_1200_and_bm2023)%>% 
  pivot_longer(cols = -c(product_id, motorization),
               names_to = "year",
               values_to = "market_shares") %>% 
  mutate(year = ifelse(year == "market_share_s1", 2019, year),
         year = str_remove_all(year, "shares_weight_1200_and_bm"),
         year = as.integer(year)) %>% 
  group_by(year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Evolution des parts de marché avec les barèmes prévus (sans norme)") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)
ggsave("outputs/plots/market_shares_evolution_BM_and_weight_1200_by_motorization.png", w = , h = 4)

  



## Graphic showing evolution of market shares without weight tax, with weight tax with different thresholds


grid.arrange(products_2019 %>% 
               dplyr::select(product_id, motorization, shares_with_BM2019, shares_with_BM2020, shares_with_BM2021, shares_with_BM2022, shares_with_BM2023) %>% 
               pivot_longer(cols = -c(product_id, motorization),
                            names_to = "year",
                            values_to = "market_shares") %>% 
               mutate(year = ifelse(year == "market_share_s1", 2019, year),
                      year = str_remove_all(year, "shares_with_BM"),
                      year = as.integer(year)) %>% 
               group_by(year, motorization) %>% 
               summarise(market_shares = sum(market_shares)) %>% 
               ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
               labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Evolution des parts de marché avec Bonus-Malus") + 
               theme_bw() + 
               scale_y_continuous(labels = scales::percent),
             products_2019 %>% 
               dplyr::select(product_id, motorization, shares_weight_and_feebate2019, shares_weight_and_feebate2020, shares_weight_and_feebate2021, shares_weight_and_feebate2022, shares_weight_and_feebate2023) %>% 
               pivot_longer(cols = -c(product_id, motorization),
                            names_to = "year",
                            values_to = "market_shares") %>% 
               mutate(year = ifelse(year == "market_share_s1", 2019, year),
                      year = str_remove_all(year, "shares_weight_and_feebate"),
                      year = as.integer(year)) %>% 
               group_by(year, motorization) %>% 
               summarise(market_shares = sum(market_shares)) %>% 
               ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
               labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Evolution des parts de marché avec Bonus-Malus et Malus poids 1800 kg") + 
               theme_bw() + 
               scale_y_continuous(labels = scales::percent),
             products_2019 %>% # graphique evolution shares avec feebate et malus poids pour 1400 kilogramme
               mutate(shares_weight_1400_and_bm2019 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2019, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2019, 1399)),
                      shares_weight_1400_and_bm2020 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2020, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2020, 1399)),
                      shares_weight_1400_and_bm2021 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2021, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2021, 1399)),
                      shares_weight_1400_and_bm2022 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2022, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2022, 1399)),
                      shares_weight_1400_and_bm2023 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2023, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2023, 1399))) %>%
               dplyr::select(product_id, motorization, shares_weight_1400_and_bm2019, shares_weight_1400_and_bm2020, shares_weight_1400_and_bm2021, shares_weight_1400_and_bm2022, shares_weight_1400_and_bm2023)%>% 
               pivot_longer(cols = -c(product_id, motorization),
                            names_to = "year",
                            values_to = "market_shares") %>% 
               mutate(year = ifelse(year == "market_share_s1", 2019, year),
                      year = str_remove_all(year, "shares_weight_1400_and_bm"),
                      year = as.integer(year)) %>% 
               group_by(year, motorization) %>% 
               summarise(market_shares = sum(market_shares)) %>% 
               ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
               labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Evolution des parts de marché avec Bonus-Malus et Malus poids 1400 kg") + 
               theme_bw() + 
               scale_y_continuous(labels = scales::percent),
             products_2019 %>%
               mutate(shares_weight_1200_and_bm2019 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2019, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2019, 1199)),
                      shares_weight_1200_and_bm2020 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2020, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2020, 1199)),
                      shares_weight_1200_and_bm2021 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2021, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2021, 1199)),
                      shares_weight_1200_and_bm2022 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2022, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2022, 1199)),
                      shares_weight_1200_and_bm2023 = shares_evolution_with_feebate_and_weight_tax(products_2019$bareme_2023, get_bareme_malus_poids(products_2019$bas_carbone, products_2019$bareme_2023, 1199))) %>%
               dplyr::select(product_id, motorization, shares_weight_1200_and_bm2019, shares_weight_1200_and_bm2020, shares_weight_1200_and_bm2021, shares_weight_1200_and_bm2022, shares_weight_1200_and_bm2023)%>% 
               pivot_longer(cols = -c(product_id, motorization),
                            names_to = "year",
                            values_to = "market_shares") %>% 
               mutate(year = ifelse(year == "market_share_s1", 2019, year),
                      year = str_remove_all(year, "shares_weight_1200_and_bm"),
                      year = as.integer(year)) %>% 
               group_by(year, motorization) %>% 
               summarise(market_shares = sum(market_shares)) %>% 
               ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
               labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Evolution des parts de marché avec Bonus-Malus et Malus poids 1200 kg") + 
               theme_bw() + 
               scale_y_continuous(labels = scales::percent),
             nrow = 2)


#graphique ayant pour ambition de representer l'évolution des parts avec feebate et bareme poids actuel : à finir
products_2019_graphique_2 <- products_2019 %>%
  dplyr::select(product_id, motorization, shares_with_BM2019, shares_weight_and_feebate2019, shares_with_BM2020, shares_weight_and_feebate2020, shares_with_BM2021, shares_weight_and_feebate2021, shares_with_BM2022, shares_weight_and_feebate2022, shares_with_BM2023, shares_weight_and_feebate2023) %>%
  pivot_longer(cols = -c(product_id, motorization), 
               names_to = "year", 
               values_to = "market_shares") %>%
  mutate(Bareme = case_when(
    year == "shares_with_BM2019" ~ "Bonus-Malus",
    year == "shares_with_BM2020" ~ "Bonus-Malus",
    year == "shares_with_BM2021" ~ "Bonus-Malus",
    year == "shares_with_BM2022" ~ "Bonus-Malus",
    year == "shares_with_BM2023" ~ "Bonus-Malus",
    year == "shares_weight_and_feebate2019" ~ "Bonus-Malus et Malus Poids",
    year == "shares_weight_and_feebate2020" ~ "Bonus-Malus et Malus Poids",
    year == "shares_weight_and_feebate2021" ~ "Bonus-Malus et Malus Poids",
    year == "shares_weight_and_feebate2022" ~ "Bonus-Malus et Malus Poids",
    year == "shares_weight_and_feebate2023" ~ "Bonus-Malus et Malus Poids")) %>%
  mutate(year = ifelse(year == "market_share_s1", 2019, year)) %>% 
  mutate(year = case_when(
    year == "shares_with_BM2019" ~ 2019,
    year == "shares_with_BM2020" ~ 2020,
    year == "shares_with_BM2021" ~ 2021,
    year == "shares_with_BM2022" ~ 2022,
    year == "shares_with_BM2023" ~ 2023,
    year == "shares_weight_and_feebate2019" ~ 2019,
    year == "shares_weight_and_feebate2020" ~ 2020,
    year == "shares_weight_and_feebate2021" ~ 2021,
    year == "shares_weight_and_feebate2022" ~ 2022,
    year == "shares_weight_and_feebate2023" ~ 2023)) %>%
  mutate(year = as.integer(year)) %>%a
  group_by( Bareme, year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Evolution des parts de marché avec les barèmes bonus-malus et poids") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)

products_2019_graphique_2



