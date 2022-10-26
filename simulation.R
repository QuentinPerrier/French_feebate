source("code/feebate.R"); 
source("code/CO2_emissions.R") ;
source("code/norm.R");
source("code/technical_progress.R");
source("code/weight.R");


library(tidyverse)

products_2019 <- readRDS("outputs/products_2019_weight.RDS")
manufacturers_constraint <- readRDS("outputs/manufacturers_constraint.RDS")
  

# Simulation des émissions avec les barèmes prévus --------------------------


#On crée une liste des produits de 2019 à 2023, avec leurs prix 

#On regroupe, pour chaque année, de 2019 à 2023
# - le prix de vente
# - le barème du bonus-malus
# - l'impact de la norme
# - le barème poids
# - le prix total


prices_2019 <- products_2019 %>% 
  select(product_id, groupe, motorization, co2, real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list <- list()
for (year in 2019:2023) {
  product_list[[year]] <- prices_2019 %>% mutate(annee = rep(year, nrow(prices_2019)))
}
product_list <- bind_rows(product_list) %>% relocate(annee)


#On ajoute les feebates officiels prévus

future_feebates <- products_2019 %>% 
  select(product_id, motorization, bareme_2019:bareme_2023) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
price_list <- left_join(product_list, future_feebates, by = c("product_id", "motorization", "annee"))


#On ajoute les malus poids prévu (qui dépendent eux-mêmes des barèmes prévus)

future_weight_fees <- products_2019 %>% 
  select(product_id, motorization, weight_tax_2019:weight_tax_2023) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

price_list <- left_join(price_list, future_weight_fees, by = c("product_id", "motorization", "annee")) 

# On ajoute le progès technique et on calcul le prix final

price_list <- price_list %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% select(groupe, lambda_m), by = "groupe") %>% 
  mutate(norm = lambda_m * (co2 - 95),
         final_price = price_with_technical_progress + feebate + norm + weight_fee,
         final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 
  
  

#On calcule les émissions attendues avec les barèmes officiellement prévus

NEDC_emissions <- c()
real_emissions <- c()

for (year in 2019:2023) {
  i <- year - 2018
  future_prices <- filter(price_list, annee == year) %>% .$final_price_without_technical_progress
  shares <- share_nested(future_prices, products_2019$delta_with_resids)
  NEDC_emissions[i] <- get_CO2(shares, filter(price_list, annee == year)$co2)
  real_emissions[i] <- get_CO2(shares, filter(price_list, annee == year)$real_emissions)
}

future_emissions  <- tibble(annee = 2019:2023, 
                            NEDC_emissions = NEDC_emissions,
                            real_emissions= real_emissions)

future_emissions


#Graph
future_emissions %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with current feebate")
ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
#ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)














