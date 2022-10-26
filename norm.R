source("code/feebate.R");
source("code/CO2_emissions.R"); 

library(tidyverse)
library(pracma) #Solver
#library(nleqslv) #Solver

   

products <- readRDS("outputs/products.RDS") 
products_sans_2020 <- readRDS("outputs/products_sans_2020.RDS")

products_2019 <- readRDS("outputs/products_2019_co2.RDS")

             

# Estimation by approximations -------------------------------------------
   
threshold <- 95

## We look at average manufacturers emissions

manufacturers_constraint <- products_2019 %>% 
  dplyr::select(product_id, motorization, groupe, volume, co2, prix_10k, bareme_2020, shares_with_BM2020)%>%
  group_by(groupe) %>% 
  summarise(emissions_BM = weighted.mean(co2, shares_with_BM2020)) 

# We add the lambdas for the different manufacturers for which the emissions are above 95gC02/km 

manufacturers_constraint$lambda_m <- ifelse(manufacturers_constraint$emissions_BM < threshold, 0, 0.01)
  

data_norm <- products_2019 %>% dplyr::select(product_id, motorization, groupe, volume, co2, prix_10k, bareme_2020, shares_with_BM2020)

data_norm$lambda <- left_join(products_2019, manufacturers_constraint, by = "groupe")$lambda_m

#We compute the new markets shares and the associated emissions

data_norm$price_with_norm <- data_norm$prix_10k + data_norm$bareme_2020/10000 + data_norm$lambda*(data_norm$co2 - threshold)

data_norm$shares_with_norm <- share_nested(data_norm$price_with_norm, products_2019$delta)

#We look at the emissions by manufacturers after adding the norm 

manufacturers_constraint <- manufacturers_constraint %>% 
  left_join(., 
            data_norm %>% group_by(groupe) %>% summarise(emissions_with_norm = weighted.mean(co2, shares_with_norm)),
            by = "groupe") %>% 
  mutate(respect_norm = ifelse(emissions_with_norm < threshold, "OK", "no"))


# ESTIMATION WITH A FUNCTION AND CONVERGENCE 


get_CO2_with_norm <- function(lambda) {
  # Function to estimate the CO2 emissions of manufacturers, if we have the constraint level (lambda)
  
  threshold <- 95
  

# We look at the average manufacturers emissions with the 2020 "bonus-malus" on the 2019 car fleet 
  manufacturers_constraint <- products_2019 %>% 
    dplyr::select(groupe, co2, shares_with_BM2020) %>% 
    group_by(groupe) %>% 
    summarise(emissions_BM = weighted.mean(co2, shares_with_BM2020)) %>% 
    # We add the lambdas for manufacturers for which the emissions are above 95gC02/km 
    mutate(lambda_m = lambda)
  
  # We create a database with all the products to include the lambda in the price and compute the new market shares 
    data_norm <- products_2019 %>% dplyr::select(product_id, motorization, groupe, volume, co2, prix_10k, bareme_2020, shares_with_BM2020)
  
  data_norm$lambda <- left_join(data_norm, manufacturers_constraint, by = "groupe")$lambda_m
  
  # We have to adjust the price to the technical progress
  
  data_norm$price_with_norm <- data_norm$prix_10k + data_norm$bareme_2020/10000 + data_norm$lambda*(data_norm$co2 - threshold)
  
  data_norm$shares_with_norm <- share_nested(data_norm$price_with_norm, products_2019$delta)
  
  # We compute the new manufacturers emissions
  manufacturers_constraint <- manufacturers_constraint %>% 
    left_join(., 
              data_norm %>% group_by(groupe) %>% summarise(emissions_with_norm = weighted.mean(co2, shares_with_norm)),
              by = "groupe") 
  
  return(manufacturers_constraint)
}

#Example : 
lambda <- rep(0.01, length(unique(products_2019$groupe)))
get_CO2_with_norm(lambda)




# Solve for lambdas --------------------------------------------------------


find_lambdas <- function() {
  #' Solver (manuel) pour trouver les lambdas
  
  
  #Initialiation
  lambda <- rep(0, length(unique(products_2019$groupe)))
  
  # Target level : We target levels observed in 2020
  
  
  target <- products %>% 
    filter(annee == 2020) %>% 
    group_by(groupe) %>% 
    summarise(emissions = weighted.mean(co2, volume)) %>% 
    .$emissions
  
  step <- 0.0001
  
  #Computation by iteration
  for (i in 1:1000000) {
    manufacturers_constraint <- get_CO2_with_norm(lambda)
    if(sum(manufacturers_constraint$emissions_with_norm <= target) == 0) {
      break
    } else {
      lambda <- ifelse(manufacturers_constraint$emissions_with_norm > target, lambda + step, lambda)
    }
  }
  
  manufacturers_constraint$target <- target
  manufacturers_constraint$target_reached <- ifelse(manufacturers_constraint$emissions_with_norm < manufacturers_constraint$target, "YES", "no")
  
    return(manufacturers_constraint)
}


#Example :
manufacturers_constraint <- find_lambdas()

saveRDS(manufacturers_constraint, "outputs/manufacturers_constraint.RDS")

# We identify two manufacturers with emissions in 2019 below 95gCO2/km 

lambda_marketing <- manufacturers_constraint %>% 
  filter(groupe == "MITSUBISHI" | groupe == "TOYOTA_LEXUS") %>%
  mutate(lambda_m = as.numeric(lambda_m)) 
lambda_marketing <- mean(lambda_marketing$lambda_m) # We recovered the non-price effects 
# We recover the lambda price and non price of each manufacturers

### ATTENTION MODIFICATION FAITE AU NIVEAU DU LAMBDA M 




manufacturers_constraint <- manufacturers_constraint %>%
  mutate(lambda_m = as.numeric(lambda_m)) %>%
  mutate(lambda_norme = case_when(
    lambda_m == 0 ~ 0,
    groupe == "MITSUBISHI" ~ 0,
    groupe == "TOYOTA_LEXUS" ~ 0,
    lambda_m < lambda_marketing ~ lambda_m,
    TRUE ~ lambda_m - lambda_marketing)) %>%
  rename(lambda_full = lambda_m)

saveRDS(manufacturers_constraint, "outputs/manufacturers_constraint.RDS")
#manufacturers_constraint <- readRDS("outputs/manufacturers_constraint.RDS")  

# We add these results to products_2019 and compute the new market shares

products_2019 <- products_2019 %>% 
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_full, lambda_norme), by = "groupe") %>%
  mutate(constraint_norme = lambda_norme * (co2 - threshold),
         constraint_full = lambda_full * (co2 - threshold),
         shares_2020_with_BM_and_norm = share_nested(prix_10k + bareme_2020/1e4 + constraint_norme, delta_with_resids) ,
         shares_2020_with_BM_and_norm_and_non_price_effects = share_nested(prix_10k + bareme_2020/1e4 + constraint_full, delta_with_resids))
         

sum(products_2019$shares_2020_with_BM_and_norm * products_2019$co2) / sum(products_2019$shares_2020_with_BM_and_norm)
sum(products_2019$shares_2020_with_BM_and_norm_and_non_price_effects * products_2019$co2) / sum(products_2019$shares_2020_with_BM_and_norm_and_non_price_effects)


products_2019_norm <- products_2019
saveRDS(products_2019_norm, "outputs/products_2019_norm.RDS")


# We compute the observed market shares in 2020 to compare them with simulations using the norm 

total_shares_2019 <- sum(products_2019$shares_2020_with_BM_and_norm_and_non_price_effects)

shares_2020 <- products %>% 
  filter(annee == 2020) %>% 
  group_by(motorization) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup() %>% 
  mutate(share = volume / sum(volume)) %>% 
  dplyr::select(motorization, share) %>% 
  mutate(instrument = "shares 2020 \n observed",
         share = share * total_shares_2019) # to compare to the theoretical market shares

# Graphic : impact of the norm on the market shares and emissions

products_2019 %>% 
  dplyr::select(motorization, shares_with_BM2020, shares_2020_with_BM_and_norm, shares_2020_with_BM_and_norm_and_non_price_effects) %>%
  pivot_longer(-motorization, names_to = "instrument", values_to = "share") %>% 
  mutate(instrument = case_when( instrument == "shares_with_BM2020" ~ "Feebate",
                                 instrument == "shares_2020_with_BM_and_norm" ~ "Norm & feebate",
                                 instrument == "shares_2020_with_BM_and_norm_and_non_price_effects" ~ "Norm, Feebate \n & non-price effects")) %>% 
  group_by(motorization, instrument) %>% summarise(share = sum(share)) %>% 
  bind_rows(shares_2020) %>% 
  ggplot(aes(x= instrument, y  = share, fill = motorization)) + geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Instrument", y = "Part de marché", fill = "Motorisation", title = "Impact de la norme") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
ggsave("outputs/plots/adding_norm_to_feebate.png", w = 7, h = 5)

products_2019 %>% summarise(emissions = weighted.mean(co2, shares_2020_with_BM_and_norm_and_non_price_effects))
products %>% filter(annee == 2020) %>% summarise(emissions = weighted.mean(co2, volume))



products_2019 %>%
  mutate(s2019 = shares_with_BM2019, s2020=shares_with_BM2019, s2021 = shares_with_BM2019, s2022=shares_with_BM2019, s2023 = shares_with_BM2019) %>%
  dplyr::select(product_id, motorization, groupe, prix_final_10k, delta_with_resids, co2, lambda_norme, s2019, s2020, s2021, s2022, s2023) %>%
  pivot_longer(-c(product_id, motorization, groupe, prix_final_10k, delta_with_resids, co2, lambda_norme), names_to = "annee", values_to = "shares") %>%
  mutate(annee = str_remove(annee, "s"),
         shares = as.numeric(shares)) %>%
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = prix_final_10k + norm ) %>% 
  group_by(annee) %>%
  mutate(new_shares = share_nested(final_price, delta_with_resids)) %>%
  summarise(NEDC_emissions = sum(get_CO2(new_shares, co2)),
            Driving_emissions = weighted.mean(get_real_driving_emissions(motorization, co2), new_shares),
            LCA_emissions = weighted.mean(get_real_emissions_LCA(motorization, co2), new_shares)) %>%
  mutate(annee = as.numeric(annee)) %>%
  pivot_longer(-annee, names_to = "emission_type", values_to = "emissions") %>%
  ggplot(aes(x = annee, y = emissions, colour = emission_type)) + geom_line() + 
  labs(x = " année ", y = "co2 emissions (gCO2/km)", title = " Evolution du taux moyen de co2 selon la norme européenne") +
  theme_bw() + ylim(50, 200)
ggsave("outputs/plots/adding_norm_solely.png", w = 7, h = 5)
    





group_by(year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = " année ", y = "Part de marché", colour = "Motorisation", title = " Evolution des parts de marché selon le Bonus-Malus") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)
ggsave("outputs/plots/market_shares_evolution_by_motorization.png", w = , h = 4)
