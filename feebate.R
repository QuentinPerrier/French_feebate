library(tidyverse)
library(ggplot2)
library(ggridges)
library(gtable)
library(gridExtra)
library("readxl")

setwd("d:/morvillier/Desktop/REVISION_BONUS_MALUS/I4CE/Bonus malus/3_Méthodologie/")


manufacturers_constraint <- readRDS("outputs/manufacturers_constraint.RDS")
bareme_bm <- read_excel("inputs/bareme_bonus_malus.xlsx", n_max = 700)


### Defining the demand parameters 


products <- readRDS("outputs/products.RDS") 
products_sans_2020 <- readRDS("outputs/products_sans_2020.RDS")
achats_menages_moyen <- readRDS("outputs/achats_menages_moyen.RDS")
regression_coefficients <- readRDS("outputs/regression_coefficients.RDS")
alpha <- regression_coefficients["alpha"]
sigma <- regression_coefficients["sigma"]


products_2019 <- products_sans_2020 %>% filter(annee == 2019) %>% dplyr::select(-annee)


### Shared Nested functions ----------
#Computes the market shares according to the final price


share_nested <- function(price, delta) {
  #' return market shares as a function of price
  products_2019_temp <- products_2019 %>%
    mutate(term_exp = exp((delta + (alpha * price))/ (1 - sigma))) %>%
    group_by(segment) %>%
    mutate(D_g = sum(term_exp),
           D_g_temp = (D_g)^(1 - sigma)) %>%
    ungroup ()
  
  sum_D_g_2019 <- sum(unique(products_2019_temp$D_g_temp))
  
  
  products_2019_temp <- products_2019_temp %>%
    mutate(sum_D_g = sum_D_g_2019) %>%
    mutate(inv_tot = (1 / (1 + sum_D_g)),
           market_shares = term_exp * (inv_tot) * (1 / (D_g^(sigma)))) 
  
  return(products_2019_temp$market_shares)
  
}






sum(share_nested(products_2019$prix_final_10k, products_2019$delta_with_resids))
sum(products_2019$market_share_s1)



# Market shares with feebates ----------------------------------------------------

# Computes the new market shares if a new feebate is applied

shares_evolution_with_feebate <- function(feebate){
  #' returns market shares according to a feebate rate applied to the park of vehicle of 2019
  data <- products_2019 %>% 
    mutate(price_with_new_feebate = prix_final_10k + (feebate - yearly_adjusted_bareme) / 10000)
  
  delta_with_residuals <- products_2019 %>% .$delta_with_resids
  
  out <- share_nested(data$price_with_new_feebate, delta_with_residuals)
  
  return(out)
}

estimated_shares_with_feebate <- tibble(
  shares_2019 = shares_evolution_with_feebate(products_2019$bareme_2019),
  shares_2020 = shares_evolution_with_feebate(products_2019$bareme_2020),
  shares_2021 = shares_evolution_with_feebate(products_2019$bareme_2021),
  shares_2022 = shares_evolution_with_feebate(products_2019$bareme_2022),
  shares_2023 = shares_evolution_with_feebate(products_2019$bareme_2023)
)

products_2019$shares_with_BM2019 <- shares_evolution_with_feebate(products_2019$bareme_2019)
products_2019$shares_with_BM2020 <- shares_evolution_with_feebate(products_2019$bareme_2020)
products_2019$shares_with_BM2021 <- shares_evolution_with_feebate(products_2019$bareme_2021)
products_2019$shares_with_BM2022 <- shares_evolution_with_feebate(products_2019$bareme_2022)
products_2019$shares_with_BM2023 <- shares_evolution_with_feebate(products_2019$bareme_2023)




saveRDS(products_2019, file = "outputs/products_2019.RDS")
saveRDS(products_2019, file = "outputs/estimated_shares_with_feebate.RDS")



# Test : Comparison between observed market shares and simulated in 2019 
test <- products_2019 %>% 
  dplyr::select(market_share_s1, shares_with_BM2019) %>% 
  mutate(market_share_s1 - shares_with_BM2019)

products_2019 %>% 
  dplyr::select(product_id, motorization, market_share_s1, shares_with_BM2019) %>% 
  pivot_longer(cols = -c(product_id, motorization),
               names_to = "type",
               values_to = "market_shares") %>% 
  mutate(type = ifelse(type == "market_share_s1", "observations", "simulations")) %>% 
  group_by(type, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = type, y = market_shares, fill = motorization)) + geom_bar(stat="identity") +
  labs(x = "", y = "Part de marché", fill = "Motorisation", title = "Observations vs simulation en 2019") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
ggsave("outputs/plots/estimation_vs_simulation_2019.png", w= 5, h = 5)

#



# Graphic : Evolution of sales with the feebates planned until 2023

products_2019 %>% 
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
  labs(x = " année ", y = "Part de marché", colour = "Motorisation", title = " Evolution des parts de marché selon le Bonus-Malus") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)
ggsave("outputs/plots/market_shares_evolution_by_motorization.png", w = , h = 4)

products_2019 %>% 
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
  labs(x = "", y = "Part de marché", colour = "Motorisation", title = "Evolution des parts de marché avec les barèmes prévus (sans norme)") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent) +
  ylim(0, 0.025)




# Graphic : Evolution of sales with the feebates planned until 2023

products_2019_fullbm <- products_2019 

products_2019_fullbm <- cbind(products_2019_fullbm, get_bareme_gvt_2024_2025(products_2019_fullbm)) %>%
  relocate(bareme_2024, .after = bareme_2023) %>%
  relocate(bareme_2025, .after = bareme_2024) 

products_2019_fullbm <- products_2019_fullbm %>%
  mutate(shares_with_BM2024 = shares_evolution_with_feebate(products_2019_fullbm$bareme_2024),
         shares_with_BM2025 = shares_evolution_with_feebate(products_2019_fullbm$bareme_2025))

products_2019_fullbm$driving_real_emissions <- get_real_driving_emissions(products_2019_fullbm$motorization, products_2019_fullbm$co2)

products_2019_fullbm %>% 
  dplyr::select(product_id, motorization, shares_with_BM2019, shares_with_BM2020, shares_with_BM2021, shares_with_BM2022, shares_with_BM2023, shares_with_BM2024, shares_with_BM2025) %>% 
  pivot_longer(cols = -c(product_id, motorization),
               names_to = "year",
               values_to = "market_shares") %>% 
  mutate(year = ifelse(year == "market_share_s1", 2019, year),
         year = str_remove_all(year, "shares_with_BM"),
         year = as.integer(year)) %>% 
  group_by(year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = " year", y = "Market Shares", colour = "Motorization") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent)
ggsave("outputs/plots/market_shares_evolution_by_motorization_2019_2025.png", w = 11.2 , h = 7.25)

products_2019_fullbm %>% 
  dplyr::select(product_id, motorization, shares_with_BM2019, shares_with_BM2020, shares_with_BM2021, shares_with_BM2022, shares_with_BM2023, shares_with_BM2024, shares_with_BM2025) %>% 
  pivot_longer(cols = -c(product_id, motorization),
               names_to = "year",
               values_to = "market_shares") %>% 
  mutate(year = ifelse(year == "market_share_s1", 2019, year),
         year = str_remove_all(year, "shares_with_BM"),
         year = as.integer(year)) %>% 
  group_by(year, motorization) %>% 
  summarise(market_shares = sum(market_shares)) %>% 
  ggplot(aes(x = year, y = market_shares, colour = motorization)) + geom_line() +
  labs(x = "", y = "", colour = "") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent) +
  ylim(0, 0.025)



