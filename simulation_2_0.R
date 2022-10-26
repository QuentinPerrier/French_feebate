source("code/feebate.R"); 
source("code/CO2_emissions.R") ;
#source("code/norm.R");
source("code/technical_progress.R");
source("code/weight.R");
source("code/bareme_simulations.R");

setwd("d:/morvillier/Desktop/REVISION_BONUS_MALUS/I4CE/Bonus malus/3_Méthodologie/")



library(tidyverse)

library(writexl) # export the results into format  
 

manufacturers_constraint <- readRDS("outputs/manufacturers_constraint.RDS")
bareme_bm <- read_excel("inputs/bareme_bonus_malus.xlsx", n_max = 700)
products_2019_clean <- products_2019
products_2019_clean_2 <- products_2019_clean



# COMPUTATION WITH STRONG TECHNICAL PROGRESS 

# SIMULATION OF GOUVERNMENTAL FEEBATES PROJECTED FOR 2024-2025 

products_2019_gvt <- products_2019
products_2019_gvt <- cbind(products_2019_gvt, get_bareme_gvt_2024_2025(products_2019_gvt)) %>%
  relocate(bareme_2024, .after = bareme_2023) %>%
  relocate(bareme_2025, .after = bareme_2024)

prices_2019 <- products_2019_gvt %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list <- list()
for (year in 2019:2025) {
  product_list[[year]] <- prices_2019 %>% mutate(annee = rep(year, nrow(prices_2019)))
}
product_list <- bind_rows(product_list) %>% relocate(annee)


# WE ADD THE OFFICIAL FEEBATES 

future_feebates <- products_2019_gvt %>% 
  dplyr::select(product_id, motorization, bareme_2019:bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list <- left_join(product_list, future_feebates, by = c("product_id", "motorization", "annee"))

# WE ADD THE WEIGHT "MALUS"


future_weight_fees <- products_2019_gvt %>% 
  dplyr::select(product_id, motorization, weight_tax_2019:weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list <- left_join(product_list, future_weight_fees, by = c("product_id", "motorization", "annee")) 

# WE ADD THE TECHNICAL PROGRESS, THE NORM AND COMPUTE THE FINAL PRICE 

product_list <- product_list %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff_advanced(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee)



# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES  

NEDC_emissions <- c()
real_emissions <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices_0 <- filter(product_list, annee == year) %>% .$final_price
  shares <- share_nested(future_prices_0, products_2019_gvt$delta_with_resids)
  NEDC_emissions[i] <- get_CO2(shares, filter(product_list, annee == year)$co2)
  real_emissions[i] <- get_CO2(shares, filter(product_list, annee == year)$driving_real_emissions)
}

future_emissions  <- tibble(annee = 2019:2025, 
                            NEDC_emissions = NEDC_emissions,
                            real_emissions= real_emissions)

future_emissions

as.data.frame(future_emissions)



#Graph
future_emissions %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with current feebate without technical progress")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
#ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)

#planned <- future_emissions %>% pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% filter(annee <= 2023)
#anticipated <- future_emissions %>% pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% filter(annee > 2022)
#ggplot(mapping = aes(annee, emissions, color = emission_scope)) + geom_line(linetype = 1, data = planned) + geom_line(linetype = 2, data = anticipated)


### WEAK TECHNICAL PROGRESS 
### WEAK TECHNICAL PROGRESS
### WEAK TECHNICAL PROGRESS



## COMPUTATION OF THE EXPECTED EMISSIONS FOR THE GOVERNMENT FEEBATE WITH TECHNICAL PROGRESS (2024-2025)


products_2019_gvt_weak_TP <- products_2019
products_2019_gvt_weak_TP <- cbind(products_2019_gvt_weak_TP, get_bareme_gvt_2024_2025(products_2019_gvt_weak_TP)) %>%
  relocate(bareme_2024, .after = bareme_2023) %>%
  relocate(bareme_2025, .after = bareme_2024)

prices_2019_weak_TP <- products_2019_gvt_weak_TP %>% 
  dplyr::select(product_id, groupe, motorization, co2, delta_with_resids, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_weak_TP <- list()
for (year in 2019:2025) {
  product_list_weak_TP[[year]] <- prices_2019_weak_TP %>% mutate(annee = rep(year, nrow(prices_2019_weak_TP)))
}
product_list_weak_TP <- bind_rows(product_list_weak_TP) %>% relocate(annee)


# WE ADD THE EXPECTED OFFICIAL FEEBATES


future_feebates_weak_TP <- products_2019_gvt_weak_TP %>% 
  dplyr::select(product_id, motorization, bareme_2019:bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_weak_TP <- left_join(product_list_weak_TP, future_feebates_weak_TP, by = c("product_id", "motorization", "annee"))


# WE ADD THE WEIGHT "MALUS"

future_weight_fees_weak_TP <- products_2019_gvt_weak_TP %>% 
  dplyr::select(product_id, motorization, weight_tax_2019:weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_weak_TP <- left_join(product_list_weak_TP, future_weight_fees_weak_TP, by = c("product_id", "motorization", "annee")) 


### WEAK TECHNICAL PROGRESS 
### WEAK TECHNICAL PROGRESS 
### WEAK TECHNICAL PROGRESS 


# WE ADD THE TECHNICAL PROGRESS, THE NORM AND COMPUTE THE FINAL PRICE 

product_list_weak_TP <- product_list_weak_TP %>% 
  mutate(price_with_technical_progress_faible = price_2019_hors_BM * get_tech_coeff(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price_pt_faible = price_with_technical_progress_faible + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee,
    final_price_without_feebate_pt_faible=final_price_pt_faible-feebate)



# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES FOR WEAK TECHNICAL PROGRESS
# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES FOR WEAK TECHNICAL PROGRESS
# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES FOR WEAK TECHNICAL PROGRESS

NEDC_emissions_weak_TP <- c()
real_emissions_weak_TP <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices_0 <- filter(product_list_weak_TP, annee == year) %>% .$final_price_pt_faible
  shares <- share_nested(future_prices_0, products_2019_gvt_weak_TP$delta_with_resids)
  NEDC_emissions_weak_TP[i] <- get_CO2(shares, filter(product_list_weak_TP, annee == year)$co2)
  real_emissions_weak_TP[i] <- get_CO2(shares, filter(product_list_weak_TP, annee == year)$driving_real_emissions)
}

future_emissions_weak_TP <- tibble(annee = 2019:2025, 
                            NEDC_emissions_weak_TP = NEDC_emissions_weak_TP,
                            real_emissions_weak_TP= real_emissions_weak_TP)

future_emissions_weak_TP

as.data.frame(future_emissions_weak_TP)



## SORTIE EXCEL DES RESULTATS 


write.csv(future_emissions_weak_TP, file="d:\\morvillier\\Desktop\\REVISION_BONUS_MALUS\\DONNEES_GRAPHIQUE\\gov_weak_pt_feebate_.csv", row.names = FALSE)


## COMPUTATION FOR STRONG TECHNICAL PROGRESS 
## COMPUTATION FOR STRONG TECHNICAL PROGRESS 
## COMPUTATION FOR STRONG TECHNICAL PROGRESS 


products_2019_gvt <- products_2019
products_2019_gvt <- cbind(products_2019_gvt, get_bareme_gvt_2024_2025(products_2019_gvt)) %>%
  relocate(bareme_2024, .after = bareme_2023) %>%
  relocate(bareme_2025, .after = bareme_2024)

prices_2019 <- products_2019_gvt %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list <- list()
for (year in 2019:2025) {
  product_list[[year]] <- prices_2019 %>% mutate(annee = rep(year, nrow(prices_2019)))
}
product_list <- bind_rows(product_list) %>% relocate(annee)


# WE ADD THE OFFICIAL FEEBATES 

future_feebates <- products_2019_gvt %>% 
  dplyr::select(product_id, motorization, bareme_2019:bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list <- left_join(product_list, future_feebates, by = c("product_id", "motorization", "annee"))

# WE ADD THE WEIGHT "MALUS"


future_weight_fees <- products_2019_gvt %>% 
  dplyr::select(product_id, motorization, weight_tax_2019:weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list <- left_join(product_list, future_weight_fees, by = c("product_id", "motorization", "annee")) 

# WE ADD THE TECHNICAL PROGRESS, THE NORM AND COMPUTE THE FINAL PRICE 

product_list <- product_list %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff_advanced(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee)



# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES  

NEDC_emissions <- c()
real_emissions <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices_0 <- filter(product_list, annee == year) %>% .$final_price
  shares <- share_nested(future_prices_0, products_2019_gvt$delta_with_resids)
  NEDC_emissions[i] <- get_CO2(shares, filter(product_list, annee == year)$co2)
  real_emissions[i] <- get_CO2(shares, filter(product_list, annee == year)$driving_real_emissions)
}

future_emissions_strong_TP_GOUV  <- tibble(annee = 2019:2025, 
                            NEDC_emissions = NEDC_emissions,
                            real_emissions= real_emissions)

future_emissions_strong_TP_GOUV
as.data.frame(future_emissions_strong_TP_GOUV)


## SORTIE EXCEL DES RESULTATS 


write.csv(future_emissions_strong_TP_GOUV, file="d:\\morvillier\\Desktop\\REVISION_BONUS_MALUS\\DONNEES_GRAPHIQUE\\future_emissions_strong_TP_GOUV.csv", row.names = FALSE)





## CALCUL DES EMISSIONS DE C02 SANS LES FEEBATES 
## CALCUL DES EMISSIONS DE C02 SANS LES FEEBATES 
## CALCUL DES EMISSIONS DE C02 SANS LES FEEBATES 
## CALCUL DES EMISSIONS DE C02 SANS LES FEEBATES 


## WEAK TECHNICAL PROGRESS CALCULATIONS 
## WEAK TECHNICAL PROGRESS CALCULATIONS 
## WEAK TECHNICAL PROGRESS CALCULATIONS 




## COMPUTATION OF THE EXPECTED EMISSIONS FOR THE GOVERNMENT FEEBATE WITH TECHNICAL PROGRESS (2024-2025)


products_2019_gvt_weak_TP <- products_2019
products_2019_gvt_weak_TP <- cbind(products_2019_gvt_weak_TP, get_bareme_gvt_2024_2025(products_2019_gvt_weak_TP)) %>%
  relocate(bareme_2024, .after = bareme_2023) %>%
  relocate(bareme_2025, .after = bareme_2024)

prices_2019_weak_TP <- products_2019_gvt_weak_TP %>% 
  dplyr::select(product_id, groupe, motorization, co2, delta_with_resids, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_weak_TP <- list()
for (year in 2019:2025) {
  product_list_weak_TP[[year]] <- prices_2019_weak_TP %>% mutate(annee = rep(year, nrow(prices_2019_weak_TP)))
}
product_list_weak_TP <- bind_rows(product_list_weak_TP) %>% relocate(annee)


# WE ADD THE EXPECTED OFFICIAL FEEBATES


future_feebates_weak_TP <- products_2019_gvt_weak_TP %>% 
  dplyr::select(product_id, motorization, bareme_2019:bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_weak_TP <- left_join(product_list_weak_TP, future_feebates_weak_TP, by = c("product_id", "motorization", "annee"))


# WE ADD THE WEIGHT "MALUS"

future_weight_fees_weak_TP <- products_2019_gvt_weak_TP %>% 
  dplyr::select(product_id, motorization, weight_tax_2019:weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_weak_TP <- left_join(product_list_weak_TP, future_weight_fees_weak_TP, by = c("product_id", "motorization", "annee")) 


### WEAK TECHNICAL PROGRESS 
### WEAK TECHNICAL PROGRESS 
### WEAK TECHNICAL PROGRESS 


# WE ADD THE TECHNICAL PROGRESS, THE NORM AND COMPUTE THE FINAL PRICE 

product_list_weak_TP <- product_list_weak_TP %>% 
  mutate(price_with_technical_progress_faible = price_2019_hors_BM * get_tech_coeff(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price_pt_faible = price_with_technical_progress_faible + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee,
    final_price_without_feebate_pt_faible=final_price_pt_faible-feebate)



# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES FOR WEAK TECHNICAL PROGRESS
# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES FOR WEAK TECHNICAL PROGRESS
# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES FOR WEAK TECHNICAL PROGRESS

NEDC_emissions_weak_TP <- c()
real_emissions_weak_TP <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices_0 <- filter(product_list_weak_TP, annee == year) %>% .$final_price_without_feebate_pt_faible
  shares <- share_nested(future_prices_0, products_2019_gvt_weak_TP$delta_with_resids)
  NEDC_emissions_weak_TP[i] <- get_CO2(shares, filter(product_list_weak_TP, annee == year)$co2)
  real_emissions_weak_TP[i] <- get_CO2(shares, filter(product_list_weak_TP, annee == year)$driving_real_emissions)
}

future_emissions_weak_TP <- tibble(annee = 2019:2025, 
                                   NEDC_emissions_weak_TP = NEDC_emissions_weak_TP,
                                   real_emissions_weak_TP= real_emissions_weak_TP)

future_emissions_weak_TP

as.data.frame(future_emissions_weak_TP)



## SORTIE EXCEL DES RESULTATS 


write.csv(future_emissions_weak_TP, file="d:\\morvillier\\Desktop\\REVISION_BONUS_MALUS\\DONNEES_GRAPHIQUE\\NO_feebate_weak_pt_.csv", row.names = FALSE)



### STRONG TECHNICAL PROGRESS 
### STRONG TECHNICAL PROGRESS 
### STRONG TECHNICAL PROGRESS 

# WE ADD THE TECHNICAL PROGRESS, THE NORM AND COMPUTE THE FINAL PRICE 


products_2019_gvt_weak_TP <- products_2019
products_2019_gvt_weak_TP <- cbind(products_2019_gvt_weak_TP, get_bareme_gvt_2024_2025(products_2019_gvt_weak_TP)) %>%
  relocate(bareme_2024, .after = bareme_2023) %>%
  relocate(bareme_2025, .after = bareme_2024)

prices_2019_weak_TP <- products_2019_gvt_weak_TP %>% 
  dplyr::select(product_id, groupe, motorization, co2, delta_with_resids, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_weak_TP <- list()
for (year in 2019:2025) {
  product_list_weak_TP[[year]] <- prices_2019_weak_TP %>% mutate(annee = rep(year, nrow(prices_2019_weak_TP)))
}
product_list_weak_TP <- bind_rows(product_list_weak_TP) %>% relocate(annee)


# WE ADD THE EXPECTED OFFICIAL FEEBATES


future_feebates_weak_TP <- products_2019_gvt_weak_TP %>% 
  dplyr::select(product_id, motorization, bareme_2019:bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_weak_TP <- left_join(product_list_weak_TP, future_feebates_weak_TP, by = c("product_id", "motorization", "annee"))


# WE ADD THE WEIGHT "MALUS"

future_weight_fees_weak_TP <- products_2019_gvt_weak_TP %>% 
  dplyr::select(product_id, motorization, weight_tax_2019:weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_weak_TP <- left_join(product_list_weak_TP, future_weight_fees_weak_TP, by = c("product_id", "motorization", "annee")) 





product_list_weak_TP <- product_list_weak_TP %>% 
  mutate(price_with_technical_progress_faible = price_2019_hors_BM * get_tech_coeff_advanced(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price_pt_faible = price_with_technical_progress_faible + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee,
    final_price_without_feebate_pt_fort=final_price_pt_faible-feebate)

NEDC_emissions_weak_TP <- c()
real_emissions_weak_TP <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices_0 <- filter(product_list_weak_TP, annee == year) %>% .$final_price_without_feebate_pt_fort
  shares <- share_nested(future_prices_0, products_2019_gvt_weak_TP$delta_with_resids)
  NEDC_emissions_weak_TP[i] <- get_CO2(shares, filter(product_list_weak_TP, annee == year)$co2)
  real_emissions_weak_TP[i] <- get_CO2(shares, filter(product_list_weak_TP, annee == year)$driving_real_emissions)
}

future_emissions_strong_TP_no_feebate <- tibble(annee = 2019:2025, 
                                   NEDC_emissions_weak_TP = NEDC_emissions_weak_TP,
                                   real_emissions_weak_TP= real_emissions_weak_TP)

future_emissions_strong_TP_no_feebate

as.data.frame(future_emissions_strong_TP_no_feebate)


## SORTIE EXCEL DES RESULTATS 

write.csv(future_emissions_strong_TP_no_feebate, file="d:\\morvillier\\Desktop\\REVISION_BONUS_MALUS\\DONNEES_GRAPHIQUE\\future_emissions_strong_TP_no_feebate_.csv", row.names = FALSE)






#Graph
future_emissions_weak_TP %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with current feebate without technical progress")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
#ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)

#planned <- future_emissions %>% pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% filter(annee <= 2023)
#anticipated <- future_emissions %>% pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% filter(annee > 2022)
#ggplot(mapping = aes(annee, emissions, color = emission_scope)) + geom_line(linetype = 1, data = planned) + geom_line(linetype = 2, data = anticipated)



## COMPUTATION OF THE EXPECTED EMISSIONS FOR THE GOVERNMENT FEEBATE WITH TECHNICAL PROGRESS (2024-2025)
# WE ADD THE EXPECTED OFFICIAL FEEBATES
# WE ADD THE WEIGHT "MALUS"
# WE ADD THE TECHNICAL PROGRESS, THE NORM AND COMPUTE THE FINAL PRICE 







# COMPUTATION OF THE EMISSIONS TO MATCH THE OBJECTIVE OF THE SNBC : decrease of the 6.27% / year between 2020 and 2025 

SNBC_average_rate <- (1 - 0.0627)

future_emissions_SNBC_computed <- tibble(annee = 2019:2025, 
                                         SNBC_emissions = c(future_emissions$NEDC_emissions[1], 123,
                                         123*SNBC_average_rate, 123*(SNBC_average_rate)^2,
                                         123*((SNBC_average_rate)^3),
                                         123*((SNBC_average_rate)^4),
                                         123*((SNBC_average_rate)^5)))

future_emissions_SNBC_harmonized <- tibble(annee = 2019:2025,
                                           NEDC_emissions = c(future_emissions$NEDC_emissions[1], future_emissions$NEDC_emissions[2],
                                                              future_emissions$NEDC_emissions[2]*SNBC_average_rate, 
                                                              future_emissions$NEDC_emissions[2]*((SNBC_average_rate)^2),
                                                              future_emissions$NEDC_emissions[2]*((SNBC_average_rate)^3),
                                                              future_emissions$NEDC_emissions[2]*((SNBC_average_rate)^4),
                                                              future_emissions$NEDC_emissions[2]*((SNBC_average_rate)^5)),
                                           Real_emissions = c(future_emissions$real_emissions[1], future_emissions$real_emissions[2],
                                                              future_emissions$real_emissions[2]*SNBC_average_rate, 
                                                              future_emissions$real_emissions[2]*((SNBC_average_rate)^2),
                                                              future_emissions$real_emissions[2]*((SNBC_average_rate)^3),
                                                              future_emissions$real_emissions[2]*((SNBC_average_rate)^4),
                                                              future_emissions$real_emissions[2]*((SNBC_average_rate)^5)))


# We consider the values transmitted by the MTE and compute the values with a decrease of 6.27%/year to reproduct the curve


future_emissions_current_SNBC <- tibble(annee = 2019 : 2025,
                                        emissions = c((141*(1 -0.02694542)^4), 123, 123 * ((SNBC_average_rate)), 123*((SNBC_average_rate)^2), 
                                                      123*((SNBC_average_rate)^3), 123*((SNBC_average_rate)^4), 123*((SNBC_average_rate)^5)))

#Graph de la courbe SNBC actuelle
future_emissions_current_SNBC %>%
  ggplot(aes(x = annee, y = emissions)) + geom_line(colour = "darkred") + 
  theme_bw() + ylim(0, 140) +
  labs(x = "année", y = "Emissions (gCO2/km)", title = "Emissions Objectif SNBC") 
ggsave("outputs/plots/Current_emissions_SNBC_objectif_to_2025.png", w = 4, h = 4)





## Computation of the emissions with simulation 1 of alternative feebate 

# Scenario with weak technical progress hypothesis (ICCT : decrease of electric vehicules' price of 7.2%/year) : simulation 1.1 


products_2019_s1_1 <- products_2019
products_2019_s1_1 <- cbind(products_2019_s1_1, get_baremes_alternatifs_simulation_1(products_2019_s1_1)) #on ajoute les barèmes de la projection 1 pour 2022 à 2025
price_2019_simulation_1_1 <- products_2019_s1_1 %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_simulation_1_1 <- list()
for (year in 2019:2025) {
  product_list_simulation_1_1[[year]] <- price_2019_simulation_1_1 %>% mutate(annee = rep(year, nrow(price_2019_simulation_1_1)))
}
product_list_simulation_1_1 <- bind_rows(product_list_simulation_1_1) %>% relocate(annee)


# WE ADD THE EXPECTED OFFICIAL FEEBATES

future_feebates_simulation_1_1 <- products_2019_s1_1 %>% 
  dplyr::select(product_id, motorization, simulation_1_bareme_2019 = bareme_2019, simulation_1_bareme_2020 = bareme_2020, simulation_1_bareme_2021 = bareme_2021,simulation_1_bareme_2022, simulation_1_bareme_2023, simulation_1_bareme_2024, simulation_1_bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "simulation_1_bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_simulation_1_1 <- left_join(product_list_simulation_1_1, future_feebates_simulation_1_1, by = c("product_id", "motorization", "annee"))


# WE ADD THE WEIGHT TAX

future_weight_fees_simulation_1_1 <- products_2019_s1_1 %>% 
  dplyr::select(product_id, motorization, weight_tax_2019, weight_tax_2020, weight_tax_2021, weight_tax_2022, weight_tax_2023, weight_tax_2024, weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_simulation_1_1 <- left_join(product_list_simulation_1_1, future_weight_fees_simulation_1_1, by = c("product_id", "motorization", "annee")) 

# WE ADD THE TECHNICAL PROGRESS, THE NORM AND COMPUTE THE FINAL PRICE 

product_list_simulation_1_1 <- product_list_simulation_1_1 %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 



## COMPUTATION OF THE EXPECTED EMISSIONS FOR THE GOVERNMENT FEEBATE WITH TECHNICAL PROGRESS (2024-2025)

NEDC_emissions_simulation_1_1 <- c()
real_emissions_simulation_1_1 <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices <- filter(product_list_simulation_1_1, annee == year) %>% .$final_price
  shares <- share_nested(future_prices, products_2019_s1_1$delta_with_resids)
  NEDC_emissions_simulation_1_1[i] <- get_CO2(shares, filter(product_list_simulation_1_1, annee == year)$co2)
  real_emissions_simulation_1_1[i] <- get_CO2(shares, filter(product_list_simulation_1_1, annee == year)$driving_real_emissions)
}

future_emissions_simulation_1_1  <- tibble(annee = 2019:2025, 
                                           NEDC_emissions_simulation_1_1 = NEDC_emissions_simulation_1_1,
                                           real_emissions_simulation_1_1= real_emissions_simulation_1_1)

future_emissions_simulation_1_1

as.data.frame(future_emissions_simulation_1_1)






#Graph
future_emissions_simulation_1_1 %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with new feebate scale 1")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)



##Scenario avec une hypothèse de progèes technique fort (T&E bnef baisse electrique 10.9%) : simulation 1.2
products_2019_s1_2 <- products_2019
products_2019_s1_2 <- cbind(products_2019_s1_2, get_baremes_alternatifs_simulation_1(products_2019_s1_2)) #on ajoute les barèmes de la projection 1 pour 2022 à 2025



price_2019_simulation_1_2 <- products_2019_s1_2 %>% 
  dplyr::select(product_id, groupe, motorization, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_simulation_1_2 <- list()
for (year in 2019:2025) {
  product_list_simulation_1_2[[year]] <- price_2019_simulation_1_2 %>% mutate(annee = rep(year, nrow(price_2019_simulation_1_2)))
}
product_list_simulation_1_2 <- bind_rows(product_list_simulation_1_2) %>% relocate(annee)


# WE ADD THE EXPECTED OFFICIAL FEEBATES

future_feebates_simulation_1_2 <- products_2019_s1_2 %>% 
  dplyr::select(product_id, motorization, simulation_1_bareme_2019 = bareme_2019, simulation_1_bareme_2020 = bareme_2020, simulation_1_bareme_2021 = bareme_2021,simulation_1_bareme_2022, simulation_1_bareme_2023, simulation_1_bareme_2024, simulation_1_bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "simulation_1_bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_simulation_1_2 <- left_join(product_list_simulation_1_2, future_feebates_simulation_1_2, by = c("product_id", "motorization", "annee"))


# WE ADD THE WEIGHT TAX

future_weight_fees_simulation_1_2 <- products_2019_s1_2 %>% 
  dplyr::select(product_id, motorization, weight_tax_2019, weight_tax_2020, weight_tax_2021, weight_tax_2022, weight_tax_2023, weight_tax_2024, weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_simulation_1_2 <- left_join(product_list_simulation_1_2, future_weight_fees_simulation_1_2, by = c("product_id", "motorization", "annee")) 

# WE ADD THE TECHNICAL PROGRESS, THE NORM AND COMPUTE THE FINAL PRICE 

product_list_simulation_1_2 <- product_list_simulation_1_2 %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff_advanced(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 



## COMPUTATION OF THE EXPECTED EMISSIONS FOR THE GOVERNMENT FEEBATE WITH TECHNICAL PROGRESS (2024-2025)


NEDC_emissions_simulation_1_2 <- c()
real_emissions_simulation_1_2 <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices_2 <- filter(product_list_simulation_1_2, annee == year) %>% .$final_price
  shares <- share_nested(future_prices_2, products_2019_s1_2$delta_with_resids)
  NEDC_emissions_simulation_1_2[i] <- get_CO2(shares, filter(product_list_simulation_1_2, annee == year)$co2)
  real_emissions_simulation_1_2[i] <- get_CO2(shares, filter(product_list_simulation_1_2, annee == year)$driving_real_emissions)
}

future_emissions_simulation_1_2  <- tibble(annee = 2019:2025, 
                                           NEDC_emissions_simulation_1_2 = NEDC_emissions_simulation_1_2,
                                           real_emissions_simulation_1_2= real_emissions_simulation_1_2)

future_emissions_simulation_1_2

as.data.frame(future_emissions_simulation_1_2)


#Graph
future_emissions_simulation_1_2 %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with new feebate scale 1")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)







# COMPUTATION OF EMISSIONS WITH SIMULATION 2 OF ALTERNATIVE FEEBATES
# COMPUTATION OF EMISSIONS WITH SIMULATION 2 OF ALTERNATIVE FEEBATES
# COMPUTATION OF EMISSIONS WITH SIMULATION 2 OF ALTERNATIVE FEEBATES 

## CORRESPOND AU BAREME 2 DU PAPIER AVEC PT FAIBLE
## CORRESPOND AU BAREME 2 DU PAPIER AVEC PT FAIBLE
## CORRESPOND AU BAREME 2 DU PAPIER AVEC PT FAIBLE 
## CORRESPOND AU BAREME 2 DU PAPIER AVEC PT FAIBLE 


products_2019_s2_1 <- products_2019
products_2019_s2_1 <- cbind(products_2019_s2_1, get_baremes_alternatifs_simulation_2(products_2019_s2_1)) #on ajoute les barèmes de la projection 1 pour 2022 à 2025

price_2019_simulation_2_1 <- products_2019_s2_1 %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_simulation_2_1 <- list()
for (year in 2019:2025) {
  product_list_simulation_2_1[[year]] <- price_2019_simulation_2_1 %>% mutate(annee = rep(year, nrow(price_2019_simulation_2_1)))
}
product_list_simulation_2_1 <- bind_rows(product_list_simulation_2_1) %>% relocate(annee)


# WE ADD THE EXPECTED OFFICIAL FEEBATES

future_feebates_simulation_2_1 <- products_2019_s2_1 %>% 
  dplyr::select(product_id, motorization, simulation_2_bareme_2019 = bareme_2019, simulation_2_bareme_2020 = bareme_2020, simulation_2_bareme_2021 = bareme_2021, simulation_2_bareme_2022, simulation_2_bareme_2023, simulation_2_bareme_2024, simulation_2_bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "simulation_2_bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_simulation_2_1 <- left_join(product_list_simulation_2_1, future_feebates_simulation_2_1, by = c("product_id", "motorization", "annee"))


# WE ADD THE WEIGHT TAX 

future_weight_fees_simulation_2_1 <- products_2019_s2_1 %>% 
  dplyr::select(product_id, motorization, weight_tax_2019, weight_tax_2020, weight_tax_2021, weight_tax_2022, weight_tax_2023, weight_tax_2024, weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_simulation_2_1 <- left_join(product_list_simulation_2_1, future_weight_fees_simulation_2_1, by = c("product_id", "motorization", "annee")) 


# WE ADD THE TECHNICAL PROGRESS, THE NORM AND COMPUTE THE FINAL PRICE 

product_list_simulation_2_1 <- product_list_simulation_2_1 %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 

# WE COMPUTE EXPECTED EMISSIONS WITH GOVERNMENTAL OFFICIAL FEEBATES

NEDC_emissions_simulation_2_1 <- c()
real_emissions_simulation_2_1 <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices <- filter(product_list_simulation_2_1, annee == year) %>% .$final_price
  shares <- share_nested(future_prices, products_2019$delta_with_resids)
  NEDC_emissions_simulation_2_1[i] <- get_CO2(shares, filter(product_list_simulation_2_1, annee == year)$co2)
  real_emissions_simulation_2_1[i] <- get_CO2(shares, filter(product_list_simulation_2_1, annee == year)$driving_real_emissions)
}

future_emissions_simulation_2_1  <- tibble(annee = 2019:2025, 
                                           NEDC_emissions_simulation_2_1 = NEDC_emissions_simulation_2_1,
                                           real_emissions_simulation_2_1= real_emissions_simulation_2_1)

future_emissions_simulation_2_1

as.data.frame(future_emissions_simulation_2_1)


write.csv(future_emissions_simulation_2_1, file="d:\\morvillier\\Desktop\\REVISION_BONUS_MALUS\\DONNEES_GRAPHIQUE\\future_emissions_simulation_2_1_barème2_weak_TP.csv", row.names = FALSE)




## CORRESPOND AU BAREME 2 DU PAPIER AVEC PT FORT
## CORRESPOND AU BAREME 2 DU PAPIER AVEC PT FORT
## CORRESPOND AU BAREME 2 DU PAPIER AVEC PT FORT 
## CORRESPOND AU BAREME 2 DU PAPIER AVEC PT FORT 




#Graph
future_emissions_simulation_2_1 %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with new feebate scale 1")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)



# SIMULATION 2 HYPOTHESE FORTE PT -----------------------------------------


products_2019_s2_2 <- products_2019
products_2019_s2_2 <- cbind(products_2019_s2_2, get_baremes_alternatifs_simulation_2(products_2019_s2_2)) #on ajoute les barèmes de la projection 1 pour 2022 à 2025

price_2019_simulation_2_2 <- products_2019_s2_2 %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_simulation_2_2 <- list()
for (year in 2019:2025) {
  product_list_simulation_2_2[[year]] <- price_2019_simulation_2_2 %>% mutate(annee = rep(year, nrow(price_2019_simulation_2_2)))
}
product_list_simulation_2_2 <- bind_rows(product_list_simulation_2_2) %>% relocate(annee)


#WE ADD THE OFFICIAL FEEBATES

future_feebates_simulation_2_2 <- products_2019_s2_2 %>% 
  dplyr::select(product_id, motorization, simulation_2_bareme_2019 = bareme_2019, simulation_2_bareme_2020 = bareme_2020, simulation_2_bareme_2021 = bareme_2021, simulation_2_bareme_2022, simulation_2_bareme_2023, simulation_2_bareme_2024, simulation_2_bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "simulation_2_bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_simulation_2_2 <- left_join(product_list_simulation_2_2, future_feebates_simulation_2_2, by = c("product_id", "motorization", "annee"))


# WE ADD THE WEIGHT TAX 

future_weight_fees_simulation_2_2 <- products_2019_s2_2 %>% 
  dplyr::select(product_id, motorization, weight_tax_2019, weight_tax_2020, weight_tax_2021, weight_tax_2022, weight_tax_2023, weight_tax_2024, weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_simulation_2_2 <- left_join(product_list_simulation_2_2, future_weight_fees_simulation_2_2, by = c("product_id", "motorization", "annee")) 

# WE ADD THE TECHNICAL PROGRESS, NORM and FINAL PRICE 

product_list_simulation_2_2 <- product_list_simulation_2_2 %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff_advanced(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 




# WE COMPUTE THE EXPECTED EMISSIONS USING THE OFFICIAL FEEBATES 

NEDC_emissions_simulation_2_2 <- c()
real_emissions_simulation_2_2 <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices <- filter(product_list_simulation_2_2, annee == year) %>% .$final_price
  shares <- share_nested(future_prices, products_2019$delta_with_resids)
  NEDC_emissions_simulation_2_2[i] <- get_CO2(shares, filter(product_list_simulation_2_2, annee == year)$co2)
  real_emissions_simulation_2_2[i] <- get_CO2(shares, filter(product_list_simulation_2_2, annee == year)$driving_real_emissions)
}

future_emissions_simulation_2_2  <- tibble(annee = 2019:2025, 
                                           NEDC_emissions_simulation_2_2 = NEDC_emissions_simulation_2_2,
                                           real_emissions_simulation_2_2= real_emissions_simulation_2_2)

future_emissions_simulation_2_2

as.data.frame(future_emissions_simulation_2_2)



write.csv(future_emissions_simulation_2_2, file="d:\\morvillier\\Desktop\\REVISION_BONUS_MALUS\\DONNEES_GRAPHIQUE\\future_emissions_simulation_2_2_barème2_strong_TP.csv", row.names = FALSE)







#Graph
future_emissions_simulation_2_2 %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with new feebate scale 1")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)

## CORRESPOND AU BAREME 1 AVEC WEAK TECHNICAL PROGRESS 
## CORRESPOND AU BAREME 1 AVEC WEAK TECHNICAL PROGRESS 
## CORRESPOND AU BAREME 1 AVEC WEAK TECHNICAL PROGRESS 
## CORRESPOND AU BAREME 1 AVEC WEAK TECHNICAL PROGRESS 




# Simulation 3 WEAK TECHNICAL PROGRESS HYPOTHESIS  ----------------------------------------


products_2019_s3_1 <- products_2019
products_2019_s3_1 <- cbind(products_2019_s3_1, get_baremes_alternatifs_simulation_3(products_2019_s3_1)) #on ajoute les barèmes de la projection 1 pour 2022 à 2025

price_2019_simulation_3_1 <- products_2019_s3_1 %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_simulation_3_1 <- list()
for (year in 2019:2025) {
  product_list_simulation_3_1[[year]] <- price_2019_simulation_3_1 %>% mutate(annee = rep(year, nrow(price_2019_simulation_3_1)))
}
product_list_simulation_3_1 <- bind_rows(product_list_simulation_3_1) %>% relocate(annee)


# WE ADD THE OFFICIAL FEEBATES 

future_feebates_simulation_3_1 <- products_2019_s3_1 %>% 
  dplyr::select(product_id, motorization, simulation_3_bareme_2019 = bareme_2019, simulation_3_bareme_2020 = bareme_2020, simulation_3_bareme_2021 = bareme_2021, simulation_3_bareme_2022, simulation_3_bareme_2023, simulation_3_bareme_2024, simulation_3_bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "simulation_3_bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_simulation_3_1 <- left_join(product_list_simulation_3_1, future_feebates_simulation_3_1, by = c("product_id", "motorization", "annee"))


# WE ADD THE WEIGHT TAX

future_weight_fees_simulation_3_1 <- products_2019_s3_1 %>% 
  dplyr::select(product_id, motorization, weight_tax_2019, weight_tax_2020, weight_tax_2021, weight_tax_2022, weight_tax_2023, weight_tax_2024, weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_simulation_3_1 <- left_join(product_list_simulation_3_1, future_weight_fees_simulation_3_1, by = c("product_id", "motorization", "annee")) 

# WE ADD THE technical PROGRESS, NORM AND COMPUTE FINAL PRICE

product_list_simulation_3_1 <- product_list_simulation_3_1 %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 


# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES

NEDC_emissions_simulation_3_1 <- c()
real_emissions_simulation_3_1 <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices <- filter(product_list_simulation_3_1, annee == year) %>% .$final_price
  shares <- share_nested(future_prices, products_2019$delta_with_resids)
  NEDC_emissions_simulation_3_1[i] <- get_CO2(shares, filter(product_list_simulation_3_1, annee == year)$co2)
  real_emissions_simulation_3_1[i] <- get_CO2(shares, filter(product_list_simulation_3_1, annee == year)$driving_real_emissions)
}

future_emissions_simulation_3_1  <- tibble(annee = 2019:2025, 
                                           NEDC_emissions_simulation_3_1 = NEDC_emissions_simulation_3_1,
                                           real_emissions_simulation_3_1= real_emissions_simulation_3_1)

future_emissions_simulation_3_1


as.data.frame(future_emissions_simulation_3_1)


write.csv(future_emissions_simulation_3_1, file="d:\\morvillier\\Desktop\\REVISION_BONUS_MALUS\\DONNEES_GRAPHIQUE\\future_emissions_simulation_3_1_barème2_weak_TP.csv", row.names = FALSE)





#Graph
future_emissions_simulation_3_1 %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with new feebate scale 1")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)





# SIMULATION 3 STRONG TECHNICAL PROGRESS HYPOTHESIS    -----------------------------------------


products_2019_s3_2 <- products_2019
products_2019_s3_2 <- cbind(products_2019_s3_2, get_baremes_alternatifs_simulation_3(products_2019_s3_2)) #on ajoute les barèmes de la projection 1 pour 2022 à 2025

price_2019_simulation_3_2 <- products_2019_s3_2 %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_simulation_3_2 <- list()
for (year in 2019:2025) {
  product_list_simulation_3_2[[year]] <- price_2019_simulation_3_2 %>% mutate(annee = rep(year, nrow(price_2019_simulation_3_2)))
}
product_list_simulation_3_2 <- bind_rows(product_list_simulation_3_2) %>% relocate(annee)


# WE ADD OFFICIAL FEEBATES 

future_feebates_simulation_3_2 <- products_2019_s3_2 %>% 
  dplyr::select(product_id, motorization, simulation_3_bareme_2019 = bareme_2019, simulation_3_bareme_2020 = bareme_2020, simulation_3_bareme_2021 = bareme_2021, simulation_3_bareme_2022, simulation_3_bareme_2023, simulation_3_bareme_2024, simulation_3_bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "simulation_3_bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_simulation_3_2 <- left_join(product_list_simulation_3_2, future_feebates_simulation_3_2, by = c("product_id", "motorization", "annee"))


# WE ADD WEIGHT TAX

future_weight_fees_simulation_3_2 <- products_2019_s3_2 %>% 
  dplyr::select(product_id, motorization, weight_tax_2019, weight_tax_2020, weight_tax_2021, weight_tax_2022, weight_tax_2023, weight_tax_2024, weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_simulation_3_2 <- left_join(product_list_simulation_3_2, future_weight_fees_simulation_3_2, by = c("product_id", "motorization", "annee")) 

# WE ADD TECHNICAL PROGRESS, NORM AND COMPUTE FINAL PRICE

product_list_simulation_3_2 <- product_list_simulation_3_2 %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff_advanced(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 



# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES

NEDC_emissions_simulation_3_2 <- c()
real_emissions_simulation_3_2 <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices <- filter(product_list_simulation_3_2, annee == year) %>% .$final_price
  shares <- share_nested(future_prices, products_2019$delta_with_resids)
  NEDC_emissions_simulation_3_2[i] <- get_CO2(shares, filter(product_list_simulation_3_2, annee == year)$co2)
  real_emissions_simulation_3_2[i] <- get_CO2(shares, filter(product_list_simulation_3_2, annee == year)$driving_real_emissions)
}

future_emissions_simulation_3_2  <- tibble(annee = 2019:2025, 
                                           NEDC_emissions_simulation_3_2 = NEDC_emissions_simulation_3_2,
                                           real_emissions_simulation_3_2= real_emissions_simulation_3_2)



shares <- product_list_simulation_3_2$shares

futures_prices <- product_list_simulation_3_2$future_prices

as.data.frame(shares)

as.data.frame(futures_prices)



write.csv(test, file="d:\\morvillier\\Desktop\\REVISION_BONUS_MALUS\\DONNEES_GRAPHIQUE\\test.csv", row.names = FALSE)



future_emissions_simulation_3_2

as.data.frame(future_emissions_simulation_3_2)




write.csv(future_emissions_simulation_3_2, file="d:\\morvillier\\Desktop\\REVISION_BONUS_MALUS\\DONNEES_GRAPHIQUE\\future_emissions_simulation_3_2_barème2_strong_TP.csv", row.names = FALSE)




#Graph
future_emissions_simulation_3_2 %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with new feebate scale 1")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)




# SIMULATION 4 WEAK TECHNICAL PROGRESS HYPOTHESIS : -----------------------------------------


products_2019_s4_1 <- products_2019
products_2019_s4_1 <- cbind(products_2019_s4_1, get_baremes_alternatifs_simulation_4(products_2019_s4_1)) #on ajoute les barèmes de la projection 1 pour 2022 à 2025

price_2019_simulation_4_1 <- products_2019_s4_1 %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_simulation_4_1 <- list()
for (year in 2019:2025) {
  product_list_simulation_4_1[[year]] <- price_2019_simulation_4_1 %>% mutate(annee = rep(year, nrow(price_2019_simulation_4_1)))
}
product_list_simulation_4_1 <- bind_rows(product_list_simulation_4_1) %>% relocate(annee)


#  WE ADD EXPECTED OFFICIAL FEEBATES

future_feebates_simulation_4_1 <- products_2019_s4_1 %>% 
  dplyr::select(product_id, motorization, simulation_4_bareme_2019 = bareme_2019, simulation_4_bareme_2020 = bareme_2020, simulation_4_bareme_2021 = bareme_2021, simulation_4_bareme_2022, simulation_4_bareme_2023, simulation_4_bareme_2024, simulation_4_bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "simulation_4_bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_simulation_4_1 <- left_join(product_list_simulation_4_1, future_feebates_simulation_4_1, by = c("product_id", "motorization", "annee"))


# WE ADD WEIGHT TAX

future_weight_fees_simulation_4_1 <- products_2019_s4_1 %>% 
  dplyr::select(product_id, motorization, weight_tax_2019, weight_tax_2020, weight_tax_2021, weight_tax_2022, weight_tax_2023, weight_tax_2024, weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_simulation_4_1 <- left_join(product_list_simulation_4_1, future_weight_fees_simulation_4_1, by = c("product_id", "motorization", "annee")) 

# WE ADD TECHNICAL PROGRESS, NORM AND COMPUTE FINAL PRICE

product_list_simulation_4_1 <- product_list_simulation_4_1 %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 



# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES

NEDC_emissions_simulation_4_1 <- c()
real_emissions_simulation_4_1 <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices <- filter(product_list_simulation_4_1, annee == year) %>% .$final_price
  shares <- share_nested(future_prices, products_2019$delta_with_resids)
  NEDC_emissions_simulation_4_1[i] <- get_CO2(shares, filter(product_list_simulation_4_1, annee == year)$co2)
  real_emissions_simulation_4_1[i] <- get_CO2(shares, filter(product_list_simulation_4_1, annee == year)$driving_real_emissions)
}

future_emissions_simulation_4_1  <- tibble(annee = 2019:2025, 
                                           NEDC_emissions_simulation_4_1 = NEDC_emissions_simulation_4_1,
                                           real_emissions_simulation_4_1= real_emissions_simulation_4_1)

future_emissions_simulation_4_1

as.data.frame(future_emissions_simulation_4_1)




#Graph
future_emissions_simulation_4_1 %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with new feebate scale 1")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)



# SIMULATION 4 HYPOTHESE FORTE PT : -----------------------------------------


products_2019_s4_2 <- products_2019
products_2019_s4_2 <- cbind(products_2019_s4_2, get_baremes_alternatifs_simulation_4(products_2019_s4_2)) #on ajoute les barèmes de la projection 1 pour 2022 à 2025

price_2019_simulation_4_2 <- products_2019_s4_2 %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids, co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_simulation_4_2 <- list()
for (year in 2019:2025) {
  product_list_simulation_4_2[[year]] <- price_2019_simulation_4_2 %>% mutate(annee = rep(year, nrow(price_2019_simulation_4_2)))
}
product_list_simulation_4_2 <- bind_rows(product_list_simulation_4_2) %>% relocate(annee)


#  WE ADD EXPECTED OFFICIAL FEEBATES

future_feebates_simulation_4_2 <- products_2019_s4_2 %>% 
  dplyr::select(product_id, motorization, simulation_4_bareme_2019 = bareme_2019, simulation_4_bareme_2020 = bareme_2020, simulation_4_bareme_2021 = bareme_2021, simulation_4_bareme_2022, simulation_4_bareme_2023, simulation_4_bareme_2024, simulation_4_bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "simulation_4_bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_simulation_4_2 <- left_join(product_list_simulation_4_2, future_feebates_simulation_4_2, by = c("product_id", "motorization", "annee"))


# WE ADD WEIGHT TAX

future_weight_fees_simulation_4_2 <- products_2019_s4_2 %>% 
  dplyr::select(product_id, motorization, weight_tax_2019, weight_tax_2020, weight_tax_2021, weight_tax_2022, weight_tax_2023, weight_tax_2024, weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_simulation_4_2 <- left_join(product_list_simulation_4_2, future_weight_fees_simulation_4_2, by = c("product_id", "motorization", "annee")) 

# WE ADD TECHNICAL PROGRESS, NORM AND COMPUTE FINAL PRICE

product_list_simulation_4_2 <- product_list_simulation_4_2 %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff_advanced(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 



# WE COMPUTE THE EXPECTED EMISSIONS WITH THE OFFICIAL FEEBATES

NEDC_emissions_simulation_4_2 <- c()
real_emissions_simulation_4_2 <- c()

for (year in 2019:2025) {
  i <- year - 2018
  future_prices <- filter(product_list_simulation_4_2, annee == year) %>% .$final_price
  shares <- share_nested(future_prices, products_2019$delta_with_resids)
  NEDC_emissions_simulation_4_2[i] <- get_CO2(shares, filter(product_list_simulation_4_2, annee == year)$co2)
  real_emissions_simulation_4_2[i] <- get_CO2(shares, filter(product_list_simulation_4_2, annee == year)$driving_real_emissions)
}

future_emissions_simulation_4_2  <- tibble(annee = 2019:2025, 
                                           NEDC_emissions_simulation_4_2 = NEDC_emissions_simulation_4_2,
                                           real_emissions_simulation_4_2= real_emissions_simulation_4_2)

future_emissions_simulation_4_2

as.data.frame(future_emissions_simulation_4_2)

write_xlsx(future_emissions_simulation_4_2,"C:\\Users\\morvi\\OneDrive\\Bureau\\Bonus malus\\DONNEES_GRAPHIQUE\\future_emissions_simulation_4_2.xlsx")


#Graph
future_emissions_simulation_4_2 %>%
  pivot_longer(-annee, names_to = "emission_scope", values_to = "emissions") %>% 
  ggplot(aes(x = annee, y = emissions, colour = emission_scope)) + geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Emissions (gCO2/km)", title = "Emissions with new feebate scale 1")
#ggsave("outputs/plots/Current_emissions_to_2023.png", w = 4, h = 4)
ggsave("outputs/plots/Current_emissions_to_2023_without_technical_progress.png", w = 4, h = 4)


# bareme infos ------------------------------------------------------------

get_yearly_infos <- function(dataset, annee_filtre){
  df <- dataset %>% 
    dplyr::select(annee, delta_with_resids, final_price, feebate)%>%
    filter(annee == annee_filtre) %>%
    mutate(new_shares = share_nested(final_price, delta_with_resids)) %>%
    mutate(malus_dummy = ifelse(feebate > 0, 1, 0),
           bonus_dummy = ifelse(feebate < 0, 1, 0),
           total_dummy = ifelse(feebate != 0, 1, 0))
  
  average_malus_rate <- sum(df$malus_dummy*(10000*df$feebate)*df$new_shares)/sum(df$new_shares*df$malus_dummy)
  average_bonus_rate <- sum(df$bonus_dummy*(10000*df$feebate)*df$new_shares)/sum(df$new_shares*df$bonus_dummy)


  Pourcentage_touches_malus <- sum(df$malus_dummy*df$new_shares)/sum(df$new_shares)*100
  Pourcentage_touches_bonus <- sum(df$bonus_dummy*df$new_shares)/sum(df$new_shares)*100
  Pourcentage_touches_total <- sum(df$total_dummy*df$new_shares)/sum(df$new_shares)*100
  
  list(average_malus_rate = average_malus_rate,
       average_bonus_rate = average_bonus_rate,
       Pourcentage_touches_malus = Pourcentage_touches_malus,
       Pourcentage_touches_bonus = Pourcentage_touches_bonus,
       Pourcentage_touches_total = Pourcentage_touches_total)
  
}


product_list


resultat_bareme_gouv <- list(get_yearly_infos(product_list, 2022),
                          get_yearly_infos(product_list, 2023),
                          get_yearly_infos(product_list, 2024),
                          get_yearly_infos(product_list, 2025))

resultat_bareme_2 <- list(get_yearly_infos(product_list_simulation_2_2, 2022),
                          get_yearly_infos(product_list_simulation_2_2, 2023),
                          get_yearly_infos(product_list_simulation_2_2, 2024),
                          get_yearly_infos(product_list_simulation_2_2, 2025))




resultat_bareme_3 <- list( get_yearly_infos(product_list_simulation_3_2, 2022),
get_yearly_infos(product_list_simulation_3_2, 2023),
get_yearly_infos(product_list_simulation_3_2, 2024),
get_yearly_infos(product_list_simulation_3_2, 2025))


resultat_bareme_4 <- list( get_yearly_infos(product_list_simulation_4_2, 2022),
get_yearly_infos(product_list_simulation_4_2, 2023),
get_yearly_infos(product_list_simulation_4_2, 2024),
get_yearly_infos(product_list_simulation_4_2, 2025))






get_yearly_infos <- function(dataset, annee_filtre){
  df <- dataset %>% 
    dplyr::select(annee, delta_with_resids, final_price, feebate)%>%
    filter(annee == annee_filtre) %>%
    mutate(new_shares = share_nested(final_price, delta_with_resids)) %>%
    mutate(malus_dummy = ifelse(feebate > 0, 1, 0),
           bonus_dummy = ifelse(feebate < 0, 1, 0),
           total_dummy = ifelse(feebate != 0, 1, 0))
  
  average_malus_rate <- sum(df$malus_dummy*(10000*df$feebate)*df$new_shares)/sum(df$new_shares*df$malus_dummy)
  average_bonus_rate <- sum(df$bonus_dummy*(10000*df$feebate)*df$new_shares)/sum(df$new_shares*df$bonus_dummy)
  
  
  Pourcentage_touches_malus <- sum(df$malus_dummy*df$new_shares)/sum(df$new_shares)*100
  Pourcentage_touches_bonus <- sum(df$bonus_dummy*df$new_shares)/sum(df$new_shares)*100
  Pourcentage_touches_total <- sum(df$total_dummy*df$new_shares)/sum(df$new_shares)*100
  
  list(average_malus_rate = average_malus_rate,
       average_bonus_rate = average_bonus_rate,
       Pourcentage_touches_malus = Pourcentage_touches_malus,
       Pourcentage_touches_bonus = Pourcentage_touches_bonus,
       Pourcentage_touches_total = Pourcentage_touches_total)
  
}


get_infos_seuil <- function(dataset, annee_filtre){
  df <- dataset %>% 
    dplyr::select(annee, delta_with_resids, final_price, feebate)%>%
    filter(annee == annee_filtre) %>%
    mutate(new_shares = share_nested(final_price, delta_with_resids)) %>%
    mutate(bonus=ifelse(feebate<0,1,0),
           malus_nul=ifelse(feebate==0, 1, 0),
           malus_dummy = ifelse(feebate>0 & feebate<=0.10 , 1, 0),
           malus_dummy_1 = ifelse(feebate>0.10 & feebate<=1 , 1, 0),
           malus_dummy_2 = ifelse(feebate>1  & feebate<=2 , 1, 0),
           malus_dummy_3 = ifelse(feebate>2  , 1, 0))
  
  pourcentage_bonus <- sum(df$bonus*df$new_shares)/sum(df$new_shares)*100
  Pourcentage_touche_malus_nul <- sum(df$malus_nul*df$new_shares)/sum(df$new_shares)*100
  Pourcentage_touches_malus <- sum(df$malus_dummy*df$new_shares)/sum(df$new_shares)*100
  Pourcentage_touches_malus_1 <- sum(df$malus_dummy_1*df$new_shares)/sum(df$new_shares)*100
  Pourcentage_touches_malus_2 <- sum(df$malus_dummy_2*df$new_shares)/sum(df$new_shares)*100
  Pourcentage_touches_malus_3 <- sum(df$malus_dummy_3*df$new_shares)/sum(df$new_shares)*100
  
  list(pourcentage_bonus=pourcentage_bonus, 
      Pourcentage_touche_malus_nul=Pourcentage_touche_malus_nul, 
      Pourcentage_touches_malus=Pourcentage_touches_malus, 
       Pourcentage_touches_malus_1 = Pourcentage_touches_malus_1,
       Pourcentage_touches_malus_2 = Pourcentage_touches_malus_2,
       Pourcentage_touches_malus_3 = Pourcentage_touches_malus_3)
}



resultat_bareme_gouv_seuil <- list(get_infos_seuil(product_list, 2022),
                             get_infos_seuil(product_list, 2023),
                             get_infos_seuil(product_list, 2024),
                             get_infos_seuil(product_list, 2025))

resultat_bareme_2_seuil <- list(get_infos_seuil(product_list_simulation_2_2, 2022),
                          get_infos_seuil(product_list_simulation_2_2, 2023),
                          get_infos_seuil(product_list_simulation_2_2, 2024),
                          get_infos_seuil(product_list_simulation_2_2, 2025))




resultat_bareme_3_seuil <- list( get_infos_seuil(product_list_simulation_3_2, 2022),
                           get_infos_seuil(product_list_simulation_3_2, 2023),
                           get_infos_seuil(product_list_simulation_3_2, 2024),
                           get_infos_seuil(product_list_simulation_3_2, 2025))


resultat_bareme_4_seuil <- list( get_infos_seuil(product_list_simulation_4_2, 2022),
                           get_infos_seuil(product_list_simulation_4_2, 2023),
                           get_infos_seuil(product_list_simulation_4_2, 2024),
                           get_infos_seuil(product_list_simulation_4_2, 2025))





#Grouging ------------------------------------------------------------

# Grouping GOVERNMENTAL and SNBC DATABASES

future_gov_SNBC <- tibble(annee = 2019:2025, 
                       SNBC_emissions = future_emissions_current_SNBC$emissions,
                       NEDC_emissions_gvt_strong_TP = NEDC_emissions,
                       NEDC_emissions_gvt_weak_TP = NEDC_emissions_weak_TP)


# GROUPING ALL THE RESULTS TOGETHER 

future_all <- tibble(annee = 2019:2025, 
                     SNBC_emissions = future_emissions_current_SNBC$emissions,
                     real_emissions_gvt_weak_TP = real_emissions_weak_TP,
                     real_emissions_gvt_strong_TP = real_emissions,
                     NEDC_emissions_gvt_weak_TP = NEDC_emissions_weak_TP,
                     NEDC_emissions_gvt_strong_TP = NEDC_emissions,
                     NEDC_emissions_simulation_1_1 = NEDC_emissions_simulation_1_1,
                     NEDC_emissions_simulation_1_2 = NEDC_emissions_simulation_1_2,
                     NEDC_emissions_simulation_2_1 = NEDC_emissions_simulation_2_1,
                     NEDC_emissions_simulation_2_2 = NEDC_emissions_simulation_2_2,
                     NEDC_emissions_simulation_3_1 = NEDC_emissions_simulation_3_1,
                     NEDC_emissions_simulation_3_2 = NEDC_emissions_simulation_3_2,
                     NEDC_emissions_simulation_4_1 = NEDC_emissions_simulation_4_1,
                     NEDC_emissions_simulation_4_2 = NEDC_emissions_simulation_4_2)

future_all <- as.data.frame(future_all)

early <- future_all %>% filter(annee < 2024) %>% select(annee, NEDC_emissions)
late <- future_all %>% filter(annee > 2023)

clr <- c("Gvt Scales Weak TP" = "brown2", "Gvt Scales Strong TP" = "chocolate1", "SNBC Objectives" = "chartreuse3", "Barème 1 Faible PT" = "steelblue", "Barème 1 Fort PT" = "deepskyblue", 
         "Barème 2 Faible PT" = "orange", "Barème 2 Fort PT" = "orange4", "Barème 3 Faible PT" = "aquamarine", "Barème 3 Fort PT" = "aquamarine4",
         "Barème 4 Faible PT" = "purple", "Barème 4 Fort PT" = "purple4") 
  
ggplot(future_all, aes(x = annee)) + 
  geom_line(mapping = aes(y= NEDC_emissions, color = "NEDC")) + 
  geom_line(mapping = aes(y= SNBC_emissions, color = "SNBC")) + 
  geom_line(mapping = aes(y = NEDC_emissions_simulation_1_2, color = "Barème 1")) +
  geom_line(mapping = aes(y = NEDC_emissions_simulation_3_2, color = "Barème 2"))+
  theme_bw() + 
  labs(x = "", y = "CO2 Emissions (gCO2/km)", color = "Legend", title = "Evolution des émissions NEDC selon barème adoptés") +
  scale_color_manual(values = clr) 
ggsave("outputs/plots/evolution_emission_baremes_1_3.png", w = 8, h = 5)





geom_line(mapping = aes(y = NEDC_emissions_simulation_1_1), color = "steelblue") 



### Graphic : Feebate and SNBC   -------------------------------------------

##SNBC et GVT 
ggplot(future_all, aes(x = annee)) + 
  geom_line(mapping = aes(y= real_emissions_gvt_weak_TP, color = "Gvt Scales Weak TP")) + 
  geom_line(mapping = aes(y= real_emissions_gvt_strong_TP, color = "Gvt Scales Strong TP")) + 
  geom_line(mapping = aes(y= SNBC_emissions, color = "SNBC Objectives")) + 
  theme_bw() + 
  labs(x = "", y = "CO2 Emissions (gCO2/km)", color = "Legend", title = "Government Feebates Scales and SNBC Strategy in real emissions") +
  scale_color_manual(values = clr) +  ylim(0, 160)
ggsave("outputs/plots/comparaison_snbc_et_gvt_bareme.png", w = 11, h = 7)




future_rearranged <- tibble(annee = 2019:2025, 
                     SNBC_emissions = future_emissions_current_SNBC$emissions,
                     real_emissions_gvt_weak_TP = real_emissions_weak_TP,
                     real_emissions_gvt_strong_TP = real_emissions,
                     real_emissions_scheme_1_weak_TP = real_emissions_simulation_2_1,
                     real_emissions_scheme_1_strong_TP = real_emissions_simulation_2_2,
                     real_emissions_scheme_2_weak_TP = real_emissions_simulation_3_1,
                     real_emissions_scheme_2_strong_TP= real_emissions_simulation_3_2)

clr2 <- c("Gvt Scheme Weak TP" = "brown2", "Gvt Scheme Strong TP" = "chocolate1", "SNBC Objectives" = "chartreuse3", "Alternative Scheme 1 Weak PT" = "steelblue", "Alternative Scheme 1 Strong PT" = "deepskyblue", 
                 "Alternative Scheme 2 Weak PT" = "orange", "Alternative Scheme 2 Strong PT" = "orange2")

## SNBC GVT SCENARIO 1 (bareme 3) and Scenario 2 (bareme2) weak Technical Progress
ggplot(future_rearranged, aes(x = annee)) + 
  geom_line(mapping = aes(y= real_emissions_gvt_weak_TP, color = "Gvt Scheme Weak TP")) + 
  geom_line(mapping = aes(y= real_emissions_scheme_1_weak_TP, color = "Alternative Scheme 1 Weak PT")) + 
  geom_line(mapping = aes(y= real_emissions_scheme_2_weak_TP, color = "Alternative Scheme 2 Weak PT")) + 
  geom_line(mapping = aes(y= SNBC_emissions, color = "SNBC Objectives")) + 
  theme_bw() + 
  labs(x = "", y = "CO2 Emissions (gCO2/km)", color = "Legend") +
  scale_color_manual(values = clr2) +  ylim(0, 160)
ggsave("outputs/plots/comparaison_weakTP_baremes_et_snbc_et_gvt_bareme.png", w = 11, h = 7)


## SNBC GVT SCENARIO 1 (bareme 3) and Scenario 2 (bareme2) strong Technical Progress
ggplot(future_rearranged, aes(x = annee)) + 
  geom_line(mapping = aes(y= real_emissions_gvt_strong_TP, color = "Gvt Scheme Weak TP")) + 
  geom_line(mapping = aes(y= real_emissions_scheme_1_strong_TP, color = "Alternative Scheme 1 Strong PT")) + 
  geom_line(mapping = aes(y= real_emissions_scheme_2_strong_TP, color = "Alternative Scheme 2 Strong PT")) + 
  geom_line(mapping = aes(y= SNBC_emissions, color = "SNBC Objectives")) + 
  theme_bw() + 
  labs(x = "", y = "CO2 Emissions (gCO2/km)", color = "Legend") +
  scale_color_manual(values = clr2) + ylim(0, 160)
ggsave("outputs/plots/comparaison_strongTP_baremes_et_snbc_et_gvt_bareme.png", w = 11, h = 7)




##Bareme 1 and  SNBC 
ggplot(future_all, aes(x = annee)) + 
  geom_line(mapping = aes(y= NEDC_emissions_simulation_1_1, color = "Barème 1 Faible PT")) + 
  geom_line(mapping = aes(y= NEDC_emissions_simulation_1_2, color = "Barème 1 Fort PT")) + 
  geom_line(mapping = aes(y= SNBC_emissions, color = "SNBC")) + 
  theme_bw() + 
  labs(x = "", y = "NEDC Emissions (gCO2/km)", color = "Legend", title = "Barèmes 1 et stratégie SNBC") +
  scale_color_manual(values = clr)
ggsave("outputs/plots/comparaison_snbc_et_baremes_1.png", w = 8, h = 5)

##Bareme 2 and SNBC 
ggplot(future_all, aes(x = annee)) + 
  geom_line(mapping = aes(y= NEDC_emissions_simulation_2_1, color = "Barème 2 Faible PT")) + 
  geom_line(mapping = aes(y= NEDC_emissions_simulation_2_2, color = "Barème 2 Fort PT")) + 
  geom_line(mapping = aes(y= SNBC_emissions, color = "SNBC")) + 
  theme_bw() + 
  labs(x = "", y = "NEDC Emissions (gCO2/km)", color = "Legend", title = "Barèmes 2 et stratégie SNBC") +
  scale_color_manual(values = clr)
ggsave("outputs/plots/comparaison_snbc_et_baremes_2.png", w = 8, h = 5)


##Bareme 3 and SNBC 
ggplot(future_all, aes(x = annee)) + 
  geom_line(mapping = aes(y= NEDC_emissions_simulation_3_1, color = "Barème 3 Faible PT")) + 
  geom_line(mapping = aes(y= NEDC_emissions_simulation_3_2, color = "Barème 3 Fort PT")) + 
  geom_line(mapping = aes(y= SNBC_emissions, color = "SNBC")) + 
  theme_bw() + 
  labs(x = "", y = "NEDC Emissions (gCO2/km)", color = "Legend", title = "Barèmes 3 et stratégie SNBC") +
  scale_color_manual(values = clr)
ggsave("outputs/plots/comparaison_snbc_et_baremes_3.png", w = 8, h = 5)


##Bareme 4 and SNBC 
ggplot(future_all, aes(x = annee)) + 
  geom_line(mapping = aes(y= NEDC_emissions_simulation_4_1, color = "Barème 4 Faible PT")) + 
  geom_line(mapping = aes(y= NEDC_emissions_simulation_4_2, color = "Barème 4 Fort PT")) + 
  geom_line(mapping = aes(y= SNBC_emissions, color = "SNBC")) + 
  theme_bw() + 
  labs(x = "", y = "NEDC Emissions (gCO2/km)", color = "Legend", title = "Barèmes 4 et stratégie SNBC") +
  scale_color_manual(values = clr)
ggsave("outputs/plots/comparaison_snbc_et_baremes_4.png", w = 8, h = 5)


#   


# BAR GRAPH vehicules distribution

# Tax repartition of the 2022 governmental feebate vs proposed feebate 

#comparison government and  simulation 1  

products_2019_s1_2 %>%
  mutate(tranche_bm_gvt = case_when(
    bareme_2022 > 0 & bareme_2022 <= 1000 ~ "0-1000",
    bareme_2022 > 1000 & bareme_2022<=2500 ~ "1000-2500",
    bareme_2022 > 2500 & bareme_2022<=5000 ~ "2500-5000",
    bareme_2022 > 5000 & bareme_2022<=7500 ~ "5000-7500",
    bareme_2022 > 7500 & bareme_2022<=10000 ~ "7500-10000",
    bareme_2022 > 10000 & bareme_2022<=15000 ~ "10000-15000",
    bareme_2022 > 15000 & bareme_2022<=20000 ~ "15000-20000",
    bareme_2022 > 20000 & bareme_2022<=30000 ~ "20000-30000",
    bareme_2022 > 30000 & bareme_2022<=40000 ~ "30000-40000",
    bareme_2022 > 40000 & bareme_2022<=60000 ~ "40000-60000",
    bareme_2022 > 60000 & bareme_2022<=80000 ~ "60000-80000",
    bareme_2022 > 80000 & bareme_2022<=100000 ~ "80000-100000",
    bareme_2022 == 0 ~ "0",
    bareme_2022 >= -1000 & bareme_2022< 0 ~ "Bonus 0-1000",
    bareme_2022 >= -2500 & bareme_2022< -1000 ~ "Bonus 1000-2500",
    bareme_2022 >= -5000 & bareme_2022< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R")) %>%
  mutate(tranche_bm_propose = case_when(
    simulation_1_bareme_2022 > 0 & simulation_1_bareme_2022 <= 1000 ~ "0-1000",
    simulation_1_bareme_2022 > 1000 & simulation_1_bareme_2022<=2500 ~ "1000-2500",
    simulation_1_bareme_2022 > 2500 & simulation_1_bareme_2022<=5000 ~ "2500-5000",
    simulation_1_bareme_2022 > 5000 & simulation_1_bareme_2022<=7500 ~ "5000-7500",
    simulation_1_bareme_2022 > 7500 & simulation_1_bareme_2022<=10000 ~ "7500-10000",
    simulation_1_bareme_2022 > 10000 & simulation_1_bareme_2022<=15000 ~ "10000-15000",
    simulation_1_bareme_2022 > 15000 & simulation_1_bareme_2022<=20000 ~ "15000-20000",
    simulation_1_bareme_2022 > 20000 & simulation_1_bareme_2022<=30000 ~ "20000-30000",
    simulation_1_bareme_2022 > 30000 & simulation_1_bareme_2022<=40000 ~ "30000-40000",
    simulation_1_bareme_2022 > 40000 & simulation_1_bareme_2022<=60000 ~ "40000-60000",
    simulation_1_bareme_2022 > 60000 & simulation_1_bareme_2022<=80000 ~ "60000-80000",
    simulation_1_bareme_2022 > 80000 & simulation_1_bareme_2022<=100000 ~ "80000-100000",
    simulation_1_bareme_2022 == 0 ~ "0",
    simulation_1_bareme_2022 >= -1000 & simulation_1_bareme_2022< 0 ~ "Bonus 0-1000",
    simulation_1_bareme_2022 >= -2500 & simulation_1_bareme_2022< -1000 ~ "Bonus 1000-2500",
    simulation_1_bareme_2022 >= -5000 & simulation_1_bareme_2022< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R"))%>%
  dplyr::select(motorization, tranche_bm_gvt,tranche_bm_propose) %>%
  pivot_longer(-motorization, names_to = "Feebate", values_to = "Treshold") %>%
  select(-motorization) %>%
  group_by(Treshold, Feebate) %>%
  summarise(total_impacted = length(Treshold)) %>%
  group_by(Feebate)%>%
  mutate(pourcentage_impacted = total_impacted / sum(total_impacted) * 100) %>%
  ggplot(aes(x = pourcentage_impacted, y = Treshold, fill = Feebate)) + geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + scale_y_discrete(limits=  c("80000-100000", "60000-80000", "40000-60000", "30000-40000", "20000-30000", "15000-20000", "10000-15000",
                                           "7500-10000", "5000-7500", "2500-5000", "1000-2500", "0-1000", "0", "Bonus 0-1000", "Bonus 1000-2500", "Bonus 2500-5000")) +
  labs(x = "Part en Pourcentage", y = "", title = "Répartition des véhicules par montant du Bonus-Malus : comparaison \n barème du gouvernment et barème proposé n°1 en 2022")
ggsave("outputs/plots/Repartition_bonus_malus_2022_gvt_sim_1.png", w = 8, h = 5)



#comparison government and simulation2 

products_2019_s2_2 %>%
  mutate(tranche_bm_gvt = case_when(
    bareme_2022 > 0 & bareme_2022 <= 1000 ~ "0-1000",
    bareme_2022 > 1000 & bareme_2022<=2500 ~ "1000-2500",
    bareme_2022 > 2500 & bareme_2022<=5000 ~ "2500-5000",
    bareme_2022 > 5000 & bareme_2022<=7500 ~ "5000-7500",
    bareme_2022 > 7500 & bareme_2022<=10000 ~ "7500-10000",
    bareme_2022 > 10000 & bareme_2022<=15000 ~ "10000-15000",
    bareme_2022 > 15000 & bareme_2022<=20000 ~ "15000-20000",
    bareme_2022 > 20000 & bareme_2022<=30000 ~ "20000-30000",
    bareme_2022 > 30000 & bareme_2022<=40000 ~ "30000-40000",
    bareme_2022 > 40000 & bareme_2022<=60000 ~ "40000-60000",
    bareme_2022 > 60000 & bareme_2022<=80000 ~ "60000-80000",
    bareme_2022 > 80000 & bareme_2022<=100000 ~ "80000-100000",
    bareme_2022 == 0 ~ "0",
    bareme_2022 >= -1000 & bareme_2022< 0 ~ "Bonus 0-1000",
    bareme_2022 >= -2500 & bareme_2022< -1000 ~ "Bonus 1000-2500",
    bareme_2022 >= -5000 & bareme_2022< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R")) %>%
  mutate(tranche_bm_propose = case_when(
    simulation_2_bareme_2022 > 0 & simulation_2_bareme_2022 <= 1000 ~ "0-1000",
    simulation_2_bareme_2022 > 1000 & simulation_2_bareme_2022<=2500 ~ "1000-2500",
    simulation_2_bareme_2022 > 2500 & simulation_2_bareme_2022<=5000 ~ "2500-5000",
    simulation_2_bareme_2022 > 5000 & simulation_2_bareme_2022<=7500 ~ "5000-7500",
    simulation_2_bareme_2022 > 7500 & simulation_2_bareme_2022<=10000 ~ "7500-10000",
    simulation_2_bareme_2022 > 10000 & simulation_2_bareme_2022<=15000 ~ "10000-15000",
    simulation_2_bareme_2022 > 15000 & simulation_2_bareme_2022<=20000 ~ "15000-20000",
    simulation_2_bareme_2022 > 20000 & simulation_2_bareme_2022<=30000 ~ "20000-30000",
    simulation_2_bareme_2022 > 30000 & simulation_2_bareme_2022<=40000 ~ "30000-40000",
    simulation_2_bareme_2022 > 40000 & simulation_2_bareme_2022<=60000 ~ "40000-60000",
    simulation_2_bareme_2022 > 60000 & simulation_2_bareme_2022<=80000 ~ "60000-80000",
    simulation_2_bareme_2022 > 80000 & simulation_2_bareme_2022<=100000 ~ "80000-100000",
    simulation_2_bareme_2022 == 0 ~ "0",
    simulation_2_bareme_2022 >= -1000 & simulation_2_bareme_2022< 0 ~ "Bonus 0-1000",
    simulation_2_bareme_2022 >= -2500 & simulation_2_bareme_2022< -1000 ~ "Bonus 1000-2500",
    simulation_2_bareme_2022 >= -5000 & simulation_2_bareme_2022< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R"))%>%
  dplyr::select(motorization, tranche_bm_gvt,tranche_bm_propose) %>%
  pivot_longer(-motorization, names_to = "Feebate", values_to = "Treshold") %>%
  select(-motorization) %>%
  group_by(Treshold, Feebate) %>%
  summarise(total_impacted = length(Treshold)) %>%
  group_by(Feebate)%>%
  mutate(pourcentage_impacted = total_impacted / sum(total_impacted) * 100) %>%
  ggplot(aes(x = pourcentage_impacted, y = Treshold, fill = Feebate)) + geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + scale_y_discrete(limits=  c("80000-100000", "60000-80000", "40000-60000", "30000-40000", "20000-30000", "15000-20000", "10000-15000",
                                           "7500-10000", "5000-7500", "2500-5000", "1000-2500", "0-1000", "0", "Bonus 0-1000", "Bonus 1000-2500", "Bonus 2500-5000")) +
  labs(x = "Part en Pourcentage", y = "", title = "Répartition des véhicules par montant du Bonus-Malus : comparaison \n barème du gouvernment et barème proposé n°2 en 2022")
ggsave("outputs/plots/Repartition_bonus_malus_2022_gvt_sim_2.png", w = 8, h = 5)





#comparison government and simulation3 

products_2019_s3_2 %>%
  mutate(tranche_bm_gvt = case_when(
    bareme_2022 > 0 & bareme_2022 <= 1000 ~ "0-1000",
    bareme_2022 > 1000 & bareme_2022<=2500 ~ "1000-2500",
    bareme_2022 > 2500 & bareme_2022<=5000 ~ "2500-5000",
    bareme_2022 > 5000 & bareme_2022<=7500 ~ "5000-7500",
    bareme_2022 > 7500 & bareme_2022<=10000 ~ "7500-10000",
    bareme_2022 > 10000 & bareme_2022<=15000 ~ "10000-15000",
    bareme_2022 > 15000 & bareme_2022<=20000 ~ "15000-20000",
    bareme_2022 > 20000 & bareme_2022<=25000 ~ "20000-25000",
    bareme_2022 > 25000 & bareme_2022<=30000 ~ "25000-30000",
    bareme_2022 > 25000 & bareme_2022<=35000 ~ "30000-35000",
    bareme_2022 > 35000 & bareme_2022<=40000 ~ "35000-40000",
    bareme_2022 == 0 ~ "0",
    bareme_2022 >= -1000 & bareme_2022< 0 ~ "Bonus 0-1000",
    bareme_2022 >= -2500 & bareme_2022< -1000 ~ "Bonus 1000-2500",
    bareme_2022 >= -5000 & bareme_2022< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R")) %>%
  mutate(tranche_bm_propose = case_when(
    simulation_3_bareme_2022 > 0 & simulation_3_bareme_2022 <= 1000 ~ "0-1000",
    simulation_3_bareme_2022 > 1000 & simulation_3_bareme_2022<=2500 ~ "1000-2500",
    simulation_3_bareme_2022 > 2500 & simulation_3_bareme_2022<=5000 ~ "2500-5000",
    simulation_3_bareme_2022 > 5000 & simulation_3_bareme_2022<=7500 ~ "5000-7500",
    simulation_3_bareme_2022 > 7500 & simulation_3_bareme_2022<=10000 ~ "7500-10000",
    simulation_3_bareme_2022 > 10000 & simulation_3_bareme_2022<=15000 ~ "10000-15000",
    simulation_3_bareme_2022 > 15000 & simulation_3_bareme_2022<=20000 ~ "15000-20000",
    simulation_3_bareme_2022 > 20000 & simulation_3_bareme_2022<=25000 ~ "20000-25000",
    simulation_3_bareme_2022 > 25000 & simulation_3_bareme_2022<=30000 ~ "25000-30000",
    simulation_3_bareme_2022 > 30000 & simulation_3_bareme_2022<=35000 ~ "30000-35000",
    simulation_3_bareme_2022 > 35000 & simulation_3_bareme_2022<=40000 ~ "35000-40000",
    simulation_3_bareme_2022 == 0 ~ "0",
    simulation_3_bareme_2022 >= -1000 & simulation_3_bareme_2022< 0 ~ "Bonus 0-1000",
    simulation_3_bareme_2022 >= -2500 & simulation_3_bareme_2022< -1000 ~ "Bonus 1000-2500",
    simulation_3_bareme_2022 >= -5000 & simulation_3_bareme_2022< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R"))%>%
  dplyr::select(motorization, tranche_bm_gvt,tranche_bm_propose) %>%
  pivot_longer(-motorization, names_to = "Feebate", values_to = "Treshold") %>%
  dplyr::select(-motorization) %>%
  group_by(Treshold, Feebate) %>%
  summarise(total_impacted = length(Treshold)) %>%
  group_by(Feebate)%>%
  mutate(pourcentage_impacted = total_impacted / sum(total_impacted) * 100) %>%
  ggplot(aes(x = pourcentage_impacted, y = Treshold, fill = Feebate)) + geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + scale_y_discrete(limits=  c("35000-40000", "30000-35000", "25000-30000", "20000-25000", "15000-20000", "10000-15000",
                                           "7500-10000", "5000-7500", "2500-5000", "1000-2500", "0-1000", "0", "Bonus 0-1000", "Bonus 1000-2500", "Bonus 2500-5000")) +
  labs(x = "Part en Pourcentage", y = "", title = "Répartition des véhicules par montant du Bonus-Malus : comparaison \n barème du gouvernment et barème proposé n°3 en 2022")
ggsave("outputs/plots/Repartition_bonus_malus_2022_gvt_sim_3.png", w = 8, h = 5)



#comparison between government feebates and simulation4


products_2019_s4_2 %>%
  mutate(tranche_bm_gvt = case_when(
    bareme_2022 > 0 & bareme_2022 <= 1000 ~ "0-1000",
    bareme_2022 > 1000 & bareme_2022<=2500 ~ "1000-2500",
    bareme_2022 > 2500 & bareme_2022<=5000 ~ "2500-5000",
    bareme_2022 > 5000 & bareme_2022<=7500 ~ "5000-7500",
    bareme_2022 > 7500 & bareme_2022<=10000 ~ "7500-10000",
    bareme_2022 > 10000 & bareme_2022<=15000 ~ "10000-15000",
    bareme_2022 > 15000 & bareme_2022<=20000 ~ "15000-20000",
    bareme_2022 > 20000 & bareme_2022<=25000 ~ "20000-25000",
    bareme_2022 > 25000 & bareme_2022<=30000 ~ "25000-30000",
    bareme_2022 > 25000 & bareme_2022<=35000 ~ "30000-40000",
    bareme_2022 > 35000 & bareme_2022<=40000 ~ "40000-50000",
    bareme_2022 == 0 ~ "0",
    bareme_2022 >= -1000 & bareme_2022< 0 ~ "Bonus 0-1000",
    bareme_2022 >= -2500 & bareme_2022< -1000 ~ "Bonus 1000-2500",
    bareme_2022 >= -5000 & bareme_2022< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R")) %>%
  mutate(tranche_bm_propose = case_when(
    simulation_4_bareme_2022 > 0 & simulation_4_bareme_2022 <= 1000 ~ "0-1000",
    simulation_4_bareme_2022 > 1000 & simulation_4_bareme_2022<=2500 ~ "1000-2500",
    simulation_4_bareme_2022 > 2500 & simulation_4_bareme_2022<=5000 ~ "2500-5000",
    simulation_4_bareme_2022 > 5000 & simulation_4_bareme_2022<=7500 ~ "5000-7500",
    simulation_4_bareme_2022 > 7500 & simulation_4_bareme_2022<=10000 ~ "7500-10000",
    simulation_4_bareme_2022 > 10000 & simulation_4_bareme_2022<=15000 ~ "10000-15000",
    simulation_4_bareme_2022 > 15000 & simulation_4_bareme_2022<=20000 ~ "15000-20000",
    simulation_4_bareme_2022 > 20000 & simulation_4_bareme_2022<=25000 ~ "20000-25000",
    simulation_4_bareme_2022 > 25000 & simulation_4_bareme_2022<=30000 ~ "25000-30000",
    simulation_4_bareme_2022 > 30000 & simulation_4_bareme_2022<=40000 ~ "30000-40000",
    simulation_4_bareme_2022 > 40000 & simulation_4_bareme_2022<=50000 ~ "40000-50000",
    simulation_4_bareme_2022 == 0 ~ "0",
    simulation_4_bareme_2022 >= -1000 & simulation_4_bareme_2022< 0 ~ "Bonus 0-1000",
    simulation_4_bareme_2022 >= -2500 & simulation_4_bareme_2022< -1000 ~ "Bonus 1000-2500",
    simulation_4_bareme_2022 >= -5000 & simulation_4_bareme_2022< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R"))%>%
  select(motorization, tranche_bm_gvt,tranche_bm_propose) %>%
  pivot_longer(-motorization, names_to = "Feebate", values_to = "Treshold") %>%
  dplyr::select(-motorization) %>%
  group_by(Treshold, Feebate) %>%
  summarise(total_impacted = length(Treshold)) %>%
  group_by(Feebate)%>%
  mutate(pourcentage_impacted = total_impacted / sum(total_impacted) * 100) %>%
  ggplot(aes(x = pourcentage_impacted, y = Treshold, fill = Feebate)) + geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + scale_y_discrete(limits=  c("40000-50000", "30000-40000", "25000-30000", "20000-25000", "15000-20000", "10000-15000",
                                           "7500-10000", "5000-7500", "2500-5000", "1000-2500", "0-1000", "0", "Bonus 0-1000", "Bonus 1000-2500", "Bonus 2500-5000")) +
  labs(x = "Part en Pourcentage", y = "", title = "Répartition des véhicules par montant du Bonus-Malus : comparaison \n barème du gouvernment et barème proposé n°4 en 2022")
ggsave("outputs/plots/Repartition_bonus_malus_2022_gvt_sim_4.png", w = 8, h = 5)












products_2019_clean_25%>%
  mutate(tranche_bm_gvt = case_when(
    bareme_2025 > 0 & bareme_2025 <= 1000 ~ "0-1000",
    bareme_2025 > 1000 & bareme_2025<=2500 ~ "1000-2500",
    bareme_2025 > 2500 & bareme_2025<=5000 ~ "2500-5000",
    bareme_2025 > 5000 & bareme_2025<=7500 ~ "5000-7500",
    bareme_2025 > 7500 & bareme_2025<=10000 ~ "7500-10000",
    bareme_2025 > 10000 & bareme_2025<=15000 ~ "10000-15000",
    bareme_2025 > 15000 & bareme_2025<=20000 ~ "15000-20000",
    bareme_2025 > 20000 & bareme_2025<=35000 ~ "20000-35000",
    bareme_2025 > 35000 & bareme_2025<=50000 ~ "35000-50000",
    bareme_2025 > 50000 & bareme_2025<=75000 ~ "50000-75000",
    bareme_2025 > 75000 & bareme_2025<=100000 ~ "75000-100000",
    bareme_2025 == 0 ~ "0",
    bareme_2025 >= -1000 & bareme_2025< 0 ~ "Bonus 0-1000",
    bareme_2025 >= -2500 & bareme_2025< -1000 ~ "Bonus 1000-2500",
    bareme_2025 >= -5000 & bareme_2025< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R")) %>%
  mutate(tranche_bm_propose = case_when(
    simulation_1_bareme_2025 > 0 & simulation_1_bareme_2025 <= 1000 ~ "0-1000",
    simulation_1_bareme_2025 > 1000 & simulation_1_bareme_2025<=2500 ~ "1000-2500",
    simulation_1_bareme_2025 > 2500 & simulation_1_bareme_2025<=5000 ~ "2500-5000",
    simulation_1_bareme_2025 > 5000 & simulation_1_bareme_2025<=7500 ~ "5000-7500",
    simulation_1_bareme_2025 > 7500 & simulation_1_bareme_2025<=10000 ~ "7500-10000",
    simulation_1_bareme_2025 > 10000 & simulation_1_bareme_2025<=15000 ~ "10000-15000",
    simulation_1_bareme_2025 > 15000 & simulation_1_bareme_2025<=20000 ~ "15000-20000",
    simulation_1_bareme_2025 > 20000 & simulation_1_bareme_2025<=35000 ~ "20000-35000",
    simulation_1_bareme_2025 > 35000 & simulation_1_bareme_2025<=50000 ~ "35000-50000",
    simulation_1_bareme_2025 > 50000 & simulation_1_bareme_2025<=75000 ~ "50000-75000",
    simulation_1_bareme_2025 > 75000 & simulation_1_bareme_2025<=100000 ~ "75000-100000",
    simulation_1_bareme_2025 == 0 ~ "0",
    simulation_1_bareme_2025 >= -1000 & simulation_1_bareme_2025< 0 ~ "Bonus 0-1000",
    simulation_1_bareme_2025 >= -2500 & simulation_1_bareme_2025< -1000 ~ "Bonus 1000-2500",
    simulation_1_bareme_2025 >= -5000 & simulation_1_bareme_2025< -2500 ~ "Bonus 2500-5000",
    TRUE ~ "R"))%>%
  select(motorization, tranche_bm_gvt,tranche_bm_propose) %>%
  pivot_longer(-motorization, names_to = "Feebate", values_to = "Treshold") %>%
  dplyr::select(-motorization) %>%
  group_by(Treshold, Feebate) %>%
  summarise(total_impacted = length(Treshold)) %>%
  group_by(Feebate)%>%
  mutate(pourcentage_impacted = total_impacted / sum(total_impacted) * 100) %>%
  ggplot(aes(x = pourcentage_impacted, y = Treshold, fill = Feebate)) + geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw()







get_baremes_alternatifs_simulation_1(products_2019)
get_baremes_alternatifs_simulation_3(products_2019)



#function_recettes dépenses


test_recette<- function(year, bar){
  data <- products %>%
    mutate(bm = bar)%>%
    filter(annee == year) %>%
    mutate(dep_temp = ifelse(bm > 0, bm*volume, 0),
           rectte_temp = ifelse(bm < 0, bm*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}



## COMPUTATION OF TAX REVENUES FOR THE EXPECTED GOVERNMENTAL FEEBATE

## COMPUTATION OF TAX REVENUES FOR THE FEEBATE SCALE NUMBER 1 

## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR YEAR 2022 


test_recette<- function(year, bar){
  data <- products_2019_s1_2 %>%
    mutate(bareme_simulation_2022 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2022 > 0, bareme_simulation_2022*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2022 < 0, bareme_simulation_2022*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s1_2$simulation_1_bareme_2022)  


## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR YEAR 2023 

test_recette<- function(year, bar){
  data <- products_2019_s1_2 %>%
    mutate(bareme_simulation_2023 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2023 > 0, bareme_simulation_2023*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2023 < 0, bareme_simulation_2023*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s1_2$simulation_1_bareme_2023)  




## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR YEAR 2024 

test_recette<- function(year, bar){
  data <- products_2019_s1_2 %>%
    mutate(bareme_simulation_2024 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2024 > 0, bareme_simulation_2024*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2024 < 0, bareme_simulation_2024*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s1_2$simulation_1_bareme_2024)  




## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR YEAR 2025 

test_recette<- function(year, bar){
  data <- products_2019_s1_2 %>%
    mutate(bareme_simulation_2025 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2025 > 0, bareme_simulation_2025*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2025 < 0, bareme_simulation_2025*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s1_2$simulation_1_bareme_2025)  






## COMPUTATION OF TAX REVENUES FOR SCALE NUMBER 2


## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR THE YEAR 2022

test_recette<- function(year, bar){
  data <- products_2019_s2_2 %>%
    mutate(bareme_simulation_2022 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2022 > 0, bareme_simulation_2022*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2022 < 0, bareme_simulation_2022*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s2_2$simulation_2_bareme_2022)  





## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR THE YEAR 2023

test_recette<- function(year, bar){
  data <- products_2019_s2_2 %>%
    mutate(bareme_simulation_2023 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2023 > 0, bareme_simulation_2023*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2023 < 0, bareme_simulation_2023*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s2_2$simulation_2_bareme_2023)  




## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR THE YEAR 2024

test_recette<- function(year, bar){
  data <- products_2019_s2_2 %>%
    mutate(bareme_simulation_2024 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2024 > 0, bareme_simulation_2024*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2024 < 0, bareme_simulation_2024*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s2_2$simulation_2_bareme_2024)  




## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR THE YEAR 2025

test_recette<- function(year, bar){
  data <- products_2019_s3_2 %>%
    mutate(bareme_simulation_2025 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2025 > 0, bareme_simulation_2025*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2025 < 0, bareme_simulation_2025*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s2_2$simulation_2_bareme_2025)  


## COMPUTATION OF TAX REVENUES FOR THE THIRD SCALE (BAREME 3)

## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR THE YEAR 2022

test_recette<- function(year, bar){
  data <- products_2019_s3_2 %>%
    mutate(bareme_simulation_2022 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2022 > 0, bareme_simulation_2022*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2022 < 0, bareme_simulation_2022*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s3_2$simulation_3_bareme_2022)  





## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR THE YEAR 2023

test_recette<- function(year, bar){
  data <- products_2019_s3_2 %>%
    mutate(bareme_simulation_2023 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2023 > 0, bareme_simulation_2023*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2023 < 0, bareme_simulation_2023*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s3_2$simulation_3_bareme_2023)  




## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR THE YEAR 2024

test_recette<- function(year, bar){
  data <- products_2019_s3_2 %>%
    mutate(bareme_simulation_2024 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2024 > 0, bareme_simulation_2024*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2024 < 0, bareme_simulation_2024*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s3_2$simulation_3_bareme_2024)  




## COMPUTATION OF THE EFFECT ON TAX REVENUES FOR THE YEAR 2025

test_recette<- function(year, bar){
  data <- products_2019_s3_2 %>%
    mutate(bareme_simulation_2025 = bar)%>%
    #  filter(annee == year) %>%
    mutate(dep_temp = ifelse(bareme_simulation_2025 > 0, bareme_simulation_2025*volume, 0),
           rectte_temp = ifelse(bareme_simulation_2025 < 0, bareme_simulation_2025*volume, 0))
  
  depenses <- sum(data$dep_temp)
  recettes <- sum(data$rectte_temp)
  
  results <- list(depenses = depenses,
                  recettes = recettes)
  return(results)
  
}


test_recette(2019, products_2019_s3_2$simulation_3_bareme_2025)  






# generalized market share computed by motorization  ----------------------------------

products_2019_gvt_2 <- products_2019
products_2019_gvt_2 <- cbind(products_2019_gvt_2, get_bareme_gvt_2024_2025(products_2019_gvt_2)) %>%
  relocate(bareme_2024, .after = bareme_2023) %>%
  relocate(bareme_2025, .after = bareme_2024)

prices_2019_2 <- products_2019_gvt_2 %>% 
  dplyr::select(product_id, groupe, motorization, delta_with_resids,  co2, driving_real_emissions, poids, price_2019_hors_BM = prix_10k)


product_list_2 <- list()
for (year in 2019:2025) {
  product_list_2[[year]] <- prices_2019_2 %>% mutate(annee = rep(year, nrow(prices_2019_2)))
}
product_list_2 <- bind_rows(product_list_2) %>% relocate(annee)


#  WE ADD EXPECTED OFFICIAL FEEBATES

future_feebates <- products_2019_gvt_2 %>% 
  dplyr::select(product_id, motorization, bareme_2019:bareme_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "feebate") %>% 
  mutate(annee = str_remove_all(annee, "bareme_") %>% as.integer(),
         feebate = feebate / 1e4)
product_list_2 <- left_join(product_list_2, future_feebates, by = c("product_id", "motorization", "annee"))


# WE ADD WEIGHT TAX

future_weight_fees <- products_2019_gvt_2 %>% 
  dplyr::select(product_id, motorization, weight_tax_2019:weight_tax_2025) %>% 
  pivot_longer(-c(product_id, motorization), names_to = "annee", values_to = "weight_fee") %>% 
  mutate(annee = str_remove_all(annee, "weight_tax_") %>% as.integer(),
         weight_fee = weight_fee / 1e4)

product_list_2 <- left_join(product_list_2, future_weight_fees, by = c("product_id", "motorization", "annee")) 

# WE ADD TECHNICAL PROGRESS, NORM AND COMPUTE FINAL PRICE

product_list_2 <- product_list_2 %>% 
  mutate(price_with_technical_progress = price_2019_hors_BM * get_tech_coeff_advanced(annee, motorization)) %>%  # * ) %>%  #
  left_join(., manufacturers_constraint %>% dplyr::select(groupe, lambda_norme), by = "groupe") %>% 
  mutate(norm = case_when(
    annee == 2019 ~ 0,
    (co2 - 95) < 0 ~ 0, 
    TRUE ~ lambda_norme * (co2 - 95)),
    final_price = price_with_technical_progress + feebate + norm + weight_fee,
    final_price_without_technical_progress = price_2019_hors_BM + feebate + norm + weight_fee) 


get_shares_by_motorization <- function(filtre_annee){
  data <- product_list_2 %>%
    filter(annee == filtre_annee)%>%
    mutate(sha = share_nested(final_price, delta_with_resids)) %>%
    group_by(motorization) %>%
    summarise(ss = sum(sha)*100)
  
  data
  
}
get_shares_by_total <- function(filtre_annee){
  data <- product_list_2 %>%
    filter(annee == filtre_annee)%>%
    mutate(sha = share_nested(final_price, delta_with_resids)) %>%
    summarise(ss = sum(sha)*100)
  
  data
  
}
get_shares_by_motorization(2021)
get_shares_by_motorization(2022)
get_shares_by_total(2022)
get_shares_by_motorization(2019)
get_shares_by_total(2019)












test_recette(2016, products$bareme_2016)  
test_recette(2017, products$bareme_2017)  
test_recette(2018, products$bareme_2018)
test_recette(2019, products$bareme_2019) 
test_recette(2020, products$bareme_2020) 












