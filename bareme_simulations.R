source("code/feebate.R"); 
source("code/CO2_emissions.R") ;
#source("code/norm.R");
source("code/technical_progress.R");
source("code/weight.R");

setwd("d:/morvillier/Desktop/REVISION_BONUS_MALUS/I4CE/Bonus malus/3_Méthodologie/")


library(tidyverse)
library(readxl)


bareme_bm <- read_excel("inputs/bareme_bonus_malus.xlsx", n_max = 700)


# Inventory of the feebates 

get_info_bareme <- function(bareme){
  data <- products_2019 %>%
    mutate(bareme_studied = bareme, 
           nombre_malus = case_when(
             bareme_studied > 0 ~ 1,
             TRUE ~ 0),
           nombre_bonus = case_when(
             bareme_studied < 0 ~ 1,
             TRUE ~ 0))
  nombre_de_voitures_touchees_par_malus <- sum(data$nombre_malus)
  nombre_de_voitures_touchees_par_bonus <- sum(data$nombre_bonus)
  
bonus_info <- list(nombre_de_voitures_touchees_par_malus =  nombre_de_voitures_touchees_par_malus , nombre_de_voitures_touchees_par_bonus = nombre_de_voitures_touchees_par_bonus)  
  
  return(bonus_info)
}

get_info_bareme(products_2019$bareme_2023)



# FUNCTION TO MAKE USABLE THE GOVERNMENT FEEBATES OF 2024 AND 2025 


get_bareme_gvt_2024_2025 <- function(data){
  data_temp_1 <- data 
  
  bareme_bm_temp <- bareme_bm %>% dplyr::select(co2, annee_2024, annee_2025)
  
  
  data_temp_1 <- data_temp_1 %>%             # On rajoute les colonnes brutes
    left_join(., bareme_bm_temp)

  
  data_temp_1 <- data_temp_1 %>% # on rentre le nouveau bar?me pour l'ann?e 2024
    mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2024 = ifelse(annee_2024  %in% c("27%  du prix, max  3000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2024 = as.numeric(ifelse(annee_2024 %in% c("27%  du prix, max  3000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, annee_2024))) %>%
    mutate(bareme_2024 = case_when(
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 3000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 3000 & prix <= 45000) ~ - 3000,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2024 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2024))
  
  
  data_temp_1 <- data_temp_1 %>% # on rentre le nouveau bar?me pour l'ann?e 2025
    mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2025 = ifelse(annee_2025  %in% c("27%  du prix, max  2000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2025 = as.numeric(ifelse(annee_2025 %in% c("27%  du prix, max  2000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, annee_2025))) %>%
    mutate(bareme_2025 = case_when(
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 2000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 2000 & prix <= 45000) ~ - 2000,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2025 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2025))
  
  
  data_temp_1 <- data_temp_1 %>%
    dplyr::select(bareme_2024, bareme_2025)
  
  return(data_temp_1)
  
}

get_bareme_gvt_2024_2025(products_2019)



# FUNCTION FOR THE ALTERNATIVE FEEBATE  1 2 3 4 ------------------------------



get_baremes_alternatifs_simulation_1 <- function(data_df){
  data_temp_1 <- data_df
  bareme_bm_temp <- bareme_bm %>% dplyr::select(co2, bareme_alternatif_2022_95, bareme_alternatif_2023_90, bareme_alternatif_2024_85, bareme_alternatif_2025_80)
  
  
  data_temp_1 <- data_temp_1 %>%             # we add gross columns
    left_join(., bareme_bm_temp)
  
  data_temp_1 <- data_temp_1 %>%  # we put the new feebate for the Year 2022
    mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1 = ifelse(bareme_alternatif_2022_95  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2022 = as.numeric(ifelse(bareme_alternatif_2022_95 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2022_95))) %>%
    mutate(simulation_1_bareme_2022 = case_when(
      (temp_bareme_1 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2022))
  
  
  
  data_temp_1 <- data_temp_1 %>%  # we put the new feebate for the Year 2023
    #   mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2023 = ifelse(bareme_alternatif_2023_90  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2023 = as.numeric(ifelse(bareme_alternatif_2023_90 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2023_90))) %>%
    mutate(simulation_1_bareme_2023 = case_when(
      (temp_bareme_1_2023 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2023 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2023))
  
  
  data_temp_1 <- data_temp_1 %>%  # we put the new feebate for the Year 2024
    #  mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2024 = ifelse(bareme_alternatif_2024_85  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2024 = as.numeric(ifelse(bareme_alternatif_2024_85 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2024_85))) %>%
    mutate(simulation_1_bareme_2024 = case_when(
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2024 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2024))
  
  
  data_temp_1 <- data_temp_1 %>%  # we put the new feebate for the Year 2025
    #   mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2025 = ifelse(bareme_alternatif_2025_80  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2025 = as.numeric(ifelse(bareme_alternatif_2025_80 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2025_80))) %>%
    mutate(simulation_1_bareme_2025 = case_when(
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2025 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2025))
  
  
  data_temp_1 <- data_temp_1 %>%
    dplyr::select(simulation_1_bareme_2022, simulation_1_bareme_2023, simulation_1_bareme_2024, simulation_1_bareme_2025)
  
  return(data_temp_1)
  
}
get_baremes_alternatifs_simulation_1(products_2019)

get_baremes_alternatifs_simulation_2 <- function(data_df){
  data_temp_1 <- data_df
  bareme_bm_temp <- bareme_bm %>% dplyr::select(co2, bareme_alternatif_2022_95bis , bareme_alternatif_2023_90bis, bareme_alternatif_2024_85bis, bareme_alternatif_2025_80bis)
  
  
  data_temp_1 <- data_temp_1 %>%             # we add gross columns
    left_join(., bareme_bm_temp)
  
  data_temp_1 <- data_temp_1 %>% # we put the new feebates for the Year 2022
    mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1 = ifelse(bareme_alternatif_2022_95bis   %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2022 = as.numeric(ifelse(bareme_alternatif_2022_95bis  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2022_95bis ))) %>%
    mutate(simulation_2_bareme_2022 = case_when(
      (temp_bareme_1 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2022))
  
  
  
  data_temp_1 <- data_temp_1 %>% # we put the new feebates for the Year 2024
    #   mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2023 = ifelse(bareme_alternatif_2023_90bis  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2023 = as.numeric(ifelse(bareme_alternatif_2023_90bis %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2023_90bis))) %>%
    mutate(simulation_2_bareme_2023 = case_when(
      (temp_bareme_1_2023 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2023 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2023))
  
  
  data_temp_1 <- data_temp_1 %>% # we put the new feebates for the Year 2024
    #  mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2024 = ifelse(bareme_alternatif_2024_85bis  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2024 = as.numeric(ifelse(bareme_alternatif_2024_85bis %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2024_85bis))) %>%
    mutate(simulation_2_bareme_2024 = case_when(
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2024 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2024))
  
  
  data_temp_1 <- data_temp_1 %>% # # we put the new feebates for the Year 2025
    #   mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2025 = ifelse(bareme_alternatif_2025_80bis  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2025 = as.numeric(ifelse(bareme_alternatif_2025_80bis %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2025_80bis))) %>%
    mutate(simulation_2_bareme_2025 = case_when(
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2025 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2025))
  
  
  data_temp_1 <- data_temp_1 %>%
    dplyr::select(simulation_2_bareme_2022, simulation_2_bareme_2023, simulation_2_bareme_2024, simulation_2_bareme_2025)
  
  return(data_temp_1)
  
}

get_baremes_alternatifs_simulation_2(products_2019)


get_baremes_alternatifs_simulation_3 <- function(data_df){
  data_temp_1 <- data_df
  bareme_bm_temp <- bareme_bm %>% dplyr::select(co2, bareme_alternatif_2022_95_bis_2, bareme_alternatif_2023_90_bis_2, bareme_alternatif_2024_85_bis_2, bareme_alternatif_2025_80_bis_2)
  
  
  data_temp_1 <- data_temp_1 %>%             # we add gross columns
    left_join(., bareme_bm_temp)
  
  data_temp_1 <- data_temp_1 %>% # # we put the new feebates for the Year 2022
    mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1 = ifelse(bareme_alternatif_2022_95_bis_2  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2022 = as.numeric(ifelse(bareme_alternatif_2022_95_bis_2 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2022_95_bis_2))) %>%
    mutate(simulation_3_bareme_2022 = case_when(
      (temp_bareme_1 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2022))
  
  
  
  data_temp_1 <- data_temp_1 %>% # # we put the new feebates for the Year 2023
    #   mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2023 = ifelse(bareme_alternatif_2023_90_bis_2  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2023 = as.numeric(ifelse(bareme_alternatif_2023_90_bis_2 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2023_90_bis_2))) %>%
    mutate(simulation_3_bareme_2023 = case_when(
      (temp_bareme_1_2023 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2023 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2023))
  
  
  data_temp_1 <- data_temp_1 %>% # # we put the new feebates for the Year 2024
    #  mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2024 = ifelse(bareme_alternatif_2024_85_bis_2  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2024 = as.numeric(ifelse(bareme_alternatif_2024_85_bis_2 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2024_85_bis_2))) %>%
    mutate(simulation_3_bareme_2024 = case_when(
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2024 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2024))
  
  
  data_temp_1 <- data_temp_1 %>% # # we put the new feebates for the Year 2025
    #   mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2025 = ifelse(bareme_alternatif_2025_80_bis_2  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2025 = as.numeric(ifelse(bareme_alternatif_2025_80_bis_2 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2025_80_bis_2))) %>%
    mutate(simulation_3_bareme_2025 = case_when(
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2025 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2025))
  
  
  data_temp_1 <- data_temp_1 %>%
    dplyr::select(simulation_3_bareme_2022, simulation_3_bareme_2023, simulation_3_bareme_2024, simulation_3_bareme_2025)
  
  return(data_temp_1)
  
}

get_baremes_alternatifs_simulation_3(products_2019)



get_baremes_alternatifs_simulation_4 <- function(data_df){
  data_temp_1 <- data_df
  bareme_bm_temp <- bareme_bm %>% dplyr::select(co2, bareme_alternatif_2022_95_bis_3, bareme_alternatif_2023_90_bis_3, bareme_alternatif_2024_85_bis_3, bareme_alternatif_2025_80_bis_3)
  
  
  data_temp_1 <- data_temp_1 %>%             # On rajoute les colonnes brutes
    left_join(., bareme_bm_temp)
  
  data_temp_1 <- data_temp_1 %>% # # we put the new feebates for the Year 2022
    mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1 = ifelse(bareme_alternatif_2022_95_bis_3  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2022 = as.numeric(ifelse(bareme_alternatif_2022_95_bis_3 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2022_95_bis_3))) %>%
    mutate(simulation_4_bareme_2022 = case_when(
      (temp_bareme_1 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2022))
  
  
  
  data_temp_1 <- data_temp_1 # we put the new feebates for the Year  2023
    #   mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2023 = ifelse(bareme_alternatif_2023_90_bis_3  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2023 = as.numeric(ifelse(bareme_alternatif_2023_90_bis_3 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2023_90_bis_3))) %>%
    mutate(simulation_4_bareme_2023 = case_when(
      (temp_bareme_1_2023 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2023 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2023 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2023))
  
  
  data_temp_1 <- data_temp_1 %>%# we put the new feebates for the Year  2024
    #  mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2024 = ifelse(bareme_alternatif_2024_85_bis_3  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2024 = as.numeric(ifelse(bareme_alternatif_2024_85_bis_3 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2024_85_bis_3))) %>%
    mutate(simulation_4_bareme_2024 = case_when(
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2024 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2024 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2024))
  
  
  data_temp_1 <- data_temp_1 %>% # # we put the new feebates for the Year  2025
    #   mutate(twenty_seven_percent = (prix * 0.27)) %>%
    mutate(temp_bareme_1_2025 = ifelse(bareme_alternatif_2025_80_bis_3  %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_b_temp_2025 = as.numeric(ifelse(bareme_alternatif_2025_80_bis_3 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, bareme_alternatif_2025_80_bis_3))) %>%
    mutate(simulation_4_bareme_2025 = case_when(
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_bareme_1_2025 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_bareme_1_2025 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_b_temp_2025))
  
  
  data_temp_1 <- data_temp_1 %>%
    dplyr::select(simulation_4_bareme_2022, simulation_4_bareme_2023, simulation_4_bareme_2024, simulation_4_bareme_2025)
  
  return(data_temp_1)
  
}

get_baremes_alternatifs_simulation_4(products_2019)


  
 