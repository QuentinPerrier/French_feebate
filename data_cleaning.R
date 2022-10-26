library(tidyverse)
library(readxl)
library(janitor) #For clean_names
library(fastDummies) #For creating dummy columns
library(modi) #for weighted quantiles
library(directlabels) #Add labels on ggplots
library(ggrepel) #Add labels on ggplots
library(arsenal)
library(tidyselect)# for selection

# Chargement des données --------------------------------------------------

setwd("d:/morvillier/Desktop/REVISION_BONUS_MALUS/I4CE/Bonus malus/3_Méthodologie/")



raw_data <- list()
for (i in 2015:2020) {
  raw_data[[i]] <- read_excel(paste0("inputs/ventes ", i, "_I4CE.xlsx")) %>% clean_names()
}      






#Vector of column names to select
parameters <- c("date", "genre", "marque", "type", "puissance_fiscale", "modele", "version", "carrosserie",
                "energie", "cylindree", "gamme", "co2", "poids_a_vide", "puissance_kw", "rapport_poid_puissance",
                "type_de_boite", "groupe", "mf_me", 
                "consommation_extra_urbaine", "consommation_mixte", "consommation_urbaine", 
                "bonus_malus", "prix_du_vehicule", "date_arrete",
                "volume", "jo")


sales0 <- list()
for (i in 2015:2020) {
  #Check that all column names are present in the table raw_data[[i]]; if yes: extract these columns
  if (all(parameters %in% colnames(raw_data[[i]]))) {
    print(paste0("year ", i," is ok"))
    sales0[[i]] <- dplyr::select(raw_data[[i]], parameters)
  } else {
    print(paste0("Warning: one parameter is not present in table of year ", i))
  }
}
sales0 <- bind_rows(sales0)


#test <- raw_data[[2015]]

# saveRDS(sales0, "outputs/sales0.RDS")
#sales0 <- readRDS("outputs/sales0.RDS")


### besoin de rajouter pour les années 2015 à 2017 sur le fichier fuel price 
fuel_prices <- read_excel("inputs/fuel prices.xlsx",
                          n_max = 7) %>%  #Attention : nmax à modifier si on change le fichier
  pivot_longer(cols = c(2015:2020) %>% as.character(), names_to = "annee", values_to = "fuel_price") %>% 
  mutate(annee = as.integer(annee))



sales <- sales0 %>% 
  mutate(date = str_sub(date, start=-4), #Nettoie les dates
         date = as.integer(date)) %>%
  rename(annee = date,
         prix = prix_du_vehicule,
         poids = poids_a_vide,
         rapport_poids_puissance = rapport_poid_puissance) %>% 
  mutate_at(c("rapport_poids_puissance", "consommation_extra_urbaine", "consommation_mixte", "consommation_urbaine"), 
            as.numeric) %>% 
  mutate(groupe = str_replace_all(groupe, " \\+ ", "_")) %>% 
  mutate(groupe = str_replace_all(groupe, " ", "_")) 


#Recode en NA les valeurs de type "999"
sales <- sales %>%
  mutate(co2 = na_if(co2, 999),         
         cylindree = na_if(cylindree, 99999),
         rapport_poids_puissance = na_if(rapport_poids_puissance, 99999),
         poids = na_if(poids, 99999),
         puissance_kw = na_if(puissance_kw, 999),
         consommation_extra_urbaine = na_if(consommation_extra_urbaine, 9999),
         consommation_mixte = na_if(consommation_mixte, 9999),
         consommation_urbaine = na_if(consommation_urbaine, 9999)) %>% 
  mutate(prix = ifelse(prix > 10^7, NA, prix)) %>% 
  mutate(groupe = ifelse(groupe == "VGF", "VOLKSWAGEN", groupe)) %>% 
  mutate(groupe = ifelse(groupe == "FIAT", "FCA", groupe)) %>%
  relocate(groupe, .after = marque)


# on distingue les ventes de 2018 à 2020 et 2015 à 2017 afin de retouver les données manquantes


old_sales <- sales %>%
  filter(annee == 2015 | annee == 2016 | annee == 2017 )
new_sales <- sales %>% 
  filter(annee == 2018 | annee == 2019 | annee == 2020 ) 

## setdiff(old_sales$energie, new_sales$energie)

## comparedf(old_sales$carrosserie, new_sales$carrosserie)


#On créé les segments de véhicules
sales <- sales %>%
  mutate(segment = case_when(
    carrosserie == "BERLINE" ~ "berline",
    carrosserie == "BREAK" ~ "break",
    carrosserie == "CABRIOLET" ~ "sport",
    carrosserie == "COMBISPACE" ~ "espace",
    carrosserie == "COUPE" ~ "sport",
    carrosserie == "MINIBUS" ~ "minibus",
    carrosserie == "MINISPACE" ~ "espace",
    carrosserie == "MONOSPACE" ~ "espace",
    carrosserie == "MONOSPACE COMPACT" ~ "espace",
    carrosserie == "TS TERRAINS/CHEMINS" ~ "SUV",
    TRUE ~ "Attention, hors classement"
  )) %>% 
  mutate(segment_nb = as.factor(segment) %>% as.numeric) %>%  
  mutate(groupe_nb = as.factor(groupe) %>% as.numeric) %>% 
  relocate(groupe_nb, .before = groupe) 


table(old_sales$energie)
table(new_sales$energie)


## #On regroupe les types de motorisation
# On retire le produit à energie "GAZOGENE" car un seul produit dans l'échantillon de 198 943 véhicule n'est pas significatif 
sales <- subset(sales, energie!="GAZOGENE") 

sales <- sales %>%  
  mutate(motorization = case_when(
    energie == "ELECTRIC" ~ "Electrique",
    energie == "ESSENCE" ~ "Essence",
    energie == "GAZOLE" ~ "Diesel",
    #grepl("\\+", energie) &  ~ "hybrid",
    grepl("HNR", energie) ~ "Hybride non_rechargeable",
    grepl("HR", energie) ~ "Hybride rechargeable",
    TRUE ~ "Autre")) %>% 
  relocate(motorization, .after = energie) %>% 
  mutate(motorization = str_replace_all(motorization, " ", "_")) %>% 
  
  #On regroupe les types de carburants pour ajouter leurs prix et trouver le coût variable par km
  mutate(fuel_type = case_when(
    grepl("ESS", energie) ~ "Super 95 - E10",
    energie == "GAZOLE" ~ "Diesel",
    grepl("GAZ\\+ELEC", energie) ~ "Diesel",
    energie == "ELECTRIC" ~"Electricité",
    energie == "GAZ NAT.VEH" ~ "Gaz naturel",
    grepl("HYDRO", energie) ~"Hydrogène",
    energie == "SUPERETHANOL" ~ "Superethanol",
    energie == "GAZOGENE" ~ "Gazogene",
    TRUE ~ "Erreur : énergie à classer"
  )) %>% 
  left_join(., fuel_prices %>% dplyr::select(fuel_type, annee, fuel_price), by = c("annee", "fuel_type")) %>% 
  mutate(consommation_estimee = case_when(
    fuel_type == "Diesel" ~ co2 / (2.6*10^3),             #2,6 kg de CO2 par litre de diesel <=> 1/(2.6^103) L/gCO2
    fuel_type == "Super 95 - E10" ~ co2 / (2.28*10^3) ,   #2.28 kg de CO2 par litre d’essence brulée
    fuel_type == "Electricité" ~ .1,                       #10 kWH/100km <=> 0.1 kWH/km
    fuel_type == "Superethanol" ~ (co2 / (2.28*10^3)) * 1/2,  # le Superéthanole produit 50% de moins d'émissions de cO2 que l'essence
    fuel_type == "Hydrogène" ~ 0,
    fuel_type == "Gaz naturel" ~ (co2 / (2.28*10^3)) * 3/4    # Le gaz naturel produit 25 % de moins d'émissions de cO2 que l'essence 
   )) %>% 
  mutate(cout_variable_carburant = consommation_estimee * fuel_price) %>% 
  dplyr::select(-genre) %>% 
  dplyr::select(annee, groupe, marque, modele, version, everything())

  


 

# Ajout des barèmes bonus-malus -------------------------------------------

get_bonus_malus <- function(sales_df){
  
  bareme_bm <- read_excel("inputs/bareme_bonus_malus_initial.xlsx", n_max = 700) 
  bareme_bm <- bareme_bm %>% dplyr::select(-bareme_alternatif_1)
  
  sales <- sales_df %>%             # On rajoute les colonnes brutes
    left_join(., bareme_bm)
  
  sales <- sales %>%
    filter(!is.na(prix)) %>%      # On enlève les valeurs sans prix ni co2
    filter(!is.na(co2))
  
  sales <- sales %>%
    mutate(five_percent = prix * 0.05,          # création de variables temporaires afin de calculer chaque bonus-malus par véhicule selon l'annee
           twenty_percent = prix * 0.2,
           twenty_seven_percent = prix * 0.27,
           temp_hybride = ifelse(annee_2015 %in% c("Hybride_non_rechargeable","Hybride_rechargeable"), 1, 0),
           temp_hybride_sans_diesel = ifelse(energie %in% c("ESS+ELEC HNR", "ESS+ELEC HR", "HYDRO+ELEC HNR"), 1, 0),
           temp_co2_21_60 = ifelse(co2 > 20 & co2 < 61, 1, 0),
           temp_hybride_rechargeable = ifelse(energie %in% c("ESS+ELEC HR", "GAZ+ELEC HR"), 1, 0)) %>%
      
    #2015
    mutate(temp_b_15_1 = ifelse(annee_2015 == "Si Hybride, 5% du prix, max 2000, min 1000", 1, 0),
             temp_b_15_2 = ifelse(annee_2015 == "20%  du prix, max  4000", 1, 0),
             temp_b_15_3 = ifelse(annee_2015== "27%  du prix, max  6300", 1, 0),
             bareme_2015_temp = as.numeric(ifelse(annee_2015 %in% c("Si Hybride, 5% du prix, max 2000, min 1000", "27%  du prix, max  6300", "20%  du prix, max  4000"), 0, annee_2015))) %>%  # création barême temporaire car certaine essense / diesel emmettent autant que les hybrides subventionnee
    mutate(bareme_2015 = case_when(
        (temp_b_15_3 > 0 & twenty_seven_percent < 6300) ~ - twenty_seven_percent,
        (temp_b_15_3 > 0 & twenty_seven_percent >= 6300) ~ - 6300,
        (temp_b_15_1 > 0 & temp_hybride < 1) ~ 0, 
        (temp_b_15_1 > 0 & temp_hybride > 0 & five_percent < 1000) ~ - 1000,
        (temp_b_15_1 > 0 & temp_hybride > 0 & five_percent > 2000) ~ - 2000,
        (temp_b_15_1 > 0 & temp_hybride & five_percent > 2000 & five_percent < 1000) ~ - five_percent,
        (temp_b_15_2 > 0 & twenty_percent < 4000) ~ -twenty_percent,
        (temp_b_15_2 > 0 & twenty_percent >= 4000) ~ - 4000,
        TRUE ~ bareme_2015_temp)) %>%
    
    #2016 
    mutate(temp_hybride_sans_diesel = ifelse(energie %in% c("ESS+ELEC HNR", "ESS+ELEC HR", "HYDRO+ELEC HNR"), 1, 0),
           temp_b_16_1 = ifelse(annee_2016 %in% c("Si Hybride sauf Hybride diesel, 750"), 1, 0), 
           temp_b_16_2 = ifelse(annee_2016 %in% c("27%  du prix, max  6300"), 1, 0), 
           temp_b_16_3 = ifelse(annee_2016 == 1000, 1, 0),
           bareme_2016_temp = as.numeric(ifelse(annee_2016 %in% c("Si Hybride sauf Hybride diesel, 750", "27%  du prix, max  6300"), 0, annee_2016))) %>%
      
    mutate(sales <- sales %>%
             mutate(bareme_2016 = case_when(
               (temp_b_16_1 > 0 & temp_hybride_sans_diesel > 0) ~ -750,
               (temp_b_16_2 > 0 & twenty_seven_percent < 6300) ~ -twenty_seven_percent,
               (temp_b_16_2 > 0 & twenty_seven_percent >= 6300) ~ -6300, 
               (temp_b_16_3 > 0 & temp_co2_21_60 > 0) ~ -1000, 
               TRUE ~ bareme_2016_temp))) %>%  
      
    #2017
    mutate(temp_b_17_1 = ifelse(annee_2017 %in% c("27%  du prix, max  6300"), 1, 0),
           temp_b_17_2 = ifelse(annee_2017 == 1000, 1, 0),
           bareme_2017_temp = as.numeric(ifelse(annee_2017 %in% c("27%  du prix, max  6300"), 0, annee_2017))) %>% 
    mutate(bareme_2017 = case_when(
      (temp_b_17_1 > 0 & twenty_seven_percent < 6300) ~ - twenty_seven_percent,
      (temp_b_17_1 > 0 & twenty_seven_percent >= 6300) ~ - 6300,
      (temp_b_17_2 > 0 & temp_co2_21_60 > 0) ~ - 1000,
      TRUE ~ bareme_2017_temp)) %>%
      
    # 2018
    mutate(temp_b_18_1 = ifelse(annee_2018 %in% c("27%  du prix, max  6000"), 1, 0),
           bareme_2018_temp = as.numeric(ifelse(annee_2018 %in% c("27%  du prix, max  6000"), 0, annee_2018))) %>%
    mutate(bareme_2018 = case_when(
      (temp_b_18_1 > 0 & twenty_seven_percent < 6000) ~ - twenty_seven_percent,
      (temp_b_18_1 > 0 & twenty_seven_percent >= 6000) ~ - 6000,
      TRUE ~ bareme_2018_temp)) %>%
      
    # 2019 
    mutate(temp_b_19_1 = ifelse(annee_2019 %in% c("27%  du prix, max  6000"), 1, 0),
           bareme_2019_temp = as.numeric(ifelse(annee_2019 %in% c("27%  du prix, max  6000"), 0, annee_2018))) %>%
    mutate(bareme_2019 = case_when(
          (temp_b_19_1 > 0 & twenty_seven_percent < 6000) ~ - twenty_seven_percent,
          (temp_b_19_1 > 0 & twenty_seven_percent >= 6000) ~ - 6000,
            TRUE ~ bareme_2019_temp)) %>%
    
    
    # 2020 
    
    mutate(temp_b_20_1 = ifelse(annee_2020 %in% c("27%  du prix, max  6000 si prix <=40k, max 3000 si 40k<p<60k, 0 sinon"), 1, 0),
           bareme_2020_temp = as.numeric(ifelse(annee_2020 %in% c("27%  du prix, max  6000 si prix <=40k, max 3000 si 40k<p<60k, 0 sinon"), 0, annee_2020))) %>% 
    mutate(bareme_2020 = case_when(
        (temp_b_20_1 > 0 & twenty_seven_percent < 6000 & prix <= 40000) ~ - twenty_seven_percent,
        (temp_b_20_1 > 0 & twenty_seven_percent >= 6000 & prix <= 40000) ~ - 6000,
        (temp_b_20_1 > 0 & twenty_seven_percent < 3000 & prix > 40000 & prix <= 60000) ~ - twenty_seven_percent,
        (temp_b_20_1 > 0 & twenty_seven_percent >= 3000 & prix > 40000 & prix <= 60000) ~ - 3000,
        (temp_b_20_1 > 0 & prix > 60000) ~ 0,
        TRUE ~ bareme_2020_temp)) %>%
      
    # 2021  
    mutate(temp_b_21_1 = ifelse(annee_2021 %in% c("27%  du prix, max  6500 si prix <=45k, max 2000 si 45k<p<60k, 0 sinon"), 1, 0),
           temp_b_21_2 = ifelse(annee_2021 %in% c("1500 Si Hybride rechargeabe et prix <=50000"), 1, 0), 
           bareme_2021_temp = as.numeric(ifelse(annee_2021 %in% c("27%  du prix, max  6500 si prix <=45k, max 2000 si 45k<p<60k, 0 sinon", "1500 Si Hybride rechargeabe et prix <=50000"), 0, annee_2021))) %>%
    mutate(bareme_2021 = case_when(
        (temp_b_21_1 > 0 & twenty_seven_percent < 6500 & prix <= 45000) ~ - twenty_seven_percent,
        (temp_b_21_1 > 0 & twenty_seven_percent >= 6500 & prix <= 45000) ~ - 6500,
        (temp_b_21_1 > 0 & twenty_seven_percent < 2500 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
        (temp_b_21_1 > 0 & twenty_seven_percent >= 2500 & prix > 45000 & prix <= 60000) ~ - 2500,
        (temp_b_21_1 > 0 & prix > 60000) ~ 0,
        (temp_b_21_2 > 0 & temp_hybride_rechargeable > 0 & prix <= 50000) ~ - 1500,
        TRUE ~ bareme_2021_temp)) %>% 
      
    # 2022
    mutate(temp_b_22_1 = ifelse(annee_2022 %in% c("27%  du prix, max  5000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_2022_temp = as.numeric(ifelse(annee_2022 %in% c("27%  du prix, max  5000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, annee_2022))) %>%
      mutate(bareme_2022 = case_when(
        (temp_b_22_1 > 0 & twenty_seven_percent < 5000 & prix <= 45000) ~ - twenty_seven_percent,
        (temp_b_22_1 > 0 & twenty_seven_percent >= 5000 & prix <= 45000) ~ - 5000,
        (temp_b_22_1 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
        (temp_b_22_1 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
        (temp_b_22_1 > 0 & prix > 60000) ~ 0,
        TRUE ~ bareme_2022_temp)) %>%
    
    # 2023
    mutate(temp_b_23_1 = ifelse(annee_2023 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 1, 0),
           bareme_2023_temp = as.numeric(ifelse(annee_2023 %in% c("27%  du prix, max  4000 si prix <=45k, max 1000 si 45k<p<60k, 0 sinon"), 0, annee_2023))) %>%
    mutate(bareme_2023 = case_when(
      (temp_b_23_1 > 0 & twenty_seven_percent < 4000 & prix <= 45000) ~ - twenty_seven_percent,
      (temp_b_23_1 > 0 & twenty_seven_percent >= 4000 & prix <= 45000) ~ - 4000,
      (temp_b_23_1 > 0 & twenty_seven_percent < 1000 & prix > 45000 & prix <= 60000) ~ - twenty_seven_percent,
      (temp_b_23_1 > 0 & twenty_seven_percent >= 1000 & prix > 45000 & prix <= 60000) ~ - 1000,
      (temp_b_23_1 > 0 & prix > 60000) ~ 0,
      TRUE ~ bareme_2023_temp)) %>%
    
    #on supprime les dummies inutiles
    dplyr::select(-c(temp_b_15_1, temp_b_15_2, temp_b_15_3, temp_b_16_1, temp_b_16_2, temp_b_16_3, temp_b_17_1, temp_b_17_2,
                temp_b_18_1, temp_b_19_1, temp_b_20_1, temp_b_21_1, temp_b_21_2, temp_b_22_1, temp_b_23_1, temp_co2_21_60, temp_hybride, temp_hybride_sans_diesel,
                temp_hybride_rechargeable, bareme_2015_temp, bareme_2016_temp, bareme_2017_temp, bareme_2018_temp, bareme_2019_temp,
                bareme_2020_temp, bareme_2021_temp, bareme_2022_temp, bareme_2023_temp, annee_2015, annee_2016, annee_2017, annee_2018, annee_2019, annee_2020, annee_2021, annee_2022, annee_2023,
                twenty_percent, twenty_seven_percent, five_percent))

    
  return(sales)
    
    
}

sales <- get_bonus_malus(sales)




## ajustement du prix 

sales <- sales %>%
  mutate(yearly_adjusted_bareme = case_when(
    (annee == 2015) ~  bareme_2015,
    (annee == 2016) ~  bareme_2016,
    (annee == 2017) ~  bareme_2017,
    (annee == 2018) ~  bareme_2018,
    (annee == 2019) ~  bareme_2019,
    TRUE ~ prix
  )) %>%
  mutate(prix_final = case_when(
    (annee == 2015) ~ prix + bareme_2015,
    (annee == 2016) ~ prix + bareme_2016,
    (annee == 2017) ~ prix + bareme_2017,
    (annee == 2018) ~ prix + bareme_2018,
    (annee == 2019) ~ prix + bareme_2019,
    TRUE ~ prix
  ))

saveRDS(sales, file = "outputs/sales.RDS")



# Premières observations --------------------------------------------------

#Résumé pour les principales variables
summary_sales <- sales %>% 
  group_by(annee) %>% 
  mutate(market_share = volume / sum(volume)) %>% 
  group_by(annee, motorization) %>% 
  summarise(co2 = weighted.mean(co2, volume, na.rm = TRUE), prix = weighted.mean(prix, volume, na.rm = T), market_share = 100*sum(market_share))
#write.table(summary_sales, "clipboard", sep="\t", row.names=FALSE) #Copy to clipboard

summary_sales %>% dplyr::select(annee, motorization, market_share) %>% 
  ggplot(aes(x = annee, y = market_share, fill = motorization)) + geom_bar(stat='identity') +
  theme_bw() +
  labs(x = "", y = "%", fill = "Motorisation", title = "Parts de marché des ventes") +
  ggsave("outputs/part_de_marche_selon_motorisation.jpg", width = 5, height = 4)


#Où sont les NA ?
sapply(sales, function(x) sum(is.na(x)))


#Emissions de CO2

sum(is.na(sales$co2))
min(sales$co2, na.rm = TRUE)
max(sales$co2, na.rm = TRUE)

sales %>% 
  group_by(annee) %>% 
  summarise(co2 = weighted.mean(co2, volume, na.rm = TRUE))


#Poids

sales %>% 
  group_by(annee) %>% 
  summarise(mean = weighted.mean(x = poids, w = volume, na.rm = TRUE))

sales %>% 
  filter(annee == 2020) %>% 
  ggplot(aes(poids)) +
  geom_histogram(aes(weight = volume), binwidth = 30)

sales %>% 
  mutate(above_limit = ifelse(poids >= 1800, volume, 0)) %>% 
  group_by(annee) %>% 
  summarise(share = sum(above_limit, na.rm = T) / sum(volume, na.rm = T))


#Ventes de véhiclues électriques : elles représentent 1.80% des ventes en 2019
sales %>% 
  group_by(annee, energie) %>% 
  summarize(volume = sum(volume)) %>% 
  group_by(annee) %>% 
  mutate(share = 100 * volume / sum(volume)) %>% 
  filter(energie == "ELECTRIC")


#Ventes de véhiclues GAZOLE : elles représentent 34.2% des ventes en 2019
sales %>% 
  group_by(annee, energie) %>% 
  summarize(volume = sum(volume)) %>% 
  group_by(annee) %>% 
  mutate(share = 100 * volume / sum(volume)) %>% 
  filter(energie == "GAZOLE")

#Ventes de véhiclues ESSENCE : elles représentent 58.1% des ventes en 2019
sales %>% 
  group_by(annee, energie) %>% 
  summarize(volume = sum(volume)) %>% 
  group_by(annee) %>% 
  mutate(share = 100 * volume / sum(volume)) %>% 
  filter(energie == "ESSENCE")


# HR ET HNR REPRESENTENT DES PARTS DE MARCHE DE 5.9 %  





#Prix
sales %>% group_by(annee) %>% 
  summarise(na = sum(is.na(prix)))
sum(is.na(sales$prix))
min(sales$prix, na.rm = TRUE)
max(sales$prix, na.rm = TRUE)

#Comment évoluent le prix des véhicules neufs ?
sales %>% 
  mutate(categorie_prix = case_when(
    prix < 20000 ~ "<20 k€",
    prix < 30000 ~ "<30 k€",
    prix < 40000 ~ "<40 k€",
    prix < 50000 ~ "<50 k€",
    prix < 60000 ~ "<60 k€",
    TRUE ~ "\u2265 60 k€"
  )) %>% 
  group_by(annee, categorie_prix) %>% 
  summarise(sales = sum(volume)) %>% 
  ggplot(aes(annee, fill = categorie_prix)) +
  geom_bar(aes(weight = sales), position="fill") +
  theme_minimal() + 
  labs(x = "", y = "Pourcentage des ventes", fill = "Prix", title = "Prix de vente des véhicules neufs") + 
  scale_y_continuous(labels = scales::percent)
ggsave(filename = "outputs/vehicule_price_evolution.png", width = 5, height = 5)


#Comment se répartissent le prix des véhicules électriques ?
sales %>% 
  mutate(prix = prix / 1000) %>% 
  filter(annee == 2020 & energie == "ELECTRIC") %>% 
  ggplot(aes(prix)) + geom_histogram(aes(weight = volume), binwidth = 3) +
  theme_bw() + 
  labs(x = "Prix (k€)", y = "Nombres de ventes", title = "Prix des véhicules électriques") 
ggsave("outputs/histogramme_ventes_EV.jpg", width = 5, height = 5)



# Analyse par groupe ------------------------------------------------

top_brands <- sales %>% 
  filter(annee == 2020) %>% 
  group_by(groupe) %>% 
  summarise(volume = sum(volume)) %>% 
  arrange(desc(volume)) %>% 
  head(10) %>% 
  .$groupe

sales %>% 
  filter(groupe %in% top_brands) %>% 
  group_by(annee, groupe) %>% 
  summarise(co2 = weighted.mean(co2, w = volume, na.rm = T)) %>%  
  filter(groupe != "G.M.") %>% 
  mutate(label = if_else(annee == 2020, groupe, NA_character_)) %>%
  ggplot(aes(x = annee, y = co2, color = groupe)) + geom_line() +
  scale_x_continuous(breaks = 2015:2020) +
  theme_bw() +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  labs(x = "", y = "gCO2/km", title = "Emissions de CO2") +
  ggsave(filename = "outputs/co2_emissions_manufacturer.jpg", w = 8, h = 5)



#Top 5% CO2 threshold for manufacturer
top_5_percent <- sales %>% 
  filter(annee == 2020) %>% 
  group_by(groupe) %>% 
  summarise(threshold_95 = weighted.quantile(co2, w = volume, prob = 0.95))


co2_95percent <- sales %>% 
  filter(annee == 2020) %>% 
  filter(groupe %in% top_brands) %>%
  left_join(top_5_percent, by = "groupe") %>% 
  mutate(volume_95percent = ifelse(co2 > threshold_95, 0, volume)) %>% 
  group_by(groupe) %>% 
  summarise(co2_95percent = weighted.mean(co2, w = volume_95percent, na.rm = T))

sales %>% 
  filter(annee == 2020) %>% 
  filter(groupe %in% top_brands) %>%
  left_join(top_5_percent, by = "groupe") %>% 
  mutate(volume_95percent = ifelse(co2 > threshold_95, 0, volume)) %>% 
  group_by(groupe) %>% 
  summarise(co2 = weighted.mean(co2, w = volume, na.rm = T)) %>% 
  left_join(co2_95percent, by = "groupe") %>% 
  pivot_longer(cols = c(co2, co2_95percent), 
               names_to = "measure",
               values_to = "co2") %>% 
  ggplot(aes(x = groupe, y = co2, fill = measure)) + geom_bar(stat="identity", position=position_dodge()) +
  geom_hline(yintercept=95, linetype="dashed", color = "blue") +
  theme_bw() +
  labs(x = "groupe", y = "gCO2/km", fill = "Mesure", title = "Emissions de CO2 en 2020")
ggsave(filename = "outputs/CO2_emissions_2020_manufacturer.jpg", w = 8, h = 5)






