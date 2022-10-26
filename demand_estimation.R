library(tidyverse)
library(AER)
library(fastDummies) #For creating dummy columns
library(stargazer) # for tables
library(sjPlot)
library(sjlabelled)
library(optimbase) # for matrixes
library(matlab)
library(ggplot2)
library(ggridges)
library(gtable)
library(gridExtra)

setwd("d:/morvillier/Desktop/REVISION_BONUS_MALUS/I4CE/Bonus malus/3_Méthodologie/")

# Mise en route ---------------------------------
sales <- readRDS("outputs/sales.RDS") 

#On regroupe les produits similaires ensemble
products <- sales %>% 
  mutate(product_id = paste(marque, modele, energie, segment, sep = "_")) %>% 
  group_by(annee, product_id) %>% 
  mutate(volumes = sum(volume)) %>% 
  arrange(annee, product_id, desc(volume)) %>% 
  slice(1) %>% 
  dplyr::select(-volume) %>% 
  rename(volume = volumes) %>% 
  ungroup()

products$vehicule_nb <- 1:nrow(products) 


sum(sales$volume)
sum(products$volume)
length(unique(products$product_id))
products %>% filter(annee == 2020)

sapply(products, function(x) sum(is.na(x)))
 


products %>% 
  filter(prix < 300000) %>% 
  mutate(prix = prix / 1000) %>% 
  ggplot(aes(x = prix, y = co2, color = motorization)) + geom_point() +
  theme_bw() +
  labs(x = "Prix (k€)", y = "Emissions de CO2 (gCO2/km)", color = "Motorisation")
ggsave("outputs/emissions_vs_price.jpg", width = 8, height = 5)

products %>% 
  filter(co2 < 80) %>% 
  ggplot(aes(x = prix, y = co2, color = energie)) + geom_point()






sapply(products, function(x) sum(is.na(x)))

# on retire les donnees où l'on n'a pas la prix ou le cout variable du carburant 
products <- products %>% 
  filter(!is.na(prix),
         !is.na(cout_variable_carburant), 
         vehicule_nb != 3800) #35 observations   

# on ajoute une mesure du cout du carburant pour 100km en euros

products$cout_variable_carburant2 <- products$cout_variable_carburant*100 


# Part de marché globale (sans outside option) (correspond à s_j dans le papier de Isis)

products <- products %>%
  group_by(annee) %>%
  mutate(total_ventes = sum(volume),
         market_share_sj = volume / total_ventes) %>%
  dplyr::select(- total_ventes) %>%
  ungroup()


# Part de marché au sein du segment: Sj|g ---------------------------------



products <- products %>%
  group_by(annee, segment) %>%
  mutate(segment_sales = sum(volume)) %>%
  ungroup()



products <- products %>%
  group_by(annee) %>%
  mutate(market_share_segment_sjg = volume / segment_sales) %>%
  ungroup

products <- products %>%
  mutate(log_sjg = log(market_share_segment_sjg))



# Création des dummies ----------------------------------------------------


products <- products %>% dummy_cols(select_columns = c("motorization",
                                                       "annee" ,             # ou energie ? 
                                                       "groupe"),            #Plutôt que les marques, trop nombreuses
                                    remove_first_dummy = FALSE)  

# creation dummy bas_carbone 


products <- products %>%
  mutate(bas_carbone = motorization_Electrique + motorization_Hybride_non_rechargeable + motorization_Hybride_rechargeable)


# on créer la dummy coupe
products$coupe <- ifelse(products$carrosserie == 'COUPE', 1, 0)


# déflatage des prix de ventes des véhicules

products_2015_bis <- products %>% filter(annee == 2015)
products_2016_bis <- products %>% filter(annee == 2016) %>% mutate(prix/0,998136811)
products_2017_bis <- products %>% filter(annee == 2017) %>% mutate(prix/0,988288778)
products_2018_bis <- products %>% filter(annee == 2018) %>% mutate(prix/0,972597077)
products_2019_bis <- products %>% filter(annee == 2019) %>% mutate(prix/0,963685132)
products_2020_bis <- products %>% filter(annee == 2020) %>% mutate(prix/0,961692579)


# Construction des instruments -----------------------------------------

#Nos instruments : puissance fiscale, poids à vide, cout_variable_carburant2, bas_carbone, coupe. Il faudrait aussi demander à l'Ademe la consommation en L/100, la hauteur et la largeur



#On calcule les sommes des caractéristiques pour tous les AUTRES groupes 


products <- products %>% 
  group_by(annee) %>% 
  mutate(instrument1 = sum(puissance_fiscale),
         instrument2 = sum(poids),
         instrument7 = sum(cout_variable_carburant, na.rm = TRUE),
         instrument10 = sum(coupe),
         instrument13 = sum(co2,  na.rm = TRUE),
         instrument16 = sum(cout_variable_carburant2, na.rm = TRUE),
         instrument19 = sum(bas_carbone)) %>% 
  group_by(annee, groupe) %>% 
  mutate(instrument1_group_tmp = sum(puissance_fiscale),
         instrument2_group_tmp = sum(poids),
         instrument7_group_tmp = sum(cout_variable_carburant, na.rm = TRUE),
         instrument10_group_tmp = sum(coupe),
         instrument13_group_tmp = sum(co2, na.rm = TRUE),
         instrument16_group_tmp = sum(cout_variable_carburant2, na.rm = TRUE),
         instrument19_group_tmp = sum(bas_carbone)) %>% 
  ungroup() %>% 
  mutate(instrument1 = instrument1 - instrument1_group_tmp,
         instrument2 = instrument2 - instrument2_group_tmp,
         instrument7 = instrument7 - instrument7_group_tmp,
         instrument10 = instrument10 - instrument10_group_tmp,
         instrument13 = instrument13 - instrument13_group_tmp,
         instrument16 = instrument16 - instrument16_group_tmp,
         instrument19 = instrument19 - instrument19_group_tmp) %>% 
  dplyr::select(-c(instrument1_group_tmp, instrument2_group_tmp, instrument7_group_tmp, instrument10_group_tmp, instrument13_group_tmp, instrument16_group_tmp, instrument19_group_tmp))

#On calcule cette fois les sommes des caractéristiques pour les AUTRES groupes au sein de MEME segment
products <- products %>% 
  group_by(annee, segment) %>% 
  mutate(instrument3 = sum(puissance_fiscale),
         instrument4 = sum(poids),
         instrument8 = sum(cout_variable_carburant, na.rm = TRUE),
         instrument11 = sum(coupe),
         instrument14 = sum(co2,  na.rm = TRUE),
         instrument17 = sum(cout_variable_carburant2, na.rm = TRUE),
         instrument20 = sum(bas_carbone)) %>% 
  group_by(annee, segment, groupe) %>% 
  mutate(instrument3_group_tmp = sum(puissance_fiscale),
         instrument4_group_tmp = sum(poids),
         instrument8_group_tmp = sum(cout_variable_carburant, na.rm = TRUE),
         instrument11_group_tmp = sum(coupe),
         instrument14_group_tmp = sum(co2,  na.rm = TRUE),
         instrument17_group_tmp = sum(cout_variable_carburant2, na.rm = TRUE),
         instrument20_group_tmp = sum(bas_carbone)) %>% 
  ungroup() %>% 
  mutate(instrument3 = instrument3 - instrument3_group_tmp,
         instrument4 = instrument4 - instrument4_group_tmp,
         instrument8 = instrument8 - instrument8_group_tmp,
         instrument11 = instrument11 - instrument11_group_tmp,
         instrument14 = instrument14 - instrument14_group_tmp,
         instrument17 = instrument17 - instrument17_group_tmp,
         instrument20 = instrument20 - instrument20_group_tmp) %>% 
  dplyr::select(-c(instrument3_group_tmp, instrument4_group_tmp, instrument8_group_tmp, instrument11_group_tmp, instrument14_group_tmp, instrument17_group_tmp, instrument20_group_tmp))


#On calcule cette fois les sommes des caractéristiques des autres produits du groupe dans le même segment
products <- products %>% 
  group_by(annee, segment, groupe) %>% 
  mutate(instrument5 = sum(puissance_fiscale),
         instrument6 = sum(poids),
         instrument9 = sum(cout_variable_carburant, na.rm = TRUE),
         instrument12 = sum(coupe),
         instrument15 = sum(co2,  na.rm = TRUE),
         instrument18 = sum(cout_variable_carburant2, na.rm = TRUE),
         instrument21 = sum(bas_carbone)) %>% 
  group_by(annee, segment, groupe, product_id) %>% 
  mutate(instrument5_group_tmp = sum(puissance_fiscale),
         instrument6_group_tmp = sum(poids),
         instrument9_group_tmp = sum(cout_variable_carburant, na.rm = TRUE),
         instrument12_group_tmp = sum(coupe),
         instrument15_group_tmp = sum(co2,  na.rm = TRUE),
         instrument18_group_tmp = sum(cout_variable_carburant2, na.rm = TRUE),
         instrument21_group_tmp = sum(bas_carbone)) %>% 
  ungroup() %>% 
  mutate(instrument5 = instrument5 - instrument5_group_tmp,
         instrument6 = instrument6 - instrument6_group_tmp,
         instrument9 = instrument9 - instrument9_group_tmp,
         instrument12 = instrument12 - instrument12_group_tmp,
         instrument15 = instrument15 - instrument15_group_tmp,
         instrument18 = instrument18 - instrument18_group_tmp,
         instrument21 = instrument21 - instrument21_group_tmp) %>% 
  dplyr::select(-c(instrument5_group_tmp, instrument6_group_tmp, instrument9_group_tmp, instrument12_group_tmp, instrument15_group_tmp, instrument18_group_tmp, instrument21_group_tmp))



# Calcul des parts de marché en fonction de l'optout s0  ------------------


#hypothèses : 
duree_remplacement <- 6 #Durée de remplacement des véhicules. Au bout de combien de temps les ménages rachètent-ils un véhicule ? De façon temporaire on prend 4 ans en moyenne, comme Durrmeyer

achats_menages_moyen <- tibble(annee = 2015:2020, 
                               nombre_menages = c(28255500, 28409800, 28559500, 28671700, 28771900, 28863600), # evolution du nombre de menage selon euromonitor : https://www.euromonitor.com/france/country-factfile
                               achats_theoriques_menages = nombre_menages / duree_remplacement)
saveRDS(achats_menages_moyen, "outputs/achats_menages_moyen.RDS")

#On calcule les parts s0 de ménages qui ont choisi de ne PAS acheter de véhicules
opt_out_shares_s0 <- products %>% group_by(annee) %>% summarise(achats_realises_annuels = sum(volume)) %>% 
  full_join(achats_menages_moyen) %>% 
  mutate(market_share_s0 = (achats_theoriques_menages - achats_realises_annuels) / achats_theoriques_menages)

#On ajoute s1 et s0 au tableau
products <- products %>% 
  left_join(., achats_menages_moyen, by = "annee") %>%   
  mutate(market_share_s1 = volume / achats_theoriques_menages) %>% 
  left_join(., opt_out_shares_s0 %>% dplyr::select(annee, market_share_s0), by = "annee")

# Creation de la nouvelle variable expliquée : 
products$y <- log(products$market_share_s1 / products$market_share_s0)       

# Creation de la dataframe sans annee 2020
products <- products %>%
  mutate(prix_10k = prix / 10000,
         prix_final_10k = prix_final / 10000) 

products_sans_2020 <- products %>%
  filter(annee < 2020) %>%
  dplyr::select(-annee_2019)
  

ou_sont_les_NA <- products_sans_2020 %>%   #il nous manque les données du prix du superéthanol donc on pas tout les prix variable du carburant
  filter(is.na(cout_variable_carburant))   # ce qui fait qu'on a pas tout les résidus non plus... à voir si on trouve les prix sinon véhicules à retirer dans data cleaning (32 valeurs sur 2015 2019)


#regression part de marché------ 

############# sans 2020 / prix en 10k : c'est significatif  

exogenous_variables <- "poids + cout_variable_carburant + coupe + bas_carbone + puissance_fiscale"
dummies <- paste(colnames(products_sans_2020[58 : 80]), collapse = " + ") # annee, constructeur
endogenous_variables <- " log_sjg + prix_final_10k "


exogenous_et_instruments <- paste0(exogenous_variables, " + ", dummies, " + ", "  instrument7 + instrument8 + instrument9 + instrument2 + instrument4 + instrument6 + instrument1 + instrument3 + instrument5 + instrument10 + instrument11 + instrument12 + instrument19 + instrument20 + instrument21 ")

regression_results<- ivreg(as.formula(paste("y~", exogenous_variables, " + ", dummies, " + ", endogenous_variables, " | ", exogenous_et_instruments)), data = products_sans_2020)

summary(regression_results)

summary(regression_results,vcov=sandwich,diagnostics = TRUE) #donne la relevance des instruments ils semblent être bons

regression_results_table <- printCoefmat(summary(regression_results)$coefficients[-c(7:27),])


  
  #Affiche les résultats sous forme de tableau LateX
  #stargazer::stargazer(regression_results_table)
  
  resid_regression_results <- residuals(regression_results)  # donne les résiduals du marchés
  shapiro.test(resid_regression_results)           #  p-value est significative. L'échantillon ne suit donc pas une loi normale.  

# resid_regression_results <- c(resid_regression_results, 0.0007)


print(resid_regression_results)


products_sans_2020 <- products_sans_2020 %>% # on ajoute les résidus à la base de données
  mutate(residus = resid_regression_results)


#On ajoute les deltas
alpha <- coef(summary(regression_results))["prix_final_10k","Estimate"]               # alpha =   -0.7635452;#sigma =  0.3029028    coeff de Isis
sigma <- coef(summary(regression_results))["log_sjg", "Estimate"]
regression_coefficients <- c(alpha = alpha, sigma = sigma)

products_sans_2020 <- products_sans_2020 %>% 
  mutate(delta = y - (sigma * log_sjg) - (alpha * prix_final_10k),
         delta_with_resids = delta + residus)



## sauvegardes des data_frames products, products_sans_2020 et regression_results
saveRDS(products, file = "outputs/products.RDS")

saveRDS(products_sans_2020, file = "outputs/products_sans_2020.RDS")

saveRDS(regression_coefficients, file = "outputs/regression_coefficients.RDS")


## sauvegardes des data_frames products, products_sans_2020 et regression_results
saveRDS(products, file = "outputs/products.RDS")

saveRDS(products_sans_2020, file = "outputs/products_sans_2020.RDS")

saveRDS(regression_coefficients, file = "outputs/regression_coefficients.RDS")


## CALCUL DES SURPLUS DU CONSOMMATEUR




##  products$y <- log(products$market_share_s1 / products$market_share_s0)      # ENDOGENOUS VARIABLE 

##  log_sjg # part de marché intra-segmentaire 

##  sigma # coefficient estimé associé à la part de marché intra-segmentaire 

##  products_sans_2020 



##  products_sans_2020 <- products %>%
##    filter(annee < 2020) %>%
##    dplyr::select(-annee_2019)
##    mutate(y <- log(products$market_share_s1 / products$market_share_s0))      # ENDOGENOUS VARIABLE 



## products_test <- products_sans_2020 %>% 
##   group_by(annee, segment, groupe) %>% 
##   mutate(residus = resid_regression_results)



##  data <- as.data.frame(products_sans_2020)


##  write.csv(data, file="d://morvillier//Desktop//REVISION_BONUS_MALUS//DONNEES_GRAPHIQUE//CALCUL_KEYVAN//Nouveau dossier//data.csv", row.names = FALSE)































