This document provides the information to replicate the results of the report entitled
“An ex-ante evaluation of the French car feebate”.
In order for the code to work correctly, you must first have installed the following libraries:

- tidyverse
- ggplot2
- ggridges
- gtable
- gridExtra
- readxl
Different R files are used for the simulation:
- products.RDS
- products_sans_2020.RDS
- achats_menages_moyen.RDS
- regression_coefficients.RDS
To ensure the correct functioning of the code, it is imperative to create a folder entitled
“outputs” in which the various files in RDS format mentioned above are stored.
List of the excel files necessary for the estimation:
- bareme_bonus_malus.xlsx
- data_calcul_tableau4. xlsx
- products_2019.xlsx
- technical_progress. xlsx

To ensure the proper functioning of the code, it is imperative to create folder entiled
“inputs” in which the various Excel files mentioned above are stored.
Concerning the codes, there are different files necessary for the simulation of the bareme
bonus-malus:
- demand_estimation
- feebate.R
- bareme_simulation.R
- CO2_emissions.R
- Simulation_2_0.R
- norm.R
- technical_progress.R
- weight.R

The sales data cannot be shared, but the sales files have the following columns:
DATE	Genre	Type	Marque	Modèle	Version	Puissance fiscale	Carrosserie	Energie	Cylindrée	Gamme	CO2	Poids à vide	Puissance KW	Rapport poid puissance	Type de boite	Groupe	MF/ME	Consommation extra-urbaine	Consommation mixte	Consommation urbaine	BONUS_MALUS	BONUS_MALUS_BAREME_2019	Prix du véhicule	DATE_ARRETE	VOLUME	JO

