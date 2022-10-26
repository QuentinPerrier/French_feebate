setwd("d:/morvillier/Desktop/REVISION_BONUS_MALUS/I4CE/Bonus malus/3_Méthodologie/")

source(file = "code/data_cleaning.R", encoding = "UTF-8")
#rm(list = setdiff(ls(), lsf.str()))

source(file = "code/demand_estimation.R", encoding =  "UTF-8")
#rm(list = setdiff(ls(), lsf.str()))

source(file = "code/feebate.R", encoding =  "UTF-8")

source(file = "code/CO2_emissions.R", encoding =  "UTF-8")

source(file = "code/norm.R", encoding =  "UTF-8")

source(file = "code/technical_progress", encoding =  "UTF-8")
