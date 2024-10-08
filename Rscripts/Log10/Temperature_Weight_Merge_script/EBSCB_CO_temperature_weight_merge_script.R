library(stats4)
library(readxl)
library(tidyverse)

###### FIRST MALE TANNERS
Male_EBSCB_weights <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/EBS_CB_Analysis_males_log10.csv")

Male_EBSCB_temps<- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/CB_MALE_CPUENUM_WEIGHTED_TEMP.csv")

merge(x= Male_EBSCB_weights, y= Male_EBSCB_temps, by.x = 'CRUISE', by.y='CRUISE', all.x=TRUE) -> Male_EBSCB_weights_temps


write.csv(Male_EBSCB_weights_temps,"//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/Male_EBSCB_weights_temps_analysis.csv")


#### REPEAT FOR MALE SNOW CRAB

Male_EBSCO_weights <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/EBS_CO_Analysis_males_log10.csv")

Male_EBSCO_temps<- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/CO_MALE_CPUENUM_WEIGHTED_TEMP.csv")

merge(x= Male_EBSCO_weights, y= Male_EBSCO_temps, by.x = 'CRUISE', by.y='CRUISE', all.x=TRUE) -> Male_EBSCO_weights_temps


write.csv(Male_EBSCO_weights_temps,"//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/Male_EBSCO_weights_temps_analysis.csv")

############################################################################################################################################
########################################### FEMALES ####################################################################################

###### FIRST FEMALE TANNERS
Female_EBSCB_weights <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/EBS_CB_Analysis_immat_matfemales_log10.csv")

Female_EBSCB_temps<- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/CB_FEMALE_CPUENUM_WEIGHTED_TEMP.csv")

merge(x= Female_EBSCB_weights, y= Female_EBSCB_temps, by.x = 'CRUISE', by.y='CRUISE', all.x=TRUE) -> Female_EBSCB_weights_temps


write.csv(Female_EBSCB_weights_temps,"//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/Female_EBSCB_weights_temps_analysis.csv")

###### THEN FEMALE OPILIO
Female_EBSCO_weights <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/EBS_CO_Analysis_immat_matfemales_log10.csv")

Female_EBSCO_temps<- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/CO_FEMALE_CPUENUM_WEIGHTED_TEMP.csv")
merge(x= Female_EBSCO_weights, y= Female_EBSCO_temps, by.x = 'CRUISE', by.y='CRUISE', all.x=TRUE) -> Female_EBSCO_weights_temps
write.csv(Female_EBSCO_weights_temps,"//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/Female_EBSCO_weights_temps_analysis.csv")

###### THEN FEMALE BBRKC
Female_BBRKC_weights <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/EBS_CO_Analysis_immat_matfemales_log10.csv")

Female_BBRKC_temps<- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/CO_FEMALE_CPUENUM_WEIGHTED_TEMP.csv")
merge(x= Female_BBRKC_weights, y= Female_BBRKC_temps, by.x = 'CRUISE', by.y='CRUISE', all.x=TRUE) -> Female_EBSCO_weights_temps
write.csv(Female_BBRKC_weights_temps,"//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/Female_EBSCO_weights_temps_analysis.csv")
