rm(list = ls())
# Jon Richar 
# 6/16/2020
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggplot2)

pct_dif<-read.csv("./DATA/Biomass_DATA/Calc_weight_differences_ML.csv")

names(pct_dif)


###############################################################################################
################################# BBRKC #######################################################
###############################################################################################


################################## Abbreviate ###################################################
Size10mm<-pct_dif$Size10[6:20]
BBRKC_MALE_COLD_NS<-pct_dif$BBRKC_MALE_COLD_NS[6:20]
BBRKC_MALE_WARM_NS<-pct_dif$BBRKC_MALE_WARM_NS[6:20]
BBRKC_MALE_COLD_OS<-pct_dif$BBRKC_MALE_COLD_OS[6:20]
BBRKC_MALE_WARM_OS<-pct_dif$BBRKC_MALE_WARM_OS[6:20]

BBRKC_WARM_COLD_NS_dif<-((BBRKC_MALE_WARM_NS-BBRKC_MALE_COLD_NS)/BBRKC_MALE_COLD_NS)*100
BBRKC_WARM_COLD_OS_dif<-((BBRKC_MALE_WARM_OS-BBRKC_MALE_COLD_OS)/BBRKC_MALE_COLD_OS)*100
BBRKC_WARM_OS_NS_dif<-((BBRKC_MALE_WARM_OS-BBRKC_MALE_WARM_NS)/BBRKC_MALE_WARM_NS)*100
BBRKC_COLD_OS_NS_dif<-((BBRKC_MALE_COLD_OS-BBRKC_MALE_COLD_NS)/BBRKC_MALE_COLD_NS)*100

rkc_dat<-as.data.frame(cbind(Size10mm,BBRKC_WARM_COLD_NS_dif,BBRKC_WARM_COLD_OS_dif,BBRKC_WARM_OS_NS_dif,BBRKC_COLD_OS_NS_dif))
rkc_dat

ggplot(rkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_WARM_COLD_NS_dif,color = "New shell - Warm/ Cold"),size=1,linetype=1)+
  geom_line(aes(y=BBRKC_WARM_COLD_OS_dif,color="Old shell - Warm/Cold"),size=1,linetype=1)+
  geom_line(aes(y=BBRKC_WARM_OS_NS_dif,color = "Warm - OS/NS"),size=1,linetype=1)+
  geom_line(aes(y=BBRKC_COLD_OS_NS_dif,color="Cold - OS/NS"),size=1,linetype=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/ Cold'='green','Old shell - Warm/Cold' = 'black','Warm - OS/NS'='red','Cold - OS/NS' = 'blue'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male Bristol Bay red king crab")

rkc_all_short<-ggplot(rkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_WARM_COLD_NS_dif,color = "New shell - Warm/ Cold"),size=1,linetype=1)+
  geom_line(aes(y=BBRKC_WARM_COLD_OS_dif,color="Old shell - Warm/Cold"),size=1,linetype=1)+
  geom_line(aes(y=BBRKC_WARM_OS_NS_dif,color = "Warm - OS/NS"),size=1,linetype=1)+
  geom_line(aes(y=BBRKC_COLD_OS_NS_dif,color="Cold - OS/NS"),size=1,linetype=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/ Cold'='green','Old shell - Warm/Cold' = 'black','Warm - OS/NS'='red','Cold - OS/NS' = 'blue'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male Bristol Bay red king crab")

#########################################################################################
################################ Females ##################################################

Size10mm<-pct_dif$Size10[6:18]
BBRKC_MATFEM_COLD<-pct_dif$BBRKC_MATFEM_COLD[6:18]
BBRKC_MATFEM_WARM<-pct_dif$BBRKC_MATFEM_WARM[6:18]

BBRKC_MATFEM_WARM_COLD_dif<-((BBRKC_MATFEM_WARM-BBRKC_MATFEM_COLD)/BBRKC_MATFEM_COLD)*100
BBRKC_MATFEM_WARM_COLD_dif
femrkc_dat<-as.data.frame(cbind(Size10mm,BBRKC_MATFEM_WARM_COLD_dif))

ggplot(femrkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MATFEM_WARM_COLD_dif,color = "Warm/Cold"),size=1)+
  scale_colour_manual(name='',values=c('Warm/Cold'='black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Mature female Bristol Bay Red king crab")

rkc_fem_short<-ggplot(femrkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MATFEM_WARM_COLD_dif,color = "Warm/Cold"),size=1)+
  scale_colour_manual(name='',values=c('Warm/Cold'='black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Mature female Bristol Bay Red king crab")

#############################################################################################
################################ SMBKC ######################################################
#############################################################################################



Size10mm<-pct_dif$Size10mm[5:20]
SMBKC_MALE_COLD_NS<-pct_dif$SMBKC_MALE_COLD_NS[5:20]
SMBKC_MALE_WARM_NS<-pct_dif$SMBKC_MALE_WARM_NS[5:20]
SMBKC_MALE_COLD_OS<-pct_dif$SMBKC_MALE_COLD_OS[5:20]
SMBKC_MALE_WARM_OS<-pct_dif$SMBKC_MALE_WARM_OS[5:20]

SMBKC_MALE_NS_WARM_COLD_dif<-((SMBKC_MALE_WARM_NS-SMBKC_MALE_COLD_NS)/SMBKC_MALE_COLD_NS)*100
SMBKC_MALE_OS_WARM_COLD_dif<-((SMBKC_MALE_WARM_OS-SMBKC_MALE_COLD_OS)/SMBKC_MALE_COLD_OS)*100
SMBKC_MALE_OS_NS_WARM_dif<-((SMBKC_MALE_WARM_OS-SMBKC_MALE_WARM_NS)/SMBKC_MALE_WARM_NS)*100
SMBKC_MALE_OS_NS_COLD_dif<-((SMBKC_MALE_COLD_OS-SMBKC_MALE_COLD_NS)/SMBKC_MALE_COLD_NS)*100

smbkc_dat<-as.data.frame(cbind(Size10mm,SMBKC_MALE_NS_WARM_COLD_dif,SMBKC_MALE_OS_WARM_COLD_dif,SMBKC_MALE_OS_NS_WARM_dif,SMBKC_MALE_OS_NS_COLD_dif))

ggplot(smbkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=SMBKC_MALE_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size=1)+
  geom_line(aes(y=SMBKC_MALE_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size=1)+
  geom_line(aes(y=SMBKC_MALE_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size = 1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male St. Matthew blue king crab")

smbkc_short<-ggplot(smbkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=SMBKC_MALE_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size=1)+
  geom_line(aes(y=SMBKC_MALE_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size=1)+
  geom_line(aes(y=SMBKC_MALE_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size = 1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male St. Matthew blue king crab")

#########################################################################################################
################################### BAIRDI ##############################################################

#################################### Immature  Males ####################################################

Size10mm<-pct_dif$Size10mm[2:14]
EBSCB_Immatmale_NS_COLD<-pct_dif$EBSCB_Immatmale_NS_COLD[2:14]
EBSCB_Immatmale_NS_WARM<-pct_dif$EBSCB_Immatmale_NS_WARM[2:14]
EBSCB_Immatmale_OS_COLD<-pct_dif$EBSCB_Immatmale_OS_COLD[2:14]
EBSCB_Immatmale_OS_WARM<-pct_dif$EBSCB_Immatmale_OS_WARM[2:14]

EBSCB_Immatmale_NS_WARM_COLD_dif<-((EBSCB_Immatmale_NS_WARM-EBSCB_Immatmale_NS_COLD)/EBSCB_Immatmale_NS_COLD)*100
EBSCB_Immatmale_OS_WARM_COLD_dif<-((EBSCB_Immatmale_OS_WARM-EBSCB_Immatmale_OS_COLD)/EBSCB_Immatmale_OS_COLD)*100
EBSCB_Immatmale_OS_NS_WARM_dif<-((EBSCB_Immatmale_OS_WARM-EBSCB_Immatmale_NS_WARM)/EBSCB_Immatmale_NS_WARM)*100
EBSCB_Immatmale_OS_NS_COLD_dif<-((EBSCB_Immatmale_OS_COLD-EBSCB_Immatmale_NS_COLD)/EBSCB_Immatmale_NS_COLD)*100

cb_immatdat<-as.data.frame(cbind(Size10mm,
                                 EBSCB_Immatmale_NS_WARM_COLD_dif,
                                 EBSCB_Immatmale_OS_WARM_COLD_dif,
                                 EBSCB_Immatmale_OS_NS_WARM_dif,
                                 EBSCB_Immatmale_OS_NS_COLD_dif))

ggplot(cb_immatdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Immatmale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male Eastern Bering Sea Bairdi")

cb_immat_short<-ggplot(cb_immatdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Immatmale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male Eastern Bering Sea Bairdi")


##########################################################################################################
##################################### Mature  Males#########################################################

Size10mm<-pct_dif$Size10mm[2:14]
EBSCB_Matmale_NS_COLD<-pct_dif$EBSCB_Matmale_NS_COLD[2:14]
EBSCB_Matmale_NS_WARM<-pct_dif$EBSCB_Matmale_NS_WARM[2:14]
EBSCB_Matmale_OS_COLD<-pct_dif$EBSCB_Matmale_OS_COLD[2:14]
EBSCB_Matmale_OS_WARM<-pct_dif$EBSCB_Matmale_OS_WARM[2:14]

EBSCB_Matmale_NS_WARM_COLD_dif<-((EBSCB_Matmale_NS_WARM-EBSCB_Matmale_NS_COLD)/EBSCB_Matmale_NS_COLD)*100
EBSCB_Matmale_OS_WARM_COLD_dif<-((EBSCB_Matmale_OS_WARM-EBSCB_Matmale_OS_COLD)/EBSCB_Matmale_OS_COLD)*100
EBSCB_Matmale_OS_NS_WARM_dif<-((EBSCB_Matmale_OS_WARM-EBSCB_Matmale_NS_WARM)/EBSCB_Matmale_NS_WARM)*100
EBSCB_Matmale_OS_NS_COLD_dif<-((EBSCB_Matmale_OS_COLD-EBSCB_Matmale_NS_COLD)/EBSCB_Matmale_NS_COLD)*100

cb_matdat<-as.data.frame(cbind(Size10mm,
                               EBSCB_Matmale_NS_WARM_COLD_dif,
                               EBSCB_Matmale_OS_WARM_COLD_dif,
                               EBSCB_Matmale_OS_NS_WARM_dif,
                               EBSCB_Matmale_OS_NS_COLD_dif))

ggplot(cb_matdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Matmale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCB_Matmale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCB_Matmale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCB_Matmale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male Eastern Bering Sea Bairdi")

cb_matmale_short<-ggplot(cb_matdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Matmale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCB_Matmale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCB_Matmale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCB_Matmale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male Eastern Bering Sea Bairdi")

########################################################################################################################################
######################################### Females #######################################################################################

Size10mm<-pct_dif$Size10mm[5:14]
EBSCB_MatFemale_NS_COLD<-pct_dif$EBSCB_MatFemale_NS_COLD[5:14]
EBSCB_MatFemale_NS_WARM<-pct_dif$EBSCB_MatFemale_NS_WARM[5:14]
EBSCB_MatFemale_OS_COLD<-pct_dif$EBSCB_MatFemale_OS_COLD[5:14]
EBSCB_MatFemale_OS_WARM<-pct_dif$EBSCB_MatFemale_OS_WARM[5:14]


EBSCB_MatFemale_NS_WARM_COLD_dif<-((EBSCB_MatFemale_NS_WARM-EBSCB_MatFemale_NS_COLD)/EBSCB_MatFemale_NS_COLD)*100
EBSCB_MatFemale_OS_WARM_COLD_dif<-((EBSCB_MatFemale_OS_WARM-EBSCB_MatFemale_OS_COLD)/EBSCB_MatFemale_OS_COLD)*100
EBSCB_MatFemale_OS_NS_WARM_dif<-((EBSCB_MatFemale_OS_WARM-EBSCB_MatFemale_NS_WARM)/EBSCB_MatFemale_NS_WARM)*100
EBSCB_MatFemale_OS_NS_COLD_dif<-((EBSCB_MatFemale_OS_COLD-EBSCB_MatFemale_NS_COLD)/EBSCB_MatFemale_NS_COLD)*100

cb_matfemdat<-as.data.frame(cbind(Size10mm,
                                  EBSCB_MatFemale_NS_WARM_COLD_dif,
                                  EBSCB_MatFemale_OS_WARM_COLD_dif,
                                  EBSCB_MatFemale_OS_NS_WARM_dif,
                                  EBSCB_MatFemale_OS_NS_COLD_dif))

ggplot(cb_matfemdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_MatFemale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature female Eastern Bering Sea Bairdi")

cb_matfem_short<-ggplot(cb_matfemdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_MatFemale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature female Eastern Bering Sea Bairdi")

####################################################################################################################################
############################################################# OPILIO#################################################################
# Immature  Males
################################## Abbreviate ####################################################################
Size10mm<-pct_dif$Size10mm[2:12]
EBSCO_Immatmale_NS_COLD<-pct_dif$EBSCO_Immatmale_NS_COLD[2:12]
EBSCO_Immatmale_NS_WARM<-pct_dif$EBSCO_Immatmale_NS_WARM[2:12]
EBSCO_Immatmale_OS_COLD<-pct_dif$EBSCO_Immatmale_OS_COLD[2:12]
EBSCO_Immatmale_OS_WARM<-pct_dif$EBSCO_Immatmale_OS_WARM[2:12]

EBSCO_Immatmale_NS_WARM_COLD_dif<-((EBSCO_Immatmale_NS_WARM-EBSCO_Immatmale_NS_COLD)/EBSCO_Immatmale_NS_COLD)*100
EBSCO_Immatmale_OS_WARM_COLD_dif<-((EBSCO_Immatmale_OS_WARM-EBSCO_Immatmale_OS_COLD)/EBSCO_Immatmale_OS_COLD)*100
EBSCO_Immatmale_OS_NS_WARM_dif<-((EBSCO_Immatmale_OS_WARM-EBSCO_Immatmale_NS_WARM)/EBSCO_Immatmale_NS_WARM)*100
EBSCO_Immatmale_OS_NS_COLD_dif<-((EBSCO_Immatmale_OS_COLD-EBSCO_Immatmale_NS_COLD)/EBSCO_Immatmale_NS_COLD)*100

co_immatdat<-as.data.frame(cbind(Size10mm,
                                 EBSCO_Immatmale_NS_WARM_COLD_dif,
                                 EBSCO_Immatmale_OS_WARM_COLD_dif,
                                 EBSCO_Immatmale_OS_NS_WARM_dif,
                                 EBSCO_Immatmale_OS_NS_COLD_dif))

ggplot(co_immatdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Immatmale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male Eastern Bering Sea opilio")

co_immat_short<-ggplot(co_immatdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Immatmale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male Eastern Bering Sea opilio")

####################################### Mature  Males ##########################################################################

########################################## Abbreviate ##################################################################################
Size10mm<-pct_dif$Size10mm[2:14]
EBSCO_Matmale_NS_COLD<-pct_dif$EBSCO_Matmale_NS_COLD[2:14]
EBSCO_Matmale_NS_WARM<-pct_dif$EBSCO_Matmale_NS_WARM[2:14]
EBSCO_Matmale_OS_COLD<-pct_dif$EBSCO_Matmale_OS_COLD[2:14]
EBSCO_Matmale_OS_WARM<-pct_dif$EBSCO_Matmale_OS_WARM[2:14]

EBSCO_Matmale_NS_WARM_COLD_dif<-((EBSCO_Matmale_NS_WARM-EBSCO_Matmale_NS_COLD)/EBSCO_Matmale_NS_COLD)*100
EBSCO_Matmale_OS_WARM_COLD_dif<-((EBSCO_Matmale_OS_WARM-EBSCO_Matmale_OS_COLD)/EBSCO_Matmale_OS_COLD)*100
EBSCO_Matmale_OS_NS_WARM_dif<-((EBSCO_Matmale_OS_WARM-EBSCO_Matmale_NS_WARM)/EBSCO_Matmale_NS_WARM)*100
EBSCO_Matmale_OS_NS_COLD_dif<-((EBSCO_Matmale_OS_COLD-EBSCO_Matmale_NS_COLD)/EBSCO_Matmale_NS_COLD)*100

co_matdat<-as.data.frame(cbind(Size10mm,
                               EBSCO_Matmale_NS_WARM_COLD_dif,
                               EBSCO_Matmale_OS_WARM_COLD_dif,
                               EBSCO_Matmale_OS_NS_WARM_dif,
                               EBSCO_Matmale_OS_NS_COLD_dif))

ggplot(co_matdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Matmale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCO_Matmale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCO_Matmale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCO_Matmale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male Eastern Bering Sea opilio")

co_matmale_short<-ggplot(co_matdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Matmale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCO_Matmale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCO_Matmale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCO_Matmale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male Eastern Bering Sea opilio")


################################################### Females######################################################
################################################### Abbreviate ##################################################
Size10mm<-pct_dif$Size10mm[4:10]
EBSCO_MatFemale_NS_COLD<-pct_dif$EBSCO_MatFemale_NS_COLD[4:10]
EBSCO_MatFemale_NS_WARM<-pct_dif$EBSCO_MatFemale_NS_WARM[4:10]
EBSCO_MatFemale_OS_COLD<-pct_dif$EBSCO_MatFemale_OS_COLD[4:10]
EBSCO_MatFemale_OS_WARM<-pct_dif$EBSCO_MatFemale_OS_WARM[4:10]

EBSCO_MatFemale_NS_WARM_COLD_dif<-((EBSCO_MatFemale_NS_WARM-EBSCO_MatFemale_NS_COLD)/EBSCO_MatFemale_NS_COLD)*100
EBSCO_MatFemale_OS_WARM_COLD_dif<-((EBSCO_MatFemale_OS_WARM-EBSCO_MatFemale_OS_COLD)/EBSCO_MatFemale_OS_COLD)*100
EBSCO_MatFemale_OS_NS_WARM_dif<-((EBSCO_MatFemale_OS_WARM-EBSCO_MatFemale_NS_WARM)/EBSCO_MatFemale_NS_WARM)*100
EBSCO_MatFemale_OS_NS_COLD_dif<-((EBSCO_MatFemale_OS_COLD-EBSCO_MatFemale_NS_COLD)/EBSCO_MatFemale_NS_COLD)*100

co_matfemdat<-as.data.frame(cbind(Size10mm,
                                  EBSCO_MatFemale_NS_WARM_COLD_dif,
                                  EBSCO_MatFemale_OS_WARM_COLD_dif,
                                  EBSCO_MatFemale_OS_NS_WARM_dif,
                                  EBSCO_MatFemale_OS_NS_COLD_dif))

ggplot(co_matfemdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_MatFemale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature female Eastern Bering Sea opilio")

co_matfem_short<-ggplot(co_matfemdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_MatFemale_NS_WARM_COLD_dif,color = "New shell - Warm/Cold"),size=1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_WARM_COLD_dif,color="Old shell - Warm/Cold"),size = 1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_NS_WARM_dif,color = "Warm - Old shell/New shell"),size = 1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_NS_COLD_dif,color="Cold - Old shell/New shell"),size=1)+
  scale_colour_manual(name='',values=c('New shell - Warm/Cold'='green','Old shell - Warm/Cold' = 'red','Warm - Old shell/New shell'='blue','Cold - Old shell/New shell' = 'black'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,1,1))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature female Eastern Bering Sea opilio")


####################################################################################################################
########################## NBS #####################################################################################

######################################## 
########################################### BBRKC + SMBKC
dev.new()

ggarrange(rkc_all_short,smbkc_short,
          labels = c("a.)", "b.)"),
          ncol = 1, nrow = 2)

##### ########################### male Bairdi and opilio

dev.new()

ggarrange(cb_immat_short,cb_matmale_short,co_immat_short,co_matmale_short,
          labels = c("a.)", "b.)","c.)", "d.)"),
          ncol = 2, nrow = 2)

################################## Female RKC, bairdi and opilio
dev.new()

ggarrange(rkc_fem_short,cb_matfem_short,co_matfem_short,
labels = c("a.)", "b.)", "c.)"),
ncol = 2, nrow = 2)


################################# NBS ###############################################

#dev.new()
#ggarrange(nsrkc,nbs_bkc, nbs_co,
#          labels = c("a.)", "b.)", "c.)"),
#          ncol = 2, nrow = 2)

