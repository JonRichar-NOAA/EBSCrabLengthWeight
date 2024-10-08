rm(list = ls())
# Jon Richar 
# 6/16/2020
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggplot2)
?ggplot()
?geom_line()
pct_dif<-read.csv("./DATA/Biomass_DATA/Calc_weight_differences_ML.csv")

names(pct_dif)
######################### Create data objects ###################
size<-pct_dif$Size10mm

######################### BBRKC
rkc_ns_cold_dif<-pct_dif$BBRKC_MALE_COLD_NS_pct_dif
mean(rkc_ns_cold_dif)

rkc_ns_warm_dif<-pct_dif$BBRKC_MALE_WARM_NS_pct_dif
mean(rkc_ns_warm_dif)

rkc_os_cold_dif<-pct_dif$BBRKC_MALE_COLD_OS_pct_dif
mean(rkc_os_cold_dif)

rkc_os_warm_dif<-pct_dif$BBRKC_MALE_WARM_OS_pct_dif
mean(rkc_os_warm_dif)

######################## BAIRDI #########################################
cb_immatmale_cold_ns_dif<-pct_dif$EBSCB_Immatmale_NS_COLD_pct_dif
mean(cb_immatmale_cold_ns_dif)

cb_immatmale_cold_os_dif<-pct_dif$EBSCB_Immatmale_OS_COLD_pct_dif
mean(cb_immatmale_cold_os_dif)


cb_immatmale_warm_ns_dif<-pct_dif$EBSCB_Immatmale_NS_WARM_pct_dif
mean(cb_immatmale_cold_ns_dif)

cb_immatmale_warm_os_dif<-pct_dif$EBSCB_Immatmale_OS_WARM_pct_dif
mean(cb_immatmale_warm_os_dif)


cb_matmale_cold_ns_dif<-pct_dif$EBSCB_Matmale_NS_COLD_pct_dif
mean(cb_matmale_cold_ns_dif)

cb_matmale_cold_os_dif<-pct_dif$EBSCB_Matmale_OS_COLD_pct_dif
mean(cb_matmale_cold_os_dif)


cb_matmale_warm_ns_dif<-pct_dif$EBSCB_Matmale_NS_WARM_pct_dif
mean(cb_matmale_cold_ns_dif)

cb_matmale_warm_os_dif<-pct_dif$EBSCB_Matmale_OS_WARM_pct_dif
mean(cb_matmale_warm_os_dif)


cb_ns_cold_matfem_dif<-pct_dif$EBSCB_MatFemale_NS_COLD 
mean(cb_ns_cold_matfem_dif)

cb_ns_warm_matfem_dif<-pct_dif$EBSCB_MatFemale_NS_WARM 
mean(cb_ns_warm_matfem_dif)

cb_os_cold_matfem_dif<-pct_dif$EBSCB_MatFemale_OS_COLD 
mean(cb_os_cold_matfem_dif)

cb_os_warm_matfem_dif<-pct_dif$EBSCB_MatFemale_OS_WARM 
mean(cb_os_warm_matfem_dif)

########################## Opilio #####################################
co_immatmale_cold_ns_dif<-pct_dif$EBSCO_Immatmale_NS_COLD_pct_dif
mean(co_immatmale_cold_ns_dif)

co_immatmale_cold_os_dif<-pct_dif$EBSCO_Immatmale_OS_COLD_pct_dif
mean(co_immatmale_cold_os_dif)


co_immatmale_warm_ns_dif<-pct_dif$EBSCO_Immatmale_NS_WARM_pct_dif
mean(co_immatmale_cold_ns_dif)

co_immatmale_warm_os_dif<-pct_dif$EBSCO_Immatmale_OS_WARM_pct_dif
mean(co_immatmale_warm_os_dif)


co_matmale_cold_ns_dif<-pct_dif$EBSCO_Matmale_NS_COLD_pct_dif
mean(co_matmale_cold_ns_dif)

co_matmale_cold_os_dif<-pct_dif$EBSCO_Matmale_OS_COLD_pct_dif
mean(co_matmale_cold_os_dif)


co_matmale_warm_ns_dif<-pct_dif$EBSCO_Matmale_NS_WARM_pct_dif
mean(co_matmale_cold_ns_dif)

co_matmale_warm_os_dif<-pct_dif$EBSCO_Matmale_OS_WARM_pct_dif
mean(co_matmale_warm_os_dif)


co_ns_cold_matfem_dif<-pct_dif$EBSCO_MatFemale_NS_COLD 
mean(co_ns_cold_matfem_dif)

co_ns_warm_matfem_dif<-pct_dif$EBSCO_MatFemale_NS_WARM 
mean(co_ns_warm_matfem_dif)

co_os_cold_matfem_dif<-pct_dif$EBSCO_MatFemale_OS_COLD 
mean(co_os_cold_matfem_dif)

co_os_warm_matfem_dif<-pct_dif$EBSCO_MatFemale_OS_WARM 
mean(co_os_warm_matfem_dif)
#################################### Plot #######################################

###############################################################################################
################################# BBRKC #######################################################
###############################################################################################

##################################################################################################
################################# Males ########################################################
ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=BBRKC_MALE_COLD_NS_pct_dif,colour = "Cold - NS"),size=1,colour="blue",show.legend = TRUE)+
geom_line(aes(y=BBRKC_MALE_WARM_NS_pct_dif,colour="Warm - NS"),size=1,colour="red",show.legend = TRUE)+
labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male new shell Bristol Bay Red king crab, warm vs. cold")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MALE_COLD_NS_pct_dif,colour = "Cold - NS"),size=1,show.legend = TRUE)+
  geom_line(aes(y=BBRKC_MALE_WARM_NS_pct_dif,colour="Warm - NS"),size=1,show.legend = TRUE)+
  scale_colour_manual(name='',values=c('Cold - NS'='blue','Warm - NS' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male new shell Bristol Bay Red king crab, warm vs. cold")

rkc_ns<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MALE_COLD_NS_pct_dif,color = "Cold"))+
  geom_line(aes(y=BBRKC_MALE_WARM_NS_pct_dif,color="Warm"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male new shell Bristol Bay Red king crab, warm vs. cold")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MALE_COLD_OS_pct_dif,color = "Cold"))+
  geom_line(aes(y=BBRKC_MALE_WARM_OS_pct_dif,color="Warm"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male old shell Bristol Bay Red king crab, warm vs. cold")

rkc_os<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MALE_COLD_OS_pct_dif,color = "Cold"))+
  geom_line(aes(y=BBRKC_MALE_WARM_OS_pct_dif,color="Warm"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male old shell Bristol Bay Red king crab, warm vs. cold")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=BBRKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"),size=1)+
  geom_line(aes(y=BBRKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"),size=1,linetype=2)+
  geom_line(aes(y=BBRKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"),size=1,linetype=2)+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male old shell Bristol Bay Red king crab, warm vs. cold")
############################################################################################
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=BBRKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"),size=1)+
  geom_line(aes(y=BBRKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"),size=1,linetype=3)+
  geom_line(aes(y=BBRKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male new shell Bristol Bay Red king")


################################## Altogether #############################################
rkc_all<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=BBRKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"),size=1)+
  geom_line(aes(y=BBRKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"),size=1,linetype=3)+
  geom_line(aes(y=BBRKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male new shell Bristol Bay Red king")

################################## Abbreviate ###################################################
Size10mm<-pct_dif$Size10[6:20]
BBRKC_MALE_COLD_NS_pct_dif<-pct_dif$BBRKC_MALE_COLD_NS_pct_dif[6:20]
BBRKC_MALE_WARM_NS_pct_dif<-pct_dif$BBRKC_MALE_WARM_NS_pct_dif[6:20]
BBRKC_MALE_COLD_OS_pct_dif<-pct_dif$BBRKC_MALE_COLD_OS_pct_dif[6:20]
BBRKC_MALE_WARM_OS_pct_dif<-pct_dif$BBRKC_MALE_WARM_OS_pct_dif[6:20]
rkc_dat<-as.data.frame(cbind(Size10mm,BBRKC_MALE_COLD_NS_pct_dif,BBRKC_MALE_WARM_NS_pct_dif,BBRKC_MALE_COLD_OS_pct_dif,BBRKC_MALE_WARM_OS_pct_dif))
rkc_dat

ggplot(rkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=BBRKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"),size=1)+
  geom_line(aes(y=BBRKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"),size=1,linetype=3)+
  geom_line(aes(y=BBRKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male new shell Bristol Bay Red king")

rkc_all_short<-ggplot(rkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=BBRKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"),size=1)+
  geom_line(aes(y=BBRKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"),size=1,linetype=3)+
  geom_line(aes(y=BBRKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male new shell Bristol Bay Red king")

#########################################################################################
################################ Females ##################################################
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MATFEM_COLD,color = "Cold"))+
  geom_line(aes(y=BBRKC_MATFEM_WARM,color="Warm"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Weight (kg) ",title="Mature female Bristol Bay Red king crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MATFEM_COLD_pct_dif,color = "Cold year"),size=1)+
  geom_line(aes(y=BBRKC_MATFEM_WARM_pct_dif,color="Warm year"),size=1)+
  scale_colour_manual(name='',values=c('Cold year'='blue','Warm year' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Mature female Bristol Bay Red king crab")

rkc_fem<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MATFEM_COLD_pct_dif,color = "Cold year"),size=1)+
  geom_line(aes(y=BBRKC_MATFEM_WARM_pct_dif,color="Warm year"),size=1)+
  scale_colour_manual(name='',values=c('Cold year'='blue','Warm year' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Mature female Bristol Bay Red king crab")
###################### Abbreviate ############################################################
Size10mm<-pct_dif$Size10[6:18]
BBRKC_MATFEM_COLD_pct_dif<-pct_dif$BBRKC_MATFEM_COLD_pct_dif[6:18]
BBRKC_MATFEM_WARM_pct_dif<-pct_dif$BBRKC_MATFEM_WARM_pct_dif[6:18]

femrkc_dat<-as.data.frame(cbind(Size10mm,BBRKC_MATFEM_COLD_pct_dif,BBRKC_MATFEM_WARM_pct_dif))

ggplot(femrkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MATFEM_COLD_pct_dif,color = "Cold year"),size=1)+
  geom_line(aes(y=BBRKC_MATFEM_WARM_pct_dif,color="Warm year"),size=1)+
  scale_colour_manual(name='',values=c('Cold year'='blue','Warm year' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Mature female Bristol Bay Red king crab")

rkc_fem_short<-ggplot(femrkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_MATFEM_COLD_pct_dif,color = "Cold year"),size=1)+
  geom_line(aes(y=BBRKC_MATFEM_WARM_pct_dif,color="Warm year"),size=1)+
  scale_colour_manual(name='',values=c('Cold year'='blue','Warm year' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Mature female Bristol Bay Red king crab")

#############################################################################################
################################ SMBKC ######################################################
#############################################################################################

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"))+
  geom_line(aes(y=SMBKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew Blue King Crab - bias corrected")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"))+
  geom_line(aes(y=SMBKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew Blue King Crab - bias corrected")

bkc_new<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"))+
  geom_line(aes(y=SMBKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew Blue King Crab - bias corrected")

bkc_old<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"))+
  geom_line(aes(y=SMBKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew Blue King Crab - bias corrected")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=SMBKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"),size=1)+
  geom_line(aes(y=SMBKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"),size=1,linetype=3)+
  geom_line(aes(y=SMBKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"),size = 1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew Blue King Crab")


bkc_all<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=SMBKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"),size=1)+
  geom_line(aes(y=SMBKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"),size=1,linetype=3)+
  geom_line(aes(y=SMBKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"),size = 1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew Blue King Crab")

###################### Abbreviate ##########################################################
Size10mm<-pct_dif$Size10mm[5:20]
SMBKC_MALE_COLD_NS_pct_dif<-pct_dif$SMBKC_MALE_COLD_NS_pct_dif[5:20]
SMBKC_MALE_WARM_NS_pct_dif<-pct_dif$SMBKC_MALE_WARM_NS_pct_dif[5:20]
SMBKC_MALE_COLD_OS_pct_dif<-pct_dif$SMBKC_MALE_COLD_OS_pct_dif[5:20]
SMBKC_MALE_WARM_OS_pct_dif<-pct_dif$SMBKC_MALE_WARM_OS_pct_dif[5:20]

smbkc_dat<-as.data.frame(cbind(Size10mm,SMBKC_MALE_COLD_NS_pct_dif,SMBKC_MALE_WARM_NS_pct_dif,SMBKC_MALE_COLD_OS_pct_dif,SMBKC_MALE_WARM_OS_pct_dif))

ggplot(smbkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=SMBKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"),size=1)+
  geom_line(aes(y=SMBKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"),size=1,linetype=3)+
  geom_line(aes(y=SMBKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"),size = 1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew Blue King Crab")

bkc_all<-ggplot(smbkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_MALE_COLD_NS_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=SMBKC_MALE_WARM_NS_pct_dif,color="New shell - Warm"),size=1)+
  geom_line(aes(y=SMBKC_MALE_COLD_OS_pct_dif,color = "Old shell - Cold"),size=1,linetype=3)+
  geom_line(aes(y=SMBKC_MALE_WARM_OS_pct_dif,color="Old shell - Warm"),size = 1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew Blue King Crab")

#########################################################################################################
################################### BAIRDI ##############################################################

#################################### Immature  Males ####################################################
ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCB_Immatmale_NS_COLD_pct_dif,color = "EBSCB NS immature males - cold"))+
geom_line(aes(y=EBSCB_Immatmale_NS_WARM_pct_dif,color="EBSCB NS immature males - warm"))+
labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea Tanner crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Immatmale_OS_COLD_pct_dif,color = "EBSCB OS immature males - cold"))+
  geom_line(aes(y=EBSCB_Immatmale_OS_WARM_pct_dif,color="EBSCB OS immature males - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea Tanner crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Immatmale_NS_COLD_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=EBSCB_Immatmale_NS_WARM_pct_dif,color="New shell - Warm"),size = 1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype=3)+
  geom_line(aes(y=EBSCB_Immatmale_OS_WARM_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea Bairdi")

cb_immat<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Immatmale_NS_COLD_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=EBSCB_Immatmale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype=3)+
  geom_line(aes(y=EBSCB_Immatmale_OS_WARM_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea Bairdi")

##################################### Abbreviate ####################################################################
Size10mm<-pct_dif$Size10mm[2:14]
EBSCB_Immatmale_NS_COLD_pct_dif<-pct_dif$EBSCB_Immatmale_NS_COLD_pct_dif[2:14]
EBSCB_Immatmale_NS_WARM_pct_dif<-pct_dif$EBSCB_Immatmale_NS_WARM_pct_dif[2:14]
EBSCB_Immatmale_OS_COLD_pct_dif<-pct_dif$EBSCB_Immatmale_OS_COLD_pct_dif[2:14]
EBSCB_Immatmale_OS_WARM_pct_dif<-pct_dif$EBSCB_Immatmale_OS_WARM_pct_dif[2:14]

cb_immatdat<-as.data.frame(cbind(Size10mm,
                                 EBSCB_Immatmale_NS_COLD_pct_dif,
                                 EBSCB_Immatmale_NS_WARM_pct_dif,
                                 EBSCB_Immatmale_OS_COLD_pct_dif,
                                 EBSCB_Immatmale_OS_WARM_pct_dif))

ggplot(cb_immatdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Immatmale_NS_COLD_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=EBSCB_Immatmale_NS_WARM_pct_dif,color="New shell - Warm"),size = 1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype=3)+
  geom_line(aes(y=EBSCB_Immatmale_OS_WARM_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea Bairdi")

cb_immat_short<-ggplot(cb_immatdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Immatmale_NS_COLD_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=EBSCB_Immatmale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCB_Immatmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype=3)+
  geom_line(aes(y=EBSCB_Immatmale_OS_WARM_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male estern Bering Sea Bairdi")


##########################################################################################################
##################################### Mature  Males#########################################################
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Matmale_NS_COLD_pct_dif,color = "EBSCB NS mature males - cold"))+
  geom_line(aes(y=EBSCB_Matmale_NS_WARM_pct_dif,color="EBSCB NS mature males - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea Tanner crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Matmale_OS_COLD_pct_dif,color = "EBSCB OS mature males - cold"))+
  geom_line(aes(y=EBSCB_Matmale_OS_WARM_pct_dif,color="EBSCB OS mature males - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea Tanner crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Matmale_NS_COLD_pct_dif,color = "New shell - Cold"),size =1)+
  geom_line(aes(y=EBSCB_Matmale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCB_Matmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1, linetype=3)+
  geom_line(aes(y=EBSCB_Matmale_OS_WARM_pct_dif,color="Old shell - Warm"),size=1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea Tanner crab")

cb_matmale<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Matmale_NS_COLD_pct_dif,color = "New shell - cold"),size =1)+
  geom_line(aes(y=EBSCB_Matmale_NS_WARM_pct_dif,color="New shell - warm"),size =1)+
  geom_line(aes(y=EBSCB_Matmale_OS_COLD_pct_dif,color = "Old shell - cold"),size = 1, linetype=3)+
  geom_line(aes(y=EBSCB_Matmale_OS_WARM_pct_dif,color="Old shell - warm"),size=1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea Tanner crab")

########################################## Abbreviate ##################################################################################
Size10mm<-pct_dif$Size10mm[5:20]
EBSCB_Matmale_NS_COLD_pct_dif<-pct_dif$EBSCB_Matmale_NS_COLD_pct_dif[5:20]
EBSCB_Matmale_NS_WARM_pct_dif<-pct_dif$EBSCB_Matmale_NS_WARM_pct_dif[5:20]
EBSCB_Matmale_OS_COLD_pct_dif<-pct_dif$EBSCB_Matmale_OS_COLD_pct_dif[5:20]
EBSCB_Matmale_OS_WARM_pct_dif<-pct_dif$EBSCB_Matmale_OS_WARM_pct_dif[5:20]

cb_matdat<-as.data.frame(cbind(Size10mm,
                                 EBSCB_Matmale_NS_COLD_pct_dif,
                                 EBSCB_Matmale_NS_WARM_pct_dif,
                                 EBSCB_Matmale_OS_COLD_pct_dif,
                                 EBSCB_Matmale_OS_WARM_pct_dif))


ggplot(cb_matdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Matmale_NS_COLD_pct_dif,color = "New shell - Cold"),size =1)+
  geom_line(aes(y=EBSCB_Matmale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCB_Matmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1, linetype=3)+
  geom_line(aes(y=EBSCB_Matmale_OS_WARM_pct_dif,color="Old shell - Warm"),size=1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea Tanner crab")

cb_matmale_short<-ggplot(cb_matdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Matmale_NS_COLD_pct_dif,color = "New shell - cold"),size =1)+
  geom_line(aes(y=EBSCB_Matmale_NS_WARM_pct_dif,color="New shell - warm"),size =1)+
  geom_line(aes(y=EBSCB_Matmale_OS_COLD_pct_dif,color = "Old shell - cold"),size = 1, linetype=3)+
  geom_line(aes(y=EBSCB_Matmale_OS_WARM_pct_dif,color="Old shell - warm"),size=1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea Tanner crab")

########################################################################################################################################
######################################### Females#######################################################################################
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_MatFemale_NS_COLD_pct_dif,color = "EBSCB MATFEM NS - cold"))+
  geom_line(aes(y=EBSCB_MatFemale_NS_WARM_pct_dif,color="EBSCB MATFEM NS - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Tanner crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_MatFemale_OS_COLD_pct_dif,color = "EBSCB MATFEM OS - cold"))+
  geom_line(aes(y=EBSCB_MatFemale_OS_WARM_pct_dif,color="EBSCB MATFEM OS - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Tanner crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_MatFemale_NS_COLD_pct_dif,color = "New shell - Cold"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_NS_WARM_pct_dif,color="New shell - Warm"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype = 3)+
  geom_line(aes(y=EBSCB_MatFemale_OS_WARM_pct_dif,color="Old shell - Warm"),size = 1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Tanner crab")

cbfem_mat<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_MatFemale_NS_COLD_pct_dif,color = "New shell - Cold"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_NS_WARM_pct_dif,color="New shell - Warm"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype = 3)+
  geom_line(aes(y=EBSCB_MatFemale_OS_WARM_pct_dif,color="Old shell - Warm"),size = 1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Tanner crab")

################################################### Abbreviate ##################################################
Size10mm<-pct_dif$Size10mm[5:14]
EBSCB_MatFemale_NS_COLD_pct_dif<-pct_dif$EBSCB_MatFemale_NS_COLD_pct_dif[5:14]
EBSCB_MatFemale_NS_WARM_pct_dif<-pct_dif$EBSCB_MatFemale_NS_WARM_pct_dif[5:14]
EBSCB_MatFemale_OS_COLD_pct_dif<-pct_dif$EBSCB_MatFemale_OS_COLD_pct_dif[5:14]
EBSCB_MatFemale_OS_WARM_pct_dif<-pct_dif$EBSCB_MatFemale_OS_WARM_pct_dif[5:14]

cb_matfemdat<-as.data.frame(cbind(Size10mm,
                               EBSCB_MatFemale_NS_COLD_pct_dif,
                               EBSCB_MatFemale_NS_WARM_pct_dif,
                               EBSCB_MatFemale_OS_COLD_pct_dif,
                               EBSCB_MatFemale_OS_WARM_pct_dif))

ggplot(cb_matfemdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_MatFemale_NS_COLD_pct_dif,color = "New shell - Cold"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_NS_WARM_pct_dif,color="New shell - Warm"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype = 3)+
  geom_line(aes(y=EBSCB_MatFemale_OS_WARM_pct_dif,color="Old shell - Warm"),size = 1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Tanner crab")

cbfem_mat_short<-ggplot(cb_matfemdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_MatFemale_NS_COLD_pct_dif,color = "New shell - Cold"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_NS_WARM_pct_dif,color="New shell - Warm"),size = 1)+
  geom_line(aes(y=EBSCB_MatFemale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype = 3)+
  geom_line(aes(y=EBSCB_MatFemale_OS_WARM_pct_dif,color="Old shell - Warm"),size = 1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Tanner crab")

####################################################################################################################################
############################################################# OPILIO#################################################################
# Immature  Males
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Immatmale_NS_COLD_pct_dif,color = "EBSCO NS immature males - cold"))+
  geom_line(aes(y=EBSCO_Immatmale_NS_WARM_pct_dif,color="EBSCO NS immature males - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea snow crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Immatmale_OS_COLD_pct_dif,color = "EBSCO OS immature males - cold"))+
  geom_line(aes(y=EBSCO_Immatmale_OS_WARM_pct_dif,color="EBSCO OS immature males - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea snow crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Immatmale_NS_COLD_pct_dif,color = "New shell - Cold"),size =1)+
  geom_line(aes(y=EBSCO_Immatmale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size =1,linetype = 3)+
  geom_line(aes(y=EBSCO_Immatmale_OS_WARM_pct_dif,color="Old shell - Warm"),size =1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea snow crab")

co_immat<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Immatmale_NS_COLD_pct_dif,color = "New shell - Cold"),size =1)+
  geom_line(aes(y=EBSCO_Immatmale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size =1,linetype = 3)+
  geom_line(aes(y=EBSCO_Immatmale_OS_WARM_pct_dif,color="Old shell - Warm"),size =1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea snow crab")
##################################### Abbreviate ####################################################################
Size10mm<-pct_dif$Size10mm[2:12]
EBSCO_Immatmale_NS_COLD_pct_dif<-pct_dif$EBSCO_Immatmale_NS_COLD_pct_dif[2:12]
EBSCO_Immatmale_NS_WARM_pct_dif<-pct_dif$EBSCO_Immatmale_NS_WARM_pct_dif[2:12]
EBSCO_Immatmale_OS_COLD_pct_dif<-pct_dif$EBSCO_Immatmale_OS_COLD_pct_dif[2:12]
EBSCO_Immatmale_OS_WARM_pct_dif<-pct_dif$EBSCO_Immatmale_OS_WARM_pct_dif[2:12]

co_immatdat<-as.data.frame(cbind(Size10mm,
                                 EBSCO_Immatmale_NS_COLD_pct_dif,
                                 EBSCO_Immatmale_NS_WARM_pct_dif,
                                 EBSCO_Immatmale_OS_COLD_pct_dif,
                                 EBSCO_Immatmale_OS_WARM_pct_dif))

ggplot(co_immatdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Immatmale_NS_COLD_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=EBSCO_Immatmale_NS_WARM_pct_dif,color="New shell - Warm"),size = 1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype=3)+
  geom_line(aes(y=EBSCO_Immatmale_OS_WARM_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea snow crab")

co_immat_short<-ggplot(co_immatdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Immatmale_NS_COLD_pct_dif,color = "New shell - Cold"),size=1)+
  geom_line(aes(y=EBSCO_Immatmale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCO_Immatmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype=3)+
  geom_line(aes(y=EBSCO_Immatmale_OS_WARM_pct_dif,color="Old shell - Warm"),size=1,linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Immature male eastern Bering Sea snow crab")

####################################### Mature  Males ##########################################################################
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Matmale_NS_COLD_pct_dif,color = "EBSCO NS mature males - cold"))+
  geom_line(aes(y=EBSCO_Matmale_NS_WARM_pct_dif,color="EBSCO NS mature males - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea snow crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Matmale_OS_COLD_pct_dif,color = "EBSCO OS mature males - cold"))+
  geom_line(aes(y=EBSCO_Matmale_OS_WARM_pct_dif,color="EBSCO OS mature males - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea snow crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Matmale_NS_COLD_pct_dif,color = "New shell - Cold"),size =1)+
  geom_line(aes(y=EBSCO_Matmale_NS_WARM_pct_dif,color="New shell - Warm"), size = 1)+
  geom_line(aes(y=EBSCO_Matmale_OS_COLD_pct_dif,color = "Old shell - Cold"), size= 1,linetype =3)+
  geom_line(aes(y=EBSCO_Matmale_OS_WARM_pct_dif,color="Old shell - Warm"),size = 1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea snow crab")

co_matmale<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Matmale_NS_COLD_pct_dif,color = "New shell - Cold"),size =1)+
  geom_line(aes(y=EBSCO_Matmale_NS_WARM_pct_dif,color="New shell - Warm"), size = 1)+
  geom_line(aes(y=EBSCO_Matmale_OS_COLD_pct_dif,color = "Old shell - Cold"), size= 1,linetype =3)+
  geom_line(aes(y=EBSCO_Matmale_OS_WARM_pct_dif,color="Old shell - Warm"),size = 1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea snow crab")

########################################## Abbreviate ##################################################################################
Size10mm<-pct_dif$Size10mm[5:20]
EBSCO_Matmale_NS_COLD_pct_dif<-pct_dif$EBSCO_Matmale_NS_COLD_pct_dif[5:20]
EBSCO_Matmale_NS_WARM_pct_dif<-pct_dif$EBSCO_Matmale_NS_WARM_pct_dif[5:20]
EBSCO_Matmale_OS_COLD_pct_dif<-pct_dif$EBSCO_Matmale_OS_COLD_pct_dif[5:20]
EBSCO_Matmale_OS_WARM_pct_dif<-pct_dif$EBSCO_Matmale_OS_WARM_pct_dif[5:20]

co_matdat<-as.data.frame(cbind(Size10mm,
                               EBSCO_Matmale_NS_COLD_pct_dif,
                               EBSCO_Matmale_NS_WARM_pct_dif,
                               EBSCO_Matmale_OS_COLD_pct_dif,
                               EBSCO_Matmale_OS_WARM_pct_dif))


ggplot(co_matdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Matmale_NS_COLD_pct_dif,color = "New shell - Cold"),size =1)+
  geom_line(aes(y=EBSCO_Matmale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCO_Matmale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1, linetype=3)+
  geom_line(aes(y=EBSCO_Matmale_OS_WARM_pct_dif,color="Old shell - Warm"),size=1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea opilio")

co_matmale_short<-ggplot(co_matdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Matmale_NS_COLD_pct_dif,color = "New shell - cold"),size =1)+
  geom_line(aes(y=EBSCO_Matmale_NS_WARM_pct_dif,color="New shell - warm"),size =1)+
  geom_line(aes(y=EBSCO_Matmale_OS_COLD_pct_dif,color = "Old shell - cold"),size = 1, linetype=3)+
  geom_line(aes(y=EBSCO_Matmale_OS_WARM_pct_dif,color="Old shell - warm"),size=1, linetype=3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Mature male eastern Bering Sea snow crab")

#################################### Females#################################################################################
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_MatFemale_NS_COLD_pct_dif,color = "EBSCO MATFEM NS - cold"))+
  geom_line(aes(y=EBSCO_MatFemale_NS_WARM_pct_dif,color="EBSCO MATFEM NS - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea mature female snow crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_MatFemale_OS_COLD_pct_dif,color = "EBSCO MATFEM OS - cold"))+
  geom_line(aes(y=EBSCO_MatFemale_OS_WARM_pct_dif,color="EBSCO MATFEM OS - warm"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea mature female snow crab")

ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_MatFemale_NS_COLD_pct_dif,color = "New shell - Cold"),size =1)+
  geom_line(aes(y=EBSCO_MatFemale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_COLD_pct_dif,color = "Old shell - Cold"),size=1,linetype = 3)+
  geom_line(aes(y=EBSCO_MatFemale_OS_WARM_pct_dif,color="Old shell - Warm"),size =1, linetype =3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea mature female snow crab")

cofem_mat<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_MatFemale_NS_COLD_pct_dif,color = "New shell - Cold"),size =1)+
  geom_line(aes(y=EBSCO_MatFemale_NS_WARM_pct_dif,color="New shell - Warm"),size =1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_COLD_pct_dif,color = "Old shell - Cold"),size=1,linetype = 3)+
  geom_line(aes(y=EBSCO_MatFemale_OS_WARM_pct_dif,color="Old shell - Warm"),size =1, linetype =3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea mature female snow crab")
################################################### Abbreviate ##################################################
Size10mm<-pct_dif$Size10mm[4:10]
EBSCO_MatFemale_NS_COLD_pct_dif<-pct_dif$EBSCO_MatFemale_NS_COLD_pct_dif[4:10]
EBSCO_MatFemale_NS_WARM_pct_dif<-pct_dif$EBSCO_MatFemale_NS_WARM_pct_dif[4:10]
EBSCO_MatFemale_OS_COLD_pct_dif<-pct_dif$EBSCO_MatFemale_OS_COLD_pct_dif[4:10]
EBSCO_MatFemale_OS_WARM_pct_dif<-pct_dif$EBSCO_MatFemale_OS_WARM_pct_dif[4:10]

co_matfemdat<-as.data.frame(cbind(Size10mm,
                                  EBSCO_MatFemale_NS_COLD_pct_dif,
                                  EBSCO_MatFemale_NS_WARM_pct_dif,
                                  EBSCO_MatFemale_OS_COLD_pct_dif,
                                  EBSCO_MatFemale_OS_WARM_pct_dif))

ggplot(co_matfemdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_MatFemale_NS_COLD_pct_dif,color = "New shell - Cold"),size = 1)+
  geom_line(aes(y=EBSCO_MatFemale_NS_WARM_pct_dif,color="New shell - Warm"),size = 1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype = 3)+
  geom_line(aes(y=EBSCO_MatFemale_OS_WARM_pct_dif,color="Old shell - Warm"),size = 1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea mature female snow crab")

cofem_mat_short<-ggplot(co_matfemdat,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_MatFemale_NS_COLD_pct_dif,color = "New shell - Cold"),size = 1)+
  geom_line(aes(y=EBSCO_MatFemale_NS_WARM_pct_dif,color="New shell - Warm"),size = 1)+
  geom_line(aes(y=EBSCO_MatFemale_OS_COLD_pct_dif,color = "Old shell - Cold"),size = 1,linetype = 3)+
  geom_line(aes(y=EBSCO_MatFemale_OS_WARM_pct_dif,color="Old shell - Warm"),size = 1,linetype = 3)+
  scale_colour_manual(name='',values=c('New shell - Cold'='blue','New shell - Warm' = 'red','Old shell - Cold'='blue','Old shell - Warm' = 'red'))+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea mature female snow crab")


####################################################################################################################
########################## NBS BKC #####################################################################################
Size10mm<-pct_dif$Size10[3:15]
NBSBKC_Male_NS_pct_dif<-pct_dif$NBSBKC_Male_NS_pct_dif[3:15]
NBSBKC_Male_OS_pct_dif<-pct_dif$NBSBKC_Male_OS_pct_dif[3:15]
NBSBKC_Male_AllSC_pct_dif<-pct_dif$NBSBKC_Male_AllSC_pct_dif[3:15]

nbsbkc_dat<-as.data.frame(cbind(Size10mm,NBSBKC_Male_AllSC_pct_dif,NBSBKC_Male_NS_pct_dif,NBSBKC_Male_OS_pct_dif))

ggplot(nbsbkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=NBSBKC_Male_AllSC_pct_dif,color = "EBS-NBS ML Null model"),size = 1,linetype = 2)+
  geom_line(aes(y=NBSBKC_Male_NS_pct_dif,color = "New Shell (SC2)"),size = 1)+
  geom_line(aes(y=NBSBKC_Male_OS_pct_dif,color = "Old Shell (SC3+)"),size = 1)+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male northern Bering Sea blue king crab")

nbs_bkc<-ggplot(nbsbkc_dat, aes(x=Size10mm))+
 geom_line(aes(y=NBSBKC_Male_AllSC_pct_dif,color = "EBS-NBS ML Null model"),size = 1,linetype = 2)+
 geom_line(aes(y=NBSBKC_Male_NS_pct_dif,color = "New Shell (SC2)"),size = 1)+
 geom_line(aes(y=NBSBKC_Male_OS_pct_dif,color = "Old Shell (SC3+)"),size = 1)+
 labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male northern Bering Sea blue king crab")

####################################################################################################################
########################## NBS RKC #####################################################################################
Size10mm<-pct_dif$Size10[5:15]
NSRKC_Male_NS_pct_dif<-pct_dif$NSRKC_Male_NS_pct_dif[5:15]
NSRKC_Male_OS_pct_dif<-pct_dif$NSRKC_Male_OS_pct_dif[5:15]
NSRKC_Male_AllSC_pct_dif<-pct_dif$NSRKC_Male_AllSC_pct_dif[5:15]

nbsrkc_dat<-as.data.frame(cbind(Size10mm,NSRKC_Male_AllSC_pct_dif,NSRKC_Male_NS_pct_dif,NSRKC_Male_OS_pct_dif))

ggplot(nbsrkc_dat,aes(x=Size10mm))+
  geom_line(aes(y=NSRKC_Male_AllSC_pct_dif,color = "EBS-NBS ML Null model"),size = 1,linetype = 2)+
  geom_line(aes(y=NSRKC_Male_NS_pct_dif,color = "New Shell (SC2)"),size = 1)+
  geom_line(aes(y=NSRKC_Male_OS_pct_dif,color = "Old Shell (SC3+)"),size = 1)+
 
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male Norton Sound red king crab ")

nsrkc<-ggplot(nbsrkc_dat,aes(x=Size10mm))+
 geom_line(aes(y=NSRKC_Male_AllSC_pct_dif,color = "EBS-NBS ML Null model"),size = 1,linetype = 2)+
 geom_line(aes(y=NSRKC_Male_NS_pct_dif,color = "New Shell (SC2)"),size = 1)+
 geom_line(aes(y=NSRKC_Male_OS_pct_dif,color = "Old Shell (SC3+)"),size = 1)+
 labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male Norton Sound red king crab ")



####################################################################################################################
########################## NBSCO #####################################################################################
Size10mm<-pct_dif$Size10[4:12]
NBSCO_Male_All_pct_dif<-pct_dif$NBSCO_Male_All_pct_dif[4:12]
NBSCO_Male_NS_pct_dif<-pct_dif$NBSCO_Male_NS_pct_dif[4:12]
NBSCO_Male_OS_pct_dif<-pct_dif$NBSCO_Male_OS_pct_dif[4:12]

nbsco_dat<-as.data.frame(cbind(Size10mm,NBSCO_Male_All_pct_dif,NBSCO_Male_NS_pct_dif,NBSCO_Male_OS_pct_dif))

ggplot(nbsco_dat,aes(x=Size10mm))+
  geom_line(aes(y=NBSCO_Male_All_pct_dif,color = "EBS-NBS ML Null model"),size = 1,linetype = 2)+
  geom_line(aes(y=NBSCO_Male_NS_pct_dif,color = "New Shell (SC2)"),size = 1)+
  geom_line(aes(y=NBSCO_Male_OS_pct_dif,color = "Old Shell (SC3)"),size = 1)+
  labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Male Northern Bering Sea snow crab")+
  guides(colour = guide_legend(override.aes = list(linetype=c(1,2,1))))


nbs_co<-ggplot(nbsco_dat,aes(x=Size10mm))+
  geom_line(aes(y=NBSCO_Male_All_pct_dif,color = "EBS-NBS ML Null model"),size = 1,linetype = 2)+
  geom_line(aes(y=NBSCO_Male_NS_pct_dif,color = "New Shell (SC2)"),size = 1)+
  geom_line(aes(y=NBSCO_Male_OS_pct_dif,color = "Old Shell (SC3)"),size = 1)+
  labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Male northern Bering Sea snow crab")
  #guides(colour = guide_legend(override.aes = list(linetype=c(1,2,1))))


##############  panel ###########################################################

########################################### BBRKC + SMBKC
dev.new()

ggarrange(bbrkc_all_short,smbkc_all_short,
          labels = c("a.)", "b.)"),
          ncol = 1, nrow = 2)

##### ########################### male Bairdi and opilio

dev.new()

ggarrange(cb_immat_short,cb_matmale_short,co_immat_short,co_matmale_short,
          labels = c("a.)", "b.)","c.)", "d.)"),
          ncol = 2, nrow = 2)

################################## Female RKC, bairdi and opilio
dev.new()

ggarrange(rkc_fem_short,cbfem_mat_short,cofem_mat_short,
labels = c("a.)", "b.)", "c.)"),
ncol = 2, nrow = 2)


################################# NBS ###############################################

dev.new()

ggarrange(nsrkc,nbs_bkc, nbs_co,
          labels = c("a.)", "b.)", "c.)"),
         ncol = 2, nrow = 2)

