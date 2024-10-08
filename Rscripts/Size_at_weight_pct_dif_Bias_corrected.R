rm(list = ls())
# Jon Richar 
# 6/16/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggplot2)
setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA/Biomass_DATA")

pct_dif<-read.csv("Calc_weight_differences.csv")

names(pct_dif)
######################### Create data objects ###################
size<-pct_dif$Size10mm

rkc_ns_dif<-pct_dif$BBRKC_NS_BC_pct_dif
mean(rkc_ns_dif)

rkc_os_dif<-pct_dif$BBRKC_OS_BC_BC_pct_dif
mean(rkc_os_dif)

cb_ns_dif<-pct_dif$EBSCB_NS_BC_pct_dif
mean(cb_ns_dif)

cb_ns_matfem_dif<-pct_dif$EBSCB_MATFEM_NS_BC_pct_dif #old
mean(cb_ns_matfem_dif)

cb_ns_allmatfem_dif<-pct_dif$EBSCB_AllMatfem_NS_pct_dif
cb_ns_cs4_matfem_dif<-pct_dif$EBSCB_Female_NS_CS4_pct_dif

cb_ns_cs6_matfem_dif<-pct_dif$EBSCB_Female_NS_CS6_pct_dif

cb_os_cs4_matfem_dif<-pct_dif$EBSCB_Female_OS_CS4_pct_dif

cb_os_cs6_matfem_dif<-pct_dif$EBSCB_Female_OS_CS6_pct_dif

cb_ns_clutch_cold_dif<-pct_dif$EBSCB_Female_w_clutch_NS_Cold_pct_dif

cb_ns_clutch_warm_dif<-pct_dif$EBSCB_Female_w_clutch_NS_Warm_pct_dif

cb_os_dif<-pct_dif$EBSCB_OS_BC_pct_dif
mean(cb_os_dif)

cb_os_matfem_dif<-pct_dif$EBSCB_MATFEM_OS_BC_pct_dif #old
mean(cb_os_matfem_dif)

co_ns_dif<-pct_dif$EBSCO_NS_BC_pct_dif
mean(co_ns_dif)

co_ns_matfem_dif<-pct_dif$EBSCO_MATFEM_NS_BIAS_CORRECTED
mean(co_ns_matfem_dif)

co_os_dif<-pct_dif$EBSCO_OS_BC_pct_dif
mean(co_os_dif)

co_os_matfem_dif<-pct_dif$EBSCO_MATFEM_OS_BIAS_CORRECTED
mean(co_os_matfem_dif)
########################## Plot #######################################

ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=BBRKC_NS_BC_pct_dif,color = "New Shell"))+
geom_line(aes(y=BBRKC_OS_BC_pct_dif,color="Old Shell"))+
labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Male Bristol Bay Red king crab")

rkc<-ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=BBRKC_NS_BC_pct_dif,color = "New Shell (SC2)"))+
geom_line(aes(y=BBRKC_OS_BC_pct_dif,color="Old Shell (SC3+)"))+
labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male Bristol Bay Red King Crab ")


ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_NS_BC_pct_dif,color = "SMBKC NS"))+
  geom_line(aes(y=SMBKC_OS_BC_pct_dif,color="SMBKKC OS"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew Blue King Crab - bias corrected")


bkc<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_NS_BC_pct_dif,color = "New Shell (SC2)"))+
  geom_line(aes(y=SMBKC_OS_BC_pct_dif,color="Old Shell (SC3+)"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male St. Matthew Blue King Crab")

bbrkc_temp<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_Warm_pct_dif,color = "Warm"))+
  geom_line(aes(y=BBRKC_Cold_pct_dif,color = "Cold"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Male BBRKC - temperature")+
  xlim(0, 200)

bbrkcfem_temp<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=BBRKC_Female_w_clutch_NS_Warm_pct_dif,color = "Warm"))+
  geom_line(aes(y=BBRKC_Female_w_clutch_NS_Cold_pct_dif,color = "Cold"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="BBRKC clutch bearing Females - temperature")+
  xlim(80, 160)+ylim(-10, 10)

#BAIRDI 
# Males
ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCB_NS_BC_pct_dif,color = "EBSCB NS"))+
geom_line(aes(y=EBSCB_OS_BC_pct_dif,color="EBSCB OS"))+
labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Tanner crab - bias corrected")

cb<-ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCB_NS_BC_pct_dif,color = "New Shell (SC2)"))+
geom_line(aes(y=EBSCB_OS_BC_pct_dif,color="Old Shell (SC3+)"))+
labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Male Eastern Bering Sea Bairdi")

cb_temp<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Warm_pct_dif,color = "Warm"))+
  geom_line(aes(y=EBSCB_Cold_pct_dif,color = "Cold"))+
  labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Male Eastern Bering Sea Bairdi - temperature")

# Females
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_NS_MATFEM_BC_pct_dif,color = "EBSCB NS"))+
  geom_line(aes(y=EBSCB_OS_MATFEM_BC_pct_dif,color="EBSCB OS"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Tanner crab - bias corrected")

cbfem<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_NS_MATFEM_BC_pct_dif,color = "New shell (SC2)"))+
  geom_line(aes(y=EBSCB_OS_MATFEM_BC_pct_dif,color="Old shell (SC3+)"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea mature female Tanner crab - bias corrected")

cbfem_mat<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_AllMatfem_NS_pct_dif,color = "New shell (SC2)"),size =1)+
  geom_line(aes(y=EBSCB_AllMatfem_OS_pct_dif,color="Old shell (SC3+)"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Bairdi crab ")+
  xlim(40, 110)


cbfem_temp<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Female_w_clutch_NS_Warm_pct_dif,color = "Warm"),size =1)+
  geom_line(aes(y=EBSCB_Female_w_clutch_NS_Cold_pct_dif,color="Cold"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Clutch-bearing Female Bairdi - Temperature")+
  xlim(40, 110)



cbfem_cs4<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Female_NS_CS4_pct_dif,color = "New shell (SC2)"),size =1)+
  geom_line(aes(y=EBSCB_Female_OS_CS4_pct_dif,color="Old shell (SC3+)"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea clutch size 4 (1/2 full) female Tanner crab")+
  xlim(40, 110)


cbfem_cs6<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Female_NS_CS6_pct_dif,color = "New shell (SC2)"),size =1)+
  geom_line(aes(y=EBSCB_Female_OS_CS6_pct_dif,color="Old shell (SC3+)"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea clutch size 6 (full) female Tanner crab")+
  xlim(40, 110)
cbfem_NS_CS4cs6<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Female_NS_CS4_pct_dif,color = "CS4-1/2 full"),size =1)+
  geom_line(aes(y=EBSCB_Female_NS_CS6_pct_dif,color = "CS6-Full"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea new shell clutch size 4 and 6 female Tanner crab")+
  xlim(40, 110)


cbfem_OS_CS4cs6<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_Female_OS_CS4_pct_dif,color = "CS4-1/2 full"),size =1)+
  geom_line(aes(y=EBSCB_Female_OS_CS6_pct_dif,color = "CS6-Full"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea old shell clutch Size 4 and 6 female Tanner crab")+
  xlim(40, 110)

#OPILIO
#Males
ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCO_NS_BC_pct_dif,color = "EBSCO NS"))+
geom_line(aes(y=EBSCO_OS_BC_pct_dif,color="EBSCO OS"))+
labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Eastern Bering Sea Opilio crab - bias corrected")

co<-ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCO_NS_BC_pct_dif,color = "New Shell (SC2)"))+
geom_line(aes(y=EBSCO_OS_BC_pct_dif,color="Old Shell (SC3+)"))+
labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Male Eastern Bering Sea Opilio crab")

co_temp<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Warm_pct_dif,color = "Warm"))+
  geom_line(aes(y=EBSCO_Cold_pct_dif,color = "Cold"))+
  labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Male Eastern Bering Sea Opilio  - temperature")

#Females
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_AllMatfem_NS_pct_dif,color = "EBSCO NS"),size =1)+
  geom_line(aes(y=EBSCO_AllMatfem_OS_pct_dif,color="EBSCO OS"),size =1)+
  labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Opilio")

cofem<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_NS_BC_pct_dif,color = "EBSCO NS"))+
  geom_line(aes(y=EBSCO_OS_BC_pct_dif,color="EBSCO OS"))+
  labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Eastern Bering Sea mature female Opilio crab - bias corrected")

cofem_mat<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_AllMatfem_NS_pct_dif,color = "New shell (SC2)"),size =1)+
  geom_line(aes(y=EBSCO_AllMatfem_OS_pct_dif,color="Old shell (SC3+)"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Mature Female Opilio")+
  xlim(30, 80)


cofem_temp<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Female_w_clutch_NS_Warm_pct_dif ,color = "Warm"),size =1)+
  geom_line(aes(y=EBSCO_Female_w_clutch_NS_Cold_pct_dif,color="Cold"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Clutch bearing Female Opilio - temperature")+
  xlim(30, 80)


cofem_cs4<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Female_NS_CS4_pct_dif,color = "New shell (SC2)"),size =1)+
  geom_line(aes(y=EBSCO_Female_OS_CS4_pct_dif,color="Old shell (SC3+)"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea clutch Size 4 (1/2 full) female snow crab")+
  xlim(30, 80)

cofem_cs6<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Female_NS_CS6_pct_dif,color = "New shell (SC2)"),size =1)+
  geom_line(aes(y=EBSCO_Female_OS_CS6_pct_dif,color="Old shell (SC3+)"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea clutch Size 6 (full) female snow crab")+
  xlim(30, 80)

cofem_NS_CS4cs6<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Female_NS_CS4_pct_dif,color = "CS4-1/2 full"),size =1)+
  geom_line(aes(y=EBSCO_Female_NS_CS6_pct_dif,color = "CS6-Full"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea new shell clutch Size 4 and 6 female snow crab")+
  xlim(30, 80)


cofem_OS_CS4cs6<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_Female_OS_CS4_pct_dif,color = "CS4-1/2 full"),size =1)+
  geom_line(aes(y=EBSCO_Female_OS_CS6_pct_dif,color = "CS6-Full"),size =1)+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea old shell clutch Size 4 and 6 female snow crab")+
  xlim(30, 80)


####################################################################################################################
########################## NBS #####################################################################################

nbs_bkc<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=NBS_BKC_Male_NS_pct_dif,color = "New Shell (SC2)"))+
  geom_line(aes(y=NBS_BKC_Male_OS_pct_dif,color = "Old Shell (SC3+)"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male Northern Bering Sea Blue King Crab")

nsrkc<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=NS_RKC_Male_NS_pct_dif,color = "New Shell (SC2)"))+
  geom_line(aes(y=NS_RKC_Male_OS_pct_dif,color = "Old Shell (SC3+)"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Male Norton Sound Red King Crab ")

nbs_co<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=NBS_CO_Male_All_pct_dif,color = "All Males"))+
  labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Male Northern Bering Sea Opilio")+
  xlim(0, 110)
############## 3 panel ###########################################################
dev.new()

ggarrange(rkc, bkc, cb, co,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)


######################################## Temperature and clutch size models 
########################################### BBRKC
dev.new()

ggarrange(bbrkc_temp,bbrkcfem_temp,
          labels = c("a.)", "b.)"),
          ncol = 1, nrow = 2)

##### ########################### male Bairdi and opilio

dev.new()

ggarrange(cb_temp,co_temp,
          labels = c("a.)", "b.)"),
          ncol = 1, nrow = 2)
#################################### BBRKC and Chionoecetes together
dev.new()

ggarrange(bbrkc_temp,bbrkcfem_temp,cb_temp,co_temp,
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)

################################## Female bairdi
dev.new()

ggarrange(cbfem_mat,cbfem_temp,cbfem_cs4,cbfem_cs6,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

dev.new()
ggarrange(cbfem_mat,cbfem_temp,cbfem_cs4,cbfem_cs6,cbfem_NS_CS4cs6,cbfem_OS_CS4cs6,
          labels = c("a.)", "b.)", "c.)", "d.)", "e.)", "f.)"),
          ncol = 2, nrow = 3)
############################################################################

ggarrange(cbfem_cs4,cbfem_cs6,cbfem_NS_CS4cs6,cbfem_OS_CS4cs6,
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)
################################## Female opilio 


dev.new()

ggarrange(cofem_mat,cofem_temp,cofem_cs4,cofem_cs6,
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)
dev.new()
ggarrange(cofem_mat,cofem_temp,cofem_cs4,cofem_cs6,cofem_NS_CS4cs6,cofem_OS_CS4cs6,
          labels = c("a.)", "b.)", "c.)", "d.)", "e.)", "f.)"),
          ncol = 2, nrow = 3)

ggarrange(cbfem_mat,cofem,
          labels = c("a.)", "b.)"),
          ncol = 1, nrow = 2)
#####################################################################################

dev.new()
ggarrange(cofem_cs4,cofem_cs6,cofem_NS_CS4cs6,cofem_OS_CS4cs6,
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)

ggarrange(cbfem_mat,cofem,
          labels = c("a.)", "b.)"),
          ncol = 1, nrow = 2)

################################# NBS ###############################################

dev.new()
ggarrange(nsrkc,nbs_bkc, nbs_co,
          labels = c("a.)", "b.)", "c.)"),
          ncol = 2, nrow = 2)

