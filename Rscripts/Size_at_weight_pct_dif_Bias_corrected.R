# Jon Richar 
# 6/16/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggplot2)
setwd("C:/Users/Jon.Richar/Work/GitRepos/LengthWeight/EBSCrabLengthWeight/DATA/Biomass_DATA")

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

cb_ns_matfem_dif<-pct_dif$EBSCB_MATFEM_NS_BC_pct_dif
mean(cb_ns_matfem_dif)

cb_os_dif<-pct_dif$EBSCB_OS_BC_pct_dif
mean(cb_os_dif)

cb_os_matfem_dif<-pct_dif$EBSCB_MATFEM_OS_BC_pct_dif
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
geom_line(aes(y=BBRKC_NS_BC_pct_dif,color = "BBRKC NS"))+
geom_line(aes(y=BBRKC_OS_BC_pct_dif,color="BBRKC OS"))+
labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference ",title="Bristol Bay Red king crab - bias corrected")

rkc<-ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=BBRKC_NS_BC_pct_dif,color = "BBRKC NS"))+
geom_line(aes(y=BBRKC_OS_BC_pct_dif,color="BBRKC OS"))+
labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Bristol Bay Red king crab - bias corrected")


ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_NS_BC_pct_dif,color = "SMBKC NS"))+
  geom_line(aes(y=SMBKC_OS_BC_pct_dif,color="SMBKKC OS"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew blue king crab - bias corrected")


bkc<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=SMBKC_NS_BC_pct_dif,color = "SMBKC NS"))+
  geom_line(aes(y=SMBKC_OS_BC_pct_dif,color="SMBKKC OS"))+
  labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="St. Matthew blue king crab - bias corrected")

#BAIRDI 
# Males
ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCB_NS_BC_pct_dif,color = "EBSCB NS"))+
geom_line(aes(y=EBSCB_OS_BC_pct_dif,color="EBSCB OS"))+
labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Bairdi crab - bias corrected")

cb<-ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCB_NS_BC_pct_dif,color = "EBSCB NS"))+
geom_line(aes(y=EBSCB_OS_BC_pct_dif,color="EBSCB OS"))+
labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Bairdi crab - bias corrected")

# Females
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_NS_MATFEM_BC_pct_dif,color = "EBSCB NS"))+
  geom_line(aes(y=EBSCB_OS_MATFEM_BC_pct_dif,color="EBSCB OS"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea mature female Bairdi crab - bias corrected")

cbfem<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCB_NS_MATFEM_BC_pct_dif,color = "EBSCB NS"))+
  geom_line(aes(y=EBSCB_OS_MATFEM_BC_pct_dif,color="EBSCB OS"))+
  labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea mature female Bairdi crab - bias corrected")

#OPILIO
#Males
ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCO_NS_BC_pct_dif,color = "EBSCO NS"))+
geom_line(aes(y=EBSCO_OS_BC_pct_dif,color="EBSCO OS"))+
labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Eastern Bering Sea Opilio crab - bias corrected")

co<-ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCO_NS_BC_pct_dif,color = "EBSCO NS"))+
geom_line(aes(y=EBSCO_OS_BC_pct_dif,color="EBSCO OS"))+
labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Eastern Bering Sea Opilio crab - bias corrected")

#Females
ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_MATFEM_NS_BC_pct_dif,color = "EBSCO NS"))+
  geom_line(aes(y=EBSCO_MATFEM_OS_BC_pct_dif,color="EBSCO OS"))+
  labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Eastern Bering Sea mature female Opilio crab - bias corrected")

cofem<-ggplot(pct_dif,aes(x=Size10mm))+
  geom_line(aes(y=EBSCO_NS_BC_pct_dif,color = "EBSCO NS"))+
  geom_line(aes(y=EBSCO_OS_BC_pct_dif,color="EBSCO OS"))+
  labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Eastern Bering Sea mature female Opilio crab - bias corrected")

############## 3 panel ###########################################################
dev.new()

ggarrange(rkc, bkc, cb,co,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

dev.new()

ggarrange(cbfem,cofem,
labels = c("a.)", "b.)"),
ncol = 1, nrow = 2)
