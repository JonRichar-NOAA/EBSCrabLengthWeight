# Jon Richar 
# 6/16/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggplot2)
setwd("C:/Users/Jon.Richar/Work/Projects/Length_weight/DATA/Biomass_DATA")

pct_dif<-read.csv("Calc_weight_differences.csv")

names(pct_dif)
######################### Create data objects ###################
size<-pct_dif$Size10mm

rkc_ns_dif<-pct_dif$BBRKC_NS_pct_dif
mean(rkc_ns_dif)

rkc_os_dif<-pct_dif$BBRKC_OS_pct_dif
mean(rkc_os_dif)

cb_ns_dif<-pct_dif$EBSCB_NS_pct_dif
mean(cb_ns_dif)
cb_os_dif<-pct_dif$EBSCB_OS_pct_dif
mean(cb_os_dif)

co_ns_dif<-pct_dif$EBSCO_NS_pct_dif
mean(co_ns_dif)
co_os_dif<-pct_dif$EBSCO_OS_pct_dif
mean(co_os_dif)
########################## Plot #######################################

ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=BBRKC_NS_pct_dif,color = "BBRKC NS"))+
geom_line(aes(y=BBRKC_OS_pct_dif,color="BBRKC OS"))+
labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Bristol Bay Red king crab")

rkc<-ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=BBRKC_NS_pct_dif,color = "BBRKC NS"))+
geom_line(aes(y=BBRKC_OS_pct_dif,color="BBRKC OS"))+
labs(color = "Category",x = "Carapace length (mm)", y = "Percent difference",title="Bristol Bay Red king crab")

ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCB_NS_pct_dif,color = "EBSCB NS"))+
geom_line(aes(y=EBSCB_OS_pct_dif,color="EBSCB OS"))+
labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Bairdi crab")

cb<-ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCB_NS_pct_dif,color = "EBSCB NS"))+
geom_line(aes(y=EBSCB_OS_pct_dif,color="EBSCB OS"))+
labs(color = "Category",x = "Carapace width(mm)", y = "Percent difference",title="Eastern Bering Sea Bairdi crab")

ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCO_NS_pct_dif,color = "EBSCO NS"))+
geom_line(aes(y=EBSCO_OS_pct_dif,color="EBSCO OS"))+
labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Eastern Bering Sea Opilio crab")

co<-ggplot(pct_dif,aes(x=Size10mm))+
geom_line(aes(y=EBSCO_NS_pct_dif,color = "EBSCO NS"))+
geom_line(aes(y=EBSCO_OS_pct_dif,color="EBSCO OS"))+
labs(color = "Category",x = "Carapace width (mm)", y = "Percent difference",title="Eastern Bering Sea Opilio crab")
############## 3 panel ###########################################################


ggarrange(rkc, cb,co,
labels = c("a.)", "b.)", "c.)"),
ncol = 1, nrow = 3)
