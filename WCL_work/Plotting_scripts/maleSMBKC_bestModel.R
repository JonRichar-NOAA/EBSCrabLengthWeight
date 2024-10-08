library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(readxl)


Male_SMBKC_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/SMBKC/data/Male_SMBKC_weights_temps_analysis.csv")
Male_SMBKC_weights_and_temps
MSMBKC=Male_SMBKC_weights_and_temps

ns_males_analysis<-subset(MSMBKC,SEX==1 & SHELL_CONDITION==2)
os_males_analysis<-subset(MSMBKC,SEX==1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

max(MSMBKC$Temperature) #5.13578 in 2019
min(MSMBKC$Temperature) #0.9980699 in 2012
#view(MBBRKC)
###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ##############################
#dev.new()
par(mfrow =c(1,2))


################################## New shell mature males ######################################################################

plot(WEIGHT~LENGTH,data=ns_males_analysis, main = "New shell male SMBKC",ylab= "Weight (g)", xlab= "LENGTH (mm)")


x<-ns_males_analysis$LENGTH

f<-function(x) {0.000250928 * x^(3.243778433)} #cold ns males
g<-function(x) {0.0003552 * x^(3.176822288)} # warm ns males
h<-function(x) {0.000502 * x^(3.107158)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline

legend(49.5,3570, c("Baseline L-W model", "SC/temp L-W model-cold", "SC/temp based L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell mature males #########################################################################

plot(WEIGHT~LENGTH,data=os_males_analysis, main = "Old shell male SMBKC",ylab= "Weight (g)", xlab= "LENGTH (mm)")


x<-os_males_analysis$LENGTH

f<-function(x) {0.000445456 * x^(3.138516893)} #cold os males
g<-function(x) {0.000900339 * x^(2.98485404)} #warm os males
h<-function(x) {0.000502 * x^(3.107158)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(43,3325, c("Baseline L-W model", "SC/temp L-W model-cold","SC/temp based L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))

#
