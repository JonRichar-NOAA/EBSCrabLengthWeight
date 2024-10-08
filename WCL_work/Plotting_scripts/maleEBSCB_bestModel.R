library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(readxl)


Male_EBSCB_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/data/Male_EBSCB_weights_temps_analysis.csv")

Male_EBSCB_weights_and_temps
MEBSCB=Male_EBSCB_weights_and_temps
MEBSCB%>% filter(Maturity=="Mature"|Maturity=="Immature")->MEBSCB_mat

ns_immales_analysis<-subset(MEBSCB_mat, Maturity=="Immature" & SEX==1 & SHELL_CONDITION==2)

os_immales_analysis<-subset(MEBSCB_mat, Maturity=="Immature" & SEX==1 & (SHELL_CONDITION==3|SHELL_CONDITION==4))


ns_matmales_analysis<-subset(MEBSCB_mat, Maturity=="Mature" & SEX==1 & SHELL_CONDITION==2)
os_matmales_analysis<-subset(MEBSCB_mat, Maturity=="Mature" & SEX==1 &  (SHELL_CONDITION==3|SHELL_CONDITION==4))

max(MEBSCB_mat$Temperature) #5.13578 in 2019
min(MEBSCB_mat$Temperature) #0.9980699 in 2012
#view(MBBRKC)
###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ##############################
#dev.new()
par(mfrow =c(2,2))

##########################################################################################################################



################################## New shell immature males ######################################################################
#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~WIDTH,data=ns_immales_analysis, main = "New shell immature male EBS Tanner crab",ylab= "Weight (g)", xlab= "Width(mm)")


x<-ns_immales_analysis$WIDTH

f<-function(x) {0.000339 * x^(2.954046)} #cold ns immature males
g<-function(x) {0.000348 * x^(2.954046)} # warm ns immature males
h<-function(x) {0.00027 * x^(3.022134)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline

legend(19,675, c("Baseline L-W model", "SC/maturity/temp L-W model-cold", "SC/maturity/temp L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell mature males #########################################################################

plot(WEIGHT~WIDTH,data=os_immales_analysis, main = "Old shell immature male EBS Tanner crab",ylab= "Weight (g)", xlab= "Width(mm)")

#view(os_immales_analysis)
x<-os_immales_analysis$WIDTH

f<-function(x) {0.000220 * x^(3.053140)} #cold os immature males
g<-function(x) {0.000228 * x^(3.0531407)} #warm os immature males
h<-function(x) {0.00027 * x^(3.022134)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(36,1120, c("Baseline L-W model", "SC/maturity/temp L-W model-cold","SC/maturity/temp L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))

###################################################################################################################
#################### Mature makes #####################################################

################################## New shell immature males ######################################################################
#dev.new()
#par(mfrow =c(2,1))
ns_matmales_analysis%>%
  filter(WIDTH>50)->ns_matmales_analysis
plot(WEIGHT~WIDTH,data=ns_matmales_analysis, main = "New shell mature male EBS Tanner crab",ylab= "Weight (g)", xlab= "Width(mm)")


x<-ns_matmales_analysis$WIDTH

f<-function(x) {0.000339 * x^(2.977018)} #cold ns mature males
g<-function(x) {0.000348 * x^(2.977018)} # warm ns mature males
h<-function(x) {0.00027 * x^(3.022134)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline

legend(48,1592, c("Baseline L-W model", "SC/maturity/temp L-W model-cold", "SC/maturity/temp L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell mature males #########################################################################
os_matmales_analysis %>%
  filter(WIDTH>50)->os_matmales_analysis
plot(WEIGHT~WIDTH,data=os_matmales_analysis, main = "Old shell mature male EBS Tanner crab",ylab= "Weight (g)", xlab= "Width(mm)")

#view(os_matmales_analysis)
x<-os_matmales_analysis$WIDTH

f<-function(x) {0.000220 * x^(3.076112)} #cold os mature males
g<-function(x) {0.000228 * x^(3.076112)} #warm os mature males
h<-function(x) {0.00027 * x^(3.022134)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(61,1920, c("Baseline L-W model", "SC/maturity/temp L-W model-cold","SC/maturity/temp L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))

