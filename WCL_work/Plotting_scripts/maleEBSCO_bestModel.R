library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(readxl)


Male_EBSCO_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/data/Male_EBSCO_weights_temps_analysis.csv")

Male_EBSCO_weights_and_temps
MEBSCO=Male_EBSCO_weights_and_temps
MEBSCO%>% filter(Maturity=="Mature"|Maturity=="Immature")->MEBSCO_mat

ns_immales_analysis<-subset(MEBSCO_mat, Maturity=="Immature" & SEX==1 & SHELL_CONDITION==2)

os_immales_analysis<-subset(MEBSCO_mat, Maturity=="Immature" & SEX==1 & (SHELL_CONDITION==3|SHELL_CONDITION==4))


ns_matmales_analysis<-subset(MEBSCO_mat, Maturity=="Mature" & SEX==1 & SHELL_CONDITION==2)
os_matmales_analysis<-subset(MEBSCO_mat, Maturity=="Mature" & SEX==1 &  (SHELL_CONDITION==3|SHELL_CONDITION==4))

max(MEBSCO_mat$Temperature) #5.13578 in 2019
min(MEBSCO_mat$Temperature) #0.9980699 in 2012
#view(MBBRKC)
###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ##############################
#dev.new()
par(mfrow =c(2,2))

##########################################################################################################################



################################## New shell immature males ######################################################################
#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~WIDTH,data=ns_immales_analysis, main = "New shell immature male EBS snow crab",ylab= "Weight (g)", xlab= "Width(mm)")


x<-ns_immales_analysis$WIDTH

f<-function(x) {0.0003857 * x^(2.990492)} #cold ns immature males
g<-function(x) {0.0003916 * x^(2.990492)} # warm ns immature males
h<-function(x) {0.000267  * x^(3.097253)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline

legend(19,740, c("Baseline L-W model", "SC/maturity/temp L-W model-cold", "SC/maturity/temp L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell immature males #########################################################################

plot(WEIGHT~WIDTH,data=os_immales_analysis, main = "Old shell immature male EBS snow crab",ylab= "Weight (g)", xlab= "Width(mm)")

#view(os_immales_analysis)
x<-os_immales_analysis$WIDTH

f<-function(x) {0.000379 * x^(3.014734)} #cold os immature males
g<-function(x) {0.000385 * x^(3.014734)} #warm os immature males
h<-function(x) {0.000267  * x^(3.097253)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(34,790, c("Baseline L-W model", "SC/maturity/temp L-W model-cold","SC/maturity/temp L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))

###################################################################################################################
#################### Mature males #####################################################

################################## New shell mature males ######################################################################
#dev.new()
#par(mfrow =c(2,1))

ns_matmales_analysis%>%
  filter(WIDTH>40) ->ns_matmales_analysis
plot(WEIGHT~WIDTH,data=ns_matmales_analysis, main = "New shell mature male EBS snow crab",ylab= "Weight (g)", xlab= "Width(mm)")


x<-ns_matmales_analysis$WIDTH

f<-function(x) {0.000386 * x^(3.018578)} #cold ns mature males
g<-function(x) {0.000392 * x^(3.018578)} # warm ns mature males
h<-function(x) {0.000267  * x^(3.097253)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(40,1150, c("Baseline L-W model", "SC/maturity/temp L-W model-cold", "SC/maturity/temp L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell mature males #########################################################################
os_matmales_analysis %>%
  filter(WIDTH>40) ->os_matmales_analysis

plot(WEIGHT~WIDTH,data=os_matmales_analysis, main = "Old shell mature male EBS snow crab",ylab= "Weight (g)", xlab= "Width(mm)")

#view(os_matmales_analysis)
x<-os_matmales_analysis$WIDTH

f<-function(x) {0.000379 * x^(3.029485)} #cold os mature males
g<-function(x) {0.000385 * x^(3.029485)} #warm os mature males
h<-function(x) {0.000267  * x^(3.097253)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(43,1072, c("Baseline L-W model", "SC/maturity/temp L-W model-cold","SC/maturity/temp L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))




