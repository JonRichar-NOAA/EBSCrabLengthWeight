library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)


Male_EBSCO_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/data/Male_EBSCO_weights_temps_analysis.csv")

Male_EBSCO_weights_and_temps
MEBSCO=Male_EBSCO_weights_and_temps
MEBSCO%>% filter(Maturity=="Mature"|Maturity=="Immature")->MEBSCO_mat


immat_males_analysis<-subset(MEBSCO_mat,SEX==1 & Maturity=="Immature")
ns_matmales_analysis<-subset(MEBSCO_mat,SEX==1 & SHELL_CONDITION==2 & WIDTH>40 & Maturity=="Mature")
os_matmales_analysis<-subset(MEBSCO_mat,SEX==1 & WIDTH>40 & Maturity=="Mature" & SHELL_CONDITION==3|SHELL_CONDITION==4)

names(MEBSCO_mat)
#view(MEBSCO_mat)
max(MEBSCO_mat$Temperature) # 3.250774 in 2018

min(MEBSCO_mat$Temperature) #-0.7328246 in 2006
###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ##############################
dev.new()
par(mfrow =c(2,2))

##########################################################################################################################

################################### Immature males ############################################################################
plot(WEIGHT~WIDTH,data=immat_males_analysis, main = "New shell immature male opilio",ylab= "Weight (g)", xlab= "Width (mm)")

#view(immat_males_analysis)
x<-immat_males_analysis$WIDTH

f<-function(x) {0.0003857 * x^(2.990492)} #cold ns immature males
g<-function(x) {0.0003916 * x^(2.990492)} #warm ns immature males
h<-function(x) {0.000267 * x^(3.097253)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns immature males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)# warm ns immature males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2)# baseline

legend(18,1100, c("Baseline L-W model", "SC/maturity/temp L-W model-cold", "SC/maturity/temp L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))



################################## New shell mature males ######################################################################
#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~WIDTH,data=ns_matmales_analysis, main = "New shell mature male opilio",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_matmales_analysis$WIDTH

f<-function(x) {0.0003857 * x^(3.018578)} #cold ns mature males
g<-function(x) {0.0003916 * x^(3.018578)} # warm ns matmales
h<-function(x) {0.000267 * x^(3.097253)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns immature males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns matmales
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline

legend(49,1605, c("Baseline L-W model", "SC/maturity/temp L-W model-cold", "SC/maturity/temp based L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell mature males #########################################################################

plot(WEIGHT~WIDTH,data=os_matmales_analysis, main = "Old shell mature male opilio",ylab= "Weight (g)", xlab= "Width (mm)")


x<-os_matmales_analysis$WIDTH
f<-function(x) {0.0003789 * x^(3.029485)} #cold os mature males
g<-function(x) {0.0003849 * x^(3.029485)} #warm os matmales
h<-function(x) {0.000267 * x^(3.097253)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os matmales
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os matmales
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(60,1930, c("Baseline L-W model", "SC/maturity/temp L-W model-cold","SC/maturity/temp based L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))

#
