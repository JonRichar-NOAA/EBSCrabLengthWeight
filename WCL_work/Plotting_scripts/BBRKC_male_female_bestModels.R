
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(readxl)


Male_BBRKC_weights_and_temps <- read_excel("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/BBRKC/data/Male BBRKC weights and temps.xlsx")
Male_BBRKC_weights_and_temps
MBBRKC=Male_BBRKC_weights_and_temps

ns_males_analysis<-subset(MBBRKC,SEX==1 & SHELL_CONDITION==2)
os_males_analysis<-subset(MBBRKC,SEX==1 & SHELL_CONDITION==3|SHELL_CONDITION==4)


###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ##############################
#dev.new()
par(mfrow =c(2,2))

################################## New shell mature males ######################################################################

plot(WEIGHT~LENGTH,data=ns_males_analysis, main = "New shell mature male BBRKC",ylab= "Weight (g)", xlab= "LENGTH (mm)")


x<-ns_males_analysis$LENGTH

f<-function(x) {0.000412 * x^(3.12765)} #cold ns males
g<-function(x) {0.00042 * x^(3.12765)} # warm ns males
h<-function(x) {0.000403 * x^(3.141334)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline

legend(17,7000, c("Baseline L-W model", "SC/temp L-W model-cold", "SC/temp L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell mature males #########################################################################

plot(WEIGHT~LENGTH,data=os_males_analysis, main = "Old shell mature male BBRKC",ylab= "Weight (g)", xlab= "LENGTH (mm)")


x<-os_males_analysis$LENGTH

f<-function(x) {0.000442 * x^(3.12765)} #cold os males
g<-function(x) {0.000471 * x^(3.12765)} #warm os males
h<-function(x) {0.000403 * x^(3.141334)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(75,6175, c("Baseline L-W model", "SC/temp L-W model-cold","SC/temp L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))

############################## Females ###########################################################################################
Female_BBRKC_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/BBRKC/data/Female_BBRKC_weights_temps_analysis.csv")

Female_BBRKC_weights_and_temps
FBBRKC=Female_BBRKC_weights_and_temps
FBBRKC%>% filter(CLUTCH_SIZE>=1 & SHELL_CONDITION ==2)->FBBRKC_mat


females_analysis<-FBBRKC_mat


#view(MBBRKC)
###################################################################################################################################
############################ Plot comparisons of groups vs baseline model ##############################
#dev.new()
#par(mfrow =c(1,2))


################################## ######################################################################
#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~LENGTH,data=females_analysis, main = "Mature female BBRKC",ylab= "Weight (g)", xlab= "LENGTH (mm)")


x<-females_analysis$LENGTH

f<-function(x) {0.004831725 * x^(2.5944127)} #cold females
g<-function(x) {0.004921769 * x^(2.5944127)} # warm females
h<-function(x) {0.003593 * x^(2.666076)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold females
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm females
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline

legend(78,2630, c("Baseline L-W model", "Temperature influenced L-W model-cold", "Temperature influenced L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))



