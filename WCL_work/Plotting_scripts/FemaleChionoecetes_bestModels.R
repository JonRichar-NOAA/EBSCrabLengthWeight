
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(readxl)


Female_EBSCB_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/data/Female_EBSCB_weights_temps_analysis.csv")

Female_EBSCB_weights_and_temps
FEBSCB=Female_EBSCB_weights_and_temps

FEBSCB%>% filter(CLUTCH_SIZE>=1)->FEBSCB_mat

ns_females_analysis<-subset(FEBSCB_mat, SHELL_CONDITION==2)
os_females_analysis<-subset(FEBSCB_mat,  SHELL_CONDITION==3|SHELL_CONDITION==4)

###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ##############################
#dev.new()
par(mfrow =c(2,2))

################################## New shell #############################################################################

plot(WEIGHT~WIDTH,data=ns_females_analysis, main = "New shell mature female EBS CB",ylab= "Weight (g)", xlab= "Width(mm)")


x<-ns_females_analysis$WIDTH

f<-function(x) {0.000490421 * x^(2.86531805)} #cold ns mature females
g<-function(x) {0.000348263 * x^(2.945600942)} # warm ns mature females
h<-function(x) {0.000441  * x^(2.898686)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(54,380, c("Baseline L-W model", "SC/temp L-W model-cold", "SC/temp L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell  #########################################################################

plot(WEIGHT~WIDTH,data=os_females_analysis, main = "Old shell mature female EBS CB",ylab= "Weight (g)", xlab= "Width(mm)")

#view(os_matmales_analysis)
x<-os_females_analysis$WIDTH

f<-function(x) {0.000730877 * x^(2.78948598)} #cold os mature females
g<-function(x) {0.000519018 * x^(2.869768872)} #warm os mature females
h<-function(x) {0.000441  * x^(2.898686)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(56,420, c("Baseline L-W model", "SC/temp L-W model-cold","SC/temp L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))

#########################################################################################################################################
Female_EBSCO_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Opilio/data/Female_EBSCO_weights_temps_analysis.csv")

Female_EBSCO_weights_and_temps
FEBSCO=Female_EBSCO_weights_and_temps
names(Female_EBSCO_weights_and_temps)
FEBSCO%>% filter(CLUTCH_SIZE>=1)->FEBSCO_mat

ns_females_analysis<-subset(FEBSCO_mat, SHELL_CONDITION==2)
os_females_analysis<-subset(FEBSCO_mat,  SHELL_CONDITION==3|SHELL_CONDITION==4)

#################################### New shell ###############################################################################

#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~WIDTH,data=ns_females_analysis, main = "New shell mature female EBS CO",ylab= "Weight (g)", xlab= "Width(mm)")


x<-ns_females_analysis$WIDTH

f<-function(x) {0.000712358 * x^(2.822660609)} #cold ns mature females
g<-function(x) {0.001054025 * x^(2.729362801)} # warm ns mature females
h<-function(x) {0.001158  * x^(2.708793)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(36,190, c("Baseline L-W model", "SC/temp L-W model-cold", "SC/temp L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell  #########################################################################

plot(WEIGHT~WIDTH,data=os_females_analysis, main = "Old shell mature female EBS CO",ylab= "Weight (g)", xlab= "Width(mm)")

#view(os_matmales_analysis)
x<-os_females_analysis$WIDTH

f<-function(x) {0.000547701 * x^(2.896349539)} #cold os mature females
g<-function(x) {0.000810394 * x^(2.803051731)} #warm os mature females
h<-function(x) {0.001158  * x^(2.708793)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(38,210, c("Baseline L-W model", "SC/temp L-W model-cold","SC/temp L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))

