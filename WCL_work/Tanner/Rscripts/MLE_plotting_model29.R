Male_EBSCB_weights_and_temps <- read_csv("//akc0ks-n220/KOD_Research/RKC-BKC/Survey L-W paper- Jon/Tanner/data/Male_EBSCB_weights_temps_analysis.csv")

Male_EBSCB_weights_and_temps
MEBSCB=Male_EBSCB_weights_and_temps
MEBSCB%>% filter(Maturity=="Mature"|Maturity=="Immature")->MEBSCB_mat


immat_males_analysis<-subset(MEBSCB_mat,SEX==1 & Maturity=="Immature")
ns_matmales_analysis<-subset(MEBSCB_mat,SEX==1 & SHELL_CONDITION==2 & WIDTH>50 & Maturity=="Mature")
os_matmales_analysis<-subset(MEBSCB_mat,SEX==1 & WIDTH>50 & Maturity=="Mature" & SHELL_CONDITION==3|SHELL_CONDITION==4)


###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ##############################
dev.new()
par(mfrow =c(2,2))

##########################################################################################################################

################################### Immature males ############################################################################
plot(WEIGHT~WIDTH,data=immat_males_analysis, main = "New shell immature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")

#view(immat_males_analysis)
x<-immat_males_analysis$WIDTH

f<-function(x) {0.0003389 * x^(2.954046)} #cold ns immature males
g<-function(x) {0.0003483 * x^(2.954046)} #warm ns immature males
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns immature males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)# warm ns immature males
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2)# baseline

legend(18,1100, c("Baseline L-W model", "SC/maturity/temp L-W model-cold", "SC/maturity/temp L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))



################################## New shell mature males ######################################################################
#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~WIDTH,data=ns_matmales_analysis, main = "New shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_matmales_analysis$WIDTH

f<-function(x) {0.0003389 * x^(2.977018)} #cold ns mature males
g<-function(x) {0.0003483 *x^(2.977018)} # warm ns matmales
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2)# cold ns immature males
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm ns matmales
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline

legend(49,1605, c("Baseline L-W model", "SC/maturity/temp L-W model-cold", "SC/maturity/temp based L-W model-warm"),col=c(1,4,2),lwd=c(2,2,2))


################################ Old shell mature males #########################################################################

plot(WEIGHT~WIDTH,data=os_matmales_analysis, main = "Old shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-os_matmales_analysis$WIDTH
f<-function(x) {0.0002198 * x^(3.076112)} #cold os mature males
g<-function(x) {0.0002278 *x^(3.076112)} #warm os matmales
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],f(x)[order(x)],col=4,lwd=2) # cold os matmales
lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # warm os matmales
lines(x[order(x)],h(x)[order(x)],col=1,lwd=2) # baseline
legend(60,1930, c("Baseline L-W model", "SC/maturity/temp L-W model-cold","SC/maturity/temp based L-W model-warm"), col=c(1,4,2),lwd=c(2,2,2))

#