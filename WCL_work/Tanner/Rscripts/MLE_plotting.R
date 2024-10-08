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

g<-function(x) {0.0003487916 * x^(2.95056407)} #ns immature males
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)# ns immature males
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(18,1100, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))



################################## New shell mature males ######################################################################
#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~WIDTH,data=ns_matmales_analysis, main = "New shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_matmales_analysis$WIDTH

g<-function(x) {0.0003487916 *x^(2.974051)} # ns matmales

h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # ns matmales
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2) # baseline
legend(49,1605, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))


################################ Old shell mature males #########################################################################

plot(WEIGHT~WIDTH,data=os_matmales_analysis, main = "Old shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-os_matmales_analysis$WIDTH
g<-function(x) {0.0002210594 *x^(3.079633)} #os matmales
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # os matmales
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2) # baseline
legend(60,1930, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))

####################################################################################################################
################################# Temperature ######################################################################
####################################################################################################################
max(MEBSCB_mat$Temperature)
min(MEBSCB_mat$Temperature)
names(MEBSCB_mat)
MEBSCB_mat %>% subset(SURVEY_YEAR == 2016) -> MEBSCB_maxtemp
view(MEBSCB_maxtemp)

immat_males_maxtemp<-subset(MEBSCB_maxtemp,SEX==1 & Maturity=="Immature")
ns_matmales_maxtemp<-subset(MEBSCB_maxtemp,SEX==1 & SHELL_CONDITION==2 & WIDTH>50 & Maturity=="Mature")
os_matmales_maxtemp<-subset(MEBSCB_maxtemp,SEX==1 & WIDTH>50 & Maturity=="Mature" & SHELL_CONDITION==3|SHELL_CONDITION==4)

###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ##############################
dev.new()
par(mfrow =c(2,2))

##########################################################################################################################

plot(WEIGHT~WIDTH,data=immat_males_maxtemp, main = "New shell immature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")

#view(immat_males_analysis)
x<-immat_males_maxtemp$WIDTH

g<-function(x) {0.0003487916 * x^(2.95056407)} #ns immature males
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)# ns immature males
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(18,1100, c("Baseline L-W model", "SC/maturity/temp based L-W model"), col=c(4,2),lwd=c(2,2))k
################################## New shell mature males ######################################################################
#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~WIDTH,data=ns_matmales_maxtemp, main = "New shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_matmales_maxtemp$WIDTH

g<-function(x) {0.0003487916 *x^(2.974051)} # ns matmales

h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # ns matmales
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2) # baseline
legend(49,1605, c("Baseline L-W model", "SC/maturity/temp based L-W model"), col=c(4,2),lwd=c(2,2))


################################ Old shell mature males #########################################################################

plot(WEIGHT~WIDTH,data=os_matmales_maxtemp, main = "Old shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-os_matmales_maxtemp$WIDTH
g<-function(x) {0.0002210594 *x^(3.079633)} #os matmales
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2) # os matmales
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2) # baseline
legend(60,1930, c("Baseline L-W model", "SC/maturity/temp based L-W model"), col=c(4,2),lwd=c(2,2))