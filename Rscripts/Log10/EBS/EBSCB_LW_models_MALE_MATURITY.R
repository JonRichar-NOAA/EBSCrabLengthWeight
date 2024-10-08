# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate width/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df<-read.csv("EBS_CB_Analysis_males_log10.csv")
df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$WIDTH)


################################################################################


immat_males<-subset(df1,SEX==1 & Maturity=="Immature")
ns_matmales<-subset(df1,SEX==1 & SHELL_CONDITION==2 & WIDTH>50 & Maturity=="Mature")
os_matmales<-subset(df1,SEX==1  & WIDTH>50 & SHELL_CONDITION==3|SHELL_CONDITION==4 & Maturity=="Mature")


hist(immat_males$WEIGHT)
hist(ns_matmales$WEIGHT)
hist(os_matmales$WEIGHT)


plot(immat_males$WEIGHT~immat_males$WIDTH)
plot(ns_matmales$WEIGHT~ns_matmales$WIDTH)
plot(os_matmales$WEIGHT~os_matmales$WIDTH)


#####################################################################################################################
############################ First model L-W relationship by maturity and shell condition #########################################
#####################################################################################################################


######################################################################################################################
############################ IMMATURE MALES ##########################################################################
######################################################################################################################

plot(immat_males$WEIGHT~immat_males$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(immat_males, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) immature males")

############################# Add fields for analysis ################################################################

############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(immat_males, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log10.weight~log10.width,data=immat_males)
summary(fit1)
coef(fit1) 
cf1<-as.matrix(coef(fit1))
10^cf1[1,1]
# log(W) = -3.473067 + 2.960256 * log(L) on transformed scale
# W = 10^(-3.473067) * L^(2.960256)  on original scale
# a = 0.0003364598
# b = 2.960256

###############################################################################################
v1<-(summary(fit1)$sigma)**2  #Variance 
v1
int<-cf1[1,1]
A<-(10^(int)*10^(v1/2))
A                         #0.0003369061 

################################################################################################
######################## BIAS CORRECTED PARAMETERS FOR IMMATURE MALE MODEL 1 ###################

  # a = 0.0003369061 
  # b = 2.960256




#################################################################################################
############################## Apply Cooks Distance to remove influential outliers ##############
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(immat_males)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

immat_males$Cooks_D <- cooks.distance(fit1)
immat_males_analysis<-subset(immat_males, Cooks_D < (4/(nrow(immat_males))))
 
nrow(immat_males)-nrow(immat_males_analysis)    #142 obs removed based on Cook's Distance
nrow(immat_males_analysis)

view(immat_males_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(immat_males_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) immature males- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(immat_males_analysis, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) immature males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log10.weight~log10.width,data=immat_males_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))
10^(cf2[1,1])

# log(W) = -3.4824180  + 2.964849 * log(L) on transformed scale
# W = 10^(-3.482418) * L^(2.964849)  on original scale
# a = 0.0003292926 
# b = 2.964849
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf2
names(fit2)
names(summary(fit2))
###############################################################################################
v2<-(summary(fit2)$sigma)**2  #Variance 
v2
int<-cf2[1,1]
A<-(10^(int)*10^(v2/2))
A                         #0.0003295923 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v2/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL IMMATURE MALE MODEL ###############################
# a = 0.0003295923 
# b = 2.964849

########################################################################################################

dev.new()
plot(WEIGHT~WIDTH,data=immat_males_analysis, main = "New shell immature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-immat_males_analysis$WIDTH

g<-function(x) {0.0003295923 *x^(2.964849)} #bias corrected
#f<-function(x) {0.0002734011 *x^(3.014254)}# not corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
#lines(x,f(x),col=4,lwd=2)# not corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(20,650, c("Baseline L-W model", "SC based L-W model"), col=c(4,2),lwd=c(2,2))



##################################################################################################################
############################################## MATURE ############################################################
##################################################################################################################

##################################################################################################################
############################################## NEW SHELL #########################################################

############################ Plot log transformeddata in GGplot ##################################################
dev.new()
p<-ggplot(ns_matmales, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ###############################################################

fit1<-lm(log10.weight~log10.width,data=ns_matmales)
summary(fit1)
coef(fit1)  
cf1<-as.matrix(coef(fit1))
10^cf1[1,1]
# log(W) = -3.551965 + 3.020149 * log(L) on transformed scale
# W = 10^(-3.551965) * L^(3.020149)  on original scale
# a = 0.0002805663 
# b = 3.020149
###############################################################################################
v1<-(summary(fit1)$sigma)**2  #Variance 
v1
int<-cf1[1,1]
A<-(10^(int)*10^(v1/2))
A                         #0.000280899 

################################################################################################
######################## BIAS CORRECTED PARAMETERS FOR NS MATURE MALE MODEL 1 ###################

# a = 0.000280899 
# b = 3.020149
################################################################################################################
############################## Apply Cooks Distance to remove influential outliers ############################
############################## check diagnostics ###############################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_matmales)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_matmales$Cooks_D <- cooks.distance(fit1)
ns_matmales_analysis<-subset(ns_matmales, Cooks_D < (4/(nrow(ns_matmales))))

nrow(ns_matmales)-nrow(ns_matmales_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_matmales_analysis)

###############################################################################################################
############################# Plot using editted dataset ######################################################
dev.new()
p<-ggplot(ns_matmales_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) mature males- infuential points removed")


############################ log transformed #################################################################
dev.new()
p<-ggplot(ns_matmales_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##############################################################
fit2<-lm(log10.weight~log10.width,data=ns_matmales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))
10^(cf2[1,1])

# log(W) = -3.614136 + 3.051405 * log(L) on transformed scale
# W = 10^(-3.614136) * L^(3.051405)  on original scale
# a = 0.0002431442
# b = 3.051405
############################################################################################################
######################## Apply bias-correction procedure ###################################################
cf2
names(fit2)
names(summary(fit2))
#############################################################################################################
v2<-(summary(fit2)$sigma)**2  #Variance 
v2
int<-cf2[1,1]
A<-(10^(int)*10^(v2/2))
A                         #0.0002433375 

####################### Variance for parameter A/intercept #################################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v2/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MATURE MALE MODEL 2 ########################################
# a = 0.0002433375  
# b = 3.051405 


#######################################################################################################################
dev.new()
plot(WEIGHT~WIDTH,data=ns_matmales_analysis, main = "New shell male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_matmales_analysis$WIDTH

g<-function(x) {0.0002433375 *x^(3.051405)} #bias corrected
#f<-function(x) {0.0002734011 *x^(3.014254)}# not corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x,g(x),col=2,lwd=2)#bias corrected
#lines(x,f(x),col=4,lwd=2)# not corrected
lines(x,h(x),col=4,lwd=2)# baseline
legend(55,1550, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))


###############################################################################################################
########################################## OLD SHELL ##########################################################
dev.new()
plot(os_matmales$WEIGHT~os_matmales$WIDTH)
p<-ggplot(os_matmales, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) males")



############################ Plot log transformed data in GGplot #############################################
dev.new()
p<-ggplot(os_matmales, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) males-log transformed")

############################## Fit initial model ###########################################################

fit3<-lm(log10.weight~log10.width,data=os_matmales)
summary(fit3)
coef(fit3)

cf3<-as.matrix(coef(fit3))
10^(cf3[1,1])
# log(W) = -3.686839 + 3.094291 * log(L) on transformed scale
# W = 10^(-3.686839) * L^(3.094291)  on original scale
# a = 0.0002056652 
# b = 3.094291
###############################################################################################
v3<-(summary(fit3)$sigma)**2  #Variance 
v3
int<-cf3[1,1]
A<-(10^(int)*10^(v3/2))
A                         # 0.0002058377 

################################################################################################
######################## BIAS CORRECTED PARAMETERS FOR OS MATURE MALE MODEL 1 ###################

# a = 0.0002058377 
# b = 3.094291
################################################################################################################
############################## Apply Cooks Distance to remove influential outliers ############################
############################## check diagnostics ###########################################################
dev.new()
par(mfrow=c(2,2))

plot(fit3)
plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(os_matmales)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_matmales$Cooks_D <- cooks.distance(fit3)
os_matmales_analysis<-subset(os_matmales, Cooks_D < (4/(nrow(os_matmales))))

nrow(os_matmales)-nrow(os_matmales_analysis)    #32 obs removed

nrow(os_matmales_analysis)
############################################################################################################
############################# Plot using editted dataset ###################################################
dev.new()
p<-ggplot(os_matmales_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) males- infuential points removed")


############################ log transformed ##############################################################
dev.new()
p<-ggplot(os_matmales_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) males-log transformed, influential points removed")

############################ Fit followup model ###########################################################
fit4<-lm(log10.weight~log10.width,data=os_matmales_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log10.weight~log10.width,data=os_matmales_analysis)

abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])

10^cf4[1,1]
# log(W) = -3.704618  + 3.102813 * log(L) on transformed scale
# W = 10^(-3.704618) * L^(3.102813)  on original scale
# a = 0.0001974161
# b = 3.102813

#########################################################################################################
######################## Apply bias-correction procedure ################################################
#########################################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(10^(int)*10^(v4/2))
A                         # 0.0001975521

####################### Variance for parameter A/intercept #############################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v4/2))
sdA                       #1.046375

sdA_base<-10^(sd)
sdA_base                  #1.044331
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL 2 ###################################
# a = 0.0001975521
# b = 3.102813

plot(WEIGHT~WIDTH,data=os_matmales_analysis, main = "Old shell mature male Bairdi")


x<-os_matmales_analysis$WIDTH
g<-function(x) {0.0001975521 *x^(3.102813)} #bias corrected
#f<-function(x) {0.0002079514 *x^(3.091966)}# not corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x,g(x),col=2,lwd=2)#bias corrected
#lines(x,f(x),col=4,lwd=2)# not corrected
lines(x,h(x),col=3,lwd=2)# baseline
legend(60,1800, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(3,2),lwd=c(2,2))


###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ###############################################
###################################################################################################################################

dev.new()
par(mfrow =c(2,2))

############################################Immature males ########################################################################
# a = 0.0003369061 
# b = 2.960256

plot(WEIGHT~WIDTH,data=immat_males, main = "New shell immature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")

#view(immat_males_analysis)
x<-immat_males$WIDTH

g<-function(x) {0.0003369061 * x^(2.960256)} #bias corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(18,1100, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))



########################################## New shell mature males###################################################################
# a = 0.000280899 
# b = 3.020149

plot(WEIGHT~WIDTH,data=ns_matmales, main = "New shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_matmales$WIDTH

g<-function(x) {0.000280899  *x^(3.020149)} #bias corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(49,1605, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))


######################################### Old shell mature males ###################################################################
# a = 0.0002058377 
# b = 3.094291
plot(WEIGHT~WIDTH,data=os_matmales, main = "Old shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-os_matmales$WIDTH
g<-function(x) {0.0002058377 *x^(3.094291)} #bias corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(54,1930, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))


###################################################################################################################################
########################################### After Second Application of Cooks Distance ############################################ 
dev.new()
par(mfrow =c(2,2))

############################################Immature males ########################################################################
plot(WEIGHT~WIDTH,data=immat_males_analysis, main = "New shell immature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")

#view(immat_males_analysis)
x<-immat_males_analysis$WIDTH

g<-function(x) {0.0003295923 * x^(2.964849)} #bias corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(18,685, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))



########################################## New shell mature males###################################################################
#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~WIDTH,data=ns_matmales_analysis, main = "New shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_matmales_analysis$WIDTH

g<-function(x) {0.0002433375 *x^(3.051405)} #bias corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(49,1605, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))


######################################### Old shell mature males ###################################################################

plot(WEIGHT~WIDTH,data=os_matmales_analysis, main = "Old shell mature male Bairdi",ylab= "Weight (g)", xlab= "Width (mm)")


x<-os_matmales_analysis$WIDTH
g<-function(x) {0.0001975521 *x^(3.102813)} #bias corrected
h<-function(x) {0.00027*x^(3.022134)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(54,1930, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))


#############################################################################################################################################################
############################ combine data sets and plot, using shell condition as grouping factor############################################################
#############################################################################################################################################################


analysis_males<-rbind(ns_matmales_analysis,os_matmales_analysis)

dev.new()
plot(WEIGHT~WIDTH,data=analysis_males, main = "Mature male Bairdi")

x<-analysis_males$WIDTH
g<-function(x) {0.0002433375 *x^(3.051405)} #New shell mature males
h<-function(x) {0.0001975521 *x^(3.102813)} #Old shell mature males

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(60,1800, c("New shell mature males", "Old shell mature males"), col=c(2,4),lwd=c(2,2))

#write.csv(analysis_males,"EBS_CB_Analysis_Mature_males_log10.csv")

ggplot(analysis_males, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_males, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="EBS CB(New shell and old shell) males")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell) log-transformed")


ebscb_sc_points<-ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell) log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB (New shell and old shell, log-transformed)")

ebscb_sc_lines<-ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell, log-transformed)")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CB(New shell and old shell) log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_males, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_males)
summary(mod1)									# p = 3.33e-11 on interaction term. Evidence slopes are different

mod2<-aov(log10.weight~log10.width+SC,data = analysis_males)
summary(mod2)									# p = <2e-16. Intercepts are significantly different

anova(mod1,mod2)									# p = 3.33e-11....removing interaction impacts model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_males)
summary(reg_mod)




