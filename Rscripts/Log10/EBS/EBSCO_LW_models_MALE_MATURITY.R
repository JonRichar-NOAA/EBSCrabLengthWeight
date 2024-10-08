# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/Jon.Richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df<-read.csv("EBS_CO_Analysis_males_log10.csv")
df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$WIDTH)


################################################################################

immat_males<-subset(df1,SEX==1 & Maturity=="Immature")
ns_matmales<-subset(df1,SEX==1 & SHELL_CONDITION==2 & WIDTH>40 & Maturity=="Mature")
os_matmales<-subset(df1,SEX==1 & SHELL_CONDITION==3|SHELL_CONDITION==4 & WIDTH>40 & Maturity=="Mature")


hist(immat_males$WEIGHT)
hist(ns_matmales$WEIGHT)
hist(os_matmales$WEIGHT)


plot(immat_males$WEIGHT~immat_males$WIDTH)
plot(ns_matmales$WEIGHT~ns_matmales$WIDTH)
plot(os_matmales$WEIGHT~os_matmales$WIDTH)



###################################################################################################################################
############################ First model L-W relationship by maturity and shell condition #########################################
###################################################################################################################################


##################################################################################################################################
############################ New shell immature males ###########################################################################################
##################################################################################################################################

plot(immat_males$WEIGHT~immat_males$WIDTH)

#view(immat_males_analysis)
############################ Plot log transformeddata in GGplot #################################################################
dev.new()
p<-ggplot(immat_males, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) immature males-log transformed")


############################## Fit initial model ###############################################################################

fit1<-lm(log10.weight~log10.width,data=immat_males)
summary(fit1)
coef(fit1)
############################## check diagnostics ###############################################################################
dev.new()
par(mfrow=c(2,2))
plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(immat_males)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

immat_males$Cooks_D <- cooks.distance(fit1)
immat_males_analysis<-subset(immat_males, Cooks_D < (4/(nrow(immat_males))))
 
nrow(immat_males)-nrow(immat_males_analysis)    #235 obs removed based on Cook's Distance

nrow(immat_males_analysis)
##############################################################################################################################
############################# Plot using editted dataset #####################################################################
dev.new()
p<-ggplot(immat_males_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) immature males- infuential points removed")


############################ log transformed ################################################################################
dev.new()
p<-ggplot(immat_males_analysis, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ############################################################################
fit2<-lm(log10.weight~log10.width,data=immat_males_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))
10^(cf2[1,1])
# log(W) = -3.445089 + 3.009003  * log(L) on transformed scale
# W = 10^(-3.445089) * L^(3.009003)  on original scale
# a = 0.000358848 
# b = 3.009003
############################################################################################################################
######################## Apply bias-correction procedure ###################################################################
cf2
names(fit2)
names(summary(fit2))
############################################################################################################################
v2<-(summary(fit2)$sigma)**2  #Variance 
v2
int<-cf2[1,1]
A<-(10^(int)*10^(v2/2))
A                         #0.0003591357

####################### Variance for parameter A/intercept ################################################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v2/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL IMMATURE MALE MODEL ######################################################
# a = 0.0003591357
# b = 3.009003



########################################################################################################################################################
############################################# New shell MATURE MALES ###################################################################################
#######################################################################################################################################################

dev.new()
plot(ns_matmales$WEIGHT~ns_matmales$WIDTH)
p<-ggplot(ns_matmales, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) males")

							 # Check column names


############################ Plot log transformeddata in GGplot #########################################################
dev.new()
p<-ggplot(ns_matmales, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) males-log transformed")

############################## Fit initial model ########################################################################

fit3<-lm(log10.weight~log10.width,data=ns_matmales)
summary(fit3)
coef(fit3)

############################## check diagnostics ########################################################################
dev.new()
par(mfrow=c(2,2))
plot(fit3)
plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(ns_matmales)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_matmales$Cooks_D <- cooks.distance(fit3)
ns_matmales_analysis<-subset(ns_matmales, Cooks_D < (4/(nrow(ns_matmales))))
 
nrow(ns_matmales)-nrow(ns_matmales_analysis)    #123 obs removed

nrow(ns_matmales_analysis)

#######################################################################################################################
############################# Plot using editted dataset ##############################################################
dev.new()
p<-ggplot(ns_matmales_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell mature males- infuential points removed")


############################ log transformed ##########################################################################
dev.new()
p<-ggplot(ns_matmales_analysis, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="new shell mature male opilio males-log transformed, influential points removed")

############################ Fit followup model #####################################################################
fit4<-lm(log10.weight~log10.width,data=ns_matmales_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log10.weight~log10.width,data=ns_matmales_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])

cf4
10^cf4[1,1]

# log(W) = -3.472514 + 3.051117 * log(L) on transformed scale
# W = 10^(-3.472514) * L^(3.051117)  on original scale
# a = 0.0003368883
# b = 3.051117

########################################################################################################################
######################## Apply bias-correction procedure ###############################################################
########################################################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(10^(int)*10^(v4/2))
A                         # 0.0003371483

####################### Variance for parameter A/intercept ############################################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v4/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MATURE MALE MODEL ####################################################
# a = 0.0003371483
# b = 3.051117


############################################################################################################################################
############################################## Old shell ###################################################################################
###########################################################################################################################################

dev.new()
plot(os_matmales$WEIGHT~os_matmales$WIDTH)
p<-ggplot(os_matmales, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) males")



############################ Plot log transformeddata in GGplot #########################################################
dev.new()
p<-ggplot(os_matmales, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) males-log transformed")

############################## Fit initial model ########################################################################

fit3<-lm(log10.weight~log10.width,data=os_matmales)
summary(fit3)
coef(fit3)

############################## check diagnostics ########################################################################
dev.new()
par(mfrow=c(2,2))
plot(fit3)
plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(os_matmales)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_matmales$Cooks_D <- cooks.distance(fit3)
os_matmales_analysis<-subset(os_matmales, Cooks_D < (4/(nrow(os_matmales))))

nrow(os_matmales)-nrow(os_matmales_analysis)    #123 obs removed

nrow(os_matmales_analysis)

#######################################################################################################################
############################# Plot using editted dataset ##############################################################
dev.new()
p<-ggplot(os_matmales_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Old shell (SC3+SC4) males- infuential points removed")


############################ log transformed ##########################################################################
dev.new()
p<-ggplot(os_matmales_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell mature male opilio-log transformed, influential points removed")

############################ Fit followup model #####################################################################
fit4<-lm(log10.weight~log10.width,data=os_matmales_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log10.weight~log10.width,data=os_matmales_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])

cf4
10^cf4[1,1]

# log(W) = -3.488739 + 3.064181 * log(L) on transformed scale
# W = 10^(-3.488739) * L^(3.064181)  on original scale
# a = 0.0003245344 
# b = 3.064181

########################################################################################################################
######################## Apply bias-correction procedure ###############################################################
########################################################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(10^(int)*10^(v4/2))
A                         # 0.0003247368  

####################### Variance for parameter A/intercept ############################################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(10^(sd)*10^(v4/2))
sdA

sdA_base<-10^(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MATURE MALE MODEL ####################################################
# a = 0.0003247368  
# b = 3.064181

###################################################################################################################################
############################ Plot comparisons of all three groups vs baseline model ##############################
dev.new()

par(mfrow =c(2,2))

plot(WEIGHT~WIDTH,data=immat_males_analysis, main = "New shell immature male Opilio",ylab= "Weight (g)", xlab= "Width (mm)")

#view(immat_males_analysis)
x<-immat_males_analysis$WIDTH

g<-function(x) {0.0003591357 * x^(3.009003)} #bias corrected
h<-function(x) {0.000267*x^(3.09725)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(18,640, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))



# New shell mature males
#dev.new()
#par(mfrow =c(2,1))
plot(WEIGHT~WIDTH,data=ns_matmales_analysis, main = "New shell mature male Opilio",ylab= "Weight (g)", xlab= "Width (mm)")


x<-ns_matmales_analysis$WIDTH

g<-function(x) {0.0003371483 *x^(3.051117)} #bias corrected
h<-function(x) {0.000267*x^(3.09725)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(39,1180, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))


#Old shell mature males

plot(WEIGHT~WIDTH,data=os_matmales_analysis, main = "Old shell mature male Opilio",ylab= "Weight (g)", xlab= "Width (mm)")


x<-os_matmales_analysis$WIDTH
g<-function(x) {0.0003247368 *x^(3.064181)} #bias corrected
h<-function(x) {0.000267*x^(3.09725)} #baseline

lines(x[order(x)],g(x)[order(x)],col=2,lwd=2)#bias corrected
lines(x[order(x)],h(x)[order(x)],col=4,lwd=2)# baseline
legend(32,1100, c("Baseline L-W model", "SC/maturity based L-W model"), col=c(4,2),lwd=c(2,2))


############################ combine data sets and plot, using shell condition as grouping factor############################################################

analysis_males<-rbind(ns_matmales_analysis,os_matmales_analysis)

write.csv(analysis_males,"EBS_CO_Analysis_males_log10.csv")

ggplot(analysis_males, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_males, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="Male opilio (New shell and old shell)")

######################Log transformed #################################################
dev.new()


ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

ebsco_sc_points<-ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, log-transformed)")

# lines ONLY
ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

ebsco_sc_lines<-ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell, log-transformed)")

# with points and lines
ggplot(analysis_males, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="EBS male opilio (New shell and old shell log-transformed)")


################ Alternative approach with lines
ggscatter(analysis_males, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_males)			# p = 1.94e-15--significant interaction -- slopes different
summary(mod1)

mod2<-aov(log10.weight~log10.width+SC,data = analysis_males)			# p < 2e-16: interceptes are significantly different
summary(mod2)

anova(mod1,mod2)										# p = 1.94e-15--interaction term is significant to model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_males)
summary(reg_mod)


