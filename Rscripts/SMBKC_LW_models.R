# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for SMBKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggsci)

setwd("C:/Users/Jon.Richar/Work/GitRepos/LengthWeight/EBSCrabLengthWeight/DATA")
df<-read.csv("SMBKC_weightDB_analysis.csv")

df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)

dev.new()
plot(df1$WEIGHT~df1$LENGTH)
#identify(df1$WEIGHT~df1$LENGTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(loglength = log(LENGTH),
#          logweight = log(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
 # filter(SEX == 1, SHELL_CONDITION == 2) -> male #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(male, aes(x = LENGTH, y = WEIGHT, group = YEAR)) +
#      geom_point(aes(colour = factor(YEAR)))


################################################################################

sc1_males<-subset(df1,SEX==1 & SHELL_CONDITION==1)
sc2_males<-subset(df1,SEX==1 & SHELL_CONDITION==2)
sc3_males<-subset(df1,SEX==1 & SHELL_CONDITION==3)
sc4_males<-subset(df1,SEX==1 & SHELL_CONDITION==4)
sc5_males<-subset(df1,SEX==1 & SHELL_CONDITION==5)

hist(sc1_males$WEIGHT)
hist(sc2_males$WEIGHT)
hist(sc3_males$WEIGHT)
hist(sc4_males$WEIGHT)
hist(sc5_males$WEIGHT)

hist(log(sc2_males$WEIGHT))
hist(sc3_males$WEIGHT)
hist(sc4_males$WEIGHT)

plot(sc2_males$WEIGHT~sc2_males$LENGTH)
plot(sc3_males$WEIGHT~sc3_males$LENGTH)
plot(sc4_males$WEIGHT~sc4_males$LENGTH)

########################## Aggregate by New shell/old shell #####################################
ns_males<-subset(df1,SEX==1 & SHELL_CONDITION==2)
os_males<-subset(df1,SEX==1 & SHELL_CONDITION==3|SHELL_CONDITION==4)
os_males





#####################################################################################################################
############################ First model L-W relationship by shell condition #########################################
#####################################################################################################################


######################################################################################################
############################ New shell ###############################################################
######################################################################################################

plot(ns_males$WEIGHT~ns_males$LENGTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_males, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) males")

############################# Add fields for analysis ########################################################
Year <- substring(ns_males$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(ns_males$LENGTH)
log.weight <- log(ns_males$WEIGHT)
ns_male<-as.data.frame(cbind(ns_males,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_male                    							  		 # inspect data
names(ns_male)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_male, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.length,data=ns_male)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))
plot(fit1)


plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(ns_male)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_male$Cooks_D <- cooks.distance(fit1)
ns_males_analysis<-subset(ns_male, Cooks_D < (4/(nrow(ns_male))))
 
nrow(ns_male)-nrow(ns_males_analysis)    #41 obs removed based on Cook's Distance

nrow(ns_males_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_males_analysis, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) males- infuential points removed")

p<-ggplot(ns_males_analysis, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) males- infuential points removed")
############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_males_analysis, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

p<-ggplot(ns_males_analysis, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed")

############################ Fit followup model ##########################################
fit2<-lm(log.weight~log.length,data=ns_males_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))

exp(cf2[1,1])
			# log(W) = -7.972545  + 3.176559 * log(L) on transformed scale
    			# W = exp(-7.972545)*L^(3.176559)  on original scale
			# a = 0.0003448003
			# b = 3.176559
p<-ggplot(ns_males_analysis, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed")+
geom_abline(intercept = -7.849620,slope = 3.147886,color = "red")


######################## Apply bias-correction procedure #####################################
cf2
names(fit2)
names(summary(fit2))
####################### Parameter A /intercept########################################################################
v2<-(summary(fit2)$sigma)**2  # Extract model sigma and calculate Variance 
v2
int<-cf2[1,1]                 # Extract intercept parameter
A<-(exp(int)*exp(v2/2))       # calculate bias corrected A parameter
A                         #0.0003462224 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MODEL ###############################
# a = 0.0003462224 
# b = 3.176559

###################################################################################################
############################ Old shell ############################################################
###################################################################################################

dev.new()

plot(os_males$WEIGHT~os_males$LENGTH)
p<-ggplot(os_males, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) males")

############################# Add fields for analysis ########################################################
Year <- substring(os_males$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(os_males$LENGTH)
log.weight <- log(os_males$WEIGHT)
os_male<-as.data.frame(cbind(os_males,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
os_male                    							  		 # inspect data
names(os_male)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()

p<-ggplot(os_male, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="Old shell (SC3+SC4) males-log transformed")

############################## Fit initial model ########################################################

fit3<-lm(log.weight~log.length,data=os_male)
summary(fit3)
coef(fit3)

############################## check diagnostics #################################################
dev.new()

par(mfrow=c(2,2))
plot(fit3)

dev.new()

par(mfrow=c(1,1))

plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(os_male)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_male$Cooks_D <- cooks.distance(fit3)
os_males_analysis<-subset(os_male, Cooks_D < (4/(nrow(os_male))))
 
nrow(os_male)-nrow(os_males_analysis)    #13 obs removed

nrow(os_males_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()

p<-ggplot(os_males_analysis, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) males- infuential points removed")


############################ log transformed ##################################################
dev.new()

p<-ggplot(os_males_analysis, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="Old shell (SC3+SC4) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log.weight~log.length,data=os_males_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))
exp(cf4[1,1])


plot(log.weight~log.length,data=os_males_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])


# log(W) = -7.504500 + 3.093953 * log(L) on transformed scale
# W = exp(-7.504500)*L^(3.093953)  on original scale
# a = 0.000550601
# b = 3.093953
################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(exp(int)*exp(v4/2))
A                         #0.0005521551  

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v4/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
# a = 0.0005521551  
# b = 3.093953
########################################################################################################################################
############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_males_analysis$SC <- "OS"
ns_males_analysis$SC <- "NS"

analysis_males<-rbind(ns_males_analysis,os_males_analysis)
names(analysis_males)

dev.new()

ggplot(analysis_males, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))


q<-ggplot(analysis_males, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Ln(Length)",y="Ln(Weight)", title="Male SMBKC (New shell and old shell) males")

######################################################################################################################
########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male SMBKC (New shell and old shell, log-transformed)")

SMBKC_sc_points <- ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male SMBKC (New shell and old shell, log-transformed)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male SMBKC(New shell and old shell, log-transformed)")


SMBKC_sc_lines <-ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male SMBKC(New shell and old shell, log-transformed)")

ggarrange(SMBKC_sc_points, SMBKC_sc_lines,
          labels = c("a.)", "b.)"),
          ncol = 2, nrow = 1)
############################# with points and lines#####################################################################
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Length)",y="Ln(Weight)", title="Male SMBKC (New shell and old shell log-transformed)")

######################  Alternative approach
ggscatter(analysis_males, x="log.length",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)



########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.length*SC,data = analysis_males)
summary(mod1)									# p = 0.0145 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.length+SC,data = analysis_males)
summary(mod2)									# p <2e-16 for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p = 0.01452...removing interaction term sinificantly affect model

reg_mod<-lm(log.weight~SC/log.length-1,data=analysis_males)		
summary(reg_mod)

# possible interpretation for different intercepts but converging slopes: epibionts affect weight at smaller sizes to greater degree than for larger crab




