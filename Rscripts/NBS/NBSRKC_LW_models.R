# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA/NBS_Data")
df<-read.csv("NBS_RKC_DATA_FOR_SIZEWEIGHT_MALE.csv")
df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$LENGTH)
#identify(df1$WEIGHT~df1$LENGTH)
######################### Tidyverse approach ###################################
df1 %>%
  mutate(loglength = log(LENGTH),
          logweight = log(WEIGHT),
          Year = substring(CRUISE, 1,4), 
          YEAR = as.factor(Year)) %>%
 filter(SEX == 1) -> male #Only SC2 as to not bias for weight of epibionts 
#male
ggplot(male, aes(x = LENGTH, y = WEIGHT, group = YEAR)) +
     geom_point(aes(colour = factor(SHELL_CONDITION)))


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

all_males<-subset(df1,SEX==1 & SHELL_CONDITION==2|SHELL_CONDITION==3|SHELL_CONDITION==4)





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
 
nrow(ns_male)-nrow(ns_males_analysis)    #235 obs removed based on Cook's Distance

nrow(ns_males_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_males_analysis, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) males- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_males_analysis, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log.weight~log.length,data=ns_males_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))


##############################################################################################
######################## Apply bias-correction procedure #####################################
cf2
names(fit2)
names(summary(fit2))
###############################################################################################
v2<-(summary(fit2)$sigma)**2  #Variance 
v2
int<-cf2[1,1]
A<-(exp(int)*exp(v2/2))
A                         #0.0004866011 

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

# a = 0.0004866011                      #Corrected for NBS NS males
# b = 3.097601                          #Corrected for NBS NS males

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
plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(os_male)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_male$Cooks_D <- cooks.distance(fit3)
os_males_analysis<-subset(os_male, Cooks_D < (4/(nrow(os_male))))
 
nrow(os_male)-nrow(os_males_analysis)    #1 obs removed

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
p+ labs(x="ln(length)",y="ln(weight)", title="Old shell (SC3+SC4) opilio males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log.weight~log.length,data=os_males_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log.weight~log.length,data=os_males_analysis)
abline(a=cf2[1,1],b=cf2[2,1]) # Plots new shell model line
abline(a=cf4[1,1],b=cf4[2,1])

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(exp(int)*exp(v4/2))
A                         #0.0008859487

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit4)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v4/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR OLD SHELL MODEL ###############################
# a = 0.0008859487          #Corrected for NBS OS males
# b = 2.984075              #Corrected for NBS OS males


######################################################################################################
############################ All males ###############################################################
######################################################################################################

plot(all_males$WEIGHT~all_males$LENGTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(all_males, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="All males combined")

############################# Add fields for analysis ########################################################
Year <- substring(all_males$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(all_males$LENGTH)
log.weight <- log(all_males$WEIGHT)
all_male<-as.data.frame(cbind(all_males,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
all_male                    							  		 # inspect data
names(all_male)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(all_male, aes(x = log.length, y = log.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="All males-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.length,data=all_male)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))
plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(all_male)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

all_male$Cooks_D <- cooks.distance(fit1)
all_males_analysis<-subset(all_male, Cooks_D < (4/(nrow(all_male))))

nrow(all_male)-nrow(all_males_analysis)    #8 obs removed based on Cook's Distance

nrow(all_males_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(all_males_analysis, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="All males- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(all_males_analysis, aes(x = log.length, y = log.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="All males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit8<-lm(log.weight~log.length,data=all_males_analysis)
summary(fit8)
coef(fit8)

cf8<-as.matrix(coef(fit8))


##############################################################################################
######################## Apply bias-correction procedure #####################################
cf8
names(fit8)
names(summary(fit8))
###############################################################################################
v2<-(summary(fit8)$sigma)**2  #Variance 
v2
int<-cf8[1,1]
A<-(exp(int)*exp(v2/2))
A                         #0.0004319995

####################### Variance for parameter A/intercept ########################################
#vcov(fit8)
Av<-vcov(fit8)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELL MODEL ###############################
# a = 0.0004319995          #Corrected for NBS all males
# b = 3.129969              #Corrected for NBS all males

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_males_analysis$SC <- "OS"
ns_males_analysis$SC <- "NS"

analysis_males<-rbind(ns_males_analysis,os_males_analysis)

write.csv(analysis_males,"NBS_RKC_Analysis_males.csv")

ggplot(analysis_males, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_males, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="length(mm)",y="Weight(g)", title="Male opilio (New shell and old shell)")

######################Log transformed #################################################
dev.new()


ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(length)",y="Ln(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

ebsco_sc_points<-ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(length)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, log-transformed)")

# lines ONLY
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(length)",y="Ln(Weight)", title="Male opilio (New shell and old shell, log-transformed)")

ebsco_sc_lines<-ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(length)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell, log-transformed)")

# with points and lines
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(length)",y="Ln(Weight)", title="Male opilio (New shell and old shell log-transformed)")


################ Alternative approach with lines
ggscatter(analysis_males, x="log.length",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
as(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC)))


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.length*SC,data = analysis_males)			# p = 1.94e-15--significant interaction -- slopes different
summary(mod1)

mod2<-aov(log.weight~log.length+SC,data = analysis_males)			# p< 2e-16: interceptes are significantly different
summary(mod2)

anova(mod1,mod2)										# p = 1.94e-15--interaction term is significant to model

reg_mod<-lm(log.weight~SC/log.length-1,data=analysis_males)
summary(reg_mod)







##########################################################################################################################################################
############################### Rerun shell condition models while limit data to same size ranges ##########################################################
###########################################################################################################################################################






x.lim=c(0,200)
?hist()
summary(os_males_analysis$LENGTH)


summary(ns_males_analysis$LENGTH)


dev.new()
par(mfrow=c(2,1))
hist(ns_males_analysis$LENGTH,breaks=30,main="New shell size frequency",xaxt='n',xlim=x.lim)
abline(v=mean(ns_males_analysis$LENGTH),col=4)
axis(side=1,at=seq(0,200,10),label=seq(0,200,10))
hist(os_males_analysis$LENGTH,breaks=20,main="Old shell size frequency",xaxt='n',xlim=x.lim)
abline(v=mean(os_males_analysis$LENGTH),col=4)
axis(side=1,at=seq(0,200,10),label=seq(0,200,10))

############################ Truncate new shells to match old shell size range ###################

ns_males_analysis_sub<-subset(ns_males_analysis,LENGTH>36.0)


############################ Fit followup model ##########################################
fit5<-lm(log.weight~log.length,data=ns_males_analysis_sub)
summary(fit5)
coef(fit5)

cf5<-as.matrix(coef(fit5))
cf5
			# log(W) = -8.414407 + 3.134860 * log(L) on transformed scale
    			# W = exp(-8.36005)*L^(3.134860 )  on original scale
			# a = 0.0002340326
			# b = 3.134860

p<-ggplot(ns_males_analysis_sub, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed")
p+ geom_abline(intercept = cf5[1,1],slope = cf5[2,1],color = "red")



####################################################################################################################################
######################## Combine old shell and truncated new shell data and plot ##############################################################
analysis_males_sub<-as.data.frame(rbind(ns_males_analysis_sub,os_males_analysis))


########################### Non transformed ############################################################################
q<-ggplot(analysis_males_sub, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = SC))
	q+ labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell)")

########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, log-transformed)")

ebsco_sc_points_sub <- ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, log transformed and truncated)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell, log-transformed)")


ebsco_sc_lines_sub <-ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell,log transformed and truncated)")


############################# with points and lines#####################################################################
ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell log-transformed)")

######################  Alternative approach #########################################################################
ggscatter(analysis_males_sub, x="log.length",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)

########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebsco_sc_points, ebsco_sc_lines, ebsco_sc_points_sub, ebsco_sc_lines_sub,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)



########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.length*SC,data = analysis_males_sub)
summary(mod1)									# p= 2e-16 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.length+SC,data = analysis_males_sub)
summary(mod2)									# p = 2e-16 for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p= 2.2e-16...removing interaction term sinificantly affects model

reg_mod<-lm(log.weight~SC/log.length-1,data=analysis_males_sub)		
summary(reg_mod)


########################################################################################################################
############################### Truncate further at length = 76 mm(based on old shell first quartile) ######################################################
ns_males_analysis_sub2<-subset(ns_males_analysis,LENGTH>76)
os_males_analysis_sub<-subset(os_males_analysis,LENGTH>76)

############################ Fit followup model - new shell ##########################################
fit6<-lm(log.weight~log.length,data=ns_males_analysis_sub2)
summary(fit6)
coef(fit6)

cf6<-as.matrix(coef(fit6))
			# log(W) = -8.588717  + 3.17365  * log(L) on transformed scale
    			# W= exp(-8.588717*L^(3.17365)  on original scale
			# a =  0.0001861948
			# b = 3.17365

############################ Fit followup model - old shell ##########################################
fit7<-lm(log.weight~log.length,data=os_males_analysis_sub)
summary(fit7)
coef(fit7)

cf7<-as.matrix(coef(fit7))
			# log(W) = -7.99453 + 3.055352  * log(L) on transformed scale
    			# W = exp(-7.99453*L^(3.055352)  on original scale
			# a =  0.0003373026
			# b = 3.055352 

p<-ggplot(ns_males_analysis_sub2, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed")
p+ geom_abline(intercept = cf6[1,1],slope = cf6[2,1],color = "red")

####################################################################################################################################
######################## Combine old shell and truncated new shell data and plot ##############################################################

analysis_males_sub2<-as.data.frame(rbind(ns_males_analysis_sub2,os_males_analysis_sub))


########################### Non transformed ############################################################################
q<-ggplot(analysis_males_sub2, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = SC))
	q+ labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell) males")

########################### Log transformed ##########################################################################
dev.new()

############################# po only ############################################################################
ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, log-transformed)")

ebsco_sc_points_sub2 <- ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell, truncated at 76 mm)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell, log-transformed)")


ebsco_sc_lines_sub2 <-ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO(New shell and old shell, truncated at 76 mm)")


############################# with points and lines#####################################################################
ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (New shell and old shell log-transformed)")

######################  Alternative approach #########################################################################
ggscatter(analysis_males_sub2, x="log.length",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)

########################################################################################################################
############################### 4 panel ################################################################################
ggarrange(ebsco_sc_points,ebsco_sc_lines,ebsco_sc_points_sub2, ebsco_sc_lines_sub2,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

dev.new()

ggarrange(ebsco_sc_points_sub,ebsco_sc_lines_sub,ebsco_sc_points_sub2, ebsco_sc_lines_sub2,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.length*SC,data = analysis_males_sub2)
summary(mod1)									# p = 7.18e-10 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.length+SC,data = analysis_males_sub2)
summary(mod2)									# p <2e-16 for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p = 7.177e-10...removing interaction term sinificantly affects model

reg_mod<-lm(log.weight~SC/log.length-1,data=analysis_males_sub2)		
summary(reg_mod)

########################################################################################################################
############################## USE ANCOVA TO COMPARE NEW SHELL AND NEW SHELL TRUNCATED SERIES ##########################
########################################################################################################################
ns_males_analysis$Series<- "Base"
ns_males_analysis_sub$Series<- "Sub1"
ns_males_analysis_sub2$Series<- "Sub2"

analysis_males_sub<-rbind(ns_males_analysis,ns_males_analysis_sub)
analysis_males_sub2<-rbind(ns_males_analysis_sub,ns_males_analysis_sub2)
analysis_males_sub3<-rbind(ns_males_analysis,ns_males_analysis_sub2)

########################################################################################################################
############################### Run ANCOVA analyses to determine if base and series 1 are statistically different ############################

mod1<-aov(log.weight~log.length*Series,data = analysis_males_sub)
summary(mod1)									# p = 0.00701 on interaction term=significant interaction--suggests different slopes

mod2<-aov(log.weight~log.length+Series,data = analysis_males_sub)
summary(mod2)									# p = 0.298 for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 0.007013...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~Series/log.length-1,data=analysis_males_sub)		
summary(reg_mod)


########################################################################################################################
############################### Run ANCOVA analyses to determine if series 1 and 2 are statistically different ############################

mod1<-aov(log.weight~log.length*Series,data = analysis_males_sub2)
summary(mod1)									# p = 0.0111 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.length+Series,data = analysis_males_sub2)
summary(mod2)									# p = 0.773 for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 0.7623...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~Series/log.length-1,data=analysis_males_sub2)		
summary(reg_mod)


########################################################################################################################
############################### Run ANCOVA analyses to determine if series 1 and 2 are statistically different ############################

mod1<-aov(log.weight~log.length*Series,data = analysis_males_sub3)
summary(mod1)									# p = 0.00352l on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.length+Series,data = analysis_males_sub3)
summary(mod2)									# p = 0.198  for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 0.003517...removing interaction term sinificantly affects model

reg_mod<-lm(log.weight~Series/log.length-1,data=analysis_males_sub2)		
summary(reg_mod)



