# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/4/2021
# Calculate width/weight regression models for EBSCO females by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

#setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA/")


setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA/NBS_Data")

df<-read.csv("NBS_CO_DATA_FOR_SIZEWEIGHT_FEMALE.csv")

df1<-subset(df, WEIGHT>0 & SEX==2)

colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$WIDTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(logwidth = log(WIDTH),
#          logweight = log(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
 # filter(SEX == 1, SHELL_CONDITION == 2) -> male #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(male, aes(x = WIDTH, y = WEIGHT, group = YEAR)) +
#      geom_point(aes(colour = factor(YEAR)))




########################## Aggregate by New shell/old shell #####################################

immatfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<1)
alleggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>=1 & SHELL_CONDITION==2|SHELL_CONDITION==3|SHELL_CONDITION==4)

ns_matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>=1 & SHELL_CONDITION==2)
os_matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>=1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

#immatfemales<-subset(df1,SEX==2 & CLUTCH_SIZE==1)
#nrow(immatfemales)


#####################################################################################################################
############################ First model L-W relationship by shell condition #########################################
#####################################################################################################################

######################################################################################################
############################ Immature females ########################################################
######################################################################################################
plot(immatfemales$WEIGHT~immatfemales$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(immatfemales, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Immature females")

############################# Add fields for analysis ########################################################
Year <- substring(immatfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(immatfemales$WIDTH)
log.weight <- log(immatfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(immatfemales,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data
names(ns_imfemale)											 # Check column names


############################ Plot log transformed data in GGplot #############################################
dev.new()
p<-ggplot(ns_imfemale, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Non egg-bearing females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.width,data=ns_imfemale)
summary(fit1)
coef(fit1)

dev.new()
par(mfrow=c(2,2))
plot(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(1,1))

plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_imfemale)), col=1,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_imfemale$Cooks_D <- cooks.distance(fit1)
immatfemales_analysis<-subset(ns_imfemale, Cooks_D < (4/(nrow(ns_imfemale))))

nrow(ns_imfemale)-nrow(immatfemales_analysis)    #110 obs removed based on Cook's Distance
nrow(immatfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(immatfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (N infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(immatfemales_analysis, aes(x = log.width, y = log.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Immaturefemales-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log.weight~log.width,data=immatfemales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))

exp(cf2[1,1])
# log(W) = -7.117287 + 2.768713 * log(L) on transformed scale       #updated for females
# W = exp(-7.117287) * L^(2.768713)  on original scale                #updated for females
# a = 0.0008109638                                                 #updated for females
# b = 2.768713                                                       #updated for females
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
A                         # 0.0008152741

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR IMMATURE FEMALE MODEL ###############################
# a = 0.0008152741      #updated for NBS immature females
# b = 2.768713          #updated for NBS immature females


######################################################################################################
############################ New shell mature females ###############################################################
######################################################################################################

plot(ns_matfemales$WEIGHT~ns_matfemales$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_matfemales, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_matfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_matfemales$WIDTH)
log.weight <- log(ns_matfemales$WEIGHT)
ns_female<-as.data.frame(cbind(ns_matfemales,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_female                    							  		 # inspect data
names(ns_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_female, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.width,data=ns_female)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)

dev.new()
par(mfrow=c(1,1))
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_female$Cooks_D <- cooks.distance(fit1)
ns_matfemales_analysis<-subset(ns_female, Cooks_D < (4/(nrow(ns_female))))
 
nrow(ns_female)-nrow(ns_matfemales_analysis)    #26 obs removed based on Cook's Distance
nrow(ns_matfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log.weight~log.width,data=ns_matfemales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))

exp(cf2[1,1])

# log(W) = -7.486701 + 2.880030 * log(L) on transformed scale       #updated for NBS mature females
# W = exp(-7.486701) * L^(2.880030)  on original scale                #updated for NBS mature females
# a = 0.0005604888                                                #updated for NBS mature females
# b = 2.880030                                                     #updated for NBS mature females
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
A                         #0.0005618623

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
# a = 0.0005618623   #updated for NBS mature NS females
# b = 2.880030         #updated for NBS mature NS females

###################################################################################################
############################ Old shell mature females ############################################################
###################################################################################################

dev.new()
plot(os_matfemales$WEIGHT~os_matfemales$WIDTH)
p<-ggplot(os_matfemales, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) females")

############################# Add fields for analysis ########################################################
Year <- substring(os_matfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(os_matfemales$WIDTH)
log.weight <- log(os_matfemales$WEIGHT)
os_female<-as.data.frame(cbind(os_matfemales,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
os_female                    							  		 # inspect data
names(os_female)											 # Check column names

nrow(os_female)
############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(os_female, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed")

############################## Fit initial model ########################################################

fit3<-lm(log.weight~log.width,data=os_female)
summary(fit3)
coef(fit3)

############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit3)

dev.new()
par(mfrow=c(1,1))

plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(os_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_female$Cooks_D <- cooks.distance(fit3)
os_matfemales_analysis<-subset(os_female, Cooks_D < (4/(nrow(os_female))))
 
nrow(os_female)-nrow(os_matfemales_analysis)    #32 obs removed

nrow(os_matfemales_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_matfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(os_matfemales_analysis, aes(x = log.width, y = log.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log.weight~log.width,data=os_matfemales_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log.weight~log.width,data=os_matfemales_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])
exp(cf4[1,1])
# log(W) = -7.145285 + 2.814424 * log(L) on transformed scale       #updated for NBS mature OS females
# W = exp(-7.145285) * L^(2.814424 )  on original scale              #updated for NBS mature OS females
# a = 0.0007885736                                                 #updated for NBS mature OS females
# b = 2.814424                                                      #updated for NBS mature OS females

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(exp(int)*exp(v4/2))
A                         #0.0007909957                             #updated for NBS mature OS females

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
# a = 0.0007909957         #updated for NBS mature OS females
# b = 2.814424             #updated for NBS mature OS females


############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_matfemales_analysis$SC <- "OS"
ns_matfemales_analysis$SC <- "NS"

analysis_matfemales<-rbind(ns_matfemales_analysis,os_matfemales_analysis)

write.csv(analysis_matfemales,"NBS_CO_Analysis_matfemales.csv")

######################### Plot #################################################################
ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="NBS CO(New shell and old shell) clutch-bearing females")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO(New shell and old shell) log-transformed")


ebsCO_sc_points<-ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO(New shell and old shell) log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO (New shell and old shell, log-transformed)")

ebsCO_sc_lines<-ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO(New shell and old shell, log-transformed)")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_matfemales, aes(x = log.width, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO(New shell and old shell) log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_matfemales, x="log.width",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*SC,data = analysis_matfemales)
summary(mod1)									# p-value = 3.28e-05 on interaction term. Evidence slopes are different

mod2<-aov(log.weight~log.width+SC,data = analysis_matfemales)
summary(mod2)									# p-value for SC = <2e-16. Intercepts are significantly different

anova(mod1,mod2)									# p-value = 3.279e-05....removing interaction impacts model

reg_mod<-lm(log.weight~SC/log.width-1,data=analysis_matfemales)
summary(reg_mod)


##########################################################################################################################
############################## Compare immature and mature crab models ###################################################

immatfemales_analysis$Maturity<-"Immature"
colnames(immatfemales_analysis)
analysis_matfemales$Maturity<-"Mature"
colnames(analysis_matfemales)

immatfem<-cbind(immatfemales_analysis$log.width,immatfemales_analysis$log.weight,immatfemales_analysis$WIDTH,immatfemales_analysis$WEIGHT,immatfemales_analysis$Maturity)
colnames(immatfem)<-c("log.width","log.weight","WIDTH","WEIGHT","Maturity")
nrow(immatfem)

matfem<-cbind(analysis_matfemales$log.width,analysis_matfemales$log.weight,analysis_matfemales$WIDTH,analysis_matfemales$WEIGHT,analysis_matfemales$Maturity)
colnames(matfem)<-c("log.width","log.weight","WIDTH","WEIGHT","Maturity")

analysis_females<-as.data.frame(as.matrix(rbind(immatfem,matfem)))

analysis_females

########################### Plot #########################################
ggplot(analysis_females, aes(x = WIDTH, y = WEIGHT, group = Maturity)) +
  geom_point(aes(colour = factor(Maturity)))

q<-ggplot(analysis_females, aes(x = WIDTH, y = WEIGHT, group = Maturity)) +
  geom_point(aes(colour = factor(Maturity)))
q+ labs(x="Width(mm)",y="Weight(g)", title="NBS CO mature and immature females")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_females, aes(x = log.width, y = log.weight, group = Maturity)) +
  geom_point(aes(colour = factor(Maturity)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO mature and immature females, log-transformed")


ebsCO_sc_points<-ggplot(analysis_females, aes(x = log.width, y = log.weight, group = Maturity)) +
  geom_point(aes(colour = factor(Maturity)))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO mature and immature log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_females, aes(x = log.width, y = log.weight, group = Maturity,color = Maturity,shape=Maturity)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO mature and immature females, log-transformed)")

ebsCO_sc_lines<-ggplot(analysis_females, aes(x = log.width, y = log.weight, group = Maturity,color = Maturity,shape=Maturity)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO( mature and immature females")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_females, aes(x = log.width, y = log.weight, group = Maturity,color = Maturity,shape=Maturity)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Female NBS CO mature and immature females, log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_females, x="log.width",y="log.weight",color="Maturity", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(Maturity))
  )


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.width*Maturity,data = analysis_females)
summary(mod1)									# p-value = 0.995 on interaction term. Evidence slopes are not different

mod2<-aov(log.weight~log.width+Maturity,data = analysis_females)
summary(mod2)									# p-value for SC = <2e-16. Intercepts are significantly different

anova(mod1,mod2)									# p-value = 0.9953....removing interaction does not impact model

reg_mod<-lm(log.weight~Maturity/log.width-1,data=analysis_females)
summary(reg_mod)