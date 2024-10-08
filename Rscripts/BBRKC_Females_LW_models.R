# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC females by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df<-read.csv("BBRKC_weightDB_analysis.csv")

df1<-subset(df, WEIGHT>0 & SEX==2)
colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$LENGTH)
#identify(df1$WEIGHT~df1$LENGTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(logwidth = log(WIDTH),
#          logweight = log(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
 #  filter(SEX == 2) -> female #Only SC2 as to not bias for weight of epibionts 
#male
ggplot(female, aes(x = LENGTH, y = WEIGHT, group = YEAR)) +
     geom_point(aes(colour = factor(SHELL_CONDITION)))


################################################################################

sc1_immatfemales<-subset(df1,SEX==2 & SHELL_CONDITION==1 & CLUTCH_SIZE==0)
sc2_immatfemales<-subset(df1,SEX==2 & SHELL_CONDITION==2 & CLUTCH_SIZE==0)

sc1_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==1 & CLUTCH_SIZE>0)
sc2_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==2 & CLUTCH_SIZE>0)
sc3_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==3 & CLUTCH_SIZE>0)
sc4_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==4 & CLUTCH_SIZE>0)
sc5_matfemales<-subset(df1,SEX==2 & SHELL_CONDITION==5 & CLUTCH_SIZE>0)


hist(sc1_matfemales$WEIGHT)
hist(sc2_matfemales$WEIGHT)
hist(sc3_matfemales$WEIGHT)
hist(sc4_matfemales$WEIGHT)
hist(sc5_matfemales$WEIGHT)

hist(log(sc2_matfemales$WEIGHT))
hist(sc3_matfemales$WEIGHT)
hist(sc4_matfemales$WEIGHT)

plot(sc2_matfemales$WEIGHT~sc2_matfemales$LENGTH)
plot(sc3_matfemales$WEIGHT~sc3_matfemales$LENGTH)
plot(sc4_matfemales$WEIGHT~sc4_matfemales$LENGTH)

########################## Aggregate by New shell/old shell #####################################
ns_immatfemales<-subset(df1,SEX==2 & CLUTCH_SIZE==0 & SHELL_CONDITION==2)
ns_matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>0 & SHELL_CONDITION==2)
os_matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>0 & SHELL_CONDITION==3|SHELL_CONDITION==4)
os_matfemales

all_matfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>0)





#####################################################################################################################
############################ First model L-W relationship by shell condition #########################################
#####################################################################################################################


######################################################################################################
############################ Immature females ########################################################
######################################################################################################
plot(ns_immatfemales$WEIGHT~ns_immatfemales$LENGTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_immatfemales, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) immature females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_immatfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(ns_immatfemales$LENGTH)
log.weight <- log(ns_immatfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(ns_immatfemales,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data
names(ns_imfemale)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_imfemale, aes(x = log.length, y = log.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) immature females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.length,data=ns_imfemale)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_imfemale)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_imfemale

ns_imfemale$Cooks_D <- cooks.distance(fit1)
ns_immatfemales_analysis<-subset(ns_imfemale, Cooks_D < (4/(nrow(ns_imfemale))))

nrow(ns_imfemale)-nrow(ns_immatfemales_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_immatfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_immatfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (SC2) immature females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_immatfemales_analysis, aes(x = log.length, y = log.weight)) +
  geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) immature females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit.im<-lm(log.weight~log.length,data=ns_immatfemales_analysis)
summary(fit.im)
coef(fit.im)

cf2<-as.matrix(coef(fit.im))

exp(-7.656292)
# log(W) = -7.656292  + 3.094701  * log(L) on transformed scale       #updated for females
# W = exp(-7.656292) * L^(3.094701)  on original scale                #updated for females
# a = 0.0004730583                                                 #updated for females
# b = 3.094701                                                     #updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf2
names(fit.im)
names(summary(fit.im))
###############################################################################################
v2<-(summary(fit.im)$sigma)**2  #Variance 
v2
int<-cf2[1,1]
A<-(exp(int)*exp(v2/2))
A                         #0.000474389 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.im)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NEW SHELLIMMATURE FEMALE MODEL ###############################
# a = 0.000474389    #updated for females
# b = 3.094701           #updated for females



######################################################################################################
############################ New shell ###############################################################
######################################################################################################

plot(ns_matfemales$WEIGHT~ns_matfemales$LENGTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_matfemales, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_matfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(ns_matfemales$LENGTH)
log.weight <- log(ns_matfemales$WEIGHT)
ns_female<-as.data.frame(cbind(ns_matfemales,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_female                    							  		 # inspect data
names(ns_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_female, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log.weight~log.length,data=ns_female)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))
plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_female$Cooks_D <- cooks.distance(fit1)
ns_matfemales_analysis<-subset(ns_female, Cooks_D < (4/(nrow(ns_female))))
 
nrow(ns_female)-nrow(ns_matfemales_analysis)    #60 obs removed based on Cook's Distance

nrow(ns_matfemales_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_matfemales_analysis, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log.weight~log.length,data=ns_matfemales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))
exp(-5.38851)
# log(W) = -5.38851  + 2.60755 * log(L) on transformed scale           #Updated for females
# W = exp(-5.38851) * L^(2.60755)  on original scale                   #Updated for females
# a = 0.004568776                                                      #Updated for females
# b = 2.60755                                                          #Updated for females
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
A                         #0.004579617

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
# a = 0.004579617                                                  #Updated for females
# b = 2.60755                                                     #Updated for females


###################################################################################################
############################ Old shell ############################################################
###################################################################################################

dev.new()
plot(os_matfemales$WEIGHT~os_matfemales$LENGTH)
p<-ggplot(os_matfemales, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) females")

############################# Add fields for analysis ########################################################
Year <- substring(os_matfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.length<-log(os_matfemales$LENGTH)
log.weight <- log(os_matfemales$WEIGHT)
os_female<-as.data.frame(cbind(os_matfemales,YEAR,log.length,log.weight))   		 # Bind new data objects and crab data in data frame  
os_female                    							  		 # inspect data
names(os_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(os_female, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed")

############################## Fit initial model ########################################################

fit3<-lm(log.weight~log.length,data=os_female)
summary(fit3)
coef(fit3)

############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))
plot(fit3)
plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(os_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_female$Cooks_D <- cooks.distance(fit3)
os_matfemales_analysis<-subset(os_female, Cooks_D < (4/(nrow(os_female))))
 
nrow(os_female)-nrow(os_matfemales_analysis)    #26 obs removed

nrow(os_matfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_matfemales_analysis, aes(x = LENGTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(os_matfemales_analysis, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="Old shell (SC3+SC4) BBRKC females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log.weight~log.length,data=os_matfemales_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log.weight~log.length,data=os_matfemales_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])

cf4
exp(-4.59768 )

# log(W) = -4.59768  + 2.461781 * log(L) on transformed scale                     #Updated for females
# W = exp(-4.59768 ) * L^(2.461781)  on original scale                            #Updated for females
# a = 0.01007518                                                               #Updated for females
# b = 2.461781                                                                   #Updated for females

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(exp(int)*exp(v4/2))
A                         #0.01009829

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
# a = 0.01009829                                                                  #Updated for females
# b = 2.461781                                                                      #Updated for females

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_matfemales_analysis$SC <- "OS"
ns_matfemales_analysis$SC <- "NS"

analysis_females<-rbind(ns_matfemales_analysis,os_matfemales_analysis)

write.csv(analysis_females,"BBRKC_Analysis_females.csv")

ggplot(analysis_females, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_females, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="Female BBRKC (New shell and old shell)")

######################Log transformed #################################################
dev.new()


ggplot(analysis_females, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell and old shell, log-transformed)")

ebsco_sc_points<-ggplot(analysis_females, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell and old shell, log-transformed)")

# lines ONLY
ggplot(analysis_females, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell and old shell, log-transformed)")

ebsco_sc_lines<-ggplot(analysis_females, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC(New shell and old shell, log-transformed)")

# with points and lines
ggplot(analysis_females, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Length)",y="Ln(Weight)", title="Female BBRKC (New shell and old shell log-transformed)")


################ Alternative approach with lines
ggscatter(analysis_females, x="log.length",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.length*SC,data = analysis_females)			# p = 1.94e-15--significant interaction -- slopes different
summary(mod1)

mod2<-aov(log.weight~log.length+SC,data = analysis_females)			# p< 2e-16: interceptes are significantly different
summary(mod2)

anova(mod1,mod2)										# p = 1.94e-15--interaction term is significant to model

reg_mod<-lm(log.weight~SC/log.length-1,data=analysis_females)
summary(reg_mod)




########################################################################################################################
############################ Assess SC2 males ONLY by cold/warm year periods - Period 1 ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_females<-subset(ns_matfemales_analysis, YEAR==2001|YEAR==2002|YEAR==2003|YEAR==2004|YEAR==2005|YEAR==2013|YEAR==2014|YEAR==2015|YEAR==2016|YEAR==2018|YEAR==2019)
cold_females<-subset(ns_matfemales_analysis, YEAR==2000|YEAR==2006|YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2017)

warm_ns_females<-subset(warm_females,SC=="NS")

cold_ns_females<-subset(cold_females,SC=="NS")

warm_ns_females$TEMP <- "WARM"
cold_ns_females$TEMP <- "COLD"
cold_ns_females$COLOR <- 1
warm_ns_females$COLOR <- 4

temp_ns_females<-rbind(warm_ns_females,cold_ns_females)

nrow(cold_ns_females)
nrow(warm_ns_females)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log.weight~log.length,data=cold_ns_females)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log.weight~log.length,data=warm_ns_females)
summary(fit.warm)
coef.warm<-as.matrix(coef(fit.warm))

###################################################################################################
######################## Apply bias-correction procedure ##########################################
###################################################################################################

###################################################################################################
######################### COLD YEARS ##############################################################
v.cold<-(summary(fit.cold)$sigma)**2  #Variance 
v.cold
int<-cf.cold[1,1]
A<-(exp(int)*exp(v.cold/2))
A                         #0.0003686092

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.cold/2))
sdA

sdA_base<-exp(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-coef.warm[1,1]
A<-(10^(int)*10^(v.warm/2))
A                         #0.0005069227
####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.warm)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.warm/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR COLD NEW SHELL MODEL ###############################
# a = 0.0003691035 
# b = 3.157545
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0005074622  
# b = 3.09928

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.length*TEMP,data = temp_ns_females)							# p = 0.000192 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log.weight~log.length+TEMP,data = temp_ns_females)							# p < 2e-16 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.0001924...removing interaction term significantly affects model								

reg_mod<-lm(log.weight~TEMP/log.length-1,data = temp_ns_females)		
summary(reg_mod)

mod10<-anova(lm(log.weight~log.length,data = temp_ns_females),lm(log.weight~log.length*TEMP,data = temp_ns_females))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_females, x="log.length",y="log.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell,log-transformed)")

bbrkc_temp_points<-ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_females, aes(x = LENGTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Mature female BBRKC (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell,log-transformed)")

bbrkc_temp_lines <- ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell, log-transformed)")

temp_ns_females
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(bbrkc_sc_points,bbrkc_sc_lines,bbrkc_temp_points, bbrkc_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)



########################################################################################################################
############################ Assess SC2 males ONLY by cold/warm year periods - Period 2 - Five most extreme years for warm and cold ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_females<-subset(ns_matfemales_analysis, YEAR==2003|YEAR==2005|YEAR==2016|YEAR==2018|YEAR==2019)
cold_females<-subset(ns_matfemales_analysis, YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2012)

warm_ns_females<-subset(warm_females,SC=="NS")

cold_ns_females<-subset(cold_females,SC=="NS")

warm_ns_females$TEMP <- "WARM"
cold_ns_females$TEMP <- "COLD"
cold_ns_females$COLOR <- 1
warm_ns_females$COLOR <- 4

temp_ns_females<-rbind(warm_ns_females,cold_ns_females)

nrow(cold_ns_females)
nrow(warm_ns_females)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log.weight~log.length,data=cold_ns_females)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log.weight~log.length,data=warm_ns_females)
summary(fit.warm)
coef.warm<-as.matrix(coef(fit.warm))

###################################################################################################
######################## Apply bias-correction procedure ##########################################
###################################################################################################

###################################################################################################
######################### COLD YEARS ##############################################################
v.cold<-(summary(fit.cold)$sigma)**2  #Variance 
v.cold
int<-cf.cold[1,1]
A<-(10^(int)*10^(v.cold/2))
A                         #0.0003686092

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.cold)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.cold/2))
sdA

sdA_base<-exp(sd)
sdA_base
################################################################################################
######################## WARM YEARS ############################################################

v.warm<-(summary(fit.warm)$sigma)**2  #Variance 
v.warm
int<-coef.warm[1,1]
A<-(exp(int)*exp(v.warm/2))
A                         #0.0005069227
####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit.warm)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
#sd
sdA<-(exp(sd)*exp(v.warm/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR COLD NEW SHELL MODEL ###############################
# a = 0.0003691035 
# b = 3.157545
##################### BIAS-CORRECTED PARAMETERS FOR WARM NEW SHELL MODEL ###############################
# a = 0.0005074622  
# b = 3.09928

########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.length*TEMP,data = temp_ns_females)							# p = 0.000192 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log.weight~log.length+TEMP,data = temp_ns_females)							# p < 2e-16 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.0001924...removing interaction term significantly affects model								

reg_mod<-lm(log.weight~TEMP/log.length-1,data = temp_ns_females)		
summary(reg_mod)

mod10<-anova(lm(log.weight~log.length,data = temp_ns_females),lm(log.weight~log.length*TEMP,data = temp_ns_females))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_females, x="log.length",y="log.weight",color="TEMP", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
  )


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell,log-transformed)")

bbrkc_temp_points<-ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP)) +
  geom_point(aes(colour = TEMP))+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_females, aes(x = LENGTH, y = WEIGHT, group = TEMP)) +
  geom_point(aes(colour = factor(TEMP)))+
  labs(x="Length",y="Weight", title="Mature female BBRKC (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell,log-transformed)")

bbrkc_temp_lines <- ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell, log-transformed)")

temp_ns_females
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_females, aes(x = log.length, y = log.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Mature female BBRKC (New shell, log-transformed)")

sp+scale_color_lancet()

########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(bbrkc_sc_points,bbrkc_sc_lines,bbrkc_temp_points, bbrkc_temp_lines +rremove("x.text"),
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)


