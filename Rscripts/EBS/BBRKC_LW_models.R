# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggsci)
setwd("C:/Users/Jon.Richar/Work/GitRepos/LengthWeight/EBSCrabLengthWeight/DATA")

df<-read.csv("BBRKC_weightDB_analysis.csv")
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
 
nrow(ns_male)-nrow(ns_males_analysis)    #94 obs removed based on Cook's Distance

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
			# log(W) = -7.849620  + 3.147886 * log(L) on transformed scale
    			# W = exp(-7.849620)*L^(3.147886)  on original scale
			# a = 0.0003899001
			# b = 3.147886
p<-ggplot(ns_males_analysis, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed")
p+ geom_abline(intercept = -7.849620,slope = 3.147886,color = "red")


######################## Apply bias-correction procedure #####################################
cf2
names(fit2)
names(summary(fit2))
####################### Parameter A /intercept########################################################################
v2<-(summary(fit2)$sigma)**2  # Extract model sigma and calculate Variance 
v2
int<-cf2[1,1]                 # Extract intercept parameter
A<-(exp(int)*exp(v2/2))       # calculate bias corrected A parameter
A                         #0.0003908445

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
# a = 0.0003908445
# b = 3.147886
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
 
nrow(os_male)-nrow(os_males_analysis)    #32 obs removed

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

			# log(W) = -7.63984  + 3.11117 * log(L) on transformed scale
    			# W = exp(-7.63984)*L^(3.11117)  on original scale
			# a = 0.000481
			# b = 3.11117

plot(log.weight~log.length,data=os_males_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])

cf4
exp(-7.639845)

# log(W) = -7.639845  + 3.111166 * log(L) on transformed scale
# W = exp(-7.639845) * L^(3.111166)  on original scale
# a = 0.000480903
# b = 3.111166
################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(exp(int)*exp(v4/2))
A                         #0.0004818917 

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
# a = 0.0004818917 
# b = 3.111166
########################################################################################################################################
############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_males_analysis$SC <- "OS"
ns_males_analysis$SC <- "NS"

analysis_males<-rbind(ns_males_analysis,os_males_analysis)
names(analysis_males)


ggplot(analysis_males, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))


q<-ggplot(analysis_males, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell) males")

######################################################################################################################
########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell, log-transformed)")

bbrkc_sc_points <- ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell, log-transformed)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC(New shell and old shell, log-transformed)")


bbrkc_sc_lines <-ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC(New shell and old shell, log-transformed)")


############################# with points and lines#####################################################################
ggplot(analysis_males, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell log-transformed)")

######################  Alternative approach
ggscatter(analysis_males, x="log.length",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)



########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.length*SC,data = analysis_males)
summary(mod1)									# p=0.149 on interaction term= No significant interaction--suggests same slopes

mod2<-aov(log.weight~log.length+SC,data = analysis_males)
summary(mod2)									# p = 6.21e-15 for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p= 0.1487...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~SC/log.length-1,data=analysis_males)		
summary(reg_mod)

# possible interpretation for different intercepts but converging slopes: epibionts affect weight at smaller sizes to greater degree than for larger crab



########################################################################################################################
########################################################################################################################
############################ Assess SC2 males ONLY by cold/warm year periods ###########################################
########################################################################################################################

# Basis for this analysis is that cold conditions may reduce fitness/growth as of survey
# rerun new shell analyses as above but with new shell data divided by warm/cold year as determined
# by whether a leg 3 retow was required for females. Apply ANCOVA procedures to test for difference 
# between regression lines.
# Cold years (from tech memo, years with retows):2000, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2017
# Warm years (from tech memo, years without retow): 2001, 2002, 2003, 2004, 2005, 2013, 2014, 2015, 2016, 2018, 2019

warm_males<-subset(analysis_males, YEAR==2001|YEAR==2002|YEAR==2003|YEAR==2004|YEAR==2005|YEAR==2013|YEAR==2014|YEAR==2015|YEAR==2016|YEAR==2018|YEAR==2019)
cold_males<-subset(analysis_males, YEAR==2000|YEAR==2006|YEAR==2007|YEAR==2008|YEAR==2009|YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2017)

warm_ns_males<-subset(warm_males,SC=="NS")

cold_ns_males<-subset(cold_males,SC=="NS")

warm_ns_males$TEMP <- "WARM"
cold_ns_males$TEMP <- "COLD"
cold_ns_males$COLOR <- 1
warm_ns_males$COLOR <- 4

temp_ns_males<-rbind(warm_ns_males,cold_ns_males)

nrow(cold_ns_males)
nrow(warm_ns_males)
###################################################################################################################
############################### fit size-weight models ############################################################
fit.cold<-lm(log.weight~log.length,data=cold_ns_males)
summary(fit.cold)
coef(fit.cold)
cf.cold<-as.matrix(coef(fit.cold))

fit.warm<-lm(log.weight~log.length,data=warm_ns_males)
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
A                         #0.0003691035 

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
A                         #0.0005074622 
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

mod1<-aov(log.weight~log.length*TEMP,data = temp_ns_males)							# p = 0.000192 on interaction term = significant interaction--suggests different slopes
summary(mod1)									

mod2<-aov(log.weight~log.length+TEMP,data = temp_ns_males)							# p = 2e-16 for TEMP: temperature-based regression lines have different slopes
summary(mod2)									

anova(mod1,mod2)														# p = 0.0001924...removing interaction term significantly affects model								

reg_mod<-lm(log.weight~TEMP/log.length-1,data = temp_ns_males)		
summary(reg_mod)

mod10<-anova(lm(log.weight~log.length,data = temp_ns_males),lm(log.weight~log.length*TEMP,data = temp_ns_males))
summary(mod10)
#################################Plot in GG plot ###############################################################################################
dev.new()

ggscatter(temp_ns_males, x="log.length",y="log.weight",color="TEMP", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(TEMP))
)


dev.new()

############################### Points only - log-transformed ###########################################################
ggplot(temp_ns_males, aes(x = log.length, y = log.weight, group = TEMP)) +
     geom_point(aes(colour = factor(TEMP)))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell,log-transformed)")

bbrkc_temp_points<-ggplot(temp_ns_males, aes(x = log.length, y = log.weight, group = TEMP)) +
     geom_point(aes(colour = TEMP))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell,log-transformed)")

# Points only - non-transformed 
ggplot(temp_ns_males, aes(x = LENGTH, y = WEIGHT, group = TEMP)) +
     geom_point(aes(colour = factor(TEMP)))+
	labs(x="Length",y="Weight", title="Male BBRKC (New shell)")

################################ lines ONLY #############################################################################
ggplot(temp_ns_males, aes(x = log.length, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell,log-transformed)")

bbrkc_temp_lines <- ggplot(temp_ns_males, aes(x = log.length, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell,log-transformed)")

################################ with points and lines ##################################################################
ggplot(temp_ns_males, aes(x = log.length, y = log.weight, group = TEMP,color = TEMP,shape=TEMP)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell, log-transformed)")

temp_ns_males
################################ with points and lines - custom color ##################################################################
sp<-ggplot(temp_ns_males, aes(x = log.length, y = log.weight, group = TEMP,colour = TEMP,shape=TEMP)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell, log-transformed)")

sp+scale_color_lancet()
########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(bbrkc_sc_points,bbrkc_sc_lines,bbrkc_temp_points, bbrkc_temp_lines +rremove("x.text"),
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)





##########################################################################################################################################################
############################### Rerun shell condition models while limit data to same size ranges ##########################################################
###########################################################################################################################################################




summary(os_males_analysis$LENGTH)
mean(os_males_analysis$LENGTH)

summary(ns_males_analysis$LENGTH)
mean(ns_males_analysis$LENGTH)

dev.new()
par(mfrow=c(2,1))
hist(os_males_analysis$LENGTH,breaks=20)
abline(v=mean(os_males_analysis$LENGTH),col=4)

hist(ns_males_analysis$LENGTH,breaks=30)
abline(v=mean(ns_males_analysis$LENGTH),col=4)

############################# subset new shell data to match size range of old shell
ns_males_analysis_sub<-subset(ns_males_analysis,LENGTH>79.0)


############################ Fit followup model ##########################################
fit5<-lm(log.weight~log.length,data=ns_males_analysis_sub)
summary(fit5)
coef(fit5)

cf5<-as.matrix(coef(fit5))
			# log(W) = -7.832641  + 3.144402 * log(L) on transformed scale
    			# W = exp(-7.832641)*L^(3.144402 )  on original scale
			# a = 0.0003965767
			# b = 3.144402

p<-ggplot(ns_males_analysis_sub, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed")
p+ geom_abline(intercept = -7.832641,slope = 3.144402,color = "red")

####################################################################################################################################
######################## Combine old shell and truncated new shell data and plot ##############################################################
analysis_males_sub<-as.data.frame(rbind(ns_males_analysis_sub,os_males_analysis))


########################### Non transformed ############################################################################
q<-ggplot(analysis_males_sub, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = SC))
	q+ labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell) males")

########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell, log-transformed)")

bbrkc_sc_points_sub <- ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell, log transformed and truncated at 79mm)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC(New shell and old shell, log-transformed)")


bbrkc_sc_lines_sub <-ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC(New shell and old shell,log transformed and truncated at 79mm)")


############################# with points and lines#####################################################################
ggplot(analysis_males_sub, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Length)",y="Ln(Weight)", title="Male opilio (New shell and old shell log-transformed)")

######################  Alternative approach #########################################################################
ggscatter(analysis_males_sub, x="log.length",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)

########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(bbrkc_sc_points,bbrkc_sc_lines,bbrkc_sc_points_sub, bbrkc_sc_lines_sub ,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)



########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log.weight~log.length*SC,data = analysis_males_sub)
summary(mod1)									# p=0.203 on interaction term= No significant interaction--suggests same slopes

mod2<-aov(log.weight~log.length+SC,data = analysis_males_sub)
summary(mod2)									# p = 9.58e-15 for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p= 0.1487...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~SC/log.length-1,data=analysis_males_sub)		
summary(reg_mod)




########################################################################################################################
############################# Truncate further at length = 132 mm ######################################################


ns_males_analysis_sub2<-subset(ns_males_analysis,LENGTH>132)
os_males_analysis_sub<-subset(os_males_analysis,LENGTH>132)

############################ Fit followup model -New shell males ##########################################
fit6<-lm(log.weight~log.length,data=ns_males_analysis_sub2)
summary(fit6)
coef(fit6)

cf6<-as.matrix(coef(fit6))
			# log(W) = -7.06208  + 2.98957  * log(L) on transformed scale
    			# W = exp(-7.06208)*L^(2.98957)  on original scale
			# a = 0.0008569937
			# b = 2.98957

############################ Fit followup model - Old shell males ##########################################
fit7<-lm(log.weight~log.length,data=os_males_analysis_sub)
summary(fit7)
coef(fit7)

cf7<-as.matrix(coef(fit7))
			# log(W) = -7.06208  + 2.98957  * log(L) on transformed scale
    			# W = exp(-7.06208)*L^(2.98957)  on original scale
			# a = 0.0008569937
			# b = 2.98957
p<-ggplot(ns_males_analysis_sub2, aes(x = log.length, y = log.weight)) +
      geom_point()
p+ labs(x="ln(length)",y="ln(weight)", title="New shell (SC2) males-log transformed")
p+ geom_abline(intercept = -7.832641,slope = 3.144402,color = "red")

####################################################################################################################################
######################## Combine old shell and truncated new shell data and plot ##############################################################
analysis_males_sub2<-as.data.frame(rbind(ns_males_analysis_sub2,os_males_analysis_sub))


########################### Non transformed ############################################################################
q<-ggplot(analysis_males_sub2, aes(x = LENGTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = SC))
	q+ labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell) males")

########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell, log-transformed)")

bbrkc_sc_points_sub2 <- ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC (New shell and old shell, log-transformed and truncated at 132 mm)")

############################# lines ONLY ###############################################################################
ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC(New shell and old shell, log-transformed)")


bbrkc_sc_lines_sub2 <-ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Length)",y="Ln(Weight)", title="Male BBRKC(New shell and old shell, log-transformed and truncated at 132 mm)")


############################# with points and lines#####################################################################
ggplot(analysis_males_sub2, aes(x = log.length, y = log.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Length)",y="Ln(Weight)", title="Male opilio (New shell and old shell log-transformed)")

######################  Alternative approach #########################################################################
ggscatter(analysis_males_sub2, x="log.length",y="log.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)

########################################################################################################################
############################### 4 panel ################################################################################
ggarrange(bbrkc_sc_points, bbrkc_sc_lines,bbrkc_sc_points_sub2, bbrkc_sc_lines_sub2,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)

ggarrange(bbrkc_sc_points_sub, bbrkc_sc_lines_sub,bbrkc_sc_points_sub2, bbrkc_sc_lines_sub2,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)


########################################################################################################################
############################## USE ANCOVA TO COMPARE NEW SHELL AND NEW SHELL TRUNCATED SERIES ##########################
########################################################################################################################
ns_males_analysis$Series<- "Base"
ns_males_analysis_sub$Series<- "Sub1"
ns_males_analysis_sub2$Series<- "Sub2"

analysis_males_sub<-rbind(ns_males_analysis,ns_males_analysis_sub)
analysis_males_sub2<-rbind(ns_males_analysis_sub,ns_males_analysis_sub2)
analysis_males_sub3<-rbind(ns_males_analysis,ns_males_analysis_sub2)

analysis_males_sub4<-rbind(os_males_analysis_sub,ns_males_analysis_sub2[,1:21])			# Series for 110 mm old shell and new shell

###########################################################################################################################################
############################### Run ANCOVA analyses to determine if 132 mm cutoff NS and OS are statistically different ############################

mod1<-aov(log.weight~log.length*SC,data = analysis_males_sub4)
summary(mod1)									# p = 0.0127 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.length+SC,data = analysis_males_sub4)
summary(mod2)									# p = <2e-16 for series, regression lines have differing intercepts

anova(mod1,mod2)									# p = 8.941e-05...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~SC/log.length-1,data=analysis_males_sub4)		
summary(reg_mod)


########################################################################################################################
############################### Run ANCOVA analyses to determine if base and series 1 are statistically different ############################

mod1<-aov(log.weight~log.length*Series,data = analysis_males_sub)
summary(mod1)									# p=0.762 on interaction term= No significant interaction--suggests same slopes

mod2<-aov(log.weight~log.length+Series,data = analysis_males_sub)
summary(mod2)									# p = 0.869 for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p= 0.7623...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~Series/log.length-1,data=analysis_males_sub)		
summary(reg_mod)


########################################################################################################################
############################### Run ANCOVA analyses to determine if series 1 and 2 are statistically different ############################

mod1<-aov(log.weight~log.length*Series,data = analysis_males_sub2)
summary(mod1)									# p=0.000238 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.length+Series,data = analysis_males_sub2)
summary(mod2)									# p =  0.881 for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p= 0.0002384...removing interaction term does not sinificantly affect model

reg_mod<-lm(log.weight~Series/log.length-1,data=analysis_males_sub2)		
summary(reg_mod)


########################################################################################################################
############################### Run ANCOVA analyses to determine if series base and 3 are statistically different ############################

mod1<-aov(log.weight~log.length*Series,data = analysis_males_sub3)
summary(mod1)									# p = 0.000135 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log.weight~log.length+Series,data = analysis_males_sub3)
summary(mod2)									# p = 0.739  for series, regression lines do not have differing intercepts

anova(mod1,mod2)									# p = 0.0001347...removing interaction term sinificantly affects model

reg_mod<-lm(log.weight~Series/log.length-1,data=analysis_males_sub2)		
summary(reg_mod)

