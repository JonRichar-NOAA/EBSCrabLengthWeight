# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/4/2021
# Calculate width/weight regression models for EBSCB females by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)

setwd("C:/Users/Jon.Richar/Work/GitRepos/EBSCrabLengthWeight/DATA")
df<-read.csv("EBSCB_weightDB_analysis.csv")

df1<-subset(df, WEIGHT>0 & SEX==2)

colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$WIDTH)
######################### Tidyverse approach ###################################
#df1 %>%
#  mutate(logwidth = log10(WIDTH),
#          logweight = log10(WEIGHT),
#          Year = substring(CRUISE, 1,4), 
#          YEAR = as.factor(Year)) %>%
 # filter(SEX == 1, SHELL_CONDITION == 2) -> male #Only SC2 as to not bias for weight of epibionts 
#male
#ggplot(male, aes(x = WIDTH, y = WEIGHT, group = YEAR)) +
#      geom_point(aes(colour = factor(YEAR)))




########################## Aggregate by New shell/old shell #####################################

noeggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<=1)
ns_noeggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<=1& SHELL_CONDITION==2)

ns_immatfemales<-subset(df1,SEX==2 & CLUTCH_SIZE==0& SHELL_CONDITION==2)

ns_eggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==2)
os_eggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

ns_egg5females<-subset(df1,SEX==2 & CLUTCH_SIZE==5 & SHELL_CONDITION==2)
os_egg5females<-subset(df1,SEX==2 & CLUTCH_SIZE==5 & SHELL_CONDITION==3|SHELL_CONDITION==4)

ns_egg6females<-subset(df1,SEX==2 & CLUTCH_SIZE==6 & SHELL_CONDITION==2)
os_egg6females<-subset(df1,SEX==2 & CLUTCH_SIZE==6 & SHELL_CONDITION==3|SHELL_CONDITION==4)

#noeggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE==1)
#nrow(noeggfemales)
hist(ns_egg5females$WEIGHT)
hist(os_egg5females$WEIGHT)
hist(ns_egg6females$WEIGHT)
hist(os_egg6females$WEIGHT)


#####################################################################################################################
############################ First model L-W relationship by shell condition #########################################
#####################################################################################################################


######################################################################################################
############################ Nonegg-bearing females ALL SC ########################################################
######################################################################################################
plot(noeggfemales$WEIGHT~noeggfemales$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(noeggfemales, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Non-egg bearing")

############################# Add fields for analysis ########################################################
Year <- substring(noeggfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(noeggfemales$WIDTH)
log.weight <- log(noeggfemales$WEIGHT)

log10.width<-log10(noeggfemales$WIDTH)
log10.weight <- log10(noeggfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(noeggfemales,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data
names(ns_imfemale)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_imfemale, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Non egg-bearing females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log10.weight~log10.width,data=ns_imfemale)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_imfemale$Cooks_D <- cooks.distance(fit1)
noeggfemales_analysis<-subset(ns_imfemale, Cooks_D < (4/(nrow(ns_imfemale))))

nrow(ns_imfemale)-nrow(noeggfemales_analysis)    #102 obs removed based on Cook's Distance
nrow(noeggfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(noeggfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (N infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(noeggfemales_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Non-egg bearing females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log10.weight~log10.width,data=noeggfemales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))

exp(cf2[1,1])
# log10(W) = -7.624769   + 2.854783   * log10(L) on transformed scale       #updated for females
# W = exp(-7.624769 ) * L^(2.854783 )  on original scale                #updated for females
# a = 0.0004882081                                                  #updated for females
# b = 2.854783                                                       #updated for females
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
A                         #0.0004902272 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit2)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NON_EGG BEARING FEMALE MODEL ###############################
# a = 0.0004902272   #updated for females
# b = 2.854783          #updated for females



######################################################################################################
############################ Nonegg-bearing females SC2 only ########################################################
######################################################################################################
plot(ns_noeggfemales$WEIGHT~ns_noeggfemales$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_noeggfemale, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="Non-egg bearing, SC2")

############################# Add fields for analysis ########################################################
Year <- substring(ns_noeggfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_noeggfemales$WIDTH)
log.weight <- log(ns_noeggfemales$WEIGHT)

log10.width<-log10(ns_noeggfemales$WIDTH)
log10.weight <- log10(ns_noeggfemales$WEIGHT)
ns_imfemale<-as.data.frame(cbind(ns_noeggfemales,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_imfemale                    							  		 # inspect data
names(ns_imfemale)											 # Check column names


############################ Plot log transformed data in GGplot #############################################
dev.new()
p<-ggplot(ns_imfemale, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Non egg-bearing females-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log10.weight~log10.width,data=ns_imfemale)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_imfemale$Cooks_D <- cooks.distance(fit1)
ns_noeggfemales_analysis<-subset(ns_imfemale, Cooks_D < (4/(nrow(ns_imfemale))))

nrow(ns_imfemale)-nrow(ns_noeggfemales_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_noeggfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_noeggfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
  geom_point()
p+ labs(title="New shell (N infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_noeggfemales_analysis, aes(x = log10.width, y = log10.weight)) +
  geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Non-egg bearing females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit20<-lm(log10.weight~log10.width,data=ns_noeggfemales_analysis)
summary(fit20)
coef(fit20)

cf20<-as.matrix(coef(fit20))

exp(cf20[1,1])
# log10(W) = -7.624769   + 2.854783   * log10(L) on transformed scale       #updated for females
# W = exp(-7.624769 ) * L^(2.854783 )  on original scale                #updated for females
# a = 0.0004882081                                                  #updated for females
# b = 2.854783                                                       #updated for females
##############################################################################################
######################## Apply bias-correction procedure #####################################
cf20
names(fit20)
names(summary(fit20))
###############################################################################################
v2<-(summary(fit20)$sigma)**2  #Variance 
v2
int<-cf20[1,1]
A<-(exp(int)*exp(v2/2))
A                         #0.0004902272 

####################### Variance for parameter A/intercept ########################################
#vcov(fit2)
Av<-vcov(fit20)[1,1]   #extract variance for intercept
sd<-sqrt(Av)          #take square root to create standard deviation
sd
sdA<-(exp(sd)*exp(v2/2))
sdA

sdA_base<-exp(sd)
sdA_base
##################### BIAS-CORRECTED PARAMETERS FOR NON_EGG BEARING FEMALE MODEL ###############################
# a = 0.0004902272   #updated for females
# b = 2.854783          #updated for females


######################################################################################################
############################ Egg bearing ###############################################################
######################################################################################################



plot(ns_eggfemales$WEIGHT~ns_eggfemales$WIDTH)
############################## Plot base data in GG plot ############################################################
dev.new()
p<-ggplot(ns_eggfemales, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) females")

############################# Add fields for analysis ########################################################
Year <- substring(ns_eggfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(ns_eggfemales$WIDTH)
log.weight <- log(ns_eggfemales$WEIGHT)

log10.width<-log10(ns_eggfemales$WIDTH)
log10.weight <- log10(ns_eggfemales$WEIGHT)
ns_female<-as.data.frame(cbind(ns_eggfemales,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
ns_female                    							  		 # inspect data
names(ns_female)											 # Check column names


############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(ns_female, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed")


############################## Fit initial model ########################################################

fit1<-lm(log10.weight~log10.width,data=ns_female)
summary(fit1)
coef(fit1)
############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit1)
plot(cooks.distance(fit1), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(ns_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

ns_female$Cooks_D <- cooks.distance(fit1)
ns_eggfemales_analysis<-subset(ns_female, Cooks_D < (4/(nrow(ns_female))))
 
nrow(ns_female)-nrow(ns_eggfemales_analysis)    #94 obs removed based on Cook's Distance
nrow(ns_eggfemales_analysis)

##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(ns_eggfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="New shell (SC2) males- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(ns_eggfemales_analysis, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) males-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit2<-lm(log10.weight~log10.width,data=ns_eggfemales_analysis)
summary(fit2)
coef(fit2)

cf2<-as.matrix(coef(fit2))

exp(cf2[1,1])
# log10(W) = -7.69351  + 2.88374  * log10(L) on transformed scale       #updated for females
# W = exp(-7.69351) * L^(2.88374)  on original scale                #updated for females
# a = 0.0004557757                                                 #updated for females
# b = 2.88374                                                      #updated for females
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
A                         #0.0004567222

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
# a = 0.0004567222   #updated for females
# b = 2.88374         #updated for females

###################################################################################################
############################ Old shell ############################################################
###################################################################################################

dev.new()
plot(os_eggfemales$WEIGHT~os_eggfemales$WIDTH)
p<-ggplot(os_eggfemales, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) females")

############################# Add fields for analysis ########################################################
Year <- substring(os_eggfemales$CRUISE, 1,4) 

YEAR <- as.factor(Year)
log.width<-log(os_eggfemales$WIDTH)
log.weight <- log(os_eggfemales$WEIGHT)

log10.width<-log10(os_eggfemales$WIDTH)
log10.weight <- log10(os_eggfemales$WEIGHT)
os_female<-as.data.frame(cbind(os_eggfemales,YEAR,log10.width,log10.weight,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
os_female                    							  		 # inspect data
names(os_female)											 # Check column names

nrow(os_female)
############################ Plot log transformeddata in GGplot #############################################
dev.new()
p<-ggplot(os_female, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed")

############################## Fit initial model ########################################################

fit3<-lm(log10.weight~log10.width,data=os_female)
summary(fit3)
coef(fit3)

############################## check diagnostics #################################################
dev.new()
par(mfrow=c(2,2))

plot(fit3)
plot(cooks.distance(fit3), pch=16, cex=0.5, main="Cook's distance with critical distance cutoff") 
     abline(h = 4/(nrow(os_female)), col=4,lwd=1.5)  # critical Cooks Distance cutline(> 4/nobs)

os_female$Cooks_D <- cooks.distance(fit3)
os_eggfemales_analysis<-subset(os_female, Cooks_D < (4/(nrow(os_female))))
 
nrow(os_female)-nrow(os_eggfemales_analysis)    #32 obs removed

nrow(os_eggfemales_analysis)
##################################################################################################
############################# Plot using editted dataset #########################################
dev.new()
p<-ggplot(os_eggfemales_analysis, aes(x = WIDTH, y = WEIGHT)) +
      geom_point()
p+ labs(title="Old shell (SC3+SC4) females- infuential points removed")


############################ log transformed ##################################################
dev.new()
p<-ggplot(os_eggfemales_analysis, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="Old shell (SC3+SC4) females-log transformed, influential points removed")

############################ Fit followup model ##########################################
fit4<-lm(log10.weight~log10.width,data=os_eggfemales_analysis)
summary(fit4)
coef(fit4)
cf4<-as.matrix(coef(fit4))

plot(log10.weight~log10.width,data=os_eggfemales_analysis)
abline(a=cf2[1,1],b=cf2[2,1])
abline(a=cf4[1,1],b=cf4[2,1])
exp(cf4[1,1])
# log10(W) = -7.367562 + 2.824072 * log10(L) on transformed scale       #updated for females
# W = exp(-7.367562) * L^(2.824072)  on original scale              #updated for females
# a = 0.0006314058                                                #updated for females
# b = 2.824072                                                     #updated for females

################################################################################################
######################## Apply bias-correction procedure #####################################
###############################################################################################
v4<-(summary(fit4)$sigma)**2  #Variance 
v4
int<-cf4[1,1]
A<-(exp(int)*exp(v4/2))
A                         #0.0006323218                               #updated for females

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
# a = 0.0006323218
# b = 2.824072

############################ combine data sets and plot, using shell condition as grouping factor############################################################
os_eggfemales_analysis$SC <- "OS"
ns_eggfemales_analysis$SC <- "NS"

analysis_matfemales<-rbind(ns_eggfemales_analysis,os_eggfemales_analysis)

write.csv(analysis_matfemales,"EBS_CB_Analysis_matfemales.csv")

ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))

q<-ggplot(analysis_matfemales, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = factor(SC)))
q+ labs(x="Width(mm)",y="Weight(g)", title="EBS CB(New shell and old shell)females")

#################### log transformed #############################################################
dev.new()

################### points only ############################
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")


ebscb_sc_points<-ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")


################# log transformed with lines ONLY
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB (New shell and old shell, log-transformed)")

ebscb_sc_lines<-ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell, log-transformed)")

################### log-transformed and with regression lines and axis titles
ggplot(analysis_matfemales, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell) log-transformed")

###########################plot as gg scatter with correlation and regression lines ######################################### 
ggscatter(analysis_matfemales, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)


########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_matfemales)
summary(mod1)									# p-value = 3.33e-11 on interaction term. Evidence slopes are different

mod2<-aov(log10.weight~log10.width+SC,data = analysis_matfemales)
summary(mod2)									# p-value for SC = <2e-16. Intercepts are significantly different

anova(mod1,mod2)									# p-value = 0.006454....removing interaction impacts model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_matfemales)
summary(reg_mod)



##################################################################################################################################################


##########################################################################################################################################################
############################### Rerun shell condition models while limit data to same size ranges ##########################################################
###########################################################################################################################################################


#######################################################################################################################################################



x.lim=c(0,200)
?hist()
summary(os_eggfemales_analysis$WIDTH)


summary(ns_eggfemales_analysis$WIDTH)


dev.new()
par(mfrow=c(2,1))
hist(ns_eggfemales_analysis$WIDTH,breaks=30,main="New shell size frequency",xaxt='n',xlim=x.lim)
abline(v=mean(ns_eggfemales_analysis$WIDTH),col=4)
axis(side=1,at=seq(0,200,10),label=seq(0,200,10))
hist(os_eggfemales_analysis$WIDTH,breaks=20,main="Old shell size frequency",xaxt='n',xlim=x.lim)
abline(v=mean(os_eggfemales_analysis$WIDTH),col=4)
axis(side=1,at=seq(0,200,10),label=seq(0,200,10))



ns_eggfemales_analysis_sub<-subset(ns_eggfemales_analysis,WIDTH>57.3)


############################ Fit followup model ##########################################
fit5<-lm(log10.weight~log10.width,data=ns_eggfemales_analysis_sub)
summary(fit5)
coef(fit5)

cf5<-as.matrix(coef(fit5))
cf5
exp(-7.779077 )
			# log10(W) = -7.779077 + 2.902537 * log10(L) on transformed scale                         #updated for females
    			# W = exp(-7.779077)*L^(2.902537)  on original scale                            #updated for females
			# a = 0.0004256845                                                                    #updated for females
			# b = 2.902537                                                                        #updated for females

p<-ggplot(ns_eggfemales_analysis_sub, aes(x = log10.width, y = log10.weight)) +
      geom_point()
p+ labs(x="ln(width)",y="ln(weight)", title="New shell (SC2) Females-log transformed")
p+ geom_abline(intercept = cf5[1,1],slope = cf5[2,1],color = "red")



####################################################################################################################################
######################## Combine old shell and truncated new shell data and plot ##############################################################
analysis_matfemales_sub<-as.data.frame(rbind(ns_eggfemales_analysis_sub,os_eggfemales_analysis))


########################### Non transformed ############################################################################
q<-ggplot(analysis_matfemales_sub, aes(x = WIDTH, y = WEIGHT, group = SC)) +
     geom_point(aes(colour = SC))
	q+ labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB (New shell and old shell)")

########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(analysis_matfemales_sub, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = factor(SC)))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB (New shell and old shell, log-transformed)")

ebscb_sc_points_sub <- ggplot(analysis_matfemales_sub, aes(x = log10.width, y = log10.weight, group = SC)) +
     geom_point(aes(colour = SC))+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB (New shell and old shell, log transformed and truncated at 40mmm)")

############################# lines ONLY ###############################################################################
ggplot(analysis_matfemales_sub, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell, log-transformed)")


ebscb_sc_lines_sub <-ggplot(analysis_matfemales_sub, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
	geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
	labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell,log transformed and truncated at 40mm)")


############################# with points and lines#####################################################################
ggplot(analysis_matfemales_sub, aes(x = log10.width, y = log10.weight, group = SC,color = SC,shape=SC)) +
     geom_point()+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
labs(x="Ln(Width)",y="Ln(Weight)", title="Female EBS CB(New shell and old shell log-transformed)")

######################  Alternative approach #########################################################################
ggscatter(analysis_matfemales_sub, x="log10.width",y="log10.weight",color="SC", add="reg.line"
)+
stat_regline_equation(
aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color = factor(SC))
)

########################################################################################################################
############################### 4 panel ################################################################################

ggarrange(ebscb_sc_points,ebscb_sc_lines,ebscb_sc_points_sub, ebscb_sc_lines_sub,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)



########################################################################################################################
############################### Run ANCOVA analyses to determine if statistically different ############################

mod1<-aov(log10.weight~log10.width*SC,data = analysis_matfemales_sub)
summary(mod1)									# p= 0.00645 on interaction term= significant interaction--suggests different slopes

mod2<-aov(log10.weight~log10.width+SC,data = analysis_matfemales_sub)
summary(mod2)									# p = 2e-16 for SC, SC-based regression lines have differing intercepts

anova(mod1,mod2)									# p= 0.006454...removing interaction term sinificantly affects model

reg_mod<-lm(log10.weight~SC/log10.width-1,data=analysis_matfemales_sub)		
summary(reg_mod)





















