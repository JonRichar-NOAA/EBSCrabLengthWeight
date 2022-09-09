library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggplot2)
setwd("C:/Users/Jon.Richar/Work/GitRepos/LengthWeight/EBSCrabLengthWeight/DATA/")
df<-read.csv("EBSCB_weightDB_analysis.csv")

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


ns_eggfemaless<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==2)

Year <- substring(ns_eggfemaless$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.width<-log(ns_eggfemaless$WIDTH)
log.weight <- log(ns_eggfemaless$WEIGHT)
ns_eggfemales<-as.data.frame(cbind(ns_eggfemaless,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  

ns_eggfemales
colnames(ns_eggfemales)

os_eggfemaless<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==3|SHELL_CONDITION==4)

Year <- substring(os_eggfemaless$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.width<-log(os_eggfemaless$WIDTH)
log.weight <- log(os_eggfemaless$WEIGHT)
os_eggfemales<-as.data.frame(cbind(os_eggfemaless,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  

os_eggfemales

########################## Aggregate by New shell/old shell #####################################

#noeggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE<=1)
#ns_eggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==2)
#os_eggfemales<-subset(df1,SEX==2 & CLUTCH_SIZE>1 & SHELL_CONDITION==3|SHELL_CONDITION==4)
#######################################################################################
sample<-distinct(ns_eggfemales, YEAR)
y<-sample$YEAR
y

ns_eggfemales
nrow(subset(ns_eggfemales, YEAR==2000))
nrow(subset(ns_eggfemales, YEAR==2001))
nrow(subset(ns_eggfemales, YEAR==2006))
nrow(subset(ns_eggfemales, YEAR==2007))
nrow(subset(ns_eggfemales, YEAR==2008))
nrow(subset(ns_eggfemales, YEAR==2009))
nrow(subset(ns_eggfemales, YEAR==2010))
nrow(subset(ns_eggfemales, YEAR==2011))
nrow(subset(ns_eggfemales, YEAR==2014))
nrow(subset(ns_eggfemales, YEAR==2016))
nrow(subset(ns_eggfemales, YEAR==2017))
nrow(subset(ns_eggfemales, YEAR==2018))
nrow(subset(ns_eggfemales, YEAR==2019))


distinct(os_eggfemales, YEAR)

sample<-distinct(os_eggfemales, YEAR)
y<-sample$YEAR
y

nrow(subset(os_eggfemales, YEAR==2000))
nrow(subset(os_eggfemales, YEAR==2001))
nrow(subset(os_eggfemales, YEAR==2006))
nrow(subset(os_eggfemales, YEAR==2007))
nrow(subset(os_eggfemales, YEAR==2008))
nrow(subset(os_eggfemales, YEAR==2009))
nrow(subset(os_eggfemales, YEAR==2010))
nrow(subset(os_eggfemales, YEAR==2011))
nrow(subset(os_eggfemales, YEAR==2012))
nrow(subset(os_eggfemales, YEAR==2013))
nrow(subset(os_eggfemales, YEAR==2014))
nrow(subset(os_eggfemales, YEAR==2016))
nrow(subset(os_eggfemales, YEAR==2017))
nrow(subset(os_eggfemales, YEAR==2018))
nrow(subset(os_eggfemales, YEAR==2019))




######################## Snow crab ############################################

# Jon Richar with tidyverse functions courtesy of Erin Fedewa
# 5/20/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)

setwd("C:/Users/Jon.Richar/Work/Projects/Length_weight/DATA")
df<-read.csv("EBSCO_weightDB_analysis.csv")
df1<-subset(df, WEIGHT>0 & SEX==2)
colnames(df1)
plot(df1$WEIGHT~df1$WIDTH)
#identify(df1$WEIGHT~df1$LENGTH)
######################### Tidyverse approach ###################################
df1 %>%
  mutate(logwidth = log(WIDTH),
          logweight = log(WEIGHT),
          Year = substring(CRUISE, 1,4), 
          YEAR = as.factor(Year)) %>%
 filter(SEX == 2) -> female #Only SC2 as to not bias for weight of epibionts 
#male
ggplot(female, aes(x = WIDTH, y = WEIGHT, group = YEAR)) +
     geom_point(aes(colour = factor(SHELL_CONDITION)))



########################## Aggregate by New shell/old shell #####################################
ns_eggfemaless<-subset(df1,SEX==2 & SHELL_CONDITION==2)

Year <- substring(ns_eggfemaless$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.width<-log(ns_eggfemaless$WIDTH)
log.weight <- log(ns_eggfemaless$WEIGHT)
ns_eggfemales<-as.data.frame(cbind(ns_eggfemaless,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  


sample<-distinct(ns_eggfemales, YEAR)
y<-sample$YEAR
y

os_eggfemaless<-subset(df1,SEX==2 & SHELL_CONDITION==3|SHELL_CONDITION==4)

Year <- substring(os_eggfemaless$CRUISE, 1,4) 
YEAR <- as.factor(Year)
log.width<-log(os_eggfemaless$WIDTH)
log.weight <- log(os_eggfemaless$WEIGHT)
os_eggfemales<-as.data.frame(cbind(os_eggfemaless,YEAR,log.width,log.weight))   		 # Bind new data objects and crab data in data frame  
      
os_eggfemales
sample<-distinct(os_eggfemales, YEAR)
y<-sample$YEAR
y

nrow(subset(ns_eggfemales, YEAR==1975))
nrow(subset(ns_eggfemales, YEAR==2000))
nrow(subset(ns_eggfemales, YEAR==2001))
nrow(subset(ns_eggfemales, YEAR==2006))
nrow(subset(ns_eggfemales, YEAR==2007))
nrow(subset(ns_eggfemales, YEAR==2008))
nrow(subset(ns_eggfemales, YEAR==2009))
nrow(subset(ns_eggfemales, YEAR==2010))
nrow(subset(ns_eggfemales, YEAR==2011))
nrow(subset(ns_eggfemales, YEAR==2012))
nrow(subset(ns_eggfemales, YEAR==2013))
nrow(subset(ns_eggfemales, YEAR==2015))
nrow(subset(ns_eggfemales, YEAR==2017))
nrow(subset(ns_eggfemales, YEAR==2019))


distinct(os_eggfemales, YEAR)
unique(os_eggfemales$YEAR)
nrow(subset(os_eggfemales, YEAR==1975))
nrow(subset(os_eggfemales, YEAR==2000))
nrow(subset(os_eggfemales, YEAR==2001))
nrow(subset(os_eggfemales, YEAR==2006))
nrow(subset(os_eggfemales, YEAR==2007))
nrow(subset(os_eggfemales, YEAR==2008))
nrow(subset(os_eggfemales, YEAR==2009))
nrow(subset(os_eggfemales, YEAR==2010))
nrow(subset(os_eggfemales, YEAR==2011))
nrow(subset(os_eggfemales, YEAR==2012))
nrow(subset(os_eggfemales, YEAR==2013))
nrow(subset(os_eggfemales, YEAR==2015))
nrow(subset(os_eggfemales, YEAR==2017))
nrow(subset(os_eggfemales, YEAR==2018))
nrow(subset(os_eggfemales, YEAR==2019))






