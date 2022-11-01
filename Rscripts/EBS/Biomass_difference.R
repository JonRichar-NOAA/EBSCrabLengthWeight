# Jon Richar 
# 6/16/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggplot2)
setwd("C:/Users/Jon.Richar/Work/Projects/Length_weight/DATA/Biomass_DATA")
##################### Add data ##############################################
bbrkc_base<-read.csv("rk_bb_bio_oldmodel.csv")
bbrkc_new<-read.csv("rk_bb_bio_newmodel.csv")

e166cb_base<-read.csv("cb_e166_bio_oldmodel.csv")
e166cb_new<-read.csv("cb_e166_bio_newmodel.csv")


w166cb_base<-read.csv("cb_w166_bio_oldmodel.csv")
w166cb_new<-read.csv("cb_w166_bio_newmodel.csv")

co_base<-read.csv("co_bio_oldmodel.csv")
co_new<-read.csv("co_bio_newmodel.csv")

##################### Create series for plotting ############################

####### Baseline data #####################

bbrkc_base_mat<-bbrkc_base$BIOMASS_MALE_GE120
bbrkc_base_leg<-bbrkc_base$BIOMASS_MALE_GE135
rk_year<-bbrkc_base$SURVEY_YEAR

e166cb_base_mat<-e166cb_base$BIOMASS_MALE_GE113
e166cb_base_leg<-e166cb_base$BIOMASS_MALE_GE120

cb_year<-e166cb_base$SURVEY_YEAR

w166cb_base_mat<-w166cb_base$BIOMASS_MALE_GE103
w166cb_base_leg<-w166cb_base$BIOMASS_MALE_GE110

co_base_mat<-co_base$BIOMASS_MALE_GE95
co_base_leg<-co_base$BIOMASS_MALE_GE78

co_year<-co_base$SURVEY_YEAR
###### New model data ######################

bbrkc_new_mat<-bbrkc_new$BIOMASS_MALE_GE120
bbrkc_new_leg<-bbrkc_new$BIOMASS_MALE_GE135

e166cb_new_mat<-e166cb_new$BIOMASS_MALE_GE113
e166cb_new_leg<-e166cb_new$BIOMASS_MALE_GE120

w166cb_new_mat<-w166cb_new$BIOMASS_MALE_GE103
w166cb_new_leg<-w166cb_new$BIOMASS_MALE_GE110

co_new_mat<-co_new$BIOMASS_MALE_GE95
co_new_leg<-co_new$BIOMASS_MALE_GE78


##################### Calculate perecent differences of series#########
##################### ((N-O)/O)*100 ###################################

bbrkc_mat_dif<-((bbrkc_new_mat-bbrkc_base_mat)/bbrkc_base_mat)*100

mean(bbrkc_mat_dif)

bbrkc_leg_dif<-((bbrkc_new_leg-bbrkc_base_leg)/bbrkc_base_leg)*100

mean(bbrkc_leg_dif)

e166cb_mat_dif<-((e166cb_new_mat - e166cb_base_mat)/e166cb_base_mat)*100

mean(e166cb_mat_dif)

e166cb_leg_dif<-((e166cb_new_leg - e166cb_base_leg)/e166cb_base_leg)*100

mean(e166cb_leg_dif)

w166cb_mat_dif<-((w166cb_new_mat - w166cb_base_mat)/w166cb_base_mat)*100

mean(w166cb_mat_dif)

w166cb_leg_dif<-((w166cb_new_leg - w166cb_base_leg)/w166cb_base_leg)*100

mean(w166cb_leg_dif)

co_mat_dif<-((co_new_mat - co_base_mat)/co_base_mat)*100

mean(co_mat_dif)

co_leg_dif<-((co_new_leg - co_base_leg)/co_base_leg)*100

mean(co_leg_dif)

##########################################################################
#################### create data frames ################################################

bbrkc<-as.data.frame(cbind(rk_year,bbrkc_mat_dif,bbrkc_leg_dif))

e166cb<-as.data.frame(cbind(cb_year,e166cb_mat_dif,e166cb_leg_dif))

w166cb<-as.data.frame(cbind(cb_year,w166cb_mat_dif,w166cb_leg_dif))

co<-as.data.frame(cbind(co_year,co_mat_dif,co_leg_dif))



###########################################################################
#################### Plot ##################################################
# BBRKC
ggplot(bbrkc,aes(x=rk_year))+
geom_line(aes(y=bbrkc_mat_dif,color = "Mature male"))+
geom_line(aes(y=bbrkc_leg_dif,color="Legal male"))+
labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Bristol Bay red king crab")


rkc<-ggplot(bbrkc,aes(x=rk_year))+
geom_line(aes(y=bbrkc_mat_dif,color = "Mature male"))+
geom_line(aes(y=bbrkc_leg_dif,color="Legal male"))+
labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Bristol Bay red king crab")

# E166 CB
ggplot(e166cb,aes(x=cb_year))+
geom_line(aes(y=e166cb_mat_dif,color = "Mature male"))+
geom_line(aes(y=e166cb_leg_dif,color="Legal male"))+
labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern district Bairdi")

e166<-ggplot(e166cb,aes(x=cb_year))+
geom_line(aes(y=e166cb_mat_dif,color = "Mature male"))+
geom_line(aes(y=e166cb_leg_dif,color="Legal male"))+
labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern district Bairdi")


# W166 CB

ggplot(w166cb,aes(x=cb_year))+
geom_line(aes(y=w166cb_mat_dif,color = "Mature male"))+
geom_line(aes(y=w166cb_leg_dif,color="Legal male"))+
labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Western district Bairdi")

w166<-ggplot(w166cb,aes(x=cb_year))+
geom_line(aes(y=w166cb_mat_dif,color = "Mature male"))+
geom_line(aes(y=w166cb_leg_dif,color="Legal male"))+
labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Western district Bairdi")

# EBS CO

ggplot(co,aes(x=co_year))+
geom_line(aes(y=co_mat_dif,color = "Mature male"))+
geom_line(aes(y=co_leg_dif,color="Legal male"))+
labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern Bering Sea opilio")

ebs_co<-ggplot(co,aes(x=co_year))+
geom_line(aes(y=co_mat_dif,color = "Mature male"))+
geom_line(aes(y=co_leg_dif,color="Legal male"))+
labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern Bering Sea opilio")



############## 4 panel ###########################################################


ggarrange(rkc, e166,w166, ebs_co,
labels = c("a.)", "b.)", "c.)", "d.)"),
ncol = 2, nrow = 2)




