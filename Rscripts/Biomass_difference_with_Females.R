# Jon Richar 
# 6/16/2020
# Calculate length/weight regression models for BBRKC by shell condition
library(tidyverse)
library(stats)
library(nlme)
library(ggpubr)
library(ggplot2)

setwd("C:/Users/Jon.Richar/Work/GitRepos/LengthWeight/EBSCrabLengthWeight/DATA/Biomass_DATA")
##################### Add data ###############################################
bbrkc_base<-read.csv("rk_bb_bio_oldmodel.csv")
bbrkc_new<-read.csv("rk_bb_bio_newmodel.csv")
bbrkc_bias<-read.csv("rk_bb_leg1_biomass_newmodel_biascorrected.csv")

bbrkc_female_base<-read.csv("RK_BB_LEG3_FEMALE_BIOMASS_OLDMODEL.csv")
bbrkc_female_ovig_status_bias<-read.csv("bbrkc_leg3_female_ovig_nonovig_bio_newmodel_biascorrection.csv")
bbrkc_female_maturity_status_bias<-read.csv("bbrkc_leg3_female_mat_immat_bio_newmodel_biascorrection.csv")

bkc_base<-read.csv("SMBKC_BIO_MATFEM_OLDMODEL.csv")
bkc_bias<-read.csv("SMBKC_Bio_New_model_bias_corrected.csv")

e166cb_base<-read.csv("cb_e166_bio_oldmodel.csv")
e166cb_new<-read.csv("cb_e166_bio_newmodel.csv")
##e166cb_bias<-read.csv("cb_e166_biomass_newmodel_biascorrected.csv")
e166cb_bias<-read.csv("CB_E166_BIO_MATFEM_NEWMODEL_BIASCORRECTED.csv") # shell condition ranges for application of parameters updated

w166cb_base<-read.csv("cb_w166_bio_oldmodel.csv")
w166cb_new<-read.csv("cb_w166_bio_newmodel.csv")
##w166cb_bias<-read.csv("cb_w166_biomass_newmodel_biascorrected.csv")
w166cb_bias<-read.csv("CB_W166_BIO_MATFEM_NEWMODEL_BIASCORRECTED.csv") # shell condition ranges for application of parameters updated

co_base<-read.csv("co_bio_oldmodel.csv")
co_new<-read.csv("co_bio_newmodel.csv")
co_bias<-read.csv("CO_BIO_MATFEM_NEWMODEL_BIASCORRECTED.csv")

##################### Create series for plotting ############################

####### Baseline data #####################

bbrkc_base_mat<-bbrkc_base$BIOMASS_MALE_GE120
bbrkc_base_leg<-bbrkc_base$BIOMASS_MALE_GE135
rk_year<-bbrkc_base$SURVEY_YEAR
bbrkc_matfem_base<-bbrkc_female_base$BIOMASS_FEMALE_MATURE
bbrkc_immatfem_base<-bbrkc_female_base$BIOMASS_FEMALE_IMMATURE

bkc_base_recruit<-bkc_base$BIOMASS_MALE_GE90
bkc_base_mat<-bkc_base$BIOMASS_MALE_GE105
bkc_base_leg<-bkc_base$BIOMASS_MALE_GE120
bk_year<-bkc_base$SURVEY_YEAR
bkc_matfem_base<-bkc_base$BIOMASS_FEMALE_MATURE
bkc_immatfem_base<-bkc_base$BIOMASS_FEMALE_IMMATURE

e166cb_base_mat<-e166cb_base$BIOMASS_MALE_GE113
e166cb_base_leg<-e166cb_base$BIOMASS_MALE_GE120
e166cb_base_matfem<-e166cb_base$BIOMASS_FEMALE_MATURE

cb_year<-e166cb_base$SURVEY_YEAR

w166cb_base_mat<-w166cb_base$BIOMASS_MALE_GE103
w166cb_base_leg<-w166cb_base$BIOMASS_MALE_GE110
w166cb_base_matfem<-w166cb_base$BIOMASS_FEMALE_MATURE

co_base_mat<-co_base$BIOMASS_MALE_GE95
co_base_leg<-co_base$BIOMASS_MALE_GE78
co_base_matfem<-co_base$BIOMASS_FEMALE_MATURE

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

###### Bias corrected data ######################

bbrkc_bias_mat<-bbrkc_bias$BIOMASS_MALE_GE120
bbrkc_bias_leg<-bbrkc_bias$BIOMASS_MALE_GE135
bbrkc_matfem_ovig_status_bias<-bbrkc_female_ovig_status_bias$BIOMASS_FEMALE_MATURE
bbrkc_immatfem_ovig_status_bias<-bbrkc_female_ovig_status_bias$BIOMASS_FEMALE_IMMATURE
bbrkc_matfem_maturity_status_bias<-bbrkc_female_maturity_status_bias$BIOMASS_FEMALE_MATURE
bbrkc_immatfem_maturity_status_bias<-bbrkc_female_maturity_status_bias$BIOMASS_FEMALE_IMMATURE

bkc_bias_recruit<-bkc_bias$BIOMASS_MALE_GE90
bkc_bias_mat<-bkc_bias$BIOMASS_MALE_GE105
bkc_bias_leg<-bkc_bias$BIOMASS_MALE_GE120
bkc_matfem_bias<-bkc_bias$BIOMASS_FEMALE_MATURE
bkc_immatfem_bias<-bkc_bias$BIOMASS_FEMALE_IMMATURE

e166cb_bias_mat<-e166cb_bias$BIOMASS_MALE_GE113
e166cb_bias_leg<-e166cb_bias$BIOMASS_MALE_GE120
e166cb_bias_matfem<-e166cb_bias$BIOMASS_FEMALE_MATURE

w166cb_bias_mat<-w166cb_bias$BIOMASS_MALE_GE103
w166cb_bias_leg<-w166cb_bias$BIOMASS_MALE_GE110
w166cb_bias_matfem<-w166cb_bias$BIOMASS_FEMALE_MATURE

co_bias_mat<-co_bias$BIOMASS_MALE_GE95
co_bias_leg<-co_bias$BIOMASS_MALE_GE78
co_bias_matfem<-co_bias$BIOMASS_FEMALE_MATURE
co_bias_matfem
#################################################################################
##################### Calculate perecent differences of series #################
############################### ((N-O)/O)*100 ###################################

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

##################################################################################################
##################### Calculate perecent differences of bias corrected vs standard series #########
##################### ((N-O)/O)*100 ##############################################################

#################### BBRKC
bbrkc_bias_mat_dif<-((bbrkc_bias_mat - bbrkc_base_mat)/bbrkc_base_mat)*100

mean(bbrkc_bias_mat_dif)

bbrkc_bias_leg_dif<-((bbrkc_bias_leg - bbrkc_base_leg)/bbrkc_base_leg)*100

mean(bbrkc_bias_leg_dif)

bbrkc_bias_matfem_ovig_status_dif<-((bbrkc_matfem_ovig_status_bias - bbrkc_matfem_base)/bbrkc_matfem_base)*100

mean(bbrkc_bias_matfem_ovig_status_dif)

bbrkc_bias_matfem_maturity_status_dif<-((bbrkc_matfem_maturity_status_bias - bbrkc_matfem_base)/bbrkc_matfem_base)*100

mean(bbrkc_bias_matfem_maturity_status_dif)

#################### SMBKC 
bkc_bias_recruit_dif<-((bkc_bias_recruit - bkc_base_recruit)/bkc_base_recruit)*100

mean(bkc_bias_recruit_dif,na.rm= T)

bkc_bias_mat_dif<-((bkc_bias_mat - bkc_base_mat)/bkc_base_mat)*100

mean(bkc_bias_mat_dif,na.rm= T)

bkc_bias_leg_dif<-((bkc_bias_leg - bkc_base_leg)/bkc_base_leg)*100

mean(bkc_bias_leg_dif,na.rm= T)

bkc_bias_matfem_dif<-((bkc_matfem_bias - bkc_matfem_base)/bkc_matfem_base)*100

mean(bkc_bias_matfem_dif,na.rm= T)


#################### Bairdi
e166cb_bias_mat_dif<-((e166cb_bias_mat - e166cb_base_mat)/e166cb_base_mat)*100

mean(e166cb_bias_mat_dif)

e166cb_bias_leg_dif<-((e166cb_bias_leg - e166cb_base_leg)/e166cb_base_leg)*100

mean(e166cb_bias_leg_dif)

w166cb_bias_mat_dif<-((w166cb_bias_mat - w166cb_base_mat)/w166cb_base_mat)*100

mean(w166cb_bias_mat_dif)

w166cb_bias_leg_dif<-((w166cb_bias_leg - w166cb_base_leg)/w166cb_base_leg)*100

mean(w166cb_bias_leg_dif)


e166cb_bias_matfem_dif<-((e166cb_bias_matfem - e166cb_base_matfem)/e166cb_base_matfem)*100

mean(e166cb_bias_matfem_dif)

w166cb_bias_matfem_dif<-((w166cb_bias_matfem - w166cb_base_matfem)/w166cb_base_matfem)*100

mean(w166cb_bias_matfem_dif)

#################### Opilio
co_bias_mat_dif<-((co_bias_mat - co_base_mat)/co_base_mat)*100

mean(co_bias_mat_dif)

co_bias_leg_dif<-((co_bias_leg - co_base_leg)/co_base_leg)*100

mean(co_bias_leg_dif)

co_bias_matfem_dif<-((co_bias_matfem - co_base_matfem)/co_base_matfem)*100

mean(co_bias_leg_dif)

##################################################################################################
##################### Calculate perecent differences of bias corrected vs new model series#########
##################### ((N-O)/O)*100 ##############################################################

bbrkc_bias2_mat_dif<-((bbrkc_bias_mat-bbrkc_new_mat)/bbrkc_new_mat)*100

mean(bbrkc_bias2_mat_dif)

bbrkc_bias2_leg_dif<-((bbrkc_bias_leg-bbrkc_new_leg)/bbrkc_new_leg)*100

mean(bbrkc_bias2_leg_dif)

e166cb_bias2_mat_dif<-((e166cb_bias_mat - e166cb_new_mat)/e166cb_new_mat)*100

mean(e166cb_bias2_mat_dif)

e166cb_bias2_leg_dif<-((e166cb_bias_leg - e166cb_new_leg)/e166cb_new_leg)*100

mean(e166cb2_bias_leg_dif)

w166cb_bias2_mat_dif<-((w166cb_bias_mat - w166cb_new_mat)/w166cb_new_mat)*100

mean(w166cb_bias2_mat_dif)

w166cb_bias2_leg_dif<-((w166cb_bias_leg - w166cb_new_leg)/w166cb_new_leg)*100

mean(w166cb_bias2_leg_dif)

co_bias2_mat_dif<-((co_bias_mat - co_new_mat)/co_new_mat)*100

mean(co_bias2_mat_dif)

co_bias2_leg_dif<-((co_bias_leg - co_base_leg)/co_base_leg)*100

mean(co_bias2_leg_dif)
########################################################################################
#################### create data frames ################################################

bbrkc<-as.data.frame(cbind(rk_year,bbrkc_mat_dif,bbrkc_leg_dif))

e166cb<-as.data.frame(cbind(cb_year,e166cb_mat_dif,e166cb_leg_dif))

w166cb<-as.data.frame(cbind(cb_year,w166cb_mat_dif,w166cb_leg_dif))

co<-as.data.frame(cbind(co_year,co_mat_dif,co_leg_dif))

#######################################################################################
#################### create data frames for bias corrected ############################

bbrkc_bias<-as.data.frame(cbind(rk_year,bbrkc_bias_mat_dif,bbrkc_bias_leg_dif))

bbrkc_bias_matfem<-as.data.frame(cbind(bbrkc_bias_matfem_ovig_status_dif,bbrkc_bias_matfem_maturity_status_dif))

bkc_bias<-as.data.frame(cbind(bk_year,bkc_bias_recruit_dif,bkc_bias_mat_dif,bkc_bias_leg_dif))

bkc_fem_bias<-as.data.frame(cbind(bk_year,bkc_bias_matfem_dif))

e166cb_bias<-as.data.frame(cbind(cb_year,e166cb_bias_mat_dif,e166cb_bias_leg_dif))

w166cb_bias<-as.data.frame(cbind(cb_year,w166cb_bias_mat_dif,w166cb_bias_leg_dif))

cb_fem_bias<-as.data.frame(cbind(cb_year,e166cb_bias_matfem_dif,w166cb_bias_matfem_dif))

co_bias<-as.data.frame(cbind(co_year,co_bias_mat_dif,co_bias_leg_dif))

co_fem_bias<-as.data.frame(co_year, co_bias_matfem_dif)
#######################################################################################
#################### create data frames for bias corrected vs new model ############################

bbrkc_bias2<-as.data.frame(cbind(rk_year,bbrkc_bias2_mat_dif,bbrkc_bias2_leg_dif))

e166cb_bias2<-as.data.frame(cbind(cb_year,e166cb_bias2_mat_dif,e166cb_bias2_leg_dif))

w166cb_bias2<-as.data.frame(cbind(cb_year,w166cb_bias2_mat_dif,w166cb_bias2_leg_dif))

co_bias2<-as.data.frame(cbind(co_year,co_bias2_mat_dif,co_bias2_leg_dif))
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


################################################################################################
#################### Plot bias corrected data ##################################################
################################################################################################

# BBRKC

ggplot(bbrkc_bias,aes(x=rk_year))+
  geom_line(aes(y=bbrkc_bias_mat_dif,color = "Mature male"))+
  geom_line(aes(y=bbrkc_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Bristol Bay red king crab - bias corrected")


rkc_bias<-ggplot(bbrkc_bias,aes(x=rk_year))+
  geom_line(aes(y=bbrkc_bias_mat_dif,color = "Mature male"))+
  geom_line(aes(y=bbrkc_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Bristol Bay red king crab - bias corrected")

# BBRKC mature females 
dev.new()
ggplot(bbrkc_bias_matfem,aes(x=rk_year))+
  geom_line(aes(y=bbrkc_bias_matfem_ovig_status_dif,color = "Mature females - clutch size based"))+
  geom_line(aes(y=bbrkc_bias_matfem_maturity_status_dif,color="Mature females - all matures based"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Bristol Bay red king crab - bias corrected")


rkc_matfem_bias<-ggplot(bbrkc_bias_matfem,aes(x=rk_year))+
  geom_line(aes(y=bbrkc_bias_matfem_ovig_status_dif,color = "Mature females - clutch size based"))+
  geom_line(aes(y=bbrkc_bias_matfem_maturity_status_dif,color="Mature females - all matures based"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Bristol Bay red king crab - bias corrected")

# SMBKC

ggplot(bkc_bias,aes(x=bk_year))+
  geom_line(aes(y=bkc_bias_recruit_dif,color = "Males GE90"))+
  geom_line(aes(y=bkc_bias_mat_dif,color="Mature male"))+
  geom_line(aes(y=bkc_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="St Matthew blue king crab - bias corrected")

bk_bias<-ggplot(bkc_bias,aes(x=bk_year))+
  geom_line(aes(y=bkc_bias_recruit_dif,color = "Males GE90"))+
  geom_line(aes(y=bkc_bias_mat_dif,color="Mature male"))+
  geom_line(aes(y=bkc_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="St Matthew blue king crab - bias corrected")



ggplot(bkc_fem_bias,aes(x=bk_year))+
  geom_line(aes(y=bkc_bias_matfem_dif,color = "Mature females"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="St Matthew blue king crab - bias corrected")


bk_fem_bias<-ggplot(bkc_fem_bias,aes(x=bk_year))+
  geom_line(aes(y=bkc_bias_matfem_dif,color = "Mature females"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="St Matthew blue king crab - bias corrected")

# E166 CB

ggplot(e166cb_bias,aes(x=cb_year))+
  geom_line(aes(y=e166cb_bias_mat_dif,color = "Mature male"))+
  geom_line(aes(y=e166cb_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern district Bairdi - bias corrected")

e166_bias<-ggplot(e166cb_bias,aes(x=cb_year))+
  geom_line(aes(y=e166cb_bias_mat_dif,color = "Mature male"))+
  geom_line(aes(y=e166cb_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern district Bairdi - bias corrected")


# W166 CB

ggplot(w166cb_bias,aes(x=cb_year))+
  geom_line(aes(y=w166cb_bias_mat_dif,color = "Mature male"))+
  geom_line(aes(y=w166cb_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Western district Bairdi - bias corrected")

w166_bias<-ggplot(w166cb_bias,aes(x=cb_year))+
  geom_line(aes(y=w166cb_bias_mat_dif,color = "Mature male"))+
  geom_line(aes(y=w166cb_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Western district Bairdi - bias corrected")

# Bairdi Females (East + West)


ggplot(cb_fem_bias,aes(x=cb_year))+
  geom_line(aes(y=e166cb_bias_matfem_dif,color = "Eastern district mature females"))+
  geom_line(aes(y=w166cb_bias_matfem_dif,color="Western district mature females"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="East/West Bairdi - bias corrected")

cbfem_bias<-ggplot(cb_fem_bias,aes(x=cb_year))+
  geom_line(aes(y=e166cb_bias_matfem_dif,color = "Eastern district mature females"))+
  geom_line(aes(y=w166cb_bias_matfem_dif,color="Western district mature females"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="East/West Bairdi - bias corrected")

# EBS CO

ggplot(co_bias,aes(x=co_year))+
  geom_line(aes(y=co_bias_mat_dif,color = "Mature male"))+
  geom_line(aes(y=co_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern Bering Sea opilio - bias corrected")

ebs_co_bias<-ggplot(co_bias,aes(x=co_year))+
  geom_line(aes(y=co_bias_mat_dif,color = "Mature male"))+
  geom_line(aes(y=co_bias_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern Bering Sea opilio - bias corrected")

dev.new()
co_fem_bias
ggplot(co_fem_bias,aes(x=co_year))+
  geom_line(aes(y=co_bias_matfem_dif,color = "EBS mature females"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern Bering Sea opilio - bias corrected")

cofem_bias<-ggplot(co_fem_bias,aes(x=co_year))+
  geom_line(aes(y=co_bias_matfem_dif,color = "EBS mature females"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern Bering Sea opilio - bias corrected")


############## 4 panel ###########################################################

dev.new()

ggarrange(rkc_bias,  bk_bias,e166_bias,w166_bias, ebs_co_bias,
          labels = c("a.)", "b.)", "c.)", "d.)","e.)"),
          ncol = 2, nrow = 3)

ggarrange(rkc_matfem_bias, bk_fem_bias, cbfem_bias,
          labels = c("a.)", "b.)", "c.)"),
          ncol = 2, nrow = 2)

ggarrange(rkc_matfem_bias, cbfem_bias,cofem_bias,
          labels = c("a.)", "b.)", "c.)"),
          ncol = 1, nrow = 3)

dev.new()

ggarrange(cbfem_bias,cofem_bias,
          labels = c("a.)", "b.)"),
          ncol = 1, nrow = 2)


################################################################################################
#################### Plot new model bias corrected data ##################################################
################################################################################################

# BBRKC
dev.new()
ggplot(bbrkc_bias2,aes(x=rk_year))+
  geom_line(aes(y=bbrkc_bias2_mat_dif,color = "Mature male"))+
  geom_line(aes(y=bbrkc_bias2_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Bristol Bay red king crab - new model vs bias corrected")


rkc_bias2<-ggplot(bbrkc_bias2,aes(x=rk_year))+
  geom_line(aes(y=bbrkc_bias2_mat_dif,color = "Mature male"))+
  geom_line(aes(y=bbrkc_bias2_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Bristol Bay red king crab - new model vs bias corrected")

# E166 CB
dev.new()
ggplot(e166cb_bias2,aes(x=cb_year))+
  geom_line(aes(y=e166cb_bias2_mat_dif,color = "Mature male"))+
  geom_line(aes(y=e166cb_bias2_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern district Bairdi - new model vs bias corrected")

e166_bias2<-ggplot(e166cb_bias2,aes(x=cb_year))+
  geom_line(aes(y=e166cb_bias2_mat_dif,color = "Mature male"))+
  geom_line(aes(y=e166cb_bias2_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern district Bairdi - new model vs bias corrected")


# W166 CB
dev.new()
ggplot(w166cb_bias2,aes(x=cb_year))+
  geom_line(aes(y=w166cb_bias2_mat_dif,color = "Mature male"))+
  geom_line(aes(y=w166cb_bias2_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Western district Bairdi - new model vs bias corrected")

w166_bias2<-ggplot(w166cb_bias2,aes(x=cb_year))+
  geom_line(aes(y=w166cb_bias2_mat_dif,color = "Mature male"))+
  geom_line(aes(y=w166cb_bias2_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Western district Bairdi - new model vs bias corrected")

# EBS CO
dev.new()
ggplot(co_bias2,aes(x=co_year))+
  geom_line(aes(y=co_bias2_mat_dif,color = "Mature male"))+
  geom_line(aes(y=co_bias2_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern Bering Sea opilio - new model vs bias corrected")

ebs_co_bias2<-ggplot(co_bias2,aes(x=co_year))+
  geom_line(aes(y=co_bias2_mat_dif,color = "Mature male"))+
  geom_line(aes(y=co_bias2_leg_dif,color="Legal male"))+
  labs(color = "Category",x = "Survey Year", y = "Percent difference",title="Eastern Bering Sea opilio - new model vs bias corrected")



############## 4 panel ###########################################################

dev.new()

ggarrange(rkc_bias2, e166_bias2,w166_bias2, ebs_co_bias2,
          labels = c("a.)", "b.)", "c.)", "d.)"),
          ncol = 2, nrow = 2)


