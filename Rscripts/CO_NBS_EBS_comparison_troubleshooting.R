setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA/NBS_Data")

nbs_data<-read.csv("NBS_CO_Analysis_males.csv")
nrow(nbs_data)
setwd("C:/Users/jon.richar/Work/GitRepos/EBSCrabLengthWeight/DATA/")

ebs_data<-read.csv("EBS_CO_Analysis_males.csv")
nrow(ebs_data)

nbs_data$Region<-"NBS"

ebs_data$Region<-"EBS"

all_data<-rbind(ebs_data,nbs_data)
nrow(all_data)

############################ Add log10 columns ######################################################################
all_data$log10_width<-log10(all_data$WIDTH)
all_data$log10_weight<-log10(all_data$WEIGHT)

#write data file
write.csv(all_data,"CO_EBS_and_NBS_combined_analysis.csv")


df<-all_data
df1<-subset(df, WEIGHT>0 & SEX==1)
colnames(df1)
dev.new()
plot(df1$WEIGHT~df1$WIDTH)

############################ Plot untransformed data ################################################################
ggplot(df1, aes(x = WIDTH, y = WEIGHT, group = Region)) +
  geom_point(aes(colour = factor(Region)))


q<-ggplot(df1, aes(x = WIDTH, y = WEIGHT, group = Region)) +
  geom_point(aes(colour = Region))
q+ labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (EBS and NBS)")

########################### Log transformed ##########################################################################
dev.new()

############################# points only ############################################################################
ggplot(df1, aes(x = log.width, y = log.weight, group = Region)) +
  geom_point(aes(colour = Region))+
  labs(x="Ln(Width)",y="Ln(Weight)", title="Male EBS CO (EBS and NBS)")


############################# New size weight model###################################################################
nbs_sub<-subset(all_data, Region == "NBS" & SC == "NS" & SEX == 1)
ebs_sub<-subset(all_data, Region == "EBS" & SEX == 1)

fit1<-lm(log.weight~log.width,data=nbs_sub)
fit2<-lm(log.weight~log.width,data=ebs_sub)

coef(fit1)
coef(fit2)

############################ Re-run models using log10 data ##########################################################
fit3<-lm(log10_weight~log10_width,data=nbs_sub)
fit4<-lm(log10_weight~log10_width,data=ebs_sub)

coef(fit3)
coef(fit4)


