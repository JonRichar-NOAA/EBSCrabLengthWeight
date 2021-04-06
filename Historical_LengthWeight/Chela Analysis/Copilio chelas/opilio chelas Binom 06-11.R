library(stats4)

Model 1
Simple
Mat=function(CW50,s) {
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=100,s=-11)
mMat=mle(Mat,start=param1)
summary(mMat)

Model 2
N/S
MatL=function(CW50N,CW50S,s) {
	CW50=CW50N*(MMat$Loc=="north")+CW50S*(MMat$Loc=="south")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50N=70,CW50S=80,s=-6)
mMatL=mle(MatL,start=param1)
summary(mMatL)

Model 3
Year
MatY=function(CW50_06,CW50_07,CW50_09,CW50_10,CW50_11,s) {
	CW50=CW50_06*(MMat$Year==2006)+CW50_07*(MMat$Year==2007)+CW50_09*(MMat$Year==2009)+CW50_10*(MMat$Year==2010)+CW50_11*(MMat$Year==2011)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50_06=70,CW50_07=70,CW50_09=70,CW50_10=70,CW50_11=70,s=-6)
mMatY=mle(MatY,start=param1)
summary(mMatY)

Model 4
Location Year
MatLY=function(CW50N,CW50_06,CW50_07,CW50_09,CW50_10,CW50_11,s) {
	CW50=CW50_06*(MMat$Year==2006)+CW50_07*(MMat$Year==2007)+CW50_09*(MMat$Year==2009)+CW50_10*(MMat$Year==2010)+CW50_11*(MMat$Year==2011)
	CW50=CW50+CW50N*(MMat$Loc=="north")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50N=-6,CW50_06=70,CW50_07=70,CW50_09=70,CW50_10=70,CW50_11=70,s=-6)
mMatLY=mle(MatLY,start=param1)
summary(mMatLY)

Model 5
location x year
MatLxY=function(CW50N,CW50N_07,CW50N_09,CW50N_10,CW50N_11,CW50_06,CW50_07,CW50_09,CW50_10,CW50_11,s) {
	CW50=CW50_06*(MMat$Year==2006)+CW50_07*(MMat$Year==2007)+CW50_09*(MMat$Year==2009)+CW50_10*(MMat$Year==2010)+CW50_11*(MMat$Year==2011)
	CW50=CW50+CW50N*(MMat$Loc=="north")+CW50N_07*(MMat$Loc=="north")*(MMat$Year==2007)+CW50N_09*(MMat$Loc=="north")*(MMat$Year==2009)+CW50N_10*(MMat$Loc=="north")*(MMat$Year==2010)+CW50N_11*(MMat$Loc=="north")*(MMat$Year==2011)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50N=-3,CW50N_07=0,CW50N_09=0,CW50N_10=0,CW50N_11=0,CW50_06=70,CW50_07=70,CW50_09=70,CW50_10=70,CW50_11=70,s=-6)
mMatLxY=mle(MatLxY,start=param1)
summary(mMatLxY)

Model 6
s(Loc)
MatL=function(CW50,sS,sN) {
	s=sS*(MMat$Loc=="south")+sN*(MMat$Loc=="north")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=70,sS=-6,sN=-6)
mMatL=mle(MatL,start=param1)
summary(mMatL)

Model 7
s (Y)
MatY=function(CW50,s_06,s_07,s_09,s_10,s_11) {
	s=s_06*(MMat$Year==2006)+s_07*(MMat$Year==2007)+s_09*(MMat$Year==2009)+s_10*(MMat$Year==2010)+s_11*(MMat$Year==2011)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=70,s_06=-6,s_07=-6,s_09=-6,s_10=-6,s_11=6)
mMatY=mle(MatY,start=param1)
summary(mMatY)

Model 8
S (L, Y)
MatLY=function(CW50,sN,s_06,s_07,s_09,s_10,s_11) {
	s=s_06*(MMat$Year==2006)+s_07*(MMat$Year==2007)+s_09*(MMat$Year==2009)+s_10*(MMat$Year==2010)+s_11*(MMat$Year==2011)
	s=s+sN*(MMat$Loc=="north")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=100,sN=0,s_06=-6,s_07=-6,s_09=-6,s_10=-6,s_11=-6)
mMatLY=mle(MatLY,start=param1)
summary(mMatLY)

Model 9
S (L*Y)
MatLxY=function(CW50,sN,sN_07,sN_09,sN_10,sN_11,s_06,s_07,s_09,s_10,s_11) {
	s=s_06*(MMat$Year==2006)+s_07*(MMat$Year==2007)+s_09*(MMat$Year==2009)+s_10*(MMat$Year==2010)+s_11*(MMat$Year==2011)
	s=s+sN*(MMat$Loc=="north")+sN_07*(MMat$Loc=="north")*(MMat$Year==2007)+sN_09*(MMat$Loc=="north")*(MMat$Year==2009)+sN_10*(MMat$Loc=="north")*(MMat$Year==2010)+sN_11*(MMat$Loc=="north")*(MMat$Year==2011)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=70,sN=0,sN_07=0,sN_09=0,sN_10=0,sN_11=0,s_06=-6,s_07=-6,s_09=-6,s_10=-6,s_11=-6)
mMatLxY=mle(MatLxY,start=param1)
summary(mMatLxY)


Model 10
CW50 Loc, Year and s (L*Y)
MatLxY=function(CW50N,CW50_06,CW50_07,CW50_09,CW50_10,CW50_11,sN,sN_07,sN_09,sN_10,sN_11,s_06,s_07,s_09,s_10,s_11) {
	CW50=CW50_06*(MMat$Year==2006)+CW50_07*(MMat$Year==2007)+CW50_09*(MMat$Year==2009)+CW50_10*(MMat$Year==2010)+CW50_11*(MMat$Year==2011)
	CW50=CW50+CW50N*(MMat$Loc=="north")
	s=s_06*(MMat$Year==2006)+s_07*(MMat$Year==2007)+s_09*(MMat$Year==2009)+s_10*(MMat$Year==2010)+s_11*(MMat$Year==2011)
	s=s+sN*(MMat$Loc=="north")+sN_07*(MMat$Loc=="north")*(MMat$Year==2007)+sN_09*(MMat$Loc=="north")*(MMat$Year==2009)+sN_10*(MMat$Loc=="north")*(MMat$Year==2010)+sN_11*(MMat$Loc=="north")*(MMat$Year==2011)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50N=-3,CW50_06=70,CW50_07=70,CW50_09=70,CW50_10=70,CW50_11=70,sN=0,sN_07=0,sN_09=0,sN_10=0,sN_11=0,s_06=-6,s_07=-6,s_09=-6,s_10=-6,s_11=-6)
mMatLxY=mle(MatLxY,start=param1)
summary(mMatLxY)


