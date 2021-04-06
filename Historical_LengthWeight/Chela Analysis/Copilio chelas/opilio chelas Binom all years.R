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
MatY=function(CW50_06,CW50_07,CW50_09,CW50_10,CW50_11,CW50_12,s) {
	CW50=CW50_06*(MMat$Year==2006)+CW50_07*(MMat$Year==2007)+CW50_09*(MMat$Year==2009)+CW50_10*(MMat$Year==2010)+CW50_11*(MMat$Year==2011)+CW50_12*(MMat$Year==2012)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50_06=70,CW50_07=70,CW50_09=70,CW50_10=70,CW50_11=70,CW50_12=70,s=-6)
mMatY=mle(MatY,start=param1)
summary(mMatY)

Model 4
Location Year
MatLY=function(CW50N,CW50_06,CW50_07,CW50_09,CW50_10,CW50_11,CW50_12,s) {
	CW50=CW50_06*(MMat$Year==2006)+CW50_07*(MMat$Year==2007)+CW50_09*(MMat$Year==2009)+CW50_10*(MMat$Year==2010)+CW50_11*(MMat$Year==2011)+CW50_12*(MMat$Year==2012)
	CW50=CW50+CW50N*(MMat$Loc=="north")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50N=-6,CW50_06=70,CW50_07=70,CW50_09=70,CW50_10=70,CW50_11=70,CW50_12=70,s=-6)
mMatLY=mle(MatLY,start=param1)
summary(mMatLY)

Model 5
N/S grad
MatG=function(CW50,CW50G,s) {
	CW50=CW50+CW50G*(MMat$Lat-54.97993)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=70,CW50G=2.077654,s=-6)
mMatG=mle(MatG,start=param1)
summary(mMatG)

Model 5
N/S grad, Year
MatGY=function(CW50G,CW50_06,CW50_07,CW50_09,CW50_10,CW50_11,CW50_12,s) {
	CW50=CW50_06*(MMat$Year==2006)+CW50_07*(MMat$Year==2007)+CW50_09*(MMat$Year==2009)+CW50_10*(MMat$Year==2010)+CW50_11*(MMat$Year==2011)+CW50_12*(MMat$Year==2012)
	CW50=CW50+CW50G*(MMat$Lat-54.97993)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50_06=70,CW50_07=70,CW50_09=70,CW50_10=70,CW50_11=70,CW50_12=70,CW50G=2.077654,s=-6)
mMatGY=mle(MatGY,start=param1)
summary(mMatGY)



summary(MMat$Lat)



