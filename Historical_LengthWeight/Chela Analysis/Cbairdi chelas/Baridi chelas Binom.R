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
E/W
MatL=function(CW50E,CW50W,s) {
	CW50=CW50E*(MMat$Loc=="East")+CW50W*(MMat$Loc=="West")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50E=90,CW50W=100,s=-11)
mMatL=mle(MatL,start=param1)
summary(mMatL)

Model 3
MatY=function(CW50_06,CW50_07,CW50_08,CW50_09,CW50_10,CW50_11,CW50_12,s) {
	CW50=CW50_06*(MMat$Year==2006)+CW50_07*(MMat$Year==2007)+CW50_08*(MMat$Year==2008)+CW50_09*(MMat$Year==2009)+CW50_10*(MMat$Year==2010)+CW50_11*(MMat$Year==2011)+CW50_12*(MMat$Year==2012)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50_06=100,CW50_07=100,CW50_08=100,CW50_09=100,CW50_10=100,CW50_11=100,CW50_12=100,s=-11)
mMatY=mle(MatY,start=param1)
summary(mMatY)

Model 4
MatLY=function(CW50W,CW50_06,CW50_07,CW50_08,CW50_09,CW50_10,CW50_11,CW50_12,s) {
	CW50=CW50_06*(MMat$Year==2006)+CW50_07*(MMat$Year==2007)+CW50_08*(MMat$Year==2008)+CW50_09*(MMat$Year==2009)+CW50_10*(MMat$Year==2010)+CW50_11*(MMat$Year==2011)+CW50_12*(MMat$Year==2012)
	CW50=CW50+CW50W*(MMat$Loc=="West")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50W=-3,CW50_06=100,CW50_07=100,CW50_08=100,CW50_09=100,CW50_10=100,CW50_11=100,CW50_12=100,s=-11)
mMatLY=mle(MatLY,start=param1)
summary(mMatLY)


