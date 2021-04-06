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
MatY=function(CW50_08,CW50_10,CW50_12,s) {
	CW50=CW50_08*(MMat$Year==2008)+CW50_10*(MMat$Year==2010)+CW50_12*(MMat$Year==2012)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50_08=100,CW50_10=100,CW50_12=100,s=-11)
mMatY=mle(MatY,start=param1)
summary(mMatY)

Model 4
MatLY=function(CW50W,CW50_08,CW50_10,CW50_12,s) {
	CW50=CW50_08*(MMat$Year==2008)+CW50_10*(MMat$Year==2010)+CW50_12*(MMat$Year==2012)
	CW50=CW50+CW50W*(MMat$Loc=="West")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50W=-3,CW50_08=100,CW50_10=100,CW50_12=100,s=-11)
mMatLY=mle(MatLY,start=param1)
summary(mMatLY)


Model 5
MatLxY=function(CW50W,CW50W_10,CW50W_12,CW50_08,CW50_10,CW50_12,s) {
	CW50=CW50_08*(MMat$Year==2008)+CW50_10*(MMat$Year==2010)+CW50_12*(MMat$Year==2012)
	CW50=CW50+CW50W*(MMat$Loc=="West")+CW50W_10*(MMat$Loc=="West")*(MMat$Year==2010)+CW50W_12*(MMat$Loc=="West")*(MMat$Year==2012)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50W=-3,CW50W_10=0,CW50W_12=0,CW50_08=100,CW50_10=100,CW50_12=100,s=-11)
mMatLxY=mle(MatLxY,start=param1)
summary(mMatLxY)

Model 6
s(Loc)
MatL=function(CW50,sE,sW) {
	s=sE*(MMat$Loc=="East")+sW*(MMat$Loc=="West")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=100,sE=-9,sW=-9)
mMatL=mle(MatL,start=param1)
summary(mMatL)

Model 7
MatY=function(CW50,s_08,s_10,s_12) {
	s=s_08*(MMat$Year==2008)+s_10*(MMat$Year==2010)+s_12*(MMat$Year==2012)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=100,s_08=-9,s_10=-9,s_12=-9)
mMatY=mle(MatY,start=param1)
summary(mMatY)

Model 8
MatLY=function(CW50,sW,s_08,s_10,s_12) {
	s=s_08*(MMat$Year==2008)+s_10*(MMat$Year==2010)+s_12*(MMat$Year==2012)
	s=s+sW*(MMat$Loc=="West")
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=100,sW=0,s_08=-9,s_10=-9,s_12=-9)
mMatLY=mle(MatLY,start=param1)
summary(mMatLY)

Model 9
MatLxY=function(CW50,sW,sW_10,sW_12,s_08,s_10,s_12) {
	s=s_08*(MMat$Year==2008)+s_10*(MMat$Year==2010)+s_12*(MMat$Year==2012)
	s=s+sW*(MMat$Loc=="West")+sW_10*(MMat$Loc=="West")*(MMat$Year==2010)+sW_12*(MMat$Loc=="West")*(MMat$Year==2012)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50=100,sW=0,sW_10=0,sW_12=0,s_08=-9,s_10=-9,s_12=-9)
mMatLxY=mle(MatLxY,start=param1)
summary(mMatLxY)


Model 10
MatLxY=function(CW50W,CW50_08,CW50_10,CW50_12,sW,sW_10,sW_12,s_08,s_10,s_12) {
	CW50=CW50_08*(MMat$Year==2008)+CW50_10*(MMat$Year==2010)+CW50_12*(MMat$Year==2012)
	CW50=CW50+CW50W*(MMat$Loc=="West")
	s=s_08*(MMat$Year==2008)+s_10*(MMat$Year==2010)+s_12*(MMat$Year==2012)
	s=s+sW*(MMat$Loc=="West")+sW_10*(MMat$Loc=="West")*(MMat$Year==2010)+sW_12*(MMat$Loc=="West")*(MMat$Year==2012)
	ave=1/(1+(MMat$CW/CW50)^s)
 	-sum(dbinom(MMat$maturity,prob=ave,size=1,log=TRUE))
 }
param1=list(CW50W=-3,CW50_08=100,CW50_10=100,CW50_12=100,sW=0,sW_10=0,sW_12=0,s_08=-9,s_10=-9,s_12=-9)
mMatLxY=mle(MatLxY,start=param1)
summary(mMatLxY)

