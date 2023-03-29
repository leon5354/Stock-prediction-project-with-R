require(car)
require(ggplot2)
require(dplyr)
require(broom)
require(ggpubr)
require(GGally)
require(corrplot)
require(leaps)
require(MASS)
load("lse.RData")
head(lse)

#Q1
#summary 
summary(lse)
#clean data
sum(is.na(lse))
#so the dataset is clean with no NAN value lets go
#remove multicollinearity variable
#set +-0.9 as a point but will examine other suspicious case with scatterplot
lse1=subset(lse,select=-c(Date,Weekday))
cor1=cor(lse1)
corrplot.mixed(cor1, lower.col = "black", number.cex = .4)
#this plot shows there are several multicollinearity variables, I would remove the less correlated one from the model.
#SMT~SPX,AHT,EXPN
scatterplot(SMT ~ SPX, data = lse1)
scatterplot(SMT ~ AHT, data = lse1)
scatterplot(SMT ~ EXPN, data = lse1)
#as SMT seems has to lowest correlation to VOD among those correlated and shows correlation to mutiple variables, so remove SMT from model.

#SSE~SVT,CCH
scatterplot(SSE~ SVT, data = lse1)
scatterplot(SSE~ CCH, data = lse1)
#SSE seems correlated to two variables. 
#SSE~SVT seems more significant, but the CCH one is having a more spread pattern, so i would give it a go.
#by comparing the SSE and SVT, as SSE has more correlation to VOD, so decide to keep SSE.

#EXPN~SPX
scatterplot(SPX~ EXPN, data = lse1)
#it shows quite a linear relationship between two variables, so EXPN would be removed as it shows less corealtion.
#Year~SSE
scatterplot(Year~SSE, data = lse1)
#In this case, its quite hard to tell if its following the pattern, it can only say the stock is decreasing by year. 
# i will still deicide to remove year to keep the model simple.

#check multicollinearity
lsenew=subset(lse1,select=-c(EXPN,SVT,SMT,Year))
summary(lsenew)
cor(lsenew)
plot(lsenew)
#Independence of observations

#is VOD normally distributed
hist(lse1$VOD)
#it is a negative skew, so it is legit for linear regression.

#Linearity
lselin1=subset(lse1,select=c(VOD,STJ,MGGT,TSCO,SPX,AUTO,SSE,AHT,RTO,ABDN))
lselin2=subset(lse1,select=c(VOD,RMV,LLOY,SDR,ABF,BATS,ENT,RR,SMIN))
lselin3=subset(lse1,select=c(VOD,ANTO,BA,PSN,PRU,CCH,CPG,WTB))

pairs(lse1)
#seems all good but some may need transform before build model
plot(lselin1)
plot(lselin2)
plot(lselin3)
#SSE,AHT,LLOY,ABF,SMIN,BA,CCH looks can be trasnformed

#Transforming
lsenew$SSE.log= log10(lsenew$SSE + 1 - min(lsenew$SSE))
#lsenew$SSE.sroot= lsenew$SSE^1/2
scatterplot(VOD ~ lsenew$SSE, data = lsenew)
scatterplot(VOD ~ lsenew$SSE.log, data = lsenew) #good
#scatterplot(VOD ~ lsenew$SSE.sroot, data = lsenew)
####

#lsenew$AHT.log= log10(lsenew$AHT + 1 - min(lsenew$AHT))
#lsenew$AHT.sroot= lsenew$AHT^1/2
scatterplot(VOD ~ AHT, data = lsenew)
#scatterplot(VOD ~ AHT.log, data = lsenew)
#scatterplot(VOD ~ AHT.sroot, data = lsenew)
#not worked

#LLOY
lsenew$LLOY.log = log(lsenew$LLOY)
#lsenew$LLOY.sroot= lsenew$LLOY^1/2
scatterplot(VOD ~ LLOY, data = lsenew)
scatterplot(VOD ~ LLOY.log, data = lsenew) #worked
#scatterplot(VOD ~ LLOY.sroot, data = lsenew)
####

#ABF
lsenew$ABF.log= log10(lsenew$ABF + 1 - min(lsenew$ABF))
#lsenew$ABF.sroot= lsenew$ABF^1/2
scatterplot(VOD ~ ABF, data = lsenew)
scatterplot(VOD ~ ABF.log, data = lsenew) #better
#scatterplot(VOD ~ ABF.sroot, data = lsenew)


#SMIN
#lsenew$SMIN.log= log10(lsenew$SMIN + 1 - min(lsenew$SMIN))
#lsenew$SMIN.sroot= lsenew$SMIN^1/2
#scatterplot(VOD ~ SMIN, data = lsenew)
#scatterplot(VOD ~ SMIN.log, data = lsenew)
#scatterplot(VOD ~ SMIN.sroot, data = lsenew)
#not works

#BA
#lsenew$BA.sqaure2= lsenew$BA^2
#lsenew$BA.sqaure3= lsenew$BA^3
#scatterplot(VOD ~ BA, data = lsenew)
#scatterplot(VOD ~ BA.sqaure2, data = lsenew)
#scatterplot(VOD ~ BA.sqaure3, data = lsenew)
#not works

#CCH
#lsenew$CCH.log= log10(lsenew$CCH + 1 - min(lsenew$CCH))
#lsenew$CCH.sroot= lsenew$CCH^1/2
#scatterplot(VOD ~ CCH, data = lsenew)
#scatterplot(VOD ~ CCH.log, data = lsenew)
#scatterplot(VOD ~ CCH.sroot, data = lsenew)
#not works

#leap and bound
leaps1= leaps(lsenew[,-c(2,8,13,15)],lsenew$VOD,names=names(lsenew)[-c(2,8,13,15)],method="adjr2",nbest = 5)
leaps_res = data.frame(size=leaps1$size,adjr2=leaps1$adjr2,leaps1$which)
plot(adjr2~size,data=leaps_res,ylim=c(0.85,0.91))
# 14 15 pick 14?
leaps_res[leaps_res$size == 14,]
leaps14 = lm(VOD~STJ+SPX+AUTO+ABDN+RMV+BATS+SMIN+ANTO+BA+PRU+CPG+SSE.log+ABF.log, data=lsenew)
#leaps14 = lm(VOD~Year+Month+SPX+AUTO+SSE+ABDN+RMV+BATS+ANTO+BA+PRU+CCH+CPG, data=lsenew)
summary(leaps14)
#Adjusted R-squared:  0.9124

#Assumption Checking
par(mfrow=c(2,2),mar=c(4.5,4,2,2))
plot(leaps14)
#the qqplot shows the residual is almost normally distributed but not the best, its shows kind of skewed to left but generally is good, can look for more
#the scale location shows no obvious trend,so as the residuals vs Leverage
#the residuals vs fitted seems can be .
#check the resid of leaps 14 against all variabel
pairs(data.frame(resid(leaps14),lsenew[,-c(2)]))

#ABDN seems kind of having a trend
plot(resid(leaps14)~ABDN,data = lsenew)

#it is not a obvious pattern but i will try transform it to make the qqplot looks better
plot(resid(leaps14)~log(ABDN^2),data = lsenew)
plot(resid(leaps14)~log(ABDN+3),data = lsenew)
plot(resid(leaps14)~sqrt(ABDN+3),data = lsenew)
#seems nothing works to me but the pattern isnt so obvious so I would leave it there.
#try box cox
boxcox(leaps14, plotit = TRUE,lambda = seq(-4,4))
abline(v = 2.6)
#lambda is around 2.6

lsenew$VOD_train1 = lsenew$VOD^2.6

par(mfrow=c(2,2),mar=c(4.5,4,2,2))
leaps14.square = lm(lsenew$VOD_train1~STJ+SPX+AUTO+ABDN+RMV+BATS+SMIN+ANTO+BA+PRU+CPG+SSE.log+ABF.log, data=lsenew)
plot(leaps14.square)
plot(leaps14)
#QQplot shows more linear relationship than turkey ladder
#lets compare the transformed VOD
boxcox(leaps14.square, plotit = TRUE,lambda = seq(-4,4))
#great
summary(leaps14.square)
#everything seems still significant
#do leap and bound again with new VOD
leaps2= leaps(lsenew[,-c(2,8,13,15,30)],lsenew$VOD_train1,names=names(lsenew)[-c(2,8,13,15,30)],method="adjr2",nbest = 5)
leaps2_res = data.frame(size=leaps2$size,adjr2=leaps2$adjr2,leaps2$which)
plot(adjr2~size,data=leaps2_res,ylim=c(0.85,0.91))
#size of 14 15 16
#similar result shows
leaps2_res[leaps2_res$size == 14,]
leaps14.2 = lm(VOD_train1~TSCO+SPX+AUTO+ABDN+RMV+BATS+SMIN+ANTO+BA+PRU+CPG+SSE.log+ABF.log, data=lsenew)
#compare two model
summary(leaps14)
summary(leaps14.2)
#both of them has similar Adjusted R-squared but leaps14.2 is lower.
par(mfrow=c(2,2),mar=c(4.5,4,2,2))
plot(leaps14)
plot(leaps14.2)
#the qqplot looks better in leaps14.2,other is similar, i would prefer leaps14.2.
#for the independence, in this type of data it is hard to gurantee the independence
acf(resid(leaps14))
acf(resid(leaps14.2))
#according to the ACF,residual temporal autocorrelation is noticed but in this case we are very hard to gurantee the independence of the data.
#######

#try another model
#stepwise
lsenew2=subset(lsenew,select=-c(VOD_train1,SSE,LLOY,ABF))
stepwisemodel=lm(VOD~1,data=lsenew2)
sum(is.na(lsenew2))

#stepwise
step(stepwisemodel,scope=~Month+STJ+MGGT+TSCO+SPX+AUTO+AHT+RTO+ABDN+RMV+SDR+BATS+ENT+RR+SMIN+ANTO+BA+PSN+PRU+CCH+CPG+WTB+SSE.log+LLOY.log+ABF.log, direction = "both")

stepwisemodeldone = lm(VOD ~ ABDN + BATS + AHT + SSE.log + BA + PRU + RMV + 
                         AUTO + ANTO + CPG + SMIN + SPX + STJ + RTO + Month + TSCO + 
                         ABF.log + SDR + PSN + RR + MGGT + WTB, data = lsenew2)

#check assumption
par(mfrow=c(2,2),mar=c(4.5,4,2,2))
plot(stepwisemodeldone)
#the QQplot and residuals vs fitted seems not prefect as it seems not showing completed linear
#attempts transform

for (i in x){
  x = c("Month","VOD","STJ","MGGT","TSCO","SPX","AUTO","AHT","RTO","ABDN","RMV", "SDR","BATS","ENT","RR","SMIN","ANTO","BA","PSN","PRU","CCH","CPG","WTB","SSE.log" ,"LLOY.log" ,"ABF.log" )
  pairs(data.frame(resid(stepwisemodeldone),lsenew2[i]))  
}
#dont see any pattern that seems can be transform here

boxcox(stepwisemodeldone, plotit = TRUE,lambda = seq(-4,4))
abline(v=2.8)
#lambda = 2.8

lsenew2$VOD.three = lsenew2$VOD^2.8

#stepwise

stepwisemodel2=lm(VOD.three~1,data=lsenew2)

step(stepwisemodel2,scope=~Month+STJ+MGGT+TSCO+SPX+AUTO+AHT+RTO+ABDN+RMV+SDR+BATS+ENT+RR+SMIN+ANTO+BA+PSN+PRU+CCH+CPG+WTB+SSE.log+LLOY.log+ABF.log, direction = "both")

stepwisemodeldone2 = lm(VOD.three ~ ABDN + BATS + PSN + CPG + SSE.log + 
                          AUTO + RMV + ANTO + BA + PRU + ABF.log + SPX + SMIN + TSCO + 
                          AHT + WTB + RTO + STJ + SDR + Month + LLOY.log, data = lsenew2)

par(mfrow=c(2,2),mar=c(4.5,4,2,2))
plot(stepwisemodeldone2)
plot(stepwisemodeldone)
#the transform seems improved the situation, stepwisemodeldone2 QQplot and residulas vs fitte looks better
summary(stepwisemodeldone2)
summary(stepwisemodeldone)
#they share the similar ADJR2, pick stepwisemodeldone2
##the residuals vs fitted is great, it looks spreaded with not a very obvious pattern
#QQ plot looks okay, maybe able to transform it a little 
#scake location and residuals vs leverage shows there is no paticular outliner.
#the transform did seems improve the linear relationship for a little bit so i would keep the transformed one.
acf(resid(stepwisemodeldone2))
#according to the ACF,residual temporal autocorrelation is noticed but in this case we are very hard to grantee the independence of the data.

#Forward
lsenew3=subset(lsenew,select=-c(VOD_train1,SSE,LLOY,ABF))
forwardmodel=lm(VOD~1,data=lsenew3)
sum(is.na(lsenew3))

step(forwardmodel,scope=~Month+STJ+MGGT+TSCO+SPX+AUTO+AHT+RTO+ABDN+RMV+SDR+BATS+ENT+RR+SMIN+ANTO+BA+PSN+PRU+CCH+CPG+WTB+SSE.log+LLOY.log+ABF.log, direction = "forward")

forwardmodeldone = lm(VOD ~ ABDN + BATS + AHT + ENT + SSE.log + BA + PRU + 
                         RMV + AUTO + ANTO + CPG + SMIN + SPX + STJ + RTO + Month + 
                         TSCO + ABF.log + SDR + PSN + RR + MGGT + WTB, data = lsenew3)

#check assumption
par(mfrow=c(2,2),mar=c(4.5,4,2,2))
plot(forwardmodeldone)
#the QQplot and residuals vs fitted seems not prefect as it seems not showing completed linear
#attempts transform


for (i in x){
  x = c("Month","VOD","STJ","MGGT","TSCO","SPX","AUTO","AHT","RTO","ABDN","RMV", "SDR","BATS","ENT","RR","SMIN","ANTO","BA","PSN","PRU","CCH","CPG","WTB","SSE.log" ,"LLOY.log" ,"ABF.log" )
  pairs(data.frame(resid(forwardmodeldone),lsenew3[i]))  
}
#dont see any pattern that seems can be transform here

boxcox(forwardmodeldone, plotit = TRUE,lambda = seq(-4,4))
abline(v=2.8)
#lambda = 2.8

lsenew3$VOD.three = lsenew3$VOD^2.8

#Forward

forwardmodel2=lm(VOD.three~1,data=lsenew3)

step(forwardmodel2,scope=~Month+STJ+MGGT+TSCO+SPX+AUTO+AHT+RTO+ABDN+RMV+SDR+BATS+ENT+RR+SMIN+ANTO+BA+PSN+PRU+CCH+CPG+WTB+SSE.log+LLOY.log+ABF.log, direction = "forward")

forwardmodeldone2 = lm(VOD.three ~ ABDN + BATS + PSN + CPG + SSE.log + 
                         AUTO + RMV + ANTO + BA + PRU + ABF.log + SPX + SMIN + TSCO + 
                         AHT + WTB + RTO + STJ + SDR + Month + LLOY.log, data = lsenew3)
#same model is generated as stepwise
par(mfrow=c(2,2),mar=c(4.5,4,2,2))
plot(forwardmodeldone2)
plot(forwardmodeldone)
#the transform seems improved the situation, forwardmodeldone2 QQplot and residulas vs fitte looks better
summary(forwardmodeldone2)
summary(forwardmodeldone)
#they share the similar ADJR2, pick forwardmodeldone2
##the residuals vs fitted is great, it looks spreaded with not a very obvious pattern
#QQ plot looks okay, maybe able to transform it a little 
#scake location and residuals vs leverage shows there is no paticular outliner.
#the transform did seems improve the linear relationship for a little bit so i would keep the transformed one.
acf(resid(forwardmodeldone2))
#according to the ACF,residual temporal autocorrelation is noticed but in this case we are very hard to grantee the independence of the data.


#Backward
lsenew4=subset(lsenew,select=-c(VOD_train1,SSE,LLOY,ABF))
backwardmodel=lm(VOD~Month+STJ+MGGT+TSCO+SPX+AUTO+AHT+RTO+ABDN+RMV+SDR+BATS+ENT+RR+SMIN+ANTO+BA+PSN+PRU+CCH+CPG+WTB+SSE.log+LLOY.log+ABF.log,data=lsenew4)
sum(is.na(lsenew4))

step(backwardmodel,scope=~Month+STJ+MGGT+TSCO+SPX+AUTO+AHT+RTO+ABDN+RMV+SDR+BATS+ENT+RR+SMIN+ANTO+BA+PSN+PRU+CCH+CPG+WTB+SSE.log+LLOY.log+ABF.log, direction = "backward")

backwardmodeldone = lm(VOD ~ Month + STJ + MGGT + TSCO + SPX + AUTO + AHT + RTO + ABDN + 
                         RMV + SDR + BATS + RR + SMIN + ANTO + BA + PSN + PRU + CPG + 
                         WTB + SSE.log + ABF.log, data = lsenew4)

#check assumption
par(mfrow=c(2,2),mar=c(4.5,4,2,2))
plot(backwardmodeldone)
#the QQplot and residuals vs fitted seems not prefect as it seems not showing completed linear
#attempts transform

for (i in x){
  x = c("Month","VOD","STJ","MGGT","TSCO","SPX","AUTO","AHT","RTO","ABDN","RMV", "SDR","BATS","ENT","RR","SMIN","ANTO","BA","PSN","PRU","CCH","CPG","WTB","SSE.log" ,"LLOY.log" ,"ABF.log" )
  pairs(data.frame(resid(backwardmodeldone),lsenew4[i]))  
}
#dont see any pattern that seems can be transform here

boxcox(backwardmodeldone, plotit = TRUE,lambda = seq(-4,4))
abline(v=2.8)
#lambda = 2.8

lsenew4$VOD.three = lsenew4$VOD^2.8

#backward

backwardmodel2=lm(VOD.three~Month+STJ+MGGT+TSCO+SPX+AUTO+AHT+RTO+ABDN+RMV+SDR+BATS+ENT+RR+SMIN+ANTO+BA+PSN+PRU+CCH+CPG+WTB+SSE.log+LLOY.log+ABF.log,data=lsenew4)
sum(is.na(lsenew4))

step(backwardmodel2,scope=~Month+STJ+MGGT+TSCO+SPX+AUTO+AHT+RTO+ABDN+RMV+SDR+BATS+ENT+RR+SMIN+ANTO+BA+PSN+PRU+CCH+CPG+WTB+SSE.log+LLOY.log+ABF.log, direction = "backward")

backwardmodeldone2 = lm(VOD.three ~ Month + STJ + MGGT + TSCO + SPX + AUTO + 
                          AHT + RTO + ABDN + RMV + SDR + BATS + RR + SMIN + ANTO + 
                          BA + PSN + PRU + CPG + WTB + SSE.log + LLOY.log + ABF.log, 
                        data = lsenew4)
#same model is generated as stepwise
par(mfrow=c(2,2),mar=c(4.5,4,2,2))
plot(backwardmodeldone2)
plot(backwardmodeldone)
#the transform seems improved the situation, backwardmodeldone2 QQplot and residulas vs fitte looks better
summary(forwardmodeldone2)
summary(forwardmodeldone)
#they share the similar ADJR2, pick forwardmodeldone2
##the residuals vs fitted is great, it looks spreaded with not a very obvious pattern
#QQ plot looks okay, maybe able to transform it a little 
#scake location and residuals vs leverage shows there is no paticular outliner.
#the transform did seems improve the linear relationship for a little bit so i would keep the transformed one.
acf(resid(forwardmodeldone2))
#according to the ACF,residual temporal autocorrelation is noticed but in this case we are very hard to grantee the independence of the data.
summary(leaps14.2)
summary(stepwisemodeldone2)
summary(forwardmodeldone2)
summary(backwardmodeldone2)
#ADJR2 backward > forward=stepwise > leap4.2
plot(leaps14.2)
#VOD_train1~TSCO+SPX+AUTO+ABDN+RMV+BATS+SMIN+ANTO+BA+PRU+CPG+SSE.log+ABF.log
plot(stepwisemodeldone2)
#VOD.three ~ ABDN + BATS + PSN + CPG + SSE.log + 
#AUTO + RMV + ANTO + BA + PRU + ABF.log + SPX + SMIN + TSCO + 
#AHT + WTB + RTO + STJ + SDR + Month + LLOY.log
plot(forwardmodeldone2)
#ABDN + BATS + PSN + CPG + SSE.log + 
#AUTO + RMV + ANTO + BA + PRU + ABF.log + SPX + SMIN + TSCO + 
#AHT + WTB + RTO + STJ + SDR + Month + LLOY.log
plot(backwardmodeldone2)
#VOD.three ~ Month + STJ + MGGT + TSCO + SPX + AUTO + 
#AHT + RTO + ABDN + RMV + SDR + BATS + RR + SMIN + ANTO + 
#BA + PSN + PRU + CPG + WTB + SSE.log + LLOY.log + ABF.log
#backward is the best in QQ and other are very similar
acf(resid(leaps14.2))
acf(resid(stepwisemodeldone2))
acf(resid(forwardmodeldone2))
acf(resid(backwardmodeldone2))
##according to the ACF,residual temporal autocorrelation is noticed but in this case we are very hard to grantee the independence of the data.
#PRESS STATISITC
sum((leaps14.2$residuals^2)/(1-hatvalues(leaps14.2))^2)

