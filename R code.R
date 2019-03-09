library(stats)
library(leaps)
library(dplyr)
library(car)
library(s20x)
library(gvlma)
library(MASS)
df<-read.csv("C:\\Users\\sundarakishore\\Desktop\\neu classes\\2nd sem\\Data Mining\\assignment-3\\Concrete Slump Test Data.csv")
df<-df[,-c(1,14,13,12)]
a<-cor(df)

####function for residplt()###
residplot<-function(fit,nbreaks=10){
  z<-rstudent(fit)
  hist(z,breaks=nbreaks,freq = FALSE)
  xlab="Studentized Residual"
  main="Distribution of Errors"
  rug(jitter(z),col="brown")
  curve(dnorm(x,mean=mean(z),sd=sd(z)),add = TRUE,col="blue",lwd=2)
  lines(density(z)$x,density(z)$y,col="red",lwd=2,lty=2)
  legend("topright",legend=c("Normal Curve","Kernle Density Curve"),lty=1:2,col=c("blue","red"),cex=.7)
}

########High Leverage points ##############33
hat.plot<-function(fit){
  p<-length(coefficients(fit))
  n<-length(fitted(fit))
  plot(hatvalues(fit),main="index plot of hat values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}


#Scatterplot
pairs(df, main = "Scatterplot matrix")

#Simple Linear Regression
fit1<-lm(Slump.Flow~Water,data=df)
summary(fit1)
#Multiple Linear Regression
fit2<-lm(Slump.Flow~Water+Cement+Slag+Fly.Ash+SP+Coarse.Aggregate+Fine.Aggregate,data=df)
summary(fit2)
#Polynomial Regression
fit3<-lm(Slump.Flow~Water+I(Water^2),data=df)
summary(fit3)

###Regression Diagnostics
confint(fit1)
confint(fit2)
confint(fit3)

###Typical approach
par(mfrow=c(2,2))
plot(fit1)
plot(fit2)
plot(fit3)

###enhanced approach
par(mfrow=c(1,1))
qqPlot(fit1,labels=row.names(df),id.method="identify",simulate=TRUE,main="Q-Q Plot")
qqPlot(fit2,labels=row.names(df),id.method="identify",simulate=TRUE,main="Q-Q Plot")
qqPlot(fit3,labels=row.names(df),id.method="identify",simulate=TRUE,main="Q-Q Plot")
#Residual plots
residplot(fit1)
residplot(fit2)
residplot(fit3)
#Independece of errors
durbinWatsonTest(fit1) 
durbinWatsonTest(fit2) 
durbinWatsonTest(fit3)
#Linearity
crPlots(fit1)
crPlots(fit2)
crPlots(fit3)
#Homoscedasticity
ncvTest(fit1)
ncvTest(fit2)
ncvTest(fit3)

spreadLevelPlot(fit1)
spreadLevelPlot(fit2)
spreadLevelPlot(fit3)
###Global test
gvmodel1<-gvlma(fit1)
summary(gvmodel1)
gvmodel2<-gvlma(fit2)
summary(gvmodel2)
gvmodel3<-gvlma(fit3)
summary(gvmodel3)
###Multicollinearity
vif(fit1)
vif(fit2)
vif(fit3)
###Unusual observations- outliers
outlierTest(fit1)
outlierTest(fit2)
outlierTest(fit3)
#High Leverage Points
hat.plot(fit1)
hat.plot(fit2)
hat.plot(fit3)
###Influential observations
cutoff <- 4/(nrow(df)-length(fit1$coefficients)-2)
plot(fit1,which=4,cook.levels=cutoff)

cutoff <- 4/(nrow(df)-length(fit2$coefficients)-2)
plot(fit2,which=4,cook.levels=cutoff)

cutoff <- 4/(nrow(df)-length(fit3$coefficients)-2)
plot(fit3,which=4,cook.levels=cutoff)
###Influence plot
influencePlot(fit1, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's distance")
influencePlot(fit2, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's distance")
influencePlot(fit3, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's distance")
###Corrective measures
sqrt(vif(fit1))>2
sqrt(vif(fit2))>2
sqrt(vif(fit3))>2
df<-df[-c(69),]
###Best regression model
AIC(fit1, fit2, fit3)

#Fine Tune
Step_fit<-lm(Slump.Flow~Water+Cement+Slag+Fly.Ash+SP+Coarse.Aggregate+Fine.Aggregate,data=df)
stepAIC(Step_fit, direction = "backward") 

#### Question 2 Forest Fires Data
library(readxl)
forest<-read_xlsx("Forest Fires Data.xlsx")
###log transformation of area
Areas<- log(forest$Area+1)
###change month and day to numeric
library(Hmisc)
tst <- capitalize(c(forest$Month))
match(tst, month.abb)
Months<-match(tst, month.abb)
Days<-factor(c(forest$Day))
Days<-as.numeric(Days)
forest[["Month"]] <- Months
forest[["Day"]]<-Days
forest[["Area"]]<-Areas
attach(forest)
###Scatterplot
pairs(forest, main = "Scatterplot matrix")
###Using pearson
library(psych)
pairs.panels(forest,method="pearson")
###Model 1 Multiple linear regression
fit1<-lm(Area~X+Y+FFMC+DMC+DC+ISI+Temp+RH+Wind+Rain+forest$Month+forest$Day,data=forest)
summary(fit1)
###Model 2 polynomial regression
fit2<-lm(Area~Month + I(Month^2)+I(Month^3)+I(Month^4))
summary(fit2)
###Model 2 Simple linear regression
fit3<-lm(Area~Month)
summary(fit3)
###Model 4 Multiple Linear regression
fit4<-lm(Area~Month+X+Wind+DC+DMC,data=forest)
summary(fit4)
###Regression Diagnostics
confint(fit1)
confint(fit2)
confint(fit3)
confint(fit4)
###Typical approach
par(mfrow=c(2,2))
plot(fit1)
plot(fit2)
plot(fit3)
plot(fit4)
###enhanced approach
par(mfrow=c(1,1))
library(car)
qqPlot(fit1,labels=row.names(forest),id.method="identify",simuate=TRUE, main ="Q-Q Plot")
qqPlot(fit2,labels=row.names(forest),id.method="identify",simuate=TRUE, main ="Q-Q Plot")
qqPlot(fit3,labels=row.names(forest),id.method="identify",simuate=TRUE, main ="Q-Q Plot")
qqPlot(fit4,labels=row.names(forest),id.method="identify",simuate=TRUE, main ="Q-Q Plot")
###residplot function

residplot(fit1)
residplot(fit2)
residplot(fit3)
residplot(fit4)
###Independence of errors
durbinWatsonTest(fit1)
durbinWatsonTest(fit2)
durbinWatsonTest(fit3)
durbinWatsonTest(fit4)
###Linearity
crPlots(fit1)
crPlots(fit2)
crPlots(fit3)
crPlots(fit4)
###Homoscedasticity
ncvTest(fit1)
ncvTest(fit2)
ncvTest(fit3)
ncvTest(fit4)
spreadLevelPlot(fit1)
spreadLevelPlot(fit2)
spreadLevelPlot(fit3)
spreadLevelPlot(fit4)
###Global test
library(gvlma)
globvalmodel1 <- gvlma(fit1) 
summary(globvalmodel1)
globvalmodel2 <- gvlma(fit2) 
summary(globvalmodel2)
globvalmodel3 <- gvlma(fit3) 
summary(globvalmodel3)
globvalmodel4 <- gvlma(fit4) 
summary(globvalmodel4)
###Multicollinearity
vif(fit1)
vif(fit2)
vif(fit3)
vif(fit4)
###Unusual observations- outliers
outlierTest(fit1)
outlierTest(fit2)
outlierTest(fit3)
outlierTest(fit4)
###High leverage points

hat.plot(fit1)
hat.plot(fit2)
hat.plot(fit3)
hat.plot(fit4)
###Influential observations
cutoff <- 4/(nrow(forest)-length(fit1$coefficients)-2)
plot(fit1,which=4,cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(forest)-length(fit2$coefficients)-2)
plot(fit2,which=4,cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(forest)-length(fit3$coefficients)-2)
plot(fit3,which=4,cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
cutoff <- 4/(nrow(forest)-length(fit4$coefficients)-2)
plot(fit4,which=4,cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
###Influence plot
influencePlot(fit1, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's distance")
influencePlot(fit2, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's distance")
influencePlot(fit3, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's distance")
influencePlot(fit4, id.method="identify", main="Influence Plot", sub="Circle size is proportional to Cook's distance")
###Corrective measures
sqrt(vif(fit1))>2
sqrt(vif(fit2))>2
sqrt(vif(fit3))>2
sqrt(vif(fit4))>2
forest_data <- forest[-c(510,380,500),]
newfit <- fit4 <- lm(Area~Month+X+Wind+DC+DMC,data=forest[-c(239,416),])
outlierTest(newfit)
###Best regression model
AIC(fit1, fit2, fit3, fit4)
###Fine tune
library(MASS)
step_fit <- lm(Area ~ X + Y + FFMC + ISI + DMC + DC + Temp + Wind + Rain, data = forest)
stepAIC(step_fit, direction = "backward") 


