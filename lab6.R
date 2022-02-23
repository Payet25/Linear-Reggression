# 1.
dat<-read.table("gala.txt",header=T)
attach(dat)
# backward selection
bbest<-stepback(Species,data.frame(Area,Elevation,Nearest,Scruz,Adjacent),alfa=0.2)
# forward selection
fbest<-stepfor(Species,data.frame(Area,Elevation,Nearest,Scruz,Adjacent),alfa=0.2)
summary(bbest)
summary(fbest)

# for both algotithmic methods the best model is the same.
# we save this model in an object with the name best
best<-bbest

#2.
rjack<-rstudent(best)
yhat<-fitted(best)

#3.
# checking for normality
# statistical test
shapiro.test(rjack)
# graphical methods
par(mfrow=c(1,2))
qqnorm(rjack)
qqline(rjack)
hist(rjack,xlab="Jackknife residuals",main="Jackknife residuals")
graphics.off()
# checking for constant error variance
plot(yhat,rjack,xlab="Predicted values", ylab="Jackknife residuals")
abline(h=0)
abline(h=2,lty=2)
abline(h=-2,lty=2)

# 4.
# sqrt-transforamtion
# backward selection
tbbest<-stepback(sqrt(Species),data.frame(Area,Elevation,Nearest,Scruz,Adjacent),alfa=0.20)
# forward selection
tfbest<-stepfor(sqrt(Species),data.frame(Area,Elevation,Nearest,Scruz,Adjacent),alfa=0.20)
summary(tbbest)
summary(tfbest)
# for both algotithmic methods the best model is the same.
# we save this model in an object with the name tbest
tbest<-tbbest
# The independent variable Scruz ($p$-value=0.184) is not statistical significant
# even for a significant level of $\a=10\%$.
# So we should remove this variable for our model.
# The final best  model takes the following form

tbest<-lm(sqrt(Species)~ Area + Elevation + Adjacent)


#Estimate the fitted values, residuals, Cook?s distances and the leverages
rjack<-rstudent(tbest)
yhat<-fitted(tbest)
rstud<-rstandard(tbest)
h<-hatvalues(tbest)
d<-cooks.distance(tbest)


# checking for normality
# statistical test
shapiro.test(rjack)
# graphical methods
par(mfrow=c(1,2))
qqnorm(rjack)
qqline(rjack)
hist(rjack,xlab="Jackknife residuals",main="Jackknife residuals")
graphics.off()
# checking for constant error variance
plot(yhat,rjack,xlab="Predicted values", ylab="Jackknife residuals")
abline(h=0)
abline(h=2,lty=2)
abline(h=-2,lty=2)
identify(yhat,rjack,id)
