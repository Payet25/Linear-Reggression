dat<-read.table("betaplasm.txt",header=T)
attach(dat)

1. # Forward selection 
mod1 = lm(betplasm ~ age)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod2 = lm(betplasm ~ sex)
summ.mod2 = summary(mod2)
summ.mod2$coefficients

mod3 = lm(betplasm ~ smokstat)
summ.mod3 = summary(mod3)
summ.mod3$coefficients

mod4 = lm(betplasm ~ quetelet)
summ.mod4 = summary(mod4)
summ.mod4$coefficients

mod5 = lm(betplasm ~ vituse)
summ.mod5 = summary(mod5)
summ.mod5$coefficients

mod6 = lm(betplasm ~ calories)
summ.mod6 = summary(mod6)
summ.mod6$coefficients

mod7 = lm(betplasm ~ fat)
summ.mod7 = summary(mod7)
summ.mod7$coefficients

mod8 = lm(betplasm ~ fiber)
summ.mod8 = summary(mod8)
summ.mod8$coefficients

mod9 = lm(betplasm ~ alcohol)
summ.mod9 = summary(mod9)
summ.mod9$coefficients

mod10 = lm(betplasm ~ chol)
summ.mod10 = summary(mod10)
summ.mod10$coefficients

mod11 = lm(betplasm ~ betadiet)
summ.mod11 = summary(mod11)
summ.mod11$coefficients

mod12 = lm(betplasm ~ retdiet)
summ.mod12 = summary(mod12)
summ.mod12$coefficients

# fiber has lowest p value (8.008823e-08)(t value = 5.501760) thus has the highest significance. we choose fiber for step 1 and look to repeat the process for step 2.
# Forward selection: step 2
mod1 = lm(betplasm ~ fiber + age)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + sex)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + smokstat)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + vituse)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + quetelet)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + calories)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + retdiet)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + alcohol)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + chol)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + fat)
summ.mod1 = summary(mod1)
summ.mod1$coefficients

mod1 = lm(betplasm ~ fiber + betadiet)
summ.mod1 = summary(mod1)
summ.mod1$coefficients
# Vitamin use has the highest significNCE in step 2 p-value=9.664842e-05 t-value=-3.951775
#2 Backwards selection method
bbest<-stepback(betplasm,data.frame(age,sex,smokstat,quetelet,vituse,calories,fat,fiber,alcohol,chol,betadiet,retdiet),alfa=0.02)
best<-bbest
plot(best)

#2.
rjack<-rstudent(best)
yhat<-fitted(best)
yhat<-fitted(best)
r<-residuals(best) 
rstud<-rstandard(best)
rjack<-rstudent(best)
h<-hatvalues(best) 
d<-cooks.distance(best) 
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
tbbest<-stepback(sqrt(betplasm),data.frame(age,sex,smokstat,quetelet,vituse,calories,fat,fiber,alcohol,chol,betadiet,retdiet),alfa=0.02)
# forward selection
tfbest<-stepfor(sqrt(betplasm),data.frame(age,sex,smokstat,quetelet,vituse,calories,fat,fiber,alcohol,chol,betadiet,retdiet),alfa=0.02)
summary(tbbest)
summary(tfbest)
# The final best  model takes the following form

tbest<-lm(sqrt(betplasm)~ age+sex+smokstat+quetelet+vituse+calories+fat+fiber+alcohol+chol+betadiet+retdiet)
# To see which outliers points affecting data
text(yhat,rjack, labels = (1:30 ), pos = 2,cex = 1,font = 2)

tbest<???lm(sqrt(Species) ??? Area + Elevation + Adjacent,subset=(d < max(d)))

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