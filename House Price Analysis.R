library(haven)
library(ggplot2)
library(lmtest) # linear model tests, like tests for heteroskedasticity, reset test, etc.
library(sandwich) # to use HC standard errors, if needed
library(car)# VIF test, etc.
library(stargazer)
house <- read_dta("Desktop/R applied econometrics/quiz/Midterm/house.dta")
View(house)
house$wall<- as.factor(house$wall)
house$garage_type <- as.factor(house$garage_type)
house$garage <- as.factor(house$garage)
house$sold93 <-as.factor(house$sold93)
house$sold94 <-as.factor(house$sold94)
house$sold95 <-as.factor(house$sold95)
house$sold96 <-as.factor(house$sold96)
house$sold97 <-as.factor(house$sold97)
house$sold98 <-as.factor(house$sold98)
###1.Data Inspection and Statistical Inference
#a
View(house)
dim(house)
str(house)
summary(house)
#b
attach(house)
options(scipen=100)
hist(price,xlab='Price',main='Histogram of Price')
#c
cor(sqft,price)
anova(lm(price ~  garage_type))
plot(garage_type,price,xlab='Garage Type',ylab='Price',main='Garage Type versus Price')
#d
hist(saledate,breaks = 'months',freq=T,xlab='Sale Date',col='grey')
#hist(saledate,breaks = 'years',freq=T)
#hist(saledate,breaks = 'weeks',,freq=T)
#hist(saledate,breaks = 'days',,freq=T)

#2.Model Selection and Output
cor(price,yrbuilt)
anova(lm(price ~  stories))
anova(lm(price ~  wall))
cor(price,sqft)
cor(price,bed)
cor(price,bath)
cor(price,halfbath)
cor(price,fullbath)
cor(price,frontage)
cor(price,depth)
anova(lm(price ~ garage_type))
cor(price,garage_sqft)
t.test(price ~ garage)
cor(price,rooms)
cor(price,lotsize)
cor(price,distance)
anova(lm(price ~ saleq))

par(mfrow=c(2,2))
plot(yrbuilt,price,cex=0.5,pch=20)#
plot(stories,price)#
plot(sqft,price,cex=0.5,pch=20)#
plot(wall,price)
plot(bed,price)#
plot(bath,price)#
plot(fullbath,price)
plot(halfbath,price)
plot(frontage,price)#
plot(depth,price)
plot(garage_type,price)#
plot(rooms,price)
plot(lotsize,price)#
plot(longitude,price)
plot(latitude,price)
plot(saledate,price)
plot(distance,price)
#create a new variable named distance
house$n1 <- abs((-(house$longitude)-83.5379))
house$n2 <- (abs(house$latitude-41.6528))/40
house$distance <- sqrt((house$n1*69)^2 + (house$n2*53)^2)
View(house)
attach(house)
#date
house$saleq <- quarters(house$saledate)
#bed bath rooms ->choose bed bath
mod1 <- lm(price ~ yrbuilt + stories + sqft + wall + bed + bath + frontage + 
             garage_type + garage_sqft + lotsize + distance + saleq)#better one
summary(mod1)
plot(mod1)
mod1.1 <- lm(price ~ yrbuilt + stories + sqft + wall + rooms + frontage + 
               garage_type + garage_sqft + lotsize + distance + saleq)#not rooms 
summary(mod1.1)
plot(mod1.1)
stargazer(mod1, mod1.1, 
          add.lines = list(
            c("AIC", round(AIC(mod1),4), round(AIC(mod1.1),4)),
            c("BIC", round(BIC(mod1),4), round(BIC(mod1.1),4))
          ),
          type="text", no.space=TRUE) # bed bath
#garage garage_type ->choose garage_type
mod1.2 <- lm(price ~ yrbuilt + stories + sqft + wall + bed + bath + frontage + 
             garage + garage_sqft + lotsize + distance + saleq)
summary(mod1.2)
plot(mod1.2)
stargazer(mod1, mod1.2, 
          add.lines = list(
            c("AIC", round(AIC(mod1),4), round(AIC(mod1.2),4)),
            c("BIC", round(BIC(mod1),4), round(BIC(mod1.2),4))
          ),
          type="text", no.space=TRUE) #mod1 is better
#mod1
e <- resid(mod1)
summary(e)
plot(mod_3)
plot(yrbuilt,price)
abline(lm(price ~ yrbuilt),col='red')
vif(mod1)# no high collinearity among x's

#should higher powers be included in regression?
#heter....
par(mfrow=c(2,2))
resettest(mod1,power=3) # reject at least one X's square term's coefficient is statistically significant
plot(yrbuilt,resid(mod1))# less
abline(h=0,col='red')
plot(stories,resid(mod1))
abline(h=0,col='red')
plot(wall,resid(mod1))
abline(h=0,col='red')
plot(sqft,resid(mod1),xlab='Area of Living Area(square footage)',
     ylab ='Residuals of Modeal 1',main='Living Area & residuals')#need square 
abline(h=0,col='red')
plot(bed,resid(mod1))
abline(h=0,col='red')
plot(bath,resid(mod1),xlab= 'Number of Bathrooms', ylab ='Residuals of Modeal 1',main='Bathrooms & residuals ')#need square
abline(h=0,col='red')
plot(frontage,resid(mod1))
abline(h=0,col='red')
plot(garage_sqft,resid(mod1))
plot(lotsize,resid(mod1))
plot(distance,resid(mod1))
abline(h=0,col='red')

# add square terms to sqft and bath
mod2 <- lm(price ~ yrbuilt + stories + sqft + I(sqft^2) + wall + bed + bath+ I(bath^2) + frontage + 
             garage_type + garage_sqft + lotsize + distance + saleq)
summary(mod2)
plot(mod2)
par(mfrow=c(1,1))#influential outlier:16017
stargazer(mod1, mod2, 
          add.lines = list(
            c("AIC", round(AIC(mod1),4), round(AIC(mod2),4)),
            c("BIC", round(BIC(mod1),4), round(BIC(mod2),4))
          ),
          type="text", no.space=TRUE)
mod2.1<- lm(price ~ yrbuilt + stories + sqft + I(sqft^2) + wall + bed + bath+ I(bath^2) + frontage + 
               garage_type + garage_sqft + lotsize + distance + saleq,data=house[-16017,])
summary(mod2.1)
stargazer(mod1, mod2,mod2.1,type="text", no.space = TRUE)
#still suspect heteroskedasticity
bptest(mod2.1) # reject H0:constant variance
#heteroskedasticity-corrected standard error
HCcov2.1 <- vcovHC(mod2.1, type="HC1")
rse2.1 <- sqrt(diag(HCcov2.1))
coeftest(mod2.1, vcov=HCcov2.1)
stargazer(mod2.1, mod2.1,
          se=list(NULL, rse2.1),
          column.labels = c("Mod 2.1", "2.1 HC"),
          no.space = TRUE,
          type = "text")
linearHypothesis(mod2.1, c('lotsize'), vcov=HCcov2.1) # not right model remove lotsize
mod2.2<- lm(price ~ yrbuilt + stories + sqft + I(sqft^2) + wall + bed + bath+ I(bath^2) + frontage + 
              garage_type + garage_sqft + distance + saleq,data=house[-16017,])#mod2.2 is best
summary(mod2.2)
bptest(mod2.2)
HCcov2.2 <- vcovHC(mod2.2, type="HC1")
rse2.2 <- sqrt(diag(HCcov2.2))
coeftest(mod2.2, vcov=HCcov2.2)
stargazer(mod2.2, mod2.2,
          se=list(NULL, rse2.2),
          column.labels = c("Mod 2.2", "2.2 HC"),
          no.space = TRUE,
          type = "text")
#log 
mod3<- lm(log(price) ~ yrbuilt + stories + sqft + I(sqft^2) + wall + bed + bath+ I(bath^2) + frontage + 
            garage_type + garage_sqft + distance + saleq,data=house[-16017,])
summary(mod3) #bad







#model compare
stargazer(mod1, mod2, mod2.1, mod2.2,
          no.space = TRUE, digits=4, star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines=list(
                         c("AIC", round(AIC(mod1),2), round(AIC(mod2),2), 
                           round(AIC(mod2.1),2), round(AIC(mod2.2),2)),
                         c("BIC", round(BIC(mod1),2), round(BIC(mod2),2), 
                           round(BIC(mod2.1),2), round(BIC(mod2.2),2))),
          covariate.labels= c("Year Built", "Stories","Living Area(square footage)","Living Area(square footage)^2",
                              "Wall2:Concrete block or tile","Wall3:Aluminum,vinyl,or steel siding","Wall4:Brick",
                              "Wall5:Stone","Wall6:Wood","Wall7:Mixed Material","Number of Bedroom",
                              "Number of Bathroom","Number of Bathroom^2","Frontage","Garage1:Basement","Garage2:Attached",
                              "Garage3:Detached","Garage4:Carport","Garage Area(square footage)","Lot Size(square feet)",
                              "Distance","Quarter2","Quarter3","Quarter4"),
          dep.var.labels = "Sale Price",
          omit = c("Constant"),
          type="html", out="~/Desktop/table-1.html")
stargazer(mod1, mod2, mod2.1, mod2.2,
          no.space = TRUE, digits=4, star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines=list(
            c("AIC", round(AIC(mod1),2), round(AIC(mod2),2), 
              round(AIC(mod2.1),2), round(AIC(mod2.2),2)),
            c("BIC", round(BIC(mod1),2), round(BIC(mod2),2), 
              round(BIC(mod2.1),2), round(BIC(mod2.2),2))),
          covariate.labels= c("Year Built", "Stories","Living Area(square footage)","Living Area(square footage)2",
                              "Wall2:Concrete block or tile","Wall3:Aluminum,vinyl,or steel siding","Wall4:Brick",
                              "Wall5:Stone","Wall6:Wood","Wall7:Mixed Material","Number of Bedroom",
                              "Number of Bathroom","Number of Bathroom2","Frontage","Garage1:Basement","Garage2:Attached",
                              "Garage3:Detached","Garage4:Carport","Garage Area(square footage)","Lot Size(square feet)",
                              "Distance","Quarter2","Quarter3","Quarter4"),
          dep.var.labels = "House Price",
          omit = c("Constant"),
          type="html", out="~/Desktop/table-1.html")


###Final Model
mod2.2<- lm(price ~ yrbuilt + stories + sqft + I(sqft^2) + wall + bed + bath+ I(bath^2) + frontage + 
              garage_type + garage_sqft + distance + saleq,data=house[-16017,])#mod2.2 is best
summary(mod2.2)
