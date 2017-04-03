getwd()
setwd("C:/R/AnalyticsEdge")
getwd()

#Comment Lecture 1
1+2
sqrt(2)
abs(-65)
?sqrt
SquareRoot2 = sqrt(2)
SquareRoot2
HoursYear <- 365*24
HoursYear
ls()
c(2,3,5,8,13)
Country = c("Brazil", "China", "India", "Switerzland", "USA")
LifeExpectancy = c(74,76,65,83,79)
Country
LifeExpectancy
Country[1]
LifeExpectancy[3]
seq(0,100,2)
CountryData = data.frame(Country, LifeExpectancy)
CountryData
CountryData$Population = c(199000, 1390000, 1240000, 7997, 318000)
CountryData
Country = c("Australia", "Greece")
LifeExpectancy = c(82, 81)
Population = c(23050,11125)
NewCountryData = data.frame(Country, LifeExpectancy, Population)
NewCountryData
AllCountryData = rbind(CountryData, NewCountryData)
AllCountryData
ls()

#Lecture2
getwd()
dir = "C:\R\AnalyticsEdge"
setwd(dir)
getwd()
WHO = read.csv("WHO.csv")
str(WHO)
summary(WHO)
WHO_Europe = subset(WHO, Region=="Europe")
str(WHO_Europe)
write.csv(WHO_Europe, "WHO_Europe.csv")
ls()
rm(WHO_Europe)
ls()
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]
plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO, GNI>10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country", "GNI", "FertilityRate")]
WHO$Country[which.min(WHO$Over60)]
WHO$Country[which.max(WHO$LiteracyRate)]
str(WHO)
hist(WHO$CellularSubscribers)
summary(WHO)
boxplot(WHO$LifeExpectancy~WHO$Region)
boxplot(WHO$LifeExpectancy~WHO$Region, xlab="", ylab="LifeExpectancy", main="Life Expectancy of Countries by Region")
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$LiteracyRate, WHO$Region, min)
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)
tapply(WHO$ChildMortality, WHO$Region, mean)

#Lecture 3 USDA
getwd()
USDA = read.csv("USDA.csv")
str(USDA)
summary(USDA)
USDA$Sodium
which.max(USDA$Sodium)
names(USDA)
USDA$Description[265]
HighSodium = subset(USDA, Sodium>10000)
nrow(HighSodium)
HighSodium$Description
match("CAVIAR", USDA$Description)
USDA$Sodium[match("CAVIAR", USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium)
sd(USDA$Sodium, na.rm=TRUE)
getwd()
setwd("C:/R/AnalyticsEdge")
getwd()
list.files()
USDA = read.csv("USDA.csv")
getwd()
setwd("c:/R/AnalyticsEdge")
getwd()
wine = read.csv("wine.csv")
str(wine)


#Linear Regression
#y = b0 + b1x + e
# y = dependent variable (wine price)
# x = independent variable (temperature)
# b0 = intercept term OR intercept coefficient
# b1 = slope of the line or coefficient for independent variable or regression coefficient
# e = error term or residual
# e can be zero if all points lie on same line
# Best model or best B0 & B1, has smallest error term/residual e
# e = actual - prediction
# SUm of squared errors terms or sum of squared residuals
#SSE = E1^2 + E2^2 + E3^2... + En^2
#Smaller SSE is better  meaning as stated above smallest error term/residuals
# SSE allows to compare models
# But SSE it can be hard to interprect when N is large
# Because of above issue RMSE Root Mean Squared Error is often used
# RMSE = SQRT(SSE/N)
# ANother measyre is R Squared, it compares best model to baseline model
# baseline model does not use any variables
# SSE for baseline is also called as SST Total Sum of Squares
# R2 = 1 - SSE/SST
# 0 <= SSE <= SST
# 0 <= SST
# Regression model will never be worse than baseline model
# Worst case SSE = SST, means R2 = 0, no improvement over baseline model
# Best case, best model has SSE = 0, means R2 = 1 or R2 is close to 1
# Good models with easy problems can have R2 close to 1
# Good models with hard problems can still have R2 close to 0
# The baseline prediction is the average value of the dependent variable. 
# Since our dependent variable takes values 2, 2, and 8 in our data set, the average is (2+2+8)/3 = 4
# SST, SSE : https://courses.edx.org/courses/course-v1:MITx+15.071x_3+1T2016/courseware/f8d71d64418146f18a066d7f0379678c/35b789067e9b469caed457cfff1645b7/

# Baseline = Average of y = Mean(x)
# SSE = 
# SST = actual value of y - baseline^2
# R2 = 1 - SSE/SST
# 

ls()
summary(wine)
model1 = lm(Price~AGST, data=wine)
summary(model1)

# Multiple R-squared will always increase if you add more independent variables.
# But Adjusted R-squared will decrease if you add an independent variable that doesn't help the model.
# This is a good way to determine if an additional variable should even be included in the model.
# Residuals are stored in the vector model1$residuals
# SSE = sum(model1$residuals^2)
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# baseline is average of y
# SST is actual - baseline square

model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)
SSE = sum(model2$residuals^2)
SSE

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
SSE = sum(model3$residuals^2)
SSE

modelTest = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelTest)
SSE = sum(modelTest$residuals^2)
SSE

# A coefficient of 0 means that the value of the independent variable does not change our prediction for the dependent variable.
# If a coefficient is not significantly different from 0,
# then we should probably remove the variable from our model
# since it's not helping to predict the dependent variable.

# The standard error column gives a measure of how much the coefficient is likely to vary from the estimate value.

# The t value is the estimate divided by the standard error.
# It will be negative if the estimate is negative and positive if the estimate is positive.
# The larger the absolute value of the t value, 
# the more likely the coefficient is to be significant.
# So we want independent variables with a large absolute value in t column.

# The last column of numbers gives a measure of how plausible it is that the coefficient is actually 0, given the data we used to build the model.
# The less plausible it is, or the smaller the probability number in this column, 
# the less likely it is that our coefficient estimate is actually 0.
# This number will be large if the absolute value of the t value is small, 
# and it will be small if the absolute value of the t value is large.
# We want independent variables with small values in this column.

model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
SSE = sum(model4$residuals^2)
SSE

#MultiColinearity
# We observed that Age and FrancePopulation are highly correlated.
# But what is correlation?
# Correlation measures the linear relationship between two
# variables and is a number between -1 and +1.
# A correlation of +1 means a perfect positive linear relationship.
# A correlation of -1 means a perfect negative linear relationship.
# In the middle of these two extremes is a correlation of 0, which means
# that there is no linear relationship between the two variables.
# When we say that two variables are highly correlated,
# we mean that the absolute value of the correlation is close to 1.
# We can compute the correlation between a pair of variables in R by using the cor function.
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
# We can also compute the correlation between all pairs of variables in our data set using the cor function.
cor(wine)
# Keep in mind that multicollinearity refers to the situation when two independent variables are highly correlated.
# A high correlation between an independent variable and the dependent variable is a good thing  ince we're trying to predict the dependent variable using the independent variables.
# Do we have any other highly-correlated independent variables?
# There is no definitive cut-off value for what makes a correlation too high.
# But typically, a correlation greater than 0.7
# or less than -0.7 is cause for concern.

model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
SSE = sum(model5$residuals^2)
SSE

summary(model4)

# Predictions
# The accuracy of the model on the test data is often referred to as out-of-sample accuracy.
wineTest = read.csv("wine_test.csv")
str(wineTest)
predictTest = predict(model4, newdata=wineTest)
predictTest

# Test Set R Squared
# SSE = sum((wineTest$Price - predictTest)^2)
# SST = sum((wineTest$Price-mean(wine$Price))^2)
# R2 = 1 - SSE/SST
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
wineR2 = 1 - SSE/SST
wineR2

#MoneyBall
moneyBall = read.csv("baseball.csv")
cor(moneyBall$Playoffs, moneyBall$W)
plot(moneyBall$W, moneyBall$Playoffs)
