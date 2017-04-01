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
