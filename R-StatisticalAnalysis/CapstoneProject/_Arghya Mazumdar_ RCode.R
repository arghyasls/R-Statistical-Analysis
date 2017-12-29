#Arghya Mazumdar
#Nit Rourkela
#Student

##Reading the dataset
hotel.df<-read.csv(paste("Cities42.csv",sep=""))
View(hotel.df)
attach(hotel.df)

#sumary statistics
library(psych)
describe(hotel.df)

#visualizing plots of y=f(x1,x2,x3,,,)
plot(~StarRating+RoomRent,main="Room rent vs Star Rating")
tablerent<-table(StarRating)
tablerent
library(lattice)
histogram(~StarRating,type="percent",data=hotel.df)

mytable3<-xtabs(~RoomRent+StarRating,data=hotel.df)
View(mytable3)

boxplot(RoomRent+HotelCapacity,data=hotel.df,xlab="rent",ylab="hotel capacity",main="Boxplot showing distribution of hotel capacity vs rent",horizontal=TRUE)
plot(~HotelCapacity+RoomRent,main="Room rent vs Hotel Capacity")
abline(0,1)
mytable<-table(IsTouristDestination)
View(mytable)
library(lattice)
histogram(~IsTouristDestination,type="percent",data=hotel.df)
plot(~IsTouristDestination+RoomRent,main="Room rent vs Is Tourist Destination")
library(Hmisc)

#correlational Matrix of variables
corMatrix<-rcorr(as.matrix(hotel.df[,c('RoomRent','IsTouristDestination','HotelCapacity','StarRating')]))
corMatrix
library(car)
library(corrgram)
corrgram(hotel.df[,c('RoomRent','IsTouristDestination','HotelCapacity','StarRating')],lower.panel=panel.pts, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

scatterplotMatrix(~RoomRent++IsMetroCity+StarRating+HotelCapacity,data=hotel.df)

#splitting it into training set and test set
library(caTools)
set.seed(123)
split = sample.split(RoomRent, SplitRatio = 0.8)
training_set = subset(hotel.df, split == TRUE)
test_set = subset(hotel.df, split == FALSE)
View(training_set)
View(test_set)


#creating a regressor of statistically significant parameters
fit1<-lm(formula=RoomRent~IsTouristDestination+HotelCapacity+StarRating, data =training_set)
summary(fit1)
library(car)
plot(HasSwimmingPool,RoomRent)
aggregate(cbind(IsTouristDestination,HasSwimmingPool,IsWeekend,IsNewYearEve,FreeWifi,FreeBreakfast) ~RoomRent,
          data =hotel.df, mean)

#creating a regressor of internal factors
fitinternal<-lm(formula=RoomRent~HotelCapacity+HasSwimmingPool+FreeWifi+FreeBreakfast, data =training_set)
summary(fitinternal)

#creating a regressor of external fators
fitexternal<-lm(formula=RoomRent~IsTouristDestination+IsWeekend+IsNewYearEve+IsMetroCity, data=training_set)
summary(fitexternal)


#creating a regressor of optimum factors
fitoptimal<-lm(formula=RoomRent~IsTouristDestination+HasSwimmingPool+IsNewYearEve+IsMetroCity+StarRating+HotelCapacity, data=training_set)
summary(fitoptimal)

#running ttests on some factors

t.test(RoomRent,IsTouristDestination)
t.test(RoomRent,HasSwimmingPool)
t.test(RoomRent,Population)
t.test(RoomRent,HotelPincode)
t.test(RoomRent,Airport)