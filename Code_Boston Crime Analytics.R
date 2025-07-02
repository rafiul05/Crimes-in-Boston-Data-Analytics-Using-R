#ECO476
#LabVI - Application of Econometrics II
#Group6
#Author: Rafiul Ahmed (ID 2017231005)
#Crimes in Boston Dataset

#initial steps- data import, viewing (data and variable)
library(readxl)
crime <- read_excel("C:/Users/Asus/Dropbox (Old)/My PC (DESKTOP-171AL3R)/Desktop/crime.xlsx")
View(crime)
head(crime)
colnames(crime)
class(crime)
class(crime$MONTH)
length(crime)
length(crime$YEAR)
unique(crime)
unique(crime$UCR_PART)


#package installations 
install.packages("ggplot2")
install.packages("stargazer")
install.packages("aggr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("janitor")
install.packages("forecast")
install.packages("magrittr")
install.packages("wooldridge")
install.packages("skedastic")
install.packages("lmtest")


#summary statistics
require(dplyr)
glimpse(crime)
summary(crime)
summary(crime$YEAR)
summary(crime$MONTH)

#creating data frame of data set and displaying
crime_df <- as.data.frame(crime)

##Recode data
require(dplyr)
crime %>% 
  select(UCR_PART) %>% 
  mutate(UCR_PART=recode(UCR_PART,"Part One"="1", "Part Two"="2", "Part Three"="3","Other"="4"))

#accessing a single variable
crime$OFFENSE_CODE

#datavisualization
require(ggplot2)
plot(crime$YEAR)
plot(crime$MONTH)
hist(crime$HOUR)

require(ggplot2)
ggplot(crime, aes(x=DISTRICT)) +
  geom_bar()
ggplot(crime, aes(x=MONTH)) +
  geom_bar()
ggplot(crime, aes(x=YEAR)) +
  geom_bar()
ggplot(crime, aes(x=HOUR)) +
  geom_bar()
ggplot(crime, aes(x=DAY_OF_WEEK)) +
  geom_bar()

#datasorting 

l<-sort(table(crime$STREET),decreasing = TRUE)[3:12]
l
m<-sort(table(crime$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11]
m
n<-sort(table(crime$REPORTING_AREA),decreasing=TRUE)[2:11]
n

#specific data fetching
headdata<-head(crime,10)
headdata$newcolumn<-c(30319,28046,21559,20916,19293,18462,17720,15427, 13117,13102 )
headdata$newcolumn1<-c("Larceny","Medical Assistance","Investigate Person","Other","Drug Violation", "Simple Assault", "Vandalism","Verbal Disputes","Towed","Investigate Property")

#Top 10 Offense count from 2015-18
library(ggplot2)
ggplot(headdata, aes(x=newcolumn1, y=newcolumn))+
  geom_bar(stat = "identity") + 
  coord_flip()+
  labs(y = "Type of offense", x = "Count",title ="Top 10 Offense Count")


#Top Areas Involved with Crime
headdata$newcolumn2<-c(3740,4107,4861,5175,5445,5468,5597,5928, 8405,9143 )
headdata$newcolumn3<-c("columbia rd","hyde park ave","commonwalth ave","centre st","harrison ave", "mass ave", "Tremont st","dorchester ave","boylston","blue hill ave")
library(ggplot2)
ggplot(headdata,aes(x=newcolumn3,y=newcolumn2))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="Count", x="Area Name",title="Top Areas Involved")

#Frequency of crime from 2015-18
ggplot(crime) +
  geom_freqpoly(binwidth = 60 * 60 * 24 * 3, mapping = aes(x = OCCURRED_ON_DATE)) +
  xlab("Date") +
  ylab("Number of Crimes")

#Which area of crime evolved over time span

crime$Lat[crime$Lat == -1] <- NA
crime$Long[crime$Long == -1] <- NA

ggplot(data = crime) +
  geom_point(mapping = aes(x = Long, y = Lat), alpha = 0.1, color = 'red') +
  facet_wrap(~ OFFENSE_CODE_GROUP) +
  xlab("") +
  ylab("") +
  ggtitle("Crime By Location") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

#Crimes by type 
ggplot(data =crime) +
  geom_bar(mapping = aes(x = OFFENSE_CODE_GROUP)) +
  xlab("Crime") +
  coord_flip()

#considering only 2016 data and showing highest offense codes reported

library(tidyverse)

offensein16<-filter(crime,YEAR==2016)
table(offensein16$OFFENSE_CODE_GROUP)
ocg1<-sort(table(ggg$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11]

headdata$newcolumn4<-c(7920,6982,5766,5542,5302,5067,4744,4100,3521,3360)
headdata$newcolumn5<-c("larceny","medical assistance","Investigate Person","Other","Drug Violation", "Vandalism", "simple assault","verbal disputes","motor vehicle larency","investigate property")

ggplot(headdata,aes(x=newcolumn5,y=newcolumn4))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="Count", x="Crime Name",title="2016 Highest Reported Crimes")

#considering only 2017 data and showing highest offense codes reported

offensein17<-filter(crime,YEAR==2017)

ocg2<-sort(table(offensein17$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11]

headdata$newcolumn6<-c(7825,7820,6662,5328,4900,4840,4748,4438,3981,3954)
headdata$newcolumn7<-c("medical assistance","larceny","Investigate Person","Other","simple assault", "Vandalism", "drug violation","verbal disputes","investigate property","towed")
ggplot(headdata,aes(x=newcolumn7,y=newcolumn6))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="Count", x="Crime Name",title="2017 Highest Reported Crimes")


#considering only 2018 data and showing highest offense codes reported
offensein18<-filter(crime,YEAR==2018)


ocg3<-sort(table(offensein18$OFFENSE_CODE_GROUP),decreasing = TRUE)[2:11]

headdata$newcolumn8<-c(8241,8072,5586,5453,5256,4793,4424,4247,3753,3709)
headdata$newcolumn9<-c("medical assistance","larceny","Other","Investigate Person","simple assault","Drug Violation","verbal disputes", "Vandalism","investigate property","towed")

ggplot(headdata,aes(x=newcolumn9,y=newcolumn8))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y="Count", x="Crime Name",title="2018 Highest Reported Crimes")

#as we see larceny is occuring most often in all the years, we further dig deeper to see what kind of larceny happens the most for all the 3 years

ldff<-filter(crime,OFFENSE_CODE_GROUP=='Larceny')

ggplot(ldff, aes(x=(OFFENSE_DESCRIPTION))) +
  coord_flip()+
  geom_bar()


#Data Frame Creation 2

tb<-table(crime$OFFENSE_CODE_GROUP)
tdf<-as.data.frame(tb)
tdf$Var1 <- NULL
offensedata <- ts(tdf,start=c(100))

attributes(offensedata)
summary(offensedata)
cycle(offensedata)
plot(offensedata,main="offense count",xlab="",ylab="frequency")
aggregate(offensedata)
abc <- aggregate(offensedata, FUN=mean) 

#time series forecasting

library(forecast)

model <- auto.arima(abc,ic='aic',trace = TRUE,seasonal = FALSE)

plot.ts(model$residuals,main="auto arima model")
Acf(ts(model$residuals))
Pacf(ts(model$residuals))
attributes(model)


#Regression (simple and Multivariate)
#Introductory Econometrics: A Modern Approach, 6e.


require(wooldridge)
library(wooldridge)
data("wage1")
View(wage1)
summary(wage1)

#single variable regression
model1 <- lm((wage) ~ educ, data=wage1)       
require(stargazer)       
stargazer(model1,type = "text", title = "table1", out="table1.txt")

#multiple variable regression 
model2 <- lm(log(wage) ~ educ+exper+tenure, data=wage1)
stargazer(model2,type = "text", title = "table2", out="table2.txt")

#Heteroscedasticity 
#Perform the Breusch-Pagan Test

library(lmtest)
lmtest::bptest(model1)
lmtest::bptest(model2)

##White test
library(skedastic)
skedastic::white_lm(model1)
skedastic::white_lm(model2)

#Multicollinearity 
library(car)
vif(model2)



#END 
  
