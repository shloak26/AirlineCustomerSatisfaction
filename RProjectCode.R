########## Airline Customer Satisfaction ##########
############# Satyen Amonkar IST 687 ##############

###### Data Wrangling and Munging ######

survey <- read.csv("C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/Satisfaction Survey.csv")
str(survey)

# The satisfaction column in the given data set is "character" data type. This needs to be converted to the numeric data type.
# We get the following code by removing  the 3 rows of data with typing mistakes.
survey <- read.csv("C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV1.csv")
str(survey)

# Day of the week might affect the ratings more than the day or the month of the year. 
# To test this day of the month and the Flight Date columns were used to make a column weekday.

survey$Weekday<-weekdays(as.Date(survey$Flight.date,'%m/%d/%Y'))
survey <- subset (survey, select =-c (Day.of.Month, Flight.date))
View(survey)

str(survey)
# It was observed that the major reason for the missing values in Arrival Delay and Departure Delay Columns is when the flight is cancelled. 
# Removing all the rows with missing value is not a proper alternative in this case.
# Along with this three columns Arrival Delay, Flight Cancelled and arrival delay Greater than 5 mins can be converted to a single column with no missing values.
# Same is with departure delay and Departure delay Greater than 5 mins.

arrivaldel <- function(vec1, vec){
  vBuckets <- replicate(length(vec),"No Delay")
  vBuckets[is.na(vec) & vec1 == 'Yes'] <- "Cancelled"
  vBuckets[vec > 0 & vec < 5] <- "5 minutes"
  vBuckets[vec >= 5 & vec < 30] <- "half hour"
  vBuckets[vec >= 30 & vec < 60] <- "1 hour"
  vBuckets[vec >= 60 & vec < 120] <- "2 hours"
  vBuckets[vec >= 120] <- "more than 2 hours"
  return (vBuckets)
}

survey$arrivalDelay <-arrivaldel(survey$Flight.cancelled, survey$Arrival.Delay.in.Minutes)
survey$DepartureDelay <- arrivaldel(survey$Flight.cancelled,survey$Departure.Delay.in.Minutes)

survey <- subset(survey, select =-c(Arrival.Delay.greater.5.Mins,Flight.cancelled))
survey <- subset(survey, select =-c(Arrival.Delay.in.Minutes,Departure.Delay.in.Minutes))

View(survey)

#Ordering the factors for arrivalDelay
survey$arrivalDelay <- factor(survey$arrivalDelay, levels=c("No Delay", "5 minutes", "half hour","1 hour","2 hours","more than 2 hours","Cancelled"))
#Ordering the factors for DepartureDelay
survey$DepartureDelay <- factor(survey$DepartureDelay,  levels=c("No Delay", "5 minutes", "half hour","1 hour","2 hours","more than 2 hours","Cancelled"))


str(survey)
#	Some missing values were also present in the Flight Time in minutes column of the dataset. 

data<- data.frame(survey$Flight.time.in.minutes, survey$Flight.Distance)
colnames(data)<-c("Flight.time.in.minutes","Flight.Distance")
data<-na.omit(data)
cor(data)
#install.packages('ggplot2')
library(ggplot2)
plot <- ggplot(data, aes(x=Flight.time.in.minutes, y=Flight.Distance))
plot <- plot + geom_point()
# In addition, it was found out that Flight Distance is highly correlated to Flight Time.
# Therefore, dropped the Flight Time.
survey <- subset(survey, select =-c(Flight.time.in.minutes))
View(survey)

# Two columns in Shopping amount at airport and Eating and Drinking at airport are very right skewed data. 
hist(survey$Shopping.Amount.at.Airport)
hist(survey$Eating.and.Drinking.at.Airport)

# Therefore, changed them to categorical variables.

shopping<- function(vec)
{
  vBuckets <- replicate(length(vec),"No shopping")
  vBuckets[vec <  20 & vec != 0] <- "less than 20$"
  vBuckets[vec >= 20 & vec <  50] <- "20$ to 50$"
  vBuckets[vec >= 50 & vec <  150] <- "50$ to 150$"
  vBuckets[vec >= 150]<- "more than 150"
  return (vBuckets)
}

survey$Shopping.Amount.at.Airport <- shopping(survey$Shopping.Amount.at.Airport)
survey$Eating.and.Drinking.at.Airport<- shopping(survey$Eating.and.Drinking.at.Airport)
survey$Shopping.Amount.at.Airport <- factor(survey$Shopping.Amount.at.Airport, levels=c("No shopping","less than 20$", "20$ to 50$", "50$ to 150$","more than 150"))
survey$Eating.and.Drinking.at.Airport <- factor(survey$Eating.and.Drinking.at.Airport, levels=c("No shopping","less than 20$", "20$ to 50$", "50$ to 150$","more than 150"))
counts <- table(survey$Shopping.Amount.at.Airport)
barplot(counts, xlab = 'Shopping', main = 'Shopping Amount Distribution ')
counts2 <- table(survey$Eating.and.Drinking.at.Airport)
barplot(counts2, xlab = 'Eating and Drinking', main = 'Eating and Drinking Amount Distribution ')


#	Airline Name and Airline Code represent the same thing in the dataset that is a unique company.
# Removed the Airline Code column from the Dataset
survey$Airline.Code<-NULL
colnames(survey)

#	How old an Airplane is might affect the rating of the customer.
# Used the Year of First Flight column and subtracted each row from 2012 as it was the latest year and we got the column Years Old.
survey$Years.Old<- 2012 - survey$Year.of.First.Flight
survey$Year.of.First.Flight <- NULL
colnames(survey)

#write.csv(survey,file="C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV2.csv")

#Removing columns based on Linear Regression
survey$Orgin.City <- NULL
survey$Origin.State <- NULL
survey$Destination.City <- NULL
survey$Destination.State <- NULL
survey$Eating.and.Drinking.at.Airport <- NULL

#write.csv(survey,file="C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV3.csv")

#=====================================================
###### Linear Regression ######

survey <- read.csv("C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV2.csv")
str(survey)

#Converting categorial variables into numeric for linear regression

airlineStatusFunc<- function(vec)
{
  x<- replicate(length(vec),1)
  x[vec == "Silver"] <- 2
  x[vec == "Gold"] <- 3
  x[vec == "Platinum"] <- 4
  return(x)
  
}
survey$Airline.Status <- airlineStatusFunc(survey$Airline.Status)

delayFunc<- function(vec)
{
  x<- replicate(length(vec),0)
  x[vec == "5 minutes"] <- 1
  x[vec == "half hours"] <- 2
  x[vec == "1 hours"] <- 3
  x[vec == "2 hours"] <- 4
  x[vec == "more than 2 hours"] <- 5
  x[vec == "Cancelled"] <- 6
  return(x)
}

survey$arrivalDelay <- delayFunc(survey$arrivalDelay)
survey$DepartureDelay <-delayFunc(survey$DepartureDelay)

shopFunc<- function(vec)
{
  x<- replicate(length(vec),0)
  x[vec == "less than 20$"] <- 1
  x[vec == "20$ to 50$"] <- 2
  x[vec == "50$ to 150$"] <- 3
  x[vec == "more than 150"] <- 4
  return(x)
}

survey$Shopping.Amount.at.Airport <- shopFunc(survey$Shopping.Amount.at.Airport)
survey$Eating.and.Drinking.at.Airport <- shopFunc(survey$Eating.and.Drinking.at.Airport)

classFunc<- function(vec)
{
  x<- replicate(length(vec),1)
  x[vec == "Eco Plus"] <- 2
  x[vec == "Business"] <- 3
  return(x)
}

survey$Class <- classFunc(survey$Class)

str(survey)

relation<-lm(formula = Satisfaction~.,data = survey)
summary.lm(relation)

relationOC<-lm(formula = Satisfaction~Orgin.City,data = survey)
summary.lm(relationOC)
survey$Orgin.City <- NULL
# The R squared value for Origin City is 0.0005, which is insignificant. 
# Therefore, removed the Orgin City column from the Dataset 

relationOS<-lm(formula = Satisfaction~Origin.State,data = survey)
summary.lm(relationOS)
# The R squared value for Origin State is 0.0007, which is insignificant. 
# Therefore, removed the Origin State column from the Dataset 
survey$Origin.State <- NULL

relationDC<-lm(formula = Satisfaction~Destination.City,data = survey)
summary.lm(relationDC)
survey$Destination.City <- NULL
# The R squared value for Destination City is 0.0001, which is insignificant. 
# Therefore, removed the Destination City column from the Dataset 

relationDS<-lm(formula = Satisfaction~Destination.State,data = survey)
summary.lm(relationDS)
survey$Destination.State <- NULL
# The R squared value for Destination State is 0.0003, which is insignificant. 
# Therefore, removed the Destination State column from the Dataset 

relation<-lm(formula = Satisfaction~.,data = survey)
summary.lm(relation)

relationEDA<-lm(formula = Satisfaction~Eating.and.Drinking.at.Airport,data = survey)
summary.lm(relationEDA)
survey$Eating.and.Drinking.at.Airport <- NULL
# The R squared value for Eating and Drinking at Airport is 0.00003, which is insignificant. 
# Therefore, removed the Eating and Drinking at Airport column from the Dataset 

#write.csv(survey,file="C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV4.csv")

View(survey)
myData<- data.frame(survey$Satisfaction, survey$Airline.Status, survey$Age, as.numeric(survey$Gender), survey$Price.Sensitivity,survey$No.of.Flights.p.a.,survey$Shopping.Amount.at.Airport,survey$Class,survey$arrivalDelay,survey$DepartureDelay,survey$Flight.Distance)
s<- cor(myData)
#install.packages("corrplot")
library(corrplot)
corrplot(s, title = "Correlation Plot", order="hclust", addrect = 4, rect.col = "green", rect.lwd = 1, cl.pos = "b", tl.col = "indianred4", tl.cex = 0.6, cl.cex = 0.5,mar = c(1,0,1,0))


#=====================================================
###### Descriptive Statistics and Visualizations ######

surveyViz <- read.csv("C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV3.csv")
#install.packages('sqldf')
library(sqldf)
library(ggplot2)

df <- sqldf('select AVG(Satisfaction) as AVG_Satisfaction, arrivalDelay from surveyViz Group by arrivalDelay')
plot <- ggplot(df, aes(x=arrivalDelay, y=AVG_Satisfaction))
plot <- plot+ geom_col(color="black", fill="red")
plot <- plot + theme(axis.text.x = element_text(angle=90,hjust = 1))
plot <- plot + ggtitle("The proportion of Average Satisfaction by arrival delay")
plot <- plot + labs(x="Arrival Delay",y="Average Satisfaction")
plot

df2 <- sqldf('select AVG(Satisfaction) as AVG_Satisfaction, DepartureDelay from surveyViz Group by DepartureDelay')
plot2 <- ggplot(df2, aes(x=DepartureDelay, y=AVG_Satisfaction))
plot2 <- plot2+ geom_col(color="black", fill="red")
plot2 <- plot2 + theme(axis.text.x = element_text(angle=90,hjust = 1))
plot2 <- plot2 + ggtitle("The proportion of Average Satisfaction by departure delay")
plot2 <- plot2 + labs(x="Departure Delay",y="Average Satisfaction")
plot2


# The average satisfaction decreases with increase in delay

df3 <- sqldf('select AVG(Satisfaction) as AVG_Satisfaction,Age from surveyViz Group by Age')
plot3 <- ggplot(df3,aes(x=Age, y=AVG_Satisfaction, fill=Age)) 
plot3 <- plot3 +geom_col() +xlab("Age")+ylab("Average Satisfaction") 
plot3

surveyViz2 <- read.csv("C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV4.csv")

surveyViz2$AirlineStatus <- surveyViz2$Airline.Status
surveyViz2$TOT <- surveyViz2$Type.of.Travel

df4 <- sqldf('select AVG(Satisfaction) as AVG_Satisfaction,AirlineStatus,TOT from surveyViz2 Group by AirlineStatus, TOT')
plot4 <- ggplot(df4,aes(x=TOT, y=AVG_Satisfaction, fill = factor(AirlineStatus), bgcolor="Black")) 
plot4 <- plot4+ geom_bar(stat="identity", position="dodge")
plot4 <- plot4 + scale_fill_manual(name="AirlineStatus",breaks=c(1,2,3,4), labels=c("Blue","Gold","Platinum","Silver"), values = c("Blue","Gold", '#e5e4e2',"Grey"))
plot4 <- plot4 + theme(panel.background = element_rect(fill = "white",colour = "white",size = 0.5, linetype = "solid"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"),panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))
plot4 <- plot4 + xlab("Airline.Status  / Type.of.Travel") + ylab("Avg.Satisfaction")
plot4 <- plot4 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot4


#=====================================================
###### Stepwise Linear Regression  ######
survey <- read.csv("C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV4.csv")
library(MASS)


FullModel <- lm(Satisfaction~.,data = survey)
StepModel <- stepAIC(FullModel,direction = "forward", trace = F)

summary(StepModel)
qqnorm(StepModel$residuals)
qqline(StepModel$residuals)

#Based on forward stepwise regression Airline.Status, Age, Gender, Price.Sensitivity, X..of.Flight.with.other.Airlines, Type.of.Travel, No.of.Flights.p.a., Shopping.Amount.at.Airport, Class, Weekday, DepartureDelay and Years.Old are the significant attributes

#fwd.model = step(lm(Satisfaction ~ 1, survey), direction='forward', scope=~ Airline.Status +	Age	+ Gender +	Price.Sensitivity	+ No.of.Flights.p.a.	+ X..of.Flight.with.other.Airlines + 	Type.of.Travel	+ No..of.other.Loyalty.Cards	+ Shopping.Amount.at.Airport	+ Class +	Airline.Name	+ Scheduled.Departure.Hour	+ Flight.Distance	+ Weekday	+ arrivalDelay + DepartureDelay	+ Years.Old)
#summary(fwd.model)


#=====================================================
###### Association Rule Mining  ######

survey <- read.csv("C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV3.csv")


#Based on stepwise regression, removed the insignificant atttributes
survey$X <- NULL
survey$No..of.other.Loyalty.Cards <- NULL
survey$Airline.Name <- NULL
survey$Flight.Distance <- NULL
survey$Scheduled.Departure.Hour <- NULL
str(survey)

#Converting to attribute values to factors


boxplot(survey$Age, main="Age Distribution", ylab="Age", col="orange", border = "brown")

#Converting age values to factor based on box plot 
age <- function(vec){ 
  vBuckets <- replicate(length(vec), "Adolescence")
  vBuckets[vec <= 35 & vec > 20] <- "Young Adult"
  vBuckets[vec <= 55 & vec > 35] <- "Senior Adult"
  vBuckets[vec > 55] <- "old"
  return(vBuckets)
}

survey$Age <- as.factor(age(survey$Age))


fpa <- function(vec){ 
  vBuckets <- replicate(length(vec), "less than 5")
  vBuckets[vec <= 30 & vec > 10] <- "5 to 25"
  vBuckets[vec > 55] <- "more than 25"
  return(vBuckets)
}

survey$No.of.Flights.p.a. <- as.factor(fpa(survey$No.of.Flights.p.a.))


#boxplot(survey$Years.Old, main="Flight Age", ylab="Years Old", col="orange", border = "brown")
yrol <- function(vec){ 
  vBuckets <- replicate(length(vec), "new")
  vBuckets[vec > 3] <- "old"
  return(vBuckets)
}

survey$Years.Old <- as.factor(yrol(survey$Years.Old))


ps <- function(vec){ 
  vBuckets <- replicate(length(vec), "high")
  vBuckets[vec <= 3 & vec > 2] <- "medium"
  vBuckets[vec <= 2] <- "low"
  return(vBuckets)
}

survey$Satisfaction <- as.factor(ps(survey$Satisfaction))
survey$Price.Sensitivity <- as.factor(ps(survey$Price.Sensitivity))


fwoa <- function(vec){ 
  vBuckets <- replicate(length(vec), "less than 30")
  vBuckets[vec <= 70 & vec >= 30] <- "30 to 70"
  vBuckets[vec > 70] <- "more than 70"
  return(vBuckets)
}

survey$X..of.Flight.with.other.Airlines <- as.factor(fwoa(survey$X..of.Flight.with.other.Airlines))

str(survey)
View(survey)
library(arules)
library(arulesViz)

#rules for low satisfaction
ruleset <- apriori(survey,parameter = list(support=0.01, confidence = 0.5),
                   appearance = list(default="lhs",rhs=("Satisfaction=low")))

plot(ruleset)
goodrules <- ruleset[quality(ruleset)$lift >= 4.64]
inspect(goodrules)

# Most common things that are associated with lower ratings are
# Airline.Status=Blue, Type.of.Travel=Personal Travel, arrivalDelay=1 hour, X..of.Flight.with.other.Airlines=less than 30,
# Price.Sensitivity=low and Gender=Female, Age=old, DepartureDelay=half hour

#rules for high satisfaction
ruleset <- apriori(survey,parameter = list(support=0.006, confidence = 0.4),
                   appearance = list(default="lhs",rhs=("Satisfaction=high")))
plot(ruleset)
goodrules <- ruleset[(quality(ruleset)$lift > 1.95)]
#goodrules <- ruleset[(quality(ruleset)$lift > 1.95) & (quality(ruleset)$count > 900)]
inspect(goodrules)

#8.	Most common things that are associated with Higher ratings are
# Airline.Status=Silver, Type.of.Travel=Mileage tickets, Price.Sensitivity=low,  Class=Eco, X..of.Flight.with.other.Airlines=less than 30
# Age=Senior Adult, DepartureDelay=No Delay, 

#=====================================================
###### Support Vector Machine  ######

survey <- read.csv("C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV3.csv")
surveySampleIndex <- sample(1:dim(survey)[1],size=30000) #created a sample for survey of size 30000
newsurvey <- survey[surveySampleIndex,]


Create_AirplaineSatisfaction <- function(vec){ 
  vBuckets <- replicate(length(vec), "medium")
  vBuckets[vec > 3] <- "high"
  vBuckets[vec <= 2] <- "low"
  return(vBuckets)
}

#Create_AirplaineSatisfactionS <- function(vec){ 
#  vBuckets <- replicate(length(vec), "low")
#  vBuckets[vec > 3] <- "high"
#  return(vBuckets)
#}
#newsurvey$SatisfactionTest <- as.factor(Create_AirplaineSatisfactionS(newsurvey$Satisfaction))
newsurvey$Satisfaction <- as.factor(Create_AirplaineSatisfaction(newsurvey$Satisfaction))
View(newsurvey) #modified new survey data for svm
cutPoint <- floor(2*dim(newsurvey)[1]/3)
randIndex <- sample(1:dim(newsurvey)[1]) 
summary(randIndex)
train <- newsurvey[randIndex[1:cutPoint],]
test <- newsurvey[randIndex[(cutPoint+1):dim(newsurvey)[1]],]
dim(train)#20000
dim(test) #10000
library(kernlab)


#svmOutputT <- ksvm(SatisfactionTest ~ Airline.Status + Age  + Price.Sensitivity + X..of.Flight.with.other.Airlines + Type.of.Travel  + Class + arrivalDelay + DepartureDelay , data= train, kernel= "rbfdot", kpar = "automatic", C = 10, cross = 3, prob.model = TRUE)
#print(svmOutputT)
#svmPredT <- predict(svmOutputT, test)
#ctableT<- data.frame(test$SatisfactionTest,svmPredT)
#tableT<-table(ctableT)
#fourfoldplot(tableT, color = c("#ff0000", "#00b300"),
#             conf.level = 0, margin = 1, main = "Confusion Matrix")
# Since there are many values that lie in medium, consider three satisfaction criterias. 


svmOutput <- ksvm(Satisfaction ~ Airline.Status + Age  + Price.Sensitivity + X..of.Flight.with.other.Airlines + Type.of.Travel  + Class + arrivalDelay + DepartureDelay , data= train, kernel= "rbfdot", kpar = "automatic", C = 10, cross = 3, prob.model = TRUE)
svmOutput
svmPred <- predict(svmOutput, test)
svmPred <- as.numeric(svmPred)
ctable1<- data.frame(test$Satisfaction,svmPred)
tableC<-table(ctable1)
t1<-tableC[1,1]+tableC[2,2]+tableC[3,3]
t2<-tableC[1,2]+tableC[1,3]+tableC[2,1]+tableC[2,3]+tableC[3,1]+tableC[3,2]
errorRate <- (t2/(t1+t2)) * 100
accuracy <- (t1/(t1+t2)) * 100
#accuracy is 70.65% 


#=====================================================
###### Decision Tree  ######
survey <- read.csv("C:/Users/Inspiron/Documents/GitHub/Syracuse/Sem1/IST 687/Project/SatisfactionSurveyV3.csv")
#Based on stepwise regression, removed the insignificant atttributes
survey$X <- NULL
survey$No..of.other.Loyalty.Cards <- NULL
survey$Airline.Name <- NULL
survey$Flight.Distance <- NULL
survey$Scheduled.Departure.Hour <- NULL
survey$Weekday <- NULL

survey$Age <- as.factor(age(survey$Age))
survey$No.of.Flights.p.a. <- as.factor(fpa(survey$No.of.Flights.p.a.))
survey$Years.Old <- as.factor(yrol(survey$Years.Old))
survey$Satisfaction <- as.factor(ps(survey$Satisfaction))
survey$Price.Sensitivity <- as.factor(ps(survey$Price.Sensitivity))
survey$X..of.Flight.with.other.Airlines <- as.factor(fwoa(survey$X..of.Flight.with.other.Airlines))
str(survey)

library(rpart)
library(rpart.plot)
#install.packages('maptree')
library(maptree)

fit <- rpart(Satisfaction~Airline.Status + Age  + Price.Sensitivity + X..of.Flight.with.other.Airlines + Type.of.Travel  + Class  + DepartureDelay, data = survey, method = 'class',cp=0.005)
rpart.plot(fit, tweak = 1.5)
draw.tree(fit,cex=0.7)

# Analysis Results
# 51% of customers have high satisfaction.
# There are 69% of customers with Business travel and Mileage tickets with 69% probability of high satisfaction.
# If the customers are not Business travel and Mileage tickets, there are only 31% of customers with high satisfaction.
# Traits of a customer that is highly satisfied: 
#  Type of travel = Business and mileage, Airline.Status = Silver, Age = greater than 24 (that is adult or old) DepartureDelay= 5 minutes or less
# Traits of a customer that is not satisfied:
#  Type of travel = Personal, DepartureDelay = half hour or more