install.packages('dplyr')
#install.packages('magrittr')
#install.packages('data.table')
library(tidyr)
library(dplyr)
library(magrittr)
library(data.table)

setwd("C:/Users/niti1/Documents/MS_imp/IST 687/Project")

HyattData <- read.csv('Subset2.csv',header=TRUE)
saveRDS(HyattData,"HyattData.rds")
HyattData <- readRDS("HyattData.rds")
View(HyattData)

#Removing irrelevant column
subset <- HyattData[,c(1,9,11:13,17:19,21:23,38,45,51,54,57,58,64,67,70,73,76,101,108,109,126,127,132,137:147,163,167,171,179,182,187,198:227,232)]
length(subset$CHECKOUT_HEADER_ID_C)
str(subset)


#Taking backup in Hotel
Hotel <- subset
summary(Hotel)

#identifying column names with blank values and replacing with NA
is.na(subset) <- subset == ''

#All the empty spaces have been replaced by NA
View(subset)

#listing columns with NAs
colnames(subset)[colSums(is.na(subset)) > 0]

#Cleaning subset
subset$Overall_Sat_H <- replaceNA(subset$Overall_Sat_H)
subset$Guest_Room_H <- replaceNA(subset$Guest_Room_H)
subset$Tranquility_H <- replaceNA(subset$Tranquility_H)
subset$Customer_SVC_H <- replaceNA(subset$Customer_SVC_H)
subset$Staff_Cared_H <- replaceNA(subset$Staff_Cared_H)
subset$Internet_Sat_H <- replaceNA(subset$Internet_Sat_H)
subset$Check_In_H <- replaceNA(subset$Check_In_H)
subset$F.B_FREQ_H <- replaceNA(subset$F.B_FREQ_H)
subset$F.B_Overall_Experience_H <- replaceNA(subset$F.B_Overall_Experience_H)
subset$Condition_Hotel_H <- replaceNA(subset$Condition_Hotel_H)
colnames(subset)[colSums(is.na(subset)) > 0]
any(is.na(subset$Customer_SVC_H))

replaceflagNA <- function(x)
{
  x[is.na(x)]<- 'N'
  return (x);
}
subset[is.na(subset[,46:75])]<- round(mean(subset[46:75],na.rm = TRUE))
subset$Valet.Parking_PL[is.na(subset$Valet.Parking_PL)] <- 'N'
View(subset$Fitness.Center_PL)
#Removing rows with NAs in NPS_Type and Likelihood_Recommend_H

b <- complete.cases(subset[, "NPS_Type"])
b
subset <- subset[b,]
colSums(is.na(subset))
View(subset)
any(is.na(subset$NPS_Type))
#Calculating NPS value
library(sqldf)

promotersCount <- sqldf("Select count(*) from subset where NPS_Type = 'Promoter'")
promotersCount

detractorsCount <- sqldf("Select count(*) from subset where NPS_Type = 'Detractor'")
detractorsCount

passiveCount <- sqldf("Select count(*) from subset where NPS_Type = 'Passive'")
passiveCount

NPSValue <- ((promotersCount - detractorsCount) * 100) /nrow(subset)
NPSValue


#Using function to calculate NPS
NPS <- function(x){
  p <- length(which(x=='Promoter'))
  d <- length(which(x=='Detractor'))
  NPS <- (p-d)*100/length(x)
  return(NPS)
}

NPS(subset$NPS_Type)

#tapply for different combination of variables:
a <- data.frame(tapply(subset$NPS_Type, subset$Country_PL, NPS))
colnames(a) <- c('NPS')
a <- a$NPS[order(a$NPS,decreasing = TRUE)]
View(a)

#Identifying the country which has highest number of detractors

sqldf("Select count(NPS_TYPE),COUNTRY_PL from subset where NPS_TYPE='Detractor' group by COUNTRY_PL order by count(NPS_TYPE)")


#Identifying the country which has highest number of passives

sqldf("Select count(NPS_TYPE),COUNTRY_PL from subset where NPS_TYPE='Passive' group by COUNTRY_PL order by count(NPS_TYPE)")



subsetUK <- subset(subset, subset$Country_PL=='United Kingdom')
View(subsetUK)
str(subsetUK)

#Identifying the number of hotels in UK
NumberOfHotels <- sqldf("Select distinct ENTRY_HOTEL_CODE_R from subsetUK")
NumberOfHotels


#Creating a new df with Hotel Entry Code and NPS_TYPE
newdf <- subsetUK[,c(13,76)]
View(newdf)

#Identifying Promoters, Detractors and Passives
newdf2 <- newdf %>% select(NPS_Type,ENTRY_HOTEL_CODE_R) %>% count(NPS_Type,ENTRY_HOTEL_CODE_R) %>% spread(NPS_Type,n)
newdf2 <- newdf2[order(newdf2$Detractor,na.last = TRUE,decreasing = TRUE),]
View(newdf2)
Summary(newdf2)

#Doing analysis for Hotel Code OMGDS as it has the maximum number of Detractor and Passive customers
UK_OMGDS <- subset(subsetUK, subsetUK$ENTRY_HOTEL_CODE_R=='OMGDS')
View(UK_OMGDS)
summary(UK_OMGDS)

# Function to replace NAs with mean in all the numeric valued columns

replaceNA <- function(x)
{
  x[is.na(x)]<- round(mean(x,na.rm = TRUE))
  return (x);
}

#Cleaning Flg columns
length(subset)



length(subset)
UK_OMGDS$Overall_Sat_H <- replaceNA(UK_OMGDS$Overall_Sat_H)
UK_OMGDS$Guest_Room_H <- replaceNA(UK_OMGDS$Guest_Room_H)
UK_OMGDS$Tranquility_H <- replaceNA(UK_OMGDS$Tranquility_H)
UK_OMGDS$Customer_SVC_H <- replaceNA(UK_OMGDS$Customer_SVC_H)
UK_OMGDS$Staff_Cared_H <- replaceNA(UK_OMGDS$Staff_Cared_H)
UK_OMGDS$Internet_Sat_H <- replaceNA(UK_OMGDS$Internet_Sat_H)
UK_OMGDS$Check_In_H <- replaceNA(UK_OMGDS$Check_In_H)
UK_OMGDS$F.B_FREQ_H <- replaceNA(UK_OMGDS$F.B_FREQ_H)
UK_OMGDS$F.B_Overall_Experience_H <- replaceNA(UK_OMGDS$F.B_Overall_Experience_H)
View(UK_OMGDS)
summary(UK_OMGDS)



#Testing OFFER_FLAG_R by using Logit
#Replacing Y -> 1 and N -> 0

UK_OMGDS$OFFER_FLG_R <- ifelse(UK_OMGDS$OFFER_FLG_R=='Y',1,0)
View(UK_OMGDS)

#Replacing all the flags with Y -> 1 and N -> 0

UK_OMGDS$WALK_IN_FLG_C <- ifelse(UK_OMGDS$WALK_IN_FLG_C=='Y',1,0)
UK_OMGDS$Clublounge_Used_H <- ifelse(UK_OMGDS$Clublounge_Used_H=='Y',1,0)
UK_OMGDS$Spa_Used_H <- ifelse(UK_OMGDS$Spa_Used_H=='Y',1,0)
UK_OMGDS$All.Suites_PL <- ifelse(UK_OMGDS$All.Suites_PL=='Y',1,0)
UK_OMGDS$Bell.Staff_PL <- ifelse(UK_OMGDS$Bell.Staff_PL=='Y',1,0)
UK_OMGDS$Boutique_PL <- ifelse(UK_OMGDS$Boutique_PL=='Y',1,0)
UK_OMGDS$Business.Center_PL <- ifelse(UK_OMGDS$Business.Center_PL=='Y',1,0)
UK_OMGDS$Casino_PL <- ifelse(UK_OMGDS$Casino_PL=='Y',1,0)
UK_OMGDS$Conference_PL <- ifelse(UK_OMGDS$Conference_PL=='Y',1,0)
UK_OMGDS$Convention_PL <- ifelse(UK_OMGDS$Convention_PL=='Y',1,0)
UK_OMGDS$Dry.Cleaning_PL <- ifelse(UK_OMGDS$Dry.Cleaning_PL=='Y',1,0)
UK_OMGDS$Elevators_PL <- ifelse(UK_OMGDS$Elevators_PL=='Y',1,0)
UK_OMGDS$Fitness.Center_PL <- ifelse(UK_OMGDS$Fitness.Center_PL=='Y',1,0)
UK_OMGDS$Fitness.Trainer_PL <- ifelse(UK_OMGDS$Fitness.Trainer_PL=='Y',1,0)
UK_OMGDS$Golf_PL <- ifelse(UK_OMGDS$Golf_PL=='Y',1,0)
UK_OMGDS$Indoor.Corridors_PL <- ifelse(UK_OMGDS$Indoor.Corridors_PL=='Y',1,0)
UK_OMGDS$Laundry_PL <- ifelse(UK_OMGDS$Laundry_PL=='Y',1,0)
UK_OMGDS$Limo.Service_PL <- ifelse(UK_OMGDS$Limo.Service_PL=='Y',1,0)
UK_OMGDS$Mini.Bar_PL <- ifelse(UK_OMGDS$Mini.Bar_PL=='Y',1,0)
UK_OMGDS$Pool.Indoor_PL <- ifelse(UK_OMGDS$Pool.Indoor_PL=='Y',1,0)
UK_OMGDS$Pool.Outdoor_PL <- ifelse(UK_OMGDS$Pool.Outdoor_PL=='Y',1,0)
UK_OMGDS$Regency.Grand.Club_PL <- ifelse(UK_OMGDS$Regency.Grand.Club_PL=='Y',1,0)
UK_OMGDS$Resort_PL <- ifelse(UK_OMGDS$Resort_PL=='Y',1,0)
UK_OMGDS$Restaurant_PL <- ifelse(UK_OMGDS$Restaurant_PL=='Y',1,0)
UK_OMGDS$Self.Parking_PL <- ifelse(UK_OMGDS$Self.Parking_PL=='Y',1,0)
UK_OMGDS$Shuttle.Service_PL <- ifelse(UK_OMGDS$Shuttle.Service_PL=='Y',1,0)
UK_OMGDS$Ski_PL <- ifelse(UK_OMGDS$Ski_PL=='Y',1,0)
UK_OMGDS$Spa_PL <- ifelse(UK_OMGDS$Spa_PL=='Y',1,0)
UK_OMGDS$Spa.services.in.fitness.center_PL <- ifelse(UK_OMGDS$Spa.services.in.fitness.center_PL=='Y',1,0)
UK_OMGDS$Spa.online.booking_PL <- ifelse(UK_OMGDS$Spa.online.booking_PL=='Y',1,0)
UK_OMGDS$Spa.F.B.offering_PL <- ifelse(UK_OMGDS$Spa.F.B.offering_PL=='Y',1,0)
UK_OMGDS$Valet.Parking_PL <- ifelse(UK_OMGDS$Valet.Parking_PL=='Y',1,0)


#SVM

install.packages("kernlab")
library(kernlab)
install.packages("e1071")
library("e1071")
dim(UK_OMGDS)


UK_OMGDS_Subset <- UK_OMGDS[,c("Likelihood_Recommend_H","City_PL","Fitness.Center_PL","Spa_PL","Casino_PL",
                        "Condition_Hotel_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H",
                        "Tranquility_H", "Customer_SVC_H", "POV_CODE_C", "CONS_GUEST_ID_C")]


randIndex <- sample(1:dim(UK_OMGDS)[1])
randIndex
any(is.na(UK_OMGDS_Subset[,1:13]))
train_cutpoint2_3 <- floor((2*dim(UK_OMGDS)[1])/3)
trainData <- UK_OMGDS[randIndex[1:train_cutpoint2_3],]
View(trainData$Likelihood_Recommend_H)


testData <- UK_OMGDS[randIndex[train_cutpoint2_3+1:dim(UK_OMGDS)[1]],]
length(testData$Internet_Sat_H)
View(testData)


SVMOutput <- svm(Likelihood_Recommend_H ~ Internet_Sat_H, data=trainData)
SVMOutput
svmpred <- predict(SVMOutput,testData)
svmpred

table(svmpred) #,testData$Likelihood_Recommend_H)

actual <- testData$Likelihood_Recommend_H
length(actual)
length(svmpred)
diff <- actual-svmpred
View(diff)
length(testData$Internet_Sat_H)
plot_scatter_3 <- ggplot(data = testData,aes(x=testData$Internet_Sat_H, y=testData$Likelihood_Recommend_H)) + geom_point(aes(size= diff,color= diff))
plot_scatter_3

points(UK_OMGDS_Subset$Internet_Sat_H, svmpred, col = "blue", pch=4)

##################
#Viz
################
install.packages("arules")
library("arules")
colnames(subsetUK)[colSums(is.na(subsetUK)) > 0]
View(subsetUK$Likelihood_Recommend_H)
subset$Likelihood_Recommend_H <- as.factor(subsetUK$Likelihood_Recommend_H)
trans <- as(subsetUK, "transactions") 
data("Subset2.csv")
str(subsetUK[1])
subsetUK <- subsetUK[,c("Likelihood_Recommend_H","City_PL","Fitness.Center_PL","Spa_PL","Casino_PL",
                               "Condition_Hotel_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H",
                               "Tranquility_H", "Customer_SVC_H", "POV_CODE_C")]

itemFrequency(subsetUK$Likelanihood_Recommend_H)
for (i in 1:12)
{subsetUK[,i]<-discretize(subsetUK[,i])}

casinoPlotRP <- ggplot(subset, aes(x=Fitness.Center_PL,y=Likelihood_Recommend_H)) + geom_point(position=position_jitter(width = .5, height=.5), alpha=.2)




  geom_smooth(method = "lm")+
  facet_grid(POV_CODE_C~Fitness.Center_PL) 
casinoPlotRP
