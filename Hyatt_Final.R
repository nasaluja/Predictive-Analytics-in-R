#install.packages('tidyr')
#install.packages('dplyr')
#install.packages('magrittr')
#install.packages('data.table')
#install.packages('sqldf')
#install.packages('arules')
#install.packages('arulesViz')
#install.packages('e1071')
#install.packages('kernlab')
#install.packages('gridExtra')
#install.packages('corrplot')
#install.packages('ggmap')

library(tidyr)
library(dplyr)
library(magrittr)
library(data.table)
library(ggplot2)
library(sqldf)
library(arules)
library(arulesViz)
library(e1071)
library(kernlab)
library(gridExtra)
library(corrplot)
library(reshape)
library(ggmap)

setwd("D:/687 - ADS/Project")

#Load the Hyatt Dataset for Aug 2014

#HyattData <- read.csv('August.csv',header=TRUE)
#saveRDS(HyattData,"HyattData.rds")
HyattData <- readRDS("HyattData.rds")
View(HyattData)

#Remvoing the unrequired columns

subset <- HyattData[,c(1,9,11:13,17:19,21:23,38,45,51,54,57,58,64,65,67,70,73,75,101,108,109,126,127,132,137:147,163,167,168,171,179,182,187,198:227,232)]
View(subset)
str(subset)

#Creating US subset

subsetUS <- subset(subset, subset$Country_PL=='United States')
View(subsetUS)
summary(subsetUS)
str(subsetUS)

#Selecting a subset based on the state with highest number of detractors
install.packages("reshape")
library("reshape")
install.packages("ggplot2")
library("ggplot2")
#Identifying Promoters, Detractors and Passives



detState <- sqldf("Select count(NPS_TYPE),STATE_PL from subsetUS where NPS_TYPE='Detractor' group by STATE_PL order by count(NPS_TYPE)")
colnames(detState) <- c('Count','State')

d.m <- melt(detState, id.vars= c("State"))
View(d.m)
ggplot(detState, aes(x=reorder(State,-Count),y=Count, fill = Count)) + geom_bar(stat="identity") + scale_x_discrete(labels = abbreviate) + ggtitle("Detractors for each state in USA")


ggplot(d.m, aes(x=State,y=value, fill = variable, color = variable)) +   
  geom_bar(position = "dodge", stat="identity")
#Creating subset with California state
Cali_subset <- subset(subsetUS, subsetUS$State_PL=='California')
View(Cali_subset)

##############################################################################
#Data Cleaning
##############################################################################

#identifying column names with blank values and replacing with NA
is.na(Cali_subset) <- Cali_subset == ''

#All the empty spaces have been replaced by NA
View(Cali_subset)

#listing columns with NAs
colnames(subset)[colSums(is.na(subset)) > 0]
summary(subset)

#Removing rows with NAs in NPS_Type and Likelihood_Recommend_H

b <- complete.cases(Cali_subset[, "NPS_Type"])
b
Cali_subset <- Cali_subset[b,]

#checking for NAs in NPS_Type
colSums(is.na(Cali_subset))
View(Cali_subset)
summary(Cali_subset)

#Function to calculate percentage of NA's
CalPercNA <- function(x)
{
  
  p <- sum(is.na(x))/length(x)
  print(paste("Percentage:",p*100,"%"))
  
}

CalPercNA(Cali_subset$Tranquility_H)

#Calculating the Percentage of NA's in all numerical valued columns
CalPercNA(Cali_subset$Overall_Sat_H) #0%
CalPercNA(Cali_subset$Guest_Room_H) #1.24%
CalPercNA(Cali_subset$Tranquility_H) #46.87%
CalPercNA(Cali_subset$Customer_SVC_H) #1.79%
CalPercNA(Cali_subset$Staff_Cared_H)   #46.52
CalPercNA(Cali_subset$Internet_Sat_H)  #62.43%
CalPercNA(Cali_subset$Check_In_H)   #46.74%
CalPercNA(Cali_subset$F.B_FREQ_H)   #60.22% 
CalPercNA(Cali_subset$F.B_Overall_Experience_H)  #60.22%
CalPercNA(Cali_subset$Condition_Hotel_H) #1.52%

#excluding F.B_FREQ_H and F.B_Overall_Experience_H in linear modeling due to high percentage of NAs


#function to replace NA with mean for all the survey columns with numerical values
replaceNA <- function(x)
{
  x[is.na(x)]<- round(mean(x,na.rm = TRUE))
  return (x);
}


Cali_subset$Overall_Sat_H <- replaceNA(Cali_subset$Overall_Sat_H)
Cali_subset$Guest_Room_H <- replaceNA(Cali_subset$Guest_Room_H)
Cali_subset$Tranquility_H <- replaceNA(Cali_subset$Tranquility_H)
Cali_subset$Customer_SVC_H <- replaceNA(Cali_subset$Customer_SVC_H)
Cali_subset$Staff_Cared_H <- replaceNA(Cali_subset$Staff_Cared_H)
Cali_subset$Internet_Sat_H <- replaceNA(Cali_subset$Internet_Sat_H)
Cali_subset$Check_In_H <- replaceNA(Cali_subset$Check_In_H)
Cali_subset$F.B_FREQ_H <- replaceNA(Cali_subset$F.B_FREQ_H)
Cali_subset$F.B_Overall_Experience_H <- replaceNA(Cali_subset$F.B_Overall_Experience_H)
View(Cali_subset)
summary(Cali_subset)

#Taking a backup of the Cali_subset

hotel <- Cali_subset


#Don't use these until necessary, such as modeling.
#converting to factors
Cali_subset$CHECKOUT_HEADER_ID_C <- as.factor(Cali_subset$CHECKOUT_HEADER_ID_C)
Cali_subset$CONS_GUEST_ID_C <- as.factor(Cali_subset$CONS_GUEST_ID_C)
Cali_subset$LENGTH_OF_STAY_C <- as.factor(Cali_subset$LENGTH_OF_STAY_C)
Cali_subset$ADULT_NUM_C <- as.factor(Cali_subset$ADULT_NUM_C)
Cali_subset$CHILDREN_NUM_C <- as.factor(Cali_subset$CHILDREN_NUM_C)
Cali_subset$PMS_TOTAL_REV_C <- as.factor(Cali_subset$PMS_TOTAL_REV_C)
Cali_subset$NUM_ROOMS_R <- as.factor(Cali_subset$NUM_ROOMS_R)
Cali_subset$REVENUE_USD_R <- as.factor(Cali_subset$REVENUE_USD_R)
Cali_subset$Survey_ID_H <- as.factor(Cali_subset$Survey_ID_H)
Cali_subset$Likelihood_Recommend_H <- as.factor(Cali_subset$Likelihood_Recommend_H)
Cali_subset$Overall_Sat_H <- as.factor(Cali_subset$Overall_Sat_H)
Cali_subset$Guest_Room_H <- as.factor(Cali_subset$Guest_Room_H)
Cali_subset$Tranquility_H <- as.factor(Cali_subset$Tranquility_H)
Cali_subset$Condition_Hotel_H <- as.factor(Cali_subset$Condition_Hotel_H)
Cali_subset$Customer_SVC_H <- as.factor(Cali_subset$Customer_SVC_H)
Cali_subset$Staff_Cared_H <- as.factor(Cali_subset$Staff_Cared_H)
Cali_subset$Internet_Sat_H <- as.factor(Cali_subset$Internet_Sat_H)
Cali_subset$Check_In_H <- as.factor(Cali_subset$Check_In_H)
Cali_subset$F.B_FREQ_H <- as.factor(Cali_subset$F.B_FREQ_H)
Cali_subset$F.B_Overall_Experience_H <- as.factor(Cali_subset$F.B_Overall_Experience_H)
Cali_subset$Guest.NPS.Goal_PL <- as.factor(Cali_subset$Guest.NPS.Goal_PL)
Cali_subset$Total.Meeting.Space_PL <- as.factor(Cali_subset$Total.Meeting.Space_PL)

Cali_subset <- hotel


#Calculating NPS
install.packages("sqldf")
library("sqldf")
promotersCount <- sqldf("Select count(*) from Cali_subset where NPS_Type = 'Promoter'")
promotersCount

detractorsCount <- sqldf("Select count(*) from Cali_subset where NPS_Type = 'Detractor'")
detractorsCount

passiveCount <- sqldf("Select count(*) from Cali_subset where NPS_Type = 'Passive'")
passiveCount

NPSValue <- ((promotersCount - detractorsCount) * 100) /nrow(Cali_subset)
NPSValue


df <- data.frame(c(Promoters,Detractors,Passives))
df <- df[,c(-4,-6)]
colnames(df) <- c('Brand','Promoters','Detractors','Passives')

View(df)

library(reshape)

d.m <- melt(df, id.vars= c("Brand"))
d.m

library(ggplot2)

ggplot(d.m, aes(x=Brand,y=value, fill = variable, color = variable,alpha = variable)) +   
  geom_bar(position = "identity", stat="identity")  + 
  scale_alpha_manual(values = c(0.7,0.3,0.1))

ggplot(d.m, aes(x=Brand,y=value, fill = variable, color = variable)) +   
  geom_bar(position = "dodge", stat="identity")

#Using dplyr tool to obtain a table view of brands and its corresponding nps_type ratings
newdf <- Cali_subset[,c(46,78)]
View(newdf)

library(reshape2)

#Identifying Promoters, Detractors and Passives
newdf2 <- newdf %>% select(NPS_Type,Brand_PL) %>% count(NPS_Type,Brand_PL) %>% spread(Brand_PL,n,fill = 0)
View(newdf2)

#Replacing all the columns with flag values as Y -> 1 and N -> 0
#Use only for logit or linear modeling

Cali_subset$OFFER_FLG_R <- ifelse(Cali_subset$OFFER_FLG_R=='Y',1,0)
Cali_subset$WALK_IN_FLG_C <- ifelse(Cali_subset$WALK_IN_FLG_C=='Y',1,0)
Cali_subset$Clublounge_Used_H <- ifelse(Cali_subset$Clublounge_Used_H=='Y',1,0)
Cali_subset$Spa_Used_H <- ifelse(Cali_subset$Spa_Used_H=='Y',1,0)
Cali_subset$All.Suites_PL <- ifelse(Cali_subset$All.Suites_PL=='Y',1,0)
Cali_subset$Bell.Staff_PL <- ifelse(Cali_subset$Bell.Staff_PL=='Y',1,0)
Cali_subset$Boutique_PL <- ifelse(Cali_subset$Boutique_PL=='Y',1,0)
Cali_subset$Business.Center_PL <- ifelse(Cali_subset$Business.Center_PL=='Y',1,0)
Cali_subset$Casino_PL <- ifelse(Cali_subset$Casino_PL=='Y',1,0)
Cali_subset$Conference_PL <- ifelse(Cali_subset$Conference_PL=='Y',1,0)
Cali_subset$Convention_PL <- ifelse(Cali_subset$Convention_PL=='Y',1,0)
Cali_subset$Dry.Cleaning_PL <- ifelse(Cali_subset$Dry.Cleaning_PL=='Y',1,0)
Cali_subset$Elevators_PL <- ifelse(Cali_subset$Elevators_PL=='Y',1,0)
Cali_subset$Fitness.Center_PL <- ifelse(Cali_subset$Fitness.Center_PL=='Y',1,0)
Cali_subset$Fitness.Trainer_PL <- ifelse(Cali_subset$Fitness.Trainer_PL=='Y',1,0)
Cali_subset$Golf_PL <- ifelse(Cali_subset$Golf_PL=='Y',1,0)
Cali_subset$Indoor.Corridors_PL <- ifelse(Cali_subset$Indoor.Corridors_PL=='Y',1,0)
Cali_subset$Laundry_PL <- ifelse(Cali_subset$Laundry_PL=='Y',1,0)
Cali_subset$Limo.Service_PL <- ifelse(Cali_subset$Limo.Service_PL=='Y',1,0)
Cali_subset$Mini.Bar_PL <- ifelse(Cali_subset$Mini.Bar_PL=='Y',1,0)
Cali_subset$Pool.Indoor_PL <- ifelse(Cali_subset$Pool.Indoor_PL=='Y',1,0)
Cali_subset$Pool.Outdoor_PL <- ifelse(Cali_subset$Pool.Outdoor_PL=='Y',1,0)
Cali_subset$Regency.Grand.Club_PL <- ifelse(Cali_subset$Regency.Grand.Club_PL=='Y',1,0)
Cali_subset$Resort_PL <- ifelse(Cali_subset$Resort_PL=='Y',1,0)
Cali_subset$Restaurant_PL <- ifelse(Cali_subset$Restaurant_PL=='Y',1,0)
Cali_subset$Self.Parking_PL <- ifelse(Cali_subset$Self.Parking_PL=='Y',1,0)
Cali_subset$Shuttle.Service_PL <- ifelse(Cali_subset$Shuttle.Service_PL=='Y',1,0)
Cali_subset$Ski_PL <- ifelse(Cali_subset$Ski_PL=='Y',1,0)
Cali_subset$Spa_PL <- ifelse(Cali_subset$Spa_PL=='Y',1,0)
Cali_subset$Spa.services.in.fitness.center_PL <- ifelse(Cali_subset$Spa.services.in.fitness.center_PL=='Y',1,0)
Cali_subset$Spa.online.booking_PL <- ifelse(Cali_subset$Spa.online.booking_PL=='Y',1,0)
Cali_subset$Spa.F.B.offering_PL <- ifelse(Cali_subset$Spa.F.B.offering_PL=='Y',1,0)
Cali_subset$Valet.Parking_PL <- ifelse(Cali_subset$Valet.Parking_PL=='Y',1,0)

################################################################################################
#California Map with average likelihood to reccomend in each of the cities
################################################################################################
install.packages("ggmap")
library("ggmap")
myMapData <- sqldf("SELECT State_PL, COUNT(*) FROM subsetUS GROUP BY State_PL ORDER BY COUNT(*) DESC")
str(myMapData)
myMapData <- myMapData[myMapData$State_PL %in% state.name,]
emptyStates <- setdiff(state.name, myMapData$State_PL) #from dplyr?
emptyStates <- data.frame(emptyStates, 0)
colnames(emptyStates) <- c("State_PL", "COUNT(*)")
myMapData <- rbind(myMapData, emptyStates)
rownames(myMapData) <- NULL
myMapData$State_PL <- tolower(myMapData$State_PL)
colnames(myMapData) <- c("State_PL", "COUNT")
View(myMapData)
USA <- map_data("state")
View(USA$region)
california <- subset(USA,USA$region=="california")
View(california)
california <- california[,-6]
caliData <- subset_USA
View(caliData$Likelihood_Recommend_H)
View(caliData$City_PL)
View(caliPoints)
caliPoints <- sqldf("Select City_PL as City,avg(Likelihood_Recommend_H) as Avg_Likelihood_Recommend_H from caliData group by City_PL")

View(caliPoints)
caliPoints$City_PL <- rownames(caliPoints)
rownames(caliPoints) <- NULL
caliPoints$State_PL <- "california"
caliPoints <- na.omit(caliPoints)
View(caliPoints)
caliPointsLatLong <- geocode(paste(caliPoints$City, caliPoints$State_PL))
caliPoints$lon <- caliPointsLatLong$lon
caliPoints$lat <- caliPointsLatLong$lat

caliPoints

caliPoints$City_PL <- tolower(caliPoints$City)

californiaMap <- ggplot(data = myMapData, aes(map_id=State_PL))+
  geom_map(map=california)+
  expand_limits(x=california$long, y=california$lat)+
  coord_fixed()+
  geom_point(data = caliPoints, aes(x=caliPoints$lon, y=caliPoints$lat, size=Avg_Likelihood_Recommend_H), color="Orange")+
  ggtitle("California Map depicting the Avg Likelihood in each City")
#scale_color_gradient(low="dark blue", high="light blue")
californiaMap


##################################################################################################
#Visual 2 - Cities in California and their NPS values
###################################################################################################
install.packages("dplyr")
library("dplyr")
install.packages("tplyr")
library("tplyr")
df1 <- Cali_subset[,c(42,78)]

#Identifying Promoters, Detractors and Passives
df2 <- df1 %>% select(NPS_Type,City_PL) %>% count(NPS_Type,City_PL) %>% spread(NPS_Type,n,fill = 0)
View(df2)

df2 <- data.frame(df2)

str(df2)

#Calculating NPS for every city 

df2$NPS <- round(((df2[,4] - df2[,2])*100)/(df2[,4] + df2[,3] + df2[,2]),2)
View(df2)

ggplot(df2, aes(x=reorder(City_PL,NPS),y=NPS, fill = NPS)) + geom_bar(stat="identity") + scale_x_discrete(labels = abbreviate) + ggtitle("NPS for each of the cities in California")
ggplot(df2, aes(x=reorder(City_PL,-NPS),y=NPS, fill = NPS)) + geom_bar(stat="identity")


###########################################################################################################
#Linear Modeling
###########################################################################################################

#Using Logit model to test dependency of Likely to Recommend on OFFER_FLG_R

glmLogit <- glm(Likelihood_Recommend_H ~ OFFER_FLG_R, data=Cali_subset)
summary(glmLogit)

#Pr(>|t|) is 0.4 which is greater than 0.1 at 99% level of confidence, hence it is non-significant 

model1 <- lm(Likelihood_Recommend_H ~ OFFER_FLG_R, data = Cali_subset)
summary(model1)

#Using Linear Model to test dependency of Likely to Recommend on LENGTH_OF_STAY_C
model2 <- lm(Likelihood_Recommend_H ~ LENGTH_OF_STAY_C, data = Cali_subset)
summary(model2)

#Using Linear Model to test dependency of Likely to Recommend on Gender_H
model3 <- lm(Likelihood_Recommend_H ~ Gender_H, data = Cali_subset)
summary(model3)

#Using Linear Model to test dependency of Likely to Recommend on Gender_H and Age_Range_H
model4 <- lm(Likelihood_Recommend_H ~ Gender_H,Age_Range_H, data = Cali_subset)
summary(model4)

#Using Linear Model to test dependency of Likely to Recommend on Clublounge_Used_H
model5 <- lm(Likelihood_Recommend_H ~ Clublounge_Used_H, data = Cali_subset)
summary(model5)

#Using Logit Model to test dependency of Likely to Recommend on Clublounge_Used_H
model6 <- glm(Likelihood_Recommend_H ~ Clublounge_Used_H, data = Cali_subset)
summary(model6)
#non-significant

#Using Linear Model to test dependency of Likely to Recommend on Spa_Used_H
model7 <- lm(Likelihood_Recommend_H ~ Spa_Used_H, data = Cali_subset)
summary(model7)

#Using Linear Model to test dependency of Likely to Recommend on GP_Tier_H
model8 <- lm(Likelihood_Recommend_H ~ GP_Tier_H, data = Cali_subset)
summary(model8)

#Using Linear Model to test dependency of Likely to Recommend on Overall_Sat_H
model9<- lm(Likelihood_Recommend_H ~ Overall_Sat_H, data = Cali_subset)
summary(model9)
#dependent on this: 81.77%

#Using Linear Model to test dependency of Likely to Recommend on Guest_Room_H
model10<- lm(Likelihood_Recommend_H ~ Guest_Room_H, data = Cali_subset)
summary(model10)
#dependent on this: 54.7%

#Using Linear Model to test dependency of Likely to Recommend on Guest_Room_H and Overall_Sat_H
model11<- lm(Likelihood_Recommend_H ~ Guest_Room_H + Overall_Sat_H, data = Cali_subset)
summary(model11)
# dependent on this: 82.66%

#Using Linear Model to test dependency of Likely to Recommend on Tranquility_H
model12<- lm(Likelihood_Recommend_H ~ Tranquility_H, data = Cali_subset)
summary(model12)
# only 18.33%

#Using Linear Model to test dependency of Likely to Recommend on Condition_Hotel_H
model13<- lm(Likelihood_Recommend_H ~ Condition_Hotel_H, data = Cali_subset)
summary(model13)
#dependent on this 54.13%

#Using Linear Model to test dependency of Likely to Recommend on Customer_SVC_H
model14<- lm(Likelihood_Recommend_H ~ Customer_SVC_H, data = Cali_subset)
summary(model14)
#dependent on this: 49.93%

#Using Linear Model to test dependency of Likely to Recommend on Customer_SVC_H and Condition_Hotel_H
model15<- lm(Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H, data = Cali_subset)
summary(model15)
# dependent on this 64.79%

#Using Linear Model to test dependency of Likely to Recommend on Staff_Cared_H
model16<- lm(Likelihood_Recommend_H ~ Staff_Cared_H, data = Cali_subset)
summary(model16)
#21%

#Using Linear Model to test dependency of Likely to Recommend on Internet_Sat_H
model17<- lm(Likelihood_Recommend_H ~ Internet_Sat_H, data = Cali_subset)
summary(model17)

#Using Linear Model to test dependency of Likely to Recommend on Check_In_H
model18<- lm(Likelihood_Recommend_H ~ Check_In_H, data = Cali_subset)
summary(model18)
#12%

#Using Linear Model to test dependency of Likely to Recommend on F.B_Overall_Experience_H
model19<- lm(Likelihood_Recommend_H ~ F.B_Overall_Experience_H, data = Cali_subset)
summary(model19)

#Using Linear Model to test dependency of Likely to Recommend on City_PL
model20<- lm(Likelihood_Recommend_H ~ City_PL, data = Cali_subset)
summary(model20)

#Using Linear Model to test dependency of Likely to Recommend on Hotel.Name.Long_PL
model21<- lm(Likelihood_Recommend_H ~ Hotel.Name.Long_PL, data = Cali_subset)
summary(model21)

View(Cali_subset)

####################################################################################
testModel<- lm(Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Tranquility_H, data = Cali_subset)
summary(testModel)
# 66.23%

testModel1<- lm(Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Guest_Room_H, data = Cali_subset)
summary(testModel1)
# 69.3% #Best Model 2

testModel2<- lm(Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel2)
# 69.82% # Best Model 1

###################################################################################################################
#Additions
######################################################################################################################

Cali_subset$Age_Range_H <- as.numeric(Cali_subset$Age_Range_H)
Cali_subset$Gender_H <- as.numeric(Cali_subset$Gender_H)
Cali_subset$Staff_Cared_H <- as.numeric(Cali_subset$Staff_Cared_H)
Cali_subset$Brand_PL <- as.numeric(Cali_subset$Brand_PL)
Cali_subset$GUEST_COUNTRY_R <- as.numeric(Cali_subset$GUEST_COUNTRY_R)
Cali_subset$ROOM_TYPE_DESCRIPTION_C <- as.numeric(Cali_subset$ROOM_TYPE_DESCRIPTION_C) # no use

testModel3<- lm(Likelihood_Recommend_H ~ Age_Range_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel3)
#69.62

testModel4<- lm(Likelihood_Recommend_H ~ Brand_PL + Staff_Cared_H + Age_Range_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel4)
#69.78


tm <- lm(Likelihood_Recommend_H ~ GUEST_COUNTRY_R, data = Cali_subset)
summary(tm)

testModel5<- lm(Likelihood_Recommend_H ~ GUEST_COUNTRY_R + Brand_PL + Staff_Cared_H + Age_Range_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel5)
#70.28 - NewBest Model
#Reduced without gues_country
#Reduced without Staff_cared


testModel6<- lm(Likelihood_Recommend_H ~  Fitness.Center_PL + GUEST_COUNTRY_R + Brand_PL + Staff_Cared_H + Age_Range_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel6)
#New Best - 70.34

Cali_subset$MEMBER_STATUS_R

testModel7<- lm(Likelihood_Recommend_H ~  MEMBER_STATUS_R + Fitness.Center_PL + GUEST_COUNTRY_R + Brand_PL + Staff_Cared_H + Age_Range_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel7)

#Double Best - 70.79

testModel8<- lm(Likelihood_Recommend_H ~  MEMBER_STATUS_R + Fitness.Center_PL + Staff_Cared_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel8)

#Best Best - 71.02

testModel9<- lm(Likelihood_Recommend_H ~  MEMBER_STATUS_R + GUEST_COUNTRY_R + Brand_PL + Age_Range_H + Condition_Hotel_H + Customer_SVC_H , data = Cali_subset)
summary(testModel9)

#65.5

#Linear Model with flag columns

lm_full <- lm(Likelihood_Recommend_H ~ Clublounge_Used_H + Spa_Used_H + 
                Fitness.Trainer_PL + Indoor.Corridors_PL + Limo.Service_PL + Mini.Bar_PL +
                Resort_PL + Self.Parking_PL + Shuttle.Service_PL + 
                Spa.online.booking_PL + Spa.F.B.offering_PL, data = Cali_subset)
summary(lm_full) #11.66

lm_full1 <- lm(Likelihood_Recommend_H ~ Spa_Used_H + 
                 Fitness.Trainer_PL + Indoor.Corridors_PL + Limo.Service_PL + Mini.Bar_PL +
                 Resort_PL + Self.Parking_PL + Shuttle.Service_PL + 
                 Spa.online.booking_PL + Spa.F.B.offering_PL, data = Cali_subset)
summary(lm_full1) #10.11


lm1 <- lm(Likelihood_Recommend_H ~ WALK_IN_FLG_C, data = Cali_subset) # Not Working

lm2 <- lm(Likelihood_Recommend_H ~ Clublounge_Used_H , data = Cali_subset)
summary(lm2)

lm3 <- lm(Likelihood_Recommend_H ~ Spa_Used_H , data = Cali_subset)
summary(lm3)

lm4 <- lm(Likelihood_Recommend_H ~ All.Suites_PL , data = Cali_subset)
summary(lm4)

lm5 <- lm(Likelihood_Recommend_H ~ Bell.Staff_PL , data = Cali_subset)
summary(lm5)

lm6 <- lm(Likelihood_Recommend_H ~ Boutique_PL , data = Cali_subset)
summary(lm6)

lm7 <- lm(Likelihood_Recommend_H ~ Business.Center_PL , data = Cali_subset)
summary(lm7)

lm8 <- lm(Likelihood_Recommend_H ~ Casino_PL , data = Cali_subset) #Not Working

lm9 <- lm(Likelihood_Recommend_H ~ Conference_PL , data = Cali_subset) # Not Working

lm10 <- lm(Likelihood_Recommend_H ~ Convention_PL, data = Cali_subset)
summary(lm10)

lm11 <- lm(Likelihood_Recommend_H ~ Dry.Cleaning_PL, data = Cali_subset) # Not Working

lm12 <- lm(Likelihood_Recommend_H ~ Elevators_PL, data = Cali_subset)
summary(lm12)

lm13 <- lm(Likelihood_Recommend_H ~ Fitness.Trainer_PL , data = Cali_subset)
summary(lm13)

lm14 <- lm(Likelihood_Recommend_H ~ Fitness.Center_PL, data = Cali_subset)
summary(lm14)

lm15 <- lm(Likelihood_Recommend_H ~ Golf_PL, data = Cali_subset)
summary(lm15)

lm16 <- lm(Likelihood_Recommend_H ~ Indoor.Corridors_PL, data = Cali_subset)
summary(lm16)

lm17 <- lm(Likelihood_Recommend_H ~ Laundry_PL, data = Cali_subset)
summary(lm17)

lm18 <- lm(Likelihood_Recommend_H ~ Limo.Service_PL, data = Cali_subset)
summary(lm18)

lm19 <- lm(Likelihood_Recommend_H ~ Mini.Bar_PL, data = Cali_subset)
summary(lm19)

lm20 <- lm(Likelihood_Recommend_H ~ Pool.Indoor_PL, data = Cali_subset)
summary(lm20)

lm21 <- lm(Likelihood_Recommend_H ~ Pool.Outdoor_PL, data = Cali_subset)
summary(lm21)

lm22 <- lm(Likelihood_Recommend_H ~ Regency.Grand.Club_PL, data = Cali_subset)
summary(lm22)

lm23 <- lm(Likelihood_Recommend_H ~ Resort_PL, data = Cali_subset)
summary(lm23)

lm24 <- lm(Likelihood_Recommend_H ~ Restaurant_PL, data = Cali_subset)
summary(lm24)

lm25 <- lm(Likelihood_Recommend_H ~ Self.Parking_PL, data = Cali_subset)
summary(lm25)

lm26 <- lm(Likelihood_Recommend_H ~ Shuttle.Service_PL, data = Cali_subset)
summary(lm26)

lm27 <- lm(Likelihood_Recommend_H ~ Ski_PL, data = Cali_subset) #Not Working

lm28 <- lm(Likelihood_Recommend_H ~ Spa_PL, data = Cali_subset)
summary(lm28)

lm29 <- lm(Likelihood_Recommend_H ~ Spa.services.in.fitness.center_PL, data = Cali_subset)
summary(lm29)

lm30 <- lm(Likelihood_Recommend_H ~ Spa.online.booking_PL, data = Cali_subset)
summary(lm30)

lm31 <- lm(Likelihood_Recommend_H ~ Spa.F.B.offering_PL, data = Cali_subset)
summary(lm31)

lm32 <- lm(Likelihood_Recommend_H ~ Valet.Parking_PL, data = Cali_subset)
summary(lm32)

str(Cali_subset)
View(Cali_subset$Spa.F.B.offering_PL)

#################################################################################################################
#Our Best linear models

#69.82
testModel2<- lm(Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
lms1 <- summary(testModel2)$r.squared

#70.34
testModel6<- lm(Likelihood_Recommend_H ~  Fitness.Center_PL + GUEST_COUNTRY_R + Brand_PL + Staff_Cared_H + Age_Range_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel6)
lms2 <- summary(testModel6)$r.squared

# 70.79
testModel7<- lm(Likelihood_Recommend_H ~  MEMBER_STATUS_R + Fitness.Center_PL + GUEST_COUNTRY_R + Brand_PL + Staff_Cared_H + Age_Range_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel7)
lms3 <- summary(testModel7)$r.squared

#Best Best - 71.02
testModel8<- lm(Likelihood_Recommend_H ~  MEMBER_STATUS_R + Fitness.Center_PL + Staff_Cared_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel8)
lms4 <- summary(testModel8)$r.squared


#Comparing our best models using bar graphs

histdata <- c(lms1,lms2,lms3,lms4)
barplot(histdata,names.arg=c("Guest+Cond+Cust+Tranq","Fit+Guest+Brand+Staff+Age+Cond+Cust+Tranq","Combined","Mem+Fit+Staff+Guest+Cond+Cust+Tranq"),col="Pink",main = "Comparision of Linear Models R Squared Values",cex.names=0.5)


#################################################################################################################
#Visualization 4
#################################################################################################################
#Best

Temp <- hotel
Cali_subset <- hotel

for(i in 1:ncol(Temp))
{
  Temp[,i] <- ifelse(is.na(Temp[,i]),"",Temp[,i])
}
View(Temp)


lmp1 <- Cali_subset[,c(20,30,32,33,34,35,36,58)]

lmp1$MEMBER_STATUS_R <- as.numeric(lmp1$MEMBER_STATUS_R)
lmp1$Cali_subset$Fitness.Center_PL <- as.numeric(lmp1$Cali_subset$Fitness.Center_PL)
lmp1$Staff_Cared_H <- as.numeric(lmp1$Staff_Cared_H)
Cali_subset$MEMBER_STATUS_R
View(lmp1)
str(lmp1)

install.packages("reshape2")
library("reshape2")
d2.m <- melt(lmp1,id.vars = "Likelihood_Recommend_H")
View(d2.m)

ggplot(d2.m,aes(x=Likelihood_Recommend_H)) +
  geom_tile(aes(y=variable, fill= value)) + scale_fill_gradient(low = "#fde0dd", high = "#756bb1")

ggplot(lmp1,aes(x= lmp1$MEMBER_STATUS_R, y = lmp1$Likelihood_Recommend_H)) + geom_point(aes(fill=lmp1$Tranquility_H,color=lmp1$Condition_Hotel_H,size=lmp1$Customer_SVC_H,alpha = lmp1$Guest_Room_H))

#################################################################################################################
#Visualization 3
#################################################################################################################

lmp <- Cali_subset[,c(30,32,32,33,34,35)]
View(lmp)
str(lmp)

d1.m <- melt(lmp,id.vars = "Likelihood_Recommend_H")
View(d1.m)

ggplot(d1.m,aes(x=Likelihood_Recommend_H)) +
  geom_tile(aes(y=variable, fill= value)) + scale_fill_gradient(low = "#fff7bc", high = "#d95f0e")


################################################################################################################
#SVM & KSVM
################################################################################################################
Cali_subset <- hotel

#install.packages("dplyr")
library("dplyr")
str(Cali_subset$Casino_PL)
View(Cali_subset)
svm_subset <- Cali_subset[,c(73,77,78,70,71,63:66,58,60,52,55,50,32:38,30)]
length(svm_subset)
#Clean
svm_new <- svm_subset
svm_new <- na.omit(svm_new)
View(svm_new)
str(svm_new$Shuttle.Service_PL)

#Convert Flag to Num
FlgNum <- function(x)
{
  x <- ifelse(x == 'Y',1, 0)
  return (factor(x))
  
}

str(svm_new$Ski_PL)
svm_new$Valet.Parking_PL <- FlgNum(svm_new$Valet.Parking_PL)
svm_new$Bell.Staff_PL <- FlgNum(svm_new$Bell.Staff_PL)
svm_new$Self.Parking_PL <- FlgNum(svm_new$Self.Parking_PL)
svm_new$Shuttle.Service_PL <- FlgNum(svm_new$Shuttle.Service_PL)
svm_new$Spa_PL <- FlgNum(svm_new$Spa_PL)
svm_new$Limo.Service_PL <- FlgNum(svm_new$Limo.Service_PL)
svm_new$Mini.Bar_PL <- FlgNum(svm_new$Mini.Bar_PL)
svm_new$Pool.Indoor_PL <- FlgNum(svm_new$Pool.Indoor_PL)
svm_new$Pool.Outdoor_PL <- FlgNum(svm_new$Pool.Outdoor_PL)
svm_new$Golf_PL <- FlgNum(svm_new$Golf_PL)
svm_new$Fitness.Center_PL <- FlgNum(svm_new$Fitness.Center_PL)
svm_new$Business.Center_PL <- FlgNum(svm_new$Business.Center_PL)
svm_new$Convention_PL <- FlgNum(svm_new$Convention_PL)
str(svm_new$Casino_PL)

#Numeric Values
NumtoCat <- function(x)
{
  x <- ifelse(x == c(1, 2, 3, 4, 5, 6),0, ifelse(x == c(7,8),1,2))
  return(factor(x))
  
}

str(svm_new$Guest_Room_H)
svm_new$Guest_Room_H <- NumtoCat(svm_new$Guest_Room_H)
svm_new$Tranquility_H <- NumtoCat(svm_new$Tranquility_H)
svm_new$Condition_Hotel_H <- NumtoCat(svm_new$Condition_Hotel_H)
svm_new$Customer_SVC_H <- NumtoCat(svm_new$Customer_SVC_H)
svm_new$Staff_Cared_H <- NumtoCat(svm_new$Staff_Cared_H)
svm_new$Internet_Sat_H <- NumtoCat(svm_new$Internet_Sat_H)
svm_new$Check_In_H <- NumtoCat(svm_new$Check_In_H)
svm_new$Likelihood_Recommend_H <- NumtoCat(svm_new$Likelihood_Recommend_H)
svm_new$NPS_Type <- factor(ifelse(svm_new$NPS_Type=="Promoter",2, ifelse(svm_new$NPS_Type=="Detractor",0,1)))
dim(svm_new)
View(svm_new)

#Train and Test
randIndex <- sample(1:dim(svm_new)[1])
randIndex
summary(randIndex)

train_cutpoint2_3 <- floor((2*dim(svm_new)[1])/3)
train_cutpoint2_3
trainData <- svm_new[randIndex[1:train_cutpoint2_3],]
dim(trainData)[1]
View(trainData)

testCutpoint <- dim(svm_new)[1]-(train_cutpoint2_3+1)
testCutpoint
testData <- svm_new[randIndex[train_cutpoint2_3+1:testCutpoint],]
View(testData)
dim(testData)


#KSVM+ 
ksvmOutput_NPS <- ksvm(NPS_Type ~ Condition_Hotel_H+Tranquility_H + Staff_Cared_H + Check_In_H + Internet_Sat_H+Customer_SVC_H+
                         Convention_PL + Business.Center_PL + Spa_PL + Mini.Bar_PL+Valet.Parking_PL+Self.Parking_PL+Shuttle.Service_PL+Limo.Service_PL+Pool.Outdoor_PL+Pool.Indoor_PL+Golf_PL+Fitness.Center_PL,
                       data=trainData,
                       kernel = "rbfdot", 
                       kpar="automatic", 
                       C = 5, 
                       cross = 3,
                       prob.model = TRUE)

ksvmOutput_NPS

ksvmOutput_Likelihood <- ksvm(Likelihood_Recommend_H ~ Condition_Hotel_H+Tranquility_H + Staff_Cared_H + Check_In_H + Internet_Sat_H+Customer_SVC_H+
                                Convention_PL + Business.Center_PL + Spa_PL + Mini.Bar_PL+Valet.Parking_PL+Self.Parking_PL+Shuttle.Service_PL+Limo.Service_PL+Pool.Outdoor_PL+Pool.Indoor_PL+Golf_PL+Fitness.Center_PL,
                              data=trainData,
                              kernel = "rbfdot", 
                              kpar="automatic", 
                              C = 5, 
                              cross = 3,
                              prob.model = TRUE)

ksvmOutput_Likelihood
#Histogram
ksvmpredLR <- predict(ksvmOutput_Likelihood,testData)
ksvmpredLR

actual <- testData$Likelihood_Recommend_H
View(actual)
View(ksvmpredLR)
Kvar2 <- abs(as.numeric(actual) - as.numeric(ksvmpredLR))
Kvar2
Kvar2 <- na.omit(Kvar2)
View(Kvar2)
hist(Kvar2, main = "Histogram of KSVM Model")


#SVM

svmOutput <- svm(NPS_Type ~ Condition_Hotel_H+Tranquility_H + Staff_Cared_H + Check_In_H + Internet_Sat_H+Customer_SVC_H+
                   Convention_PL + Business.Center_PL + Spa_PL + Mini.Bar_PL+Valet.Parking_PL+Self.Parking_PL+Shuttle.Service_PL+Limo.Service_PL+Pool.Outdoor_PL+Pool.Indoor_PL+Golf_PL+Fitness.Center_PL,
                 data=trainData)
svmOutput

svmpred <- predict(svmOutput,testData)
svmpred
table(svmpred)
actual <- testData$NPS_Type
actual
length(actual)
length(svmpred)
var2 <- as.numeric(actual) - as.numeric(svmpred)
View(var2)
var2 <- na.omit(var2)
hist(var2, main = "Histogram of SVM Model")

#######################################################################################################################