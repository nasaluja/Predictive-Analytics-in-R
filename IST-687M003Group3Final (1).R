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
sqldf("Select count(NPS_TYPE),STATE_PL from subsetUS where NPS_TYPE='Detractor' group by STATE_PL order by count(NPS_TYPE)")

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
promotersCount <- sqldf("Select count(*) from Cali_subset where NPS_Type = 'Promoter'")
promotersCount

detractorsCount <- sqldf("Select count(*) from Cali_subset where NPS_Type = 'Detractor'")
detractorsCount

passiveCount <- sqldf("Select count(*) from Cali_subset where NPS_Type = 'Passive'")
passiveCount

NPSValue <- ((promotersCount - detractorsCount) * 100) /nrow(Cali_subset)
NPSValue

#Create function which will calculate NPS after likelihood to recommend has had NAs replaced with mean.
Cali_subset$Likelihood_Recommend_H[is.na(Cali_subset$Likelihood_Recommend_H)] <- mean(Calii_subset$Likelihood_Recommend_H, na.rm = TRUE)
NPS <- function(vector){
  n <- as.numeric(vector)
  passives1 <- length(which(n==7))
  passives2 <- length(which(n==8))
  passives3 <- passives1 + passives2
  p <- length(which(n>=9))
  d <- length(which(n<=6))
  total <- p + d + passives3
  nps1 <- p-d
  NPS1 <- nps1/total
  perc <- NPS1 * 100
  return(perc)
}

ab <- NPS(Cali_subset$Likelihood_Recommend_H)
ab
############################################################################################
#Visualization
############################################################################################

#Using dplyr tool to obtain a table view of brands and its corresponding nps_type ratings
newdf <- Cali_subset[,c(46,78)]
View(newdf)

library(reshape2)

#Identifying Promoters, Detractors and Passives

Promoters <- data.frame(sqldf("Select Brand_PL,count(NPS_Type) from Cali_subset WHERE NPS_TYPE = 'Promoter' group by Brand_PL order by Brand_PL"))
View(Promoters)
colnames(Promoters) <- c('Brand','Promoters')

Detractors <- data.frame(sqldf("Select count(NPS_Type),Brand_PL from Cali_subset WHERE NPS_TYPE = 'Detractor' group by Brand_PL order by Brand_PL"))
View(Detractors)

Passives <- data.frame(sqldf("Select count(NPS_Type),Brand_PL from Cali_subset WHERE NPS_TYPE = 'Passive' group by Brand_PL order by Brand_PL"))
View(Passives)

Promoters$Brand_PL

df <- data.frame(c(Promoters,Detractors,Passives))
df <- df[,c(-4,-6)]
colnames(df) <- c('Brand','Promoters','Detractors','Passives')

View(Cali_subset)
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





newdf2 <- newdf %>% select(NPS_Type,Brand_PL) %>% count(NPS_Type,Brand_PL) %>% spread(Brand_PL,n,fill = 0)
View(newdf2)


df <- data.frame(c(Promoters,Detractors,Passives))
df <- df[,c(-4,-6)]
colnames(df) <- c('Brand','Promoters','Detractors','Passives')

View(df)

library(reshape)

d.m <- melt(df, id.vars= c("Brand"))
View(d.m)


library(ggplot2)
#Wrong
ggplot(d.m, aes(x=Brand,y=value, fill = variable, color = variable,alpha = variable)) +   
  geom_bar(position = "identity", stat="identity")  + 
  scale_alpha_manual(values = c(0.7,0.3,0.1))

ggplot(d.m, aes(x=reorder(Brand,value),y=value, fill = variable, color = variable)) +   
  geom_bar(position = "dodge", stat="identity")

########################################################################################################

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


#########################################################################################################################
#Descriptive Stats
#########################################################################################################################
Cali_subset <- hotel

#Use tapply to see relationships between room type code and likelihood to recommend.
RoomTypeMean <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$ROOM_TYPE_CODE_C, mean)
RoomTypeMin <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$ROOM_TYPE_CODE_C, min)
RoomTypeMax <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$ROOM_TYPE_CODE_C, max)

RTypeLike <- data.frame(RoomTypeMean, RoomTypeMin, RoomTypeMax)
View(RTypeLike)                                                      #Room types are codes, so not very useful.

#Check room type description column to see if it's more useful than codes.
View(Cali_subset$ROOM_TYPE_DESCRIPTION_C)

#Use tapply to see relationships between room type description and likelihood to recommend.
RoomTypeDMean <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, mean)
RoomTypeDMin <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, min)
RoomTypeDMax <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, max)
RoomTypeDMedian <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, median)
RTypeDLike <- data.frame(RoomTypeDMean, RoomTypeDMin, RoomTypeDMax, RoomTypeDMedian)
View(RTypeDLike)

#Use tapply to see how many people used each of the room types.
RoomTypeCount <- tapply(Cali_subset$ROOM_TYPE_DESCRIPTION_C, Cali_subset$ROOM_TYPE_DESCRIPTION_C, length)
RoomTypeCount

RoomTypeCountdf <- data.frame(ROOM_TYPE_DESCRIPTION_C=names(RoomTypeCount),length=RoomTypeCount)
View(RoomTypeCountdf)

#Combine the two to see ratings per room type and how many stayed in each.
RoomRatings <- data.frame(RoomTypeCountdf, RTypeDLike)
View(RoomRatings)

#Explore the data for those in Guest Room Double, since the values were lower for those people.
k <- sqldf("select * from Cali_subset where ROOM_TYPE_DESCRIPTION_C= 'Guest Room Double'")
View(k)
tapply(k$Condition_Hotel_H, k$ROOM_TYPE_DESCRIPTION_C, mean)
tapply(k$Check_In_H, k$ROOM_TYPE_DESCRIPTION_C, mean)

tapply(k$Guest_Room_H, k$ROOM_TYPE_DESCRIPTION_C, mean)     #8.25
tapply(k$Guest_Room_H, k$ROOM_TYPE_DESCRIPTION_C, min)      #1
tapply(k$Guest_Room_H, k$ROOM_TYPE_DESCRIPTION_C, max)      #10
tapply(k$Guest_Room_H, k$ROOM_TYPE_DESCRIPTION_C, median)   #9


#Do the same for 2 Queen Beds.
j <- sqldf("select * from Cali_subset where ROOM_TYPE_DESCRIPTION_C= '2 Queen Beds'")
View(j)
tapply(j$Condition_Hotel_H, j$ROOM_TYPE_DESCRIPTION_C, mean)
tapply(j$Check_In_H, j$ROOM_TYPE_DESCRIPTION_C, mean)

tapply(j$Guest_Room_H, j$ROOM_TYPE_DESCRIPTION_C, mean)   #9
tapply(j$Guest_Room_H, j$ROOM_TYPE_DESCRIPTION_C, min)    #1
tapply(j$Guest_Room_H, j$ROOM_TYPE_DESCRIPTION_C, max)    #10
tapply(j$Guest_Room_H, j$ROOM_TYPE_DESCRIPTION_C, median) #10

#Look at Room condition rating and room type.
RoomConMean <- tapply(Cali_subset$Guest_Room_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, mean)
RoomConMin <- tapply(Cali_subset$Guest_Room_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, min)
RoomConMax <- tapply(Cali_subset$Guest_Room_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, max)
RoomConMedian <- tapply(Cali_subset$Guest_Room_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, median)

RTypeConLike <- data.frame(RoomConMean, RoomConMin, RoomConMax, RoomConMedian)

#Combine the two to see ratings per room type and how many stayed in each.
RoomConRatings <- data.frame(RoomTypeCountdf, RTypeConLike)
View(RoomConRatings)  #Means are all over 8 for most used rooms.

#Look at Hotel condition rating and room type.
HotelConMean <- tapply(Cali_subset$Condition_Hotel_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, mean)
HotelConMin <- tapply(Cali_subset$Condition_Hotel_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, min)
HotelConMax <- tapply(Cali_subset$Condition_Hotel_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, max)
HotelConMedian <- tapply(Cali_subset$Condition_Hotel_H, Cali_subset$ROOM_TYPE_DESCRIPTION_C, median)

RTypeHConLike <- data.frame(HotelConMean, HotelConMin, HotelConMax, HotelConMedian)

#Combine the two to see ratings per room type and how many stayed in each.
RoomHConRatings <- data.frame(RoomTypeCountdf, RTypeHConLike)
View(RoomHConRatings)

#Use tapply to see relationships between length of stay and likelihood to recommend.
StayLengthMean <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$LENGTH_OF_STAY_C, mean)
StayLengthMin <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$LENGTH_OF_STAY_C, min)
StayLengthMax <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$LENGTH_OF_STAY_C, max)
StayLength <- data.frame(StayLengthMean, StayLengthMin, StayLengthMax)
View(StayLength)    

#Use tapply to see how many values there are for each length of stay.
StayCount <- tapply(Cali_subset$LENGTH_OF_STAY_C, Cali_subset$LENGTH_OF_STAY_C, length)                 #Not many people stay longer than 1 week.
StayCount

StayLength1 <- tapply(Cali_subset$LENGTH_OF_STAY_C, Cali_subset$LENGTH_OF_STAY_C, unique)
View(StayLength1)
StayLength1 <- data.frame(StayLength1)
names(StayLength1) <- c("Length of Stay")
StayLength2 <- data.frame(StayLength, StayLength1, StayCount) 
View(StayLength2)                                                                               #See graphs too. Not much difference with mean in 1st week.
#The first 7 days are the ones with the most data. 

#Use tapply to see relationships between length of stay and likelihood to recommend for those who had 'Guest Room Double'
kStayLengthMean <- tapply(k$Likelihood_Recommend_H, k$LENGTH_OF_STAY_C, mean)
kStayLengthMin <- tapply(k$Likelihood_Recommend_H, k$LENGTH_OF_STAY_C, min)
kStayLengthMax <- tapply(k$Likelihood_Recommend_H, k$LENGTH_OF_STAY_C, max)
kStayLength <- data.frame(StayLengthMean, StayLengthMin, StayLengthMax)
View(kStayLength)    

#Use tapply to see how many values there are for each length of stay.
kStayCount <- tapply(k$LENGTH_OF_STAY_C, k$LENGTH_OF_STAY_C, length)             #Not many people stay longer than 1 week.
kStayCount

kStayLength1 <- tapply(k$LENGTH_OF_STAY_C, k$LENGTH_OF_STAY_C, unique)
View(kStayLength1)
kStayLength1 <- data.frame(kStayLength1)
names(kStayLength1) <- c("Length of Stay")
kStayLength2 <- data.frame(kStayLength, kStayLength1, kStayCount)
View(kStayLength2)


#Use tapply to see relationships between likelihood to recommend and columns with hotel amenities. 
#I replaced Likelihood NAs with mean, so I will not omit the NAs in these columns. Code is there just in case.
s <- as.factor(Cali_subset$Spa_Used_H)
s <- na.omit(s)
tapply(Cali_subset$Likelihood_Recommend_H, s, mean)       #Slightly more likely.  No: 8.31 yes: 8.70
tapply(Cali_subset$Likelihood_Recommend_H, s, min)        #Both 1.
tapply(Cali_subset$Likelihood_Recommend_H, s, max)        #Both 10.
tapply(Cali_subset$Likelihood_Recommend_H, s, median)     # 9 and 10

tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Spa_Used_H, length)        #No: 1916 Yes: 123

l <- as.factor(Cali_subset$Clublounge_Used_H)
l <- na.omit(l)
tapply(Cali_subset$Likelihood_Recommend_H, l, mean)       #Slightly more likely.   No: 8.41 Yes: 8.61
tapply(Cali_subset$Likelihood_Recommend_H, l, min)        #Both 1.
tapply(Cali_subset$Likelihood_Recommend_H, l, max)        #Both 10.
tapply(Cali_subset$Likelihood_Recommend_H, l, median)     #Both 9.

tapply(Cali_subset$Clublounge_Used_H, Cali_subset$Clublounge_Used_H, length)    #No: 2427 Yes: 401

t <- as.factor(Cali_subset$Valet.Parking_PL)
t <- na.omit(t)
tapply(Cali_subset$Likelihood_Recommend_H, t, mean)       #No is higher.   No: 8.8 Yes: 8.33
tapply(Cali_subset$Likelihood_Recommend_H, t, min)        #Both 1.
tapply(Cali_subset$Likelihood_Recommend_H, t, max)        #Both 10.
tapply(Cali_subset$Likelihood_Recommend_H, t, median)     #Both 9.

#Use tapply to see how many No and Yes values there were.
tapply(Cali_subset$Valet.Parking_PL, Cali_subset$Valet.Parking_PL, length)   #No 2985, Yes 7089

r <- as.factor(Cali_subset$Pool.Indoor_PL)
r <- na.omit(r)
tapply(Cali_subset$Likelihood_Recommend_H, r, mean)       #Slightly less likely.
tapply(Cali_subset$Likelihood_Recommend_H, r, min)        #Both 1.
tapply(Cali_subset$Likelihood_Recommend_H, r, max)        #Both 10.
tapply(Cali_subset$Likelihood_Recommend_H, r, median)     #Both 9.

tapply(Cali_subset$Pool.Indoor_PL, Cali_subset$Pool.Indoor_PL, length)
#Use tapply to see relationships between likelihood to recommend and columns with hotel brand.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Brand_PL, mean)    #Rank: Hyatt House, Park, Grand Hyatt, Hyatt Place, Andaz, Regency, Hyatt.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Brand_PL, min)     #All 1.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Brand_PL, max)     #All 10.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Brand_PL, median)  #Park Hyatt, Hyatt House are 10, others are 9.

#Use tapply to see how many values there were for each brand.
tapply(Cali_subset$Brand_PL, Cali_subset$Brand_PL, length)    #Park and Andaz are fewest. Regular Hyatt is 3rd fewest, but lowest ratings.

hyatt <- sqldf("select * from Cali_subset where Brand_PL='Hyatt'")
#Look at Room condition rating and room type.
RoomConMean <- tapply(hyatt$Guest_Room_H, hyatt$ROOM_TYPE_DESCRIPTION_C, mean)
RoomConMin <- tapply(hyatt$Guest_Room_H, hyatt$ROOM_TYPE_DESCRIPTION_C, min)
RoomConMax <- tapply(hyatt$Guest_Room_H, hyatt$ROOM_TYPE_DESCRIPTION_C, max)
RoomConMedian <- tapply(hyatt$Guest_Room_H, hyatt$ROOM_TYPE_DESCRIPTION_C, median)

RTypeConLike <- data.frame(RoomConMean, RoomConMin, RoomConMax, RoomConMedian)
#Use tapply to see how many people used each of the room types.
RoomTypeCount <- tapply(hyatt$ROOM_TYPE_DESCRIPTION_C, hyatt$ROOM_TYPE_DESCRIPTION_C, length)
RoomTypeCount

RoomTypeCountdf <- data.frame(ROOM_TYPE_DESCRIPTION_C=names(RoomTypeCount),length=RoomTypeCount)
View(RoomTypeCountdf)
#Combine the two to see ratings per room type and how many stayed in each.
RoomConRatings <- data.frame(RoomTypeCountdf, RTypeConLike)
View(RoomConRatings) 

#Use tapply to see relationships between likelihood to recommend and columns with GP_Tier_H, which is the guest's 'passport' tier.
View(Cali_subset$GP_Tier_H)
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$GP_Tier_H, mean)   #Rank:DIAM, CARD, GOLD, PLAT.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$GP_Tier_H, min)    #Card is 2, others are 1.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$GP_Tier_H, max)    #All 10.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$GP_Tier_H, median) #CARD is 10, others are 9.

#Use tapply to see how many values there were for each tier.
tapply(Cali_subset$GP_Tier_H, Cali_subset$GP_Tier_H, length)   #Only 16 for CARD. 503 for DIAM. 1772 PLAT, 6282 GOLD.

#Use tapply to see relationships between likelihood to recommend and columns with guest's age.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Age_Range_H, mean)  
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Age_Range_H, min)    #All 1.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Age_Range_H, max)    #All 10.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Age_Range_H, median) #18-25, 66-75, and 76+ are 10. Others are 9.
#Significant differences. Those 18-15 and 76+ are happier than others. 
#Use tapply to count number of guests in each age range.
tapply(Cali_subset$Age_Range_H, Cali_subset$Age_Range_H, length)        

#Use tapply to see relationships between likelihood to recommend and columns with guest's gender.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Gender_H, mean)   #Females a little more likely.F: 8.54, M: 8.43
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Gender_H, min)    #Both 1.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Gender_H, max)    #Both 10.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$Gender_H, median) #Both 9.

tapply(Cali_subset$Gender_H, Cali_subset$Gender_H, length)  #F: 5215, M: 5954

#Use tapply to see relationships between likelihood to recommend and columns with revenue from reservation.

p <- tapply(Cali_subset$REVENUE_USD_R, Cali_subset$Likelihood_Recommend_H, mean)
p <- data.frame(p)
View(p)
plot(p)
barplot(p$p, main="Room Revenue by Likelihood to Recommend", col="darkblue")

tapply(Cali_subset$REVENUE_USD_R, Cali_subset$Likelihood_Recommend_H, min)
tapply(Cali_subset$REVENUE_USD_R, Cali_subset$Likelihood_Recommend_H, max)     #Higest overall revenue=rating of 10.
tapply(Cali_subset$REVENUE_USD_R, Cali_subset$Likelihood_Recommend_H, median)  #Highest mean and median revenue=rating of 4
#Lowest mean and median rev=rating of 10.

#Use sqldf to see how many rows have revenue over $600 since the most revenue=rating of 4. Also, check Revenue under 500, since that was the lowest revenue, but highest rating.
sqldf("select count(*) from Cali_subset where REVENUE_USD_R>600")                    #3337
sqldf("select count(*) from Cali_subset where REVENUE_USD_R<500")                    #7336

#Use tapply to see relationships between reservation revenue and the condition of the hotel room rating.
m <- tapply(Cali_subset$REVENUE_USD_R, Cali_subset$Guest_Room_H, mean)
m                                                                         #Highest revenue=3 rating. Lowest=8 and 10 ratings.
plot(m)                                                                   #Is this revenue from one night or multiple? Does it include F&B?
barplot(m, main="Room Revenue by Guest Room Condition", col="darkblue")

#Use tapply to see relationships between likelihood to recommend and columns with revenue from room.
q <- tapply(Cali_subset$PMS_ROOM_REV_USD_C, Cali_subset$Likelihood_Recommend_H, mean)     #Highest revenue=4 rating. Lowest=1, 3, and 10 ratings.
q
barplot(q, main="PMS Room Rev by Likelihood to Recommend", col="darkblue")

#Use tapply to see relationships between likelihood to recommend and columns with average daily rate.
w <- tapply(Cali_subset$average_daily_rate_CC, Cali_subset$Likelihood_Recommend_H, mean)     #Seem to be pretty similar. Lowest price=10 rating though.
w
barplot(w, main="Average daily rate by likelihood to recommend", col="darkblue")

#Use tapply to see relationships between likelihood to recommend and columns with purpose of visit.
v <- as.factor(Cali_subset$POV_CODE_C)
tapply(Cali_subset$Likelihood_Recommend_H, v, mean)          #Almost no difference. Leisure is a little higher.
tapply(Cali_subset$Likelihood_Recommend_H, v, min)           #Both 1.
tapply(Cali_subset$Likelihood_Recommend_H, v, max)           #Both 10.
tapply(Cali_subset$Likelihood_Recommend_H, v, median)        #Both 9.
tapply(Cali_subset$POV_CODE_C, Cali_subset$POV_CODE_C, length)    #Bus:9912, Leisure:1614

#Use tapply to see relationships between likelihood to recommend and columns with number of children.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$CHILDREN_NUM_C, mean)     #People with more kids seem to rate higher.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$CHILDREN_NUM_C, min)     #More kids=Higher rating.
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$CHILDREN_NUM_C, max)   
tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$CHILDREN_NUM_C, median) 

#Use tapply to count how many times kids were guests, and how many.
tapply(Cali_subset$CHILDREN_NUM_C, Cali_subset$CHILDREN_NUM_C, length)           #1086 had 2 kids, 749 had 1, 110 had 3, 25 had 4.

#Use tapply to see relationships between likelihood to recommend and columns with City.
city <- tapply(Cali_subset$Likelihood_Recommend_H, Cali_subset$City_PL, mean)     #Santa Barbara, Monterey are lowest. 
View(city)
cityl <- tapply(Cali_subset$City_PL, Cali_subset$City_PL, length) 
c <- data.frame(city, cityl)
View(c)

#Explore the data for those in Guest Room Double, since the values were lower for those people.
k <- sqldf("select * from Cali_subset where ROOM_TYPE_DESCRIPTION_C= 'Guest Room Double' and NPS_Type= 'Detractor'")
View(k)
tapply(k$Condition_Hotel_H, k$ROOM_TYPE_DESCRIPTION_C, mean) #5.669
tapply(k$Check_In_H, k$ROOM_TYPE_DESCRIPTION_C, mean)        #8.463

tapply(k$Guest_Room_H, k$ROOM_TYPE_DESCRIPTION_C, mean)     #5.265
tapply(k$Guest_Room_H, k$ROOM_TYPE_DESCRIPTION_C, min)      #1
tapply(k$Guest_Room_H, k$ROOM_TYPE_DESCRIPTION_C, max)      #10
tapply(k$Guest_Room_H, k$ROOM_TYPE_DESCRIPTION_C, median)   #5

#Use tapply to see relationships between likelihood to recommend and columns with hotel amenities. for those in 'Guest Room Double'
#I replaced Likelihood NAs with mean, so I will not omit the NAs in these columns. Code is there just in case.

s <- as.factor(k$Spa_Used_H)
s <- na.omit(s)
tapply(k$Likelihood_Recommend_H, s, mean)       #Much higher.
tapply(k$Likelihood_Recommend_H, s, min)        
tapply(k$Likelihood_Recommend_H, s, max)        
tapply(k$Likelihood_Recommend_H, s, median)     

tapply(k$Spa_Used_H, k$Spa_Used_H, length)     #Only 6 used spa.

l <- as.factor(k$Clublounge_Used_H)
l <- na.omit(l)
tapply(k$Likelihood_Recommend_H, l, mean)       #Slightly more likely.
tapply(k$Likelihood_Recommend_H, l, min)        #Both 1.
tapply(k$Likelihood_Recommend_H, l, max)        #Both 10.
tapply(k$Likelihood_Recommend_H, l, median)     #No is 9, yes is 9.5.

t <- as.factor(k$Valet.Parking_PL)
t <- na.omit(t)
tapply(k$Likelihood_Recommend_H, t, mean)       #No is higher.
tapply(k$Likelihood_Recommend_H, t, min)        #Both 1.
tapply(k$Likelihood_Recommend_H, t, max)        #Both 10.
tapply(k$Likelihood_Recommend_H, t, median)     #Both 9.

#Use tapply to see how many No and Yes values there were.
tapply(k$Valet.Parking_PL, k$Valet.Parking_PL, length)   #No 185, Yes 776

r <- as.factor(k$Pool.Indoor_PL)
r <- na.omit(r)
tapply(k$Likelihood_Recommend_H, r, mean)       #No Yes values.
tapply(k$Likelihood_Recommend_H, r, min)        
tapply(k$Likelihood_Recommend_H, r, max)        
tapply(k$Likelihood_Recommend_H, r, median)     


#Use tapply to see relationships between likelihood to recommend and columns with hotel brand.
tapply(k$Likelihood_Recommend_H, k$Brand_PL, mean)    #Rank: Park, Hyatt House, Grand Hyatt, Hyatt Place, Andaz, Regency, Hyatt.
tapply(k$Likelihood_Recommend_H, k$Brand_PL, min)     #All 1.
tapply(k$Likelihood_Recommend_H, k$Brand_PL, max)     #All 10.
tapply(k$Likelihood_Recommend_H, k$Brand_PL, median)  #Park Hyatt, Hyatt House are 10, others are 9.

#Use tapply to see how many values there were for each brand.
tapply(k$Brand_PL, k$Brand_PL, length)    #Park and Andaz are fewest.

#Use tapply to see relationships between likelihood to recommend and columns with GP_Tier_H, which is the guest's 'passport' tier.
View(subset$GP_Tier_H)
tapply(k$Likelihood_Recommend_H, k$GP_Tier_H, mean)   
tapply(k$Likelihood_Recommend_H, k$GP_Tier_H, min)    
tapply(k$Likelihood_Recommend_H, k$GP_Tier_H, max)    .
tapply(k$Likelihood_Recommend_H, k$GP_Tier_H, median) 

#Use tapply to see how many values there were for each tier.
tapply(k$GP_Tier_H, k$GP_Tier_H, length)   

#Use tapply to see relationships between likelihood to recommend and columns with guest's age.
tapply(k$Likelihood_Recommend_H, k$Age_Range_H, mean)    
tapply(k$Likelihood_Recommend_H, k$Age_Range_H, min)    
tapply(k$Likelihood_Recommend_H, k$Age_Range_H, max)    
tapply(k$Likelihood_Recommend_H, k$Age_Range_H, median) 

#Use tapply to count number of guests in each age range.
tapply(k$Age_Range_H, k$Age_Range_H, length)        #76+ and 18-25 have few.

#Use tapply to see relationships between likelihood to recommend and columns with guest's gender.
tapply(k$Likelihood_Recommend_H, k$Gender_H, mean)   #Females a little more likely.
tapply(k$Likelihood_Recommend_H, k$Gender_H, min)    #Both 1.
tapply(k$Likelihood_Recommend_H, k$Gender_H, max)    #Both 10.
tapply(k$Likelihood_Recommend_H, k$Gender_H, median) #Both 9.

tapply(k$POV_CODE_C, k$POV_CODE_C, length)
tapply(subset$POV_CODE_C, subset$POV_CODE_C, length)
tapply(k$Likelihood_Recommend_H, k$POV_CODE_C, mean)  #bus=4.338 leisure=3.869

p<- sqldf("select * from Cali_subset where NPS_Type= 'Passive'")

aMean <- tapply(p$Guest_Room_H, p$ROOM_TYPE_DESCRIPTION_C, mean)
bMin <- tapply(p$Guest_Room_H, p$ROOM_TYPE_DESCRIPTION_C, min)
cMax <- tapply(p$Guest_Room_H, p$ROOM_TYPE_DESCRIPTION_C, max)
dMed <- tapply(p$Guest_Room_H, p$ROOM_TYPE_DESCRIPTION_C, median)
DetType <- data.frame(aMean, bMin, cMax, dMed)

#Sort Room Type data frame to see which room types seem to be rated best.
BestRoom1 <- DetType[order(DetType$bMin),]      
BestRoom1

#Use tapply to see how many people used each of the room types.
RoomTypeCount1 <- tapply(p$ROOM_TYPE_DESCRIPTION_C, p$ROOM_TYPE_DESCRIPTION_C, length)
RoomTypeCount1

RoomTypeCountdf1 <- data.frame(ROOM_TYPE_DESCRIPTION_C=names(RoomTypeCount1),length=RoomTypeCount1)
View(RoomTypeCountdf1)

#Combine the two to see ratings per room type and how many stayed in each.
RoomRatings1 <- data.frame(RoomTypeCountdf1, BestRoom1)
View(RoomRatings1)    #35 people didn't like '1 Bedroom King'


mid <- sqldf("select * from Cali_subset where Age_Range_H='26-35' and '36-45'")
tapply(mid$Likelihood_Recommend_H, mid$Clublounge_Used_H, mean)

grk <- sqldf("select * from Cali_subset where ROOM_TYPE_DESCRIPTION_C= 'Guest Room King'")
tapply(grk$Condition_Hotel_H, grk$Brand_PL, length)


GRDetractors <- sqldf("select * from SuperTest where Guest_Room_H='Detractor'")
View(GRDetractors)

s <- as.factor(GRDetractors$Spa_Used_H)
s <- na.omit(s)
tapply(GRDetractors$Likelihood_Recommend_H, s, mean)       
tapply(GRDetractors$Likelihood_Recommend_H, s, min)        
tapply(GRDetractors$Likelihood_Recommend_H, s, max)        
tapply(GRDetractors$Likelihood_Recommend_H, s, median)     

tapply(GRDetractors$Spa_Used_H, GRDetractors$Spa_Used_H, length)     #Only 13 used spa.

l <- as.factor(GRDetractors$Clublounge_Used_H)
l <- na.omit(l)
tapply(GRDetractors$Likelihood_Recommend_H, l, mean)       #Slightly more likely.
tapply(GRDetractors$Likelihood_Recommend_H, l, min)        #Both 1.
tapply(GRDetractors$Likelihood_Recommend_H, l, max)        #Both 10.
tapply(GRDetractors$Likelihood_Recommend_H, l, median)     

t <- as.factor(GRDetractors$Valet.Parking_PL)
t <- na.omit(t)
tapply(GRDetractors$Likelihood_Recommend_H, t, mean)       #No is higher.
tapply(GRDetractors$Likelihood_Recommend_H, t, min)        #Both 1.
tapply(GRDetractors$Likelihood_Recommend_H, t, max)        #Both 10.
tapply(GRDetractors$Likelihood_Recommend_H, t, median)     #Both 5.

#Use tapply to see how many No and Yes values there were.
tapply(GRDetractors$Valet.Parking_PL, GRDetractors$Valet.Parking_PL, length)   #No 158, Yes 903

r <- as.factor(GRDetractors$Pool.Indoor_PL)
r <- na.omit(r)
tapply(GRDetractors$Likelihood_Recommend_H, r, mean)       
tapply(GRDetractors$Likelihood_Recommend_H, r, min)        
tapply(GRDetractors$Likelihood_Recommend_H, r, max)        
tapply(GRDetractors$Likelihood_Recommend_H, r, median)     


#Use tapply to see relationships between liklihood to recommend and columns with hotel brand.
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Brand_PL, mean)    
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Brand_PL, min)     
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Brand_PL, max)     
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Brand_PL, median)  #They like the Grand Hyatt the most, at 6.

#Use tapply to see how many values there were for each brand.
tapply(GRDetractors$Brand_PL, GRDetractors$Brand_PL, length)    #Mostly at Regency.

#Use tapply to see relationships between liklihood to recommend and columns with GP_Tier_H, which is the guest's 'passport' tier.
View(subset$GP_Tier_H)
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$GP_Tier_H, mean)   #Rank:DIAM, GOLD, PLAT, CARD.
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$GP_Tier_H, min)    #Card is 2, others are 1.
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$GP_Tier_H, max)    #All 10 except 5 for CARD.
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$GP_Tier_H, median) #CARD lowest, DIAM highest.

#Use tapply to see how many values there were for each tier.
tapply(GRDetractors$GP_Tier_H, GRDetractors$GP_Tier_H, length)   #Only 2 for CARD. 43 for DIAM. 204 PLAT, 683 GOLD.

#Use tapply to see relationships between liklihood to recommend and columns with guest's age.
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Age_Range_H, mean)    
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Age_Range_H, min)    
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Age_Range_H, max)    
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Age_Range_H, median) 

#Use tapply to count number of guests in each age range.
tapply(GRDetractors$Age_Range_H, GRDetractors$Age_Range_H, length)        #76+ only has 7. 

#Use tapply to see relationships between likelihood to recommend and columns with guest's gender.
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Gender_H, mean)   
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Gender_H, min)    
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Gender_H, max)    
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Gender_H, median) 

tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Conference_PL, mean)   
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Conference_PL, min)    
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Conference_PL, max)    
tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Conference_PL, median)

tapply(subset$Likelihood_Recommend_H, subset$Conference_PL, mean)   
tapply(subset$Likelihood_Recommend_H, subset$Business.Center_PL, length)
tapply(subset$Likelihood_Recommend_H, subset$Bell.Staff_PL, length)
tapply(subset$Likelihood_Recommend_H, subset$Limo.Service_PL, length)
tapply(subset$Likelihood_Recommend_H, subset$Mini.Bar_PL, length)
tapply(subset$Likelihood_Recommend_H, subset$Limo.Service_PL, length)
tapply(subset$Likelihood_Recommend_H, subset$Laundry_PL, mean)

tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$Mini.Bar_PL, mean) 
subset$avg
tapply(GRDetractors$POV_CODE_C, GRDetractors$POV_CODE_C, length)    #Both mostly business.
tapply(subset$POV_CODE_C, subset$POV_CODE_C, length)

aMean <- tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$ROOM_TYPE_DESCRIPTION_C, mean)
bMin <- tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$ROOM_TYPE_DESCRIPTION_C, min)
cMax <- tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$ROOM_TYPE_DESCRIPTION_C, max)
dMed <- tapply(GRDetractors$Likelihood_Recommend_H, GRDetractors$ROOM_TYPE_DESCRIPTION_C, median)
DetType <- data.frame(aMean, bMin, cMax, dMed)

#Sort Room Type data frame to see which room types seem to be rated best.
BestRoom1 <- DetType[order(DetType$bMin),]      
BestRoom1

#Use tapply to see how many people used each of the room types.
RoomTypeCount1 <- tapply(GRDetractors$ROOM_TYPE_DESCRIPTION_C, GRDetractors$ROOM_TYPE_DESCRIPTION_C, length)
RoomTypeCount1

RoomTypeCountdf1 <- data.frame(ROOM_TYPE_DESCRIPTION_C=names(RoomTypeCount1),length=RoomTypeCount1)
View(RoomTypeCountdf1)

#Combine the two to see ratings per room type and how many stayed in each.
RoomRatings1 <- data.frame(RoomTypeCountdf1, BestRoom1)
View(RoomRatings1)    #35 people didn't like '1 Bedroom King'
############################################VISUALIZATIONS###########################################
#Create visualizations for the subset. 
#Create line graphs for the analysis of length of stay and likelihood to recommend.
LenStayLine <- ggplot(StayLength2, aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMean, color="Mean")) + geom_line()
LenStayLine <- LenStayLine + geom_line(aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMin, color="Min"))
LenStayLine <- LenStayLine + geom_line(aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMax, color="Max"))
LenStayLine <- LenStayLine + labs(x="Length of Stay", y="Value", color="Variable", title="Length of Stay and Likelihood to Recommend")
LenStayLine

scatter <- ggplot(StayLength2, aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMin)) + geom_point(color="red") +
  labs(title="Length of Stay and Likelihood to Recommend", x="Length of Stay", y="Likelihood to Recommend")
scatter <- scatter + geom_point(aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMax), color="blue")
scatter <- scatter + geom_point(aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMean), color="black") 
scatter <- scatter  + scale_color_manual(labels = c("Min", "Max", "Mean"), values = c("red", "blue", "black"))
scatter

scatter <- ggplot(StayLength2, aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMin)) + geom_point() +
  labs(title="Length of Stay and Likelihood to Recommend", x="Length of Stay", y="Likelihood to Recommend")
scatter <- scatter + geom_point(aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMax))
scatter <- scatter + geom_point(aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMean)) 
scatter <- scatter + 
  scatter

scatter1 <- ggplot(StayLength2, aes(x=StayLength2$Length.of.Stay, y=StayLength2$StayLengthMean)) + geom_point(size=StayLength2$StayLengthMin, shape=StayLength2$StayLengthMax, color="red") +
  labs(title="Length of Stay and Likelihood to Recommend", x="Length of Stay", y="Value", size="Min", shape="Max")
scatter1

with(Cali_subset, plot(Spa_Used_H, Likelihood_Recommend_H)) + title(main="Effect of Spa Use on Likelihood to Recommend", ylab="Likelihood to Recommend") 
with(Cali_subset, plot(Spa_Used_H, Guest_Room_H))+ title(main="Effect of Spa Use on Room Condition Score", ylab="Room Condition")
with(Cali_subset, plot(NPS_Type, Condition_Hotel_H))+ title(main="Hotel Condition by NPS Type", ylab="Hotel Condition")              #shows importance of hotel condition
with(Cali_subset, plot(Mini.Bar_PL, Condition_Hotel_H))+ title(main="Hotel Condition if Mini Bar Present", ylab="Hotel Condition")
with(Cali_subset, plot(NPS_Type, Guest_Room_H))+ title(main="Room Condition by NPS Type", ylab="Room Condition")                    #shows importance of room condition
with(Cali_subset, plot(Age_Range_H, Likelihood_Recommend_H))+ title(main="Effect of Age Range on Likelihood to Recommend", ylab="Likelihood to Recommend")



##################################################################################################
#Visualization - Brands and POV in Cali
###################################################################################################

ndf <- Cali_subset[,c(11,46,78)]
View(ndf)

#Identifying Promoters, Detractors and Passives
ndf2 <- ndf %>% select(NPS_Type,POV_CODE_C,Brand_PL) %>% count(NPS_Type,POV_CODE_C,Brand_PL) %>% spread(NPS_Type,n,fill = 0)


ndf2 <- data.frame(ndf2)
View(ndf2)

str(df2)

#Calculating NPS for each POV type and brand type

ndf2$NPS <- round(((ndf2[,5] - ndf2[,3])*100)/(ndf2[,5] + ndf2[,4] + ndf2[,3]),2)
View(ndf2)

ndf3 <- ndf2[,c(1,2,6)]
View(ndf3)

ggplot(ndf3, aes(x=reorder(Brand_PL,NPS),y=NPS, fill = POV_CODE_C, color = POV_CODE_C,alpha = POV_CODE_C)) +   
  geom_bar(position = "identity", stat="identity")  + scale_color_manual(values = c("lightblue","brown1")) +
  scale_fill_manual(values = c("lightblue","brown1")) + scale_alpha_manual(values = c(0.7,0.1)) 
  

ggplot(ndf3, aes(reorder(x=Brand_PL,NPS),y=NPS, fill = POV_CODE_C, color = POV_CODE_C)) +   
  geom_bar(position = "dodge", stat="identity")

##########################################################################################################
#Statewise NPS graph
View(subsetUS)
d1 <- subsetUS[,c(43,78)]
View(d1)

#Identifying Promoters, Detractors and Passives
d2 <- d1 %>% select(NPS_Type,State_PL) %>% count(NPS_Type,State_PL) %>% spread(NPS_Type,n,fill = 0)
View(d2)

d2 <- data.frame(d2)

str(d2)

#Calculating NPS for every city 

d2$NPS <- round(((d2[,4] - d2[,2])*100)/(d2[,4] + d2[,3] + d2[,2]),2)
View(d2)

ggplot(d2, aes(x=reorder(State_PL,NPS),y=NPS, fill = NPS)) + geom_bar(stat="identity") + scale_x_discrete(labels = abbreviate) + ggtitle("NPS for each of the cities in California")
ggplot(d2, aes(x=reorder(State_PL,-NPS),y=NPS, fill = NPS)) + geom_bar(stat="identity")


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
Cali_subset <- hotel

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
summary(testModel7) #Double Best - 70.79



testModel8<- lm(Likelihood_Recommend_H ~  MEMBER_STATUS_R + Fitness.Center_PL + Staff_Cared_H + Condition_Hotel_H + Customer_SVC_H + Guest_Room_H + Tranquility_H, data = Cali_subset)
summary(testModel8)#Best Best - 71.02



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
barplot(histdata,names.arg=c("Guest+Cond+Cust+Tranq (0.69)","Fit.C+Guest.C+Brand+Staff.C+Age+Guest.R+Cond+Cust+Tranq (0.7034)","Combined (0.7079)","Mem.S+Fit.C+Staff.C+Guest.R+Cond+Cust+Tranq (0.7102)"),col="Pink",main = "Comparision of Linear Models R Squared Values",cex.names=0.8)


#################################################################################################################
#Visualization
#################################################################################################################
#Best

Temp <- hotel
Cali_subset <- hotel

View(Cali_subset)

for(i in 1:ncol(Temp))
{
  Temp[,i] <- ifelse(is.na(Temp[,i]),"",Temp[,i])
}
View(Temp)

lmp1 <- Cali_subset[,c(26,30,32,33,34,35,36,58)]
View(lmp1)
#################################################################################################################
#Visualization - Heatmap
#################################################################################################################


lmp1$Fitness.Center_PL <- as.numeric(lmp1$Fitness.Center_PL)
lmp1$Staff_Cared_H <- as.numeric(lmp1$Staff_Cared_H)
lmp1$Age_Range_H <- as.numeric(lmp1$Age_Range_H)

View(lmp1)
str(lmp1)

d2.m <- melt(lmp1,id.vars = "Likelihood_Recommend_H")
View(d2.m)

ggplot(d2.m,aes(x=Likelihood_Recommend_H)) +
  geom_tile(aes(y=variable, fill= value)) + scale_fill_gradient(low = "#fde0dd", high = "#756bb1")

ggplot(lmp1,aes(x= lmp1$MEMBER_STATUS_R, y = lmp1$Likelihood_Recommend_H)) + geom_point(aes(fill=lmp1$Tranquility_H,color=lmp1$Condition_Hotel_H,size=lmp1$Customer_SVC_H,alpha = lmp1$Guest_Room_H))

#################################################################################################################
#Visualization - Heatmap
#################################################################################################################

lmp <- Cali_subset[,c(30,32,33,34,35)]
View(lmp)
str(lmp)

d1.m <- melt(lmp,id.vars = "Likelihood_Recommend_H")
View(d1.m)

ggplot(d1.m,aes(x=Likelihood_Recommend_H)) +
  geom_tile(aes(y=variable, fill= value)) + scale_fill_gradient(low = "#addd8e", high = "#31a354")

################################################################################################################
#Visualization - Geom_Jitter - Scatterplot
################################################################################################################
#Plot1

lmp$Customer_SVC_H
for(i in 1:ncol(lmp))
{
  lmp[,i] <- ifelse(is.na(lmp[,i]),"",lmp[,i])
}

lmp$Likelihood_Recommend_H <- as.numeric(lmp$Likelihood_Recommend_H)
lmp$Guest_Room_H <- as.numeric(lmp$Guest_Room_H)
lmp$Tranquility_H <- as.numeric(lmp$Tranquility_H)
lmp$Condition_Hotel_H <- as.numeric(lmp$Condition_Hotel_H)
lmp$Customer_SVC_H <- as.numeric(lmp$Customer_SVC_H)

ggplot(lmp,aes(x=Condition_Hotel_H,y=Likelihood_Recommend_H,size=Guest_Room_H, color=Tranquility_H, fill=Customer_SVC_H)) + geom_jitter(width = 0.5,height=1)

#Plot2
lmp1$Customer_SVC_H
for(i in 1:ncol(lmp1))
{
  lmp1[,i] <- ifelse(is.na(lmp1[,i]),"",lmp1[,i])
}

lmp1$Likelihood_Recommend_H <- as.numeric(lmp1$Likelihood_Recommend_H)
lmp1$Guest_Room_H <- as.numeric(lmp1$Guest_Room_H)
lmp1$Tranquility_H <- as.numeric(lmp1$Tranquility_H)
lmp1$Condition_Hotel_H <- as.numeric(lmp1$Condition_Hotel_H)
lmp1$Customer_SVC_H <- as.numeric(lmp1$Customer_SVC_H)
lmp1$Age_Range_H <- as.numeric(lmp1$Age_Range_H)
lmp1$Staff_Cared_H <- as.numeric(lmp1$Staff_Cared_H)
lmp1$Fitness.Center_PL <- as.numeric(lmp1$Fitness.Center_PL)

str(lmp1)

ggplot(lmp,aes(x=Customer_SVC_H,y=Likelihood_Recommend_H,size=Guest_Room_H, color=Condition_Hotel_H, fill=Tranquility_H)) + geom_jitter(width = 0.5,height=1)


################################################################################################################
#SVM & KSVM Model
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

#Making a training cutpoint
train_cutpoint2_3 <- floor((2*dim(svm_new)[1])/3)
train_cutpoint2_3
trainData <- svm_new[randIndex[1:train_cutpoint2_3],]
dim(trainData)[1]
View(trainData)
str(trainData)

#making testing cutpoint
testCutpoint <- dim(svm_new)[1]-(train_cutpoint2_3+1)
testCutpoint
testData <- svm_new[randIndex[train_cutpoint2_3+1:testCutpoint],]
View(testData)
dim(testData)


#KSVM Model1
ksvmOutput_NPS1 <- ksvm(NPS_Type ~ Condition_Hotel_H+Tranquility_H + Staff_Cared_H + Check_In_H + Internet_Sat_H+ Customer_SVC_H +
                         Convention_PL + Business.Center_PL + Spa_PL + Mini.Bar_PL+Valet.Parking_PL+Guest_Room_H+
                         Self.Parking_PL+Shuttle.Service_PL+Limo.Service_PL+Pool.Outdoor_PL+Pool.Indoor_PL+Golf_PL+Fitness.Center_PL,
                       data=trainData,
                       kernel = "rbfdot", 
                       kpar="automatic", C = 10, cross = 10, prob.model = TRUE)

ksvmOutput_NPS1
ksvmpredNPS <- predict(ksvmOutput_NPS1,testData)
View(ksvmpredNPS)

#comparison Table
compTable1 <- data.frame(testData[,3],ksvmpredNPS)
table(compTable1)

colnames(compTable1) <- c('Test','Predicted')

View(compTable)

#Histogram
actual <- testData$NPS_Type
View(actual)
View(ksvmpredNPS)
diff <- abs(as.numeric(actual) - as.numeric(ksvmpredNPS))
diff
diff <- na.omit(diff)
View(diff)
hist(diff, main = "Histogram of KSVM Model")


#SVM Model 1

svmOutput <- svm(NPS_Type ~ Condition_Hotel_H+Tranquility_H + Staff_Cared_H + Check_In_H + Internet_Sat_H+Customer_SVC_H+
                   Convention_PL + Business.Center_PL + Spa_PL + Mini.Bar_PL+Valet.Parking_PL+
                   Self.Parking_PL+Shuttle.Service_PL+Limo.Service_PL+Pool.Outdoor_PL+Pool.Indoor_PL+Golf_PL+Fitness.Center_PL,
                 data=trainData)
svmOutput

svmpred <- predict(svmOutput,testData)
svmpred
table(svmpred)

actual <- testData$NPS_Type
actual
length(actual)
length(svmpred)

#Comparison table

compTable <- data.frame(testData[,22],svmpred)
table(compTable)

colnames(compTable) <- c('Test','Predicted')

View(compTable)

#plotting histogram
diff2 <- as.numeric(actual) - as.numeric(svmpred)
View(diff2)
diff2 <- na.omit(diff2)
hist(diff2, main = "Histogram of SVM Model")

########### KSVM with Likelihood_Recommend ###############
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
diff3 <- abs(as.numeric(actual) - as.numeric(ksvmpredLR))
diff3
diff3 <- na.omit(diff3)
View(diff3)
hist(diff3, main = "Histogram of KSVM Model")



############################################################################
#Association Mining
############################################################################

asubset <- hotel


asubset$Guest_Room_H <- as.factor(asubset$Guest_Room_H)
asubset$Tranquility_H <- as.factor(asubset$Tranquility_H)
asubset$Condition_Hotel_H <- as.factor(asubset$Condition_Hotel_H)
asubset$Customer_SVC_H <- as.factor(asubset$Customer_SVC_H)
asubset$Staff_Cared_H <- as.factor(asubset$Staff_Cared_H)
asubset$Internet_Sat_H <- as.factor(asubset$Internet_Sat_H)
asubset$Check_In_H <- as.factor(asubset$Check_In_H)


#Function to convert numerical values into categories

NumtoCat <- function(x)
{
  x <- ifelse(x == c(1, 2, 3, 4, 5, 6),"Low", ifelse(x == c(7,8),"Mid","High"))
  return(x)
  
}



asubset$Overall_Sat_H <-  NumtoCat(asubset$Overall_Sat_H)
asubset$Guest_Room_H <- NumtoCat(asubset$Guest_Room_H)
asubset$Tranquility_H <- NumtoCat(asubset$Tranquility_H)
asubset$Condition_Hotel_H <- NumtoCat(asubset$Condition_Hotel_H)
asubset$Customer_SVC_H <- NumtoCat(asubset$Customer_SVC_H)
asubset$Staff_Cared_H <- NumtoCat(asubset$Staff_Cared_H)
asubset$Internet_Sat_H <- NumtoCat(asubset$Internet_Sat_H)
asubset$Check_In_H <- NumtoCat(asubset$Check_In_H)
asubset$F.B_FREQ_H <- NumtoCat(asubset$F.B_FREQ_H)
asubset$F.B_Overall_Experience_H <- NumtoCat(asubset$F.B_Overall_Experience_H)


asubset <- asubset[,c(73,77,78,70,71,63:66,58,60,52,55,50,32:38,30)]
asubset <- na.omit(asubset)
asubset <- asubset[,-22]
asubset <- asubset[,-2]
asubset <- asubset[,-20]

View(asubset)
backup <- asubset
asubset <- backup
asubset <- asubset[,-c(3,7,12)]
asubset <- asubset[,-9]
asubset <- asubset[,-c(1,8)]


library(arules)
library(arulesViz)

#For promoter

ruleset1 <- apriori(asubset, 
                    parameter=list(support=0.5,confidence=0.6),
                    appearance=list(default="lhs",rhs=("NPS_Type=Promoter")))
inspect(ruleset1)  
inspect(sort(ruleset1,by="lift")[1:10])
plot(ruleset1, method = "paracoord")
plot(ruleset1, method = "grouped")

interesting <- ruleset1[quality(ruleset1)$lift > 0.9975]
inspect(interesting)
plot(interesting, method = "graph",control=list(type="items"))


#For detractor

ruleset2 <- apriori(asubset, parameter=list(support=0.02,confidence=0.24), 
                    appearance=list(default="lhs",rhs=("NPS_Type=Detractor")))
inspect()
inspect(sort(ruleset2,by="lift")[1:20])

##############################################################################################################
ps <- data.frame(sqldf("Select * from asubset where NPS_Type = 'Promoter'"))
View(ps)
ps <- as(ps,"transactions")
itemFrequencyPlot(ps, support=0.1)

ds <- data.frame(sqldf("Select * from asubset where NPS_Type = 'Detractor'"))
View(ds)
ds <- as(ds,"transactions")
itemFrequencyPlot(ds, support=0.1)

###############################################################################################################
Cali_subset <- hotel
View(Cali_subset)


##############################################################################################################
#Association Mining - All attempted combinations
##############################################################################################################
#Function to convert numerical values into categories

NumtoCat <- function(x)
{
  x <- ifelse(x == c(1, 2, 3, 4, 5, 6),"Low", ifelse(x == c(7,8),"Mid","High"))
  return(x)
  
}


Cali_subset$Overall_Sat_H <-  NumtoCat(Cali_subset$Overall_Sat_H)
Cali_subset$Guest_Room_H <- NumtoCat(Cali_subset$Guest_Room_H)
Cali_subset$Tranquility_H <- NumtoCat(Cali_subset$Tranquility_H)
Cali_subset$Condition_Hotel_H <- NumtoCat(Cali_subset$Condition_Hotel_H)
Cali_subset$Customer_SVC_H <- NumtoCat(Cali_subset$Customer_SVC_H)
Cali_subset$Staff_Cared_H <- NumtoCat(Cali_subset$Staff_Cared_H)
Cali_subset$Internet_Sat_H <- NumtoCat(Cali_subset$Internet_Sat_H)
Cali_subset$Check_In_H <- NumtoCat(Cali_subset$Check_In_H)
Cali_subset$F.B_FREQ_H <- NumtoCat(Cali_subset$F.B_FREQ_H)
Cali_subset$F.B_Overall_Experience_H <- NumtoCat(Cali_subset$F.B_Overall_Experience_H)


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

#Store survey columns only in new test data frame. 
test <- Cali_subset[,c(31:37,78)]
#Check structure to confirm data types are factors and the levels are correct.
str(test)

#Check for NAs.
colnames(test)[colSums(is.na(test)) > 0]

ruleset <- apriori(test, 
                   parameter=list(support=0.05,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Promoter")))
#Look at the result.
summary(ruleset)
inspect(ruleset)                        
#Visualize the results.
plot(ruleset)
plot(ruleset, method="graph", control=list(type="items"))

interesting <- ruleset[quality(ruleset)$lift > 1.464]
inspect(interesting)

ruleset <- apriori(test, 
                   parameter=list(support=0.05,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Detractor")))
#Look at the result.
summary(ruleset)
inspect(ruleset)                        
#Visualize the results.
plot(ruleset)
plot(ruleset, method="graph", control=list(type="items"))

interesting <- ruleset[quality(ruleset)$lift > 6.3]    #Detractors are most likely to be detractors for overall_sat, guest_room, customer svc, and condition_hotel
inspect(interesting)

ruleset <- apriori(test, 
                   parameter=list(support=0.05,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Passive")))
#Look at the result.
summary(ruleset)
inspect(ruleset)                        
#Visualize the results.
plot(ruleset)

interesting <- ruleset[quality(ruleset)$lift > 3.0]    #Passives are most likely to be passive about overall sat, hotel condition and cus sat or overall sat, room condition, and cus sat.
inspect(interesting)


#Try arules with other columns.
Test <- subset[,c(25:29,49:79)]
str(Test)
View(Test)
colnames(Test)[colSums(is.na(Test)) > 0]

Test <- Test[complete.cases(Test),]     #Will remove NAs. Unfortunately, this means 0 rows remaining.
NROW(Test)
str(Test)


Test1 <- subset[,c(4,8,10,11,25:29,79)]
str(Test1)
colnames(Test1)[colSums(is.na(Test1)) > 0]

Test1$LENGTH_OF_STAY_C <- as.factor(Test1$LENGTH_OF_STAY_C)
Test1$CHILDREN_NUM_C <- as.factor(Test1$CHILDREN_NUM_C)

Test1 <- Test1[complete.cases(Test1),]
NROW(Test1)                                        #Left with 243 rows.
str(Test1)                                         

ruleset <- apriori(Test1, 
                   parameter=list(support=0.05,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Promoter")))
#Look at the result.
summary(ruleset)
inspect(ruleset)                        
#Visualize the results.
plot(ruleset) 
plot(ruleset, method="graph", control=list(type="items"))

interesting <- ruleset[quality(ruleset)$lift > 1.34]    
inspect(interesting)

#Had no rules for Detractors or Passives.
Test1 <- grk[,c(4,8,10,11,25:29,79)]
str(Test1)
colnames(Test1)[colSums(is.na(Test1)) > 0]

Test1$LENGTH_OF_STAY_C <- as.factor(Test1$LENGTH_OF_STAY_C)
Test1$CHILDREN_NUM_C <- as.factor(Test1$CHILDREN_NUM_C)
Test1 <- Test1[complete.cases(Test1),]
NROW(Test1)                                        #Left with 171 rows.
str(Test1)                                         

ruleset <- apriori(Test1, 
                   parameter=list(support=0.05,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Promoter")))
#Look at the result.
summary(ruleset)
inspect(ruleset)                        
#Visualize the results.
plot(ruleset) 

interesting <- ruleset[quality(ruleset)$lift > 1.3]    
inspect(interesting)

#Try arules with amenities columns and others.
test3 <- subset[,c(49:60, 79)]
str(test3)

#Check for NAs.
colnames(test3)[colSums(is.na(test3)) > 0]

test3 <- test3[complete.cases(test3),]     #Will remove NAs. 9947 remaining.
NROW(test3)
str(test3)

ruleset <- apriori(test3, 
                   parameter=list(support=0.2,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Promoter")))
#Look at the result.
summary(ruleset)             #3997 rules
inspect(ruleset)                        
#Visualize the results.
plot(ruleset)                   

interesting <- ruleset[quality(ruleset)$lift > 1.0495]     
inspect(interesting)

ruleset <- apriori(test3, 
                   parameter=list(support=0.2,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Detractor")))
#Look at the result.
summary(ruleset)          #No rules.
inspect(ruleset)                        
#Visualize the results.
plot(ruleset)

ruleset <- apriori(test3, 
                   parameter=list(support=0.3,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Passive")))
#Look at the result.
summary(ruleset)         #No rules.
inspect(ruleset)                        
#Visualize the results.
plot(ruleset)

#Try arules with amenities columns and others.
test4 <- subset[,c(61:75,78:79)]
str(test4)

#Check for NAs.
colnames(test4)[colSums(is.na(test4)) > 0]

test4 <- test4[complete.cases(test4),]     #Will remove NAs. 5810 remaining.
NROW(test4)
str(test4)

ruleset <- apriori(test4, 
                   parameter=list(support=0.3,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Promoter")))
#Look at the result.
summary(ruleset)             #6373 rules
inspect(ruleset)                        
#Visualize the results.
plot(ruleset)                   

interesting <- ruleset[quality(ruleset)$lift > 1.0564]     
inspect(interesting)

ruleset <- apriori(test4, 
                   parameter=list(support=0.3,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Detractor")))
#Look at the result.
summary(ruleset)          #No rules.
inspect(ruleset)                        
#Visualize the results.
plot(ruleset)


ruleset <- apriori(test4, 
                   parameter=list(support=0.3,confidence=0.5),
                   appearance=list(default="lhs",rhs=("NPS_Type=Passive")))
#Look at the result.
summary(ruleset)         #No rules.
inspect(ruleset)                        
#Visualize the results.
plot(ruleset)

