install.packages("dplyr")
library("dplyr")
unique(subset_USA$Casino_PL)

svm_subset <- subset_USA[,c(77,76,69:72,62:65,59,60,57,51:55,31:37,29)]
length(svm_subset)
#Clean
svm_new <- svm_subset
svm_new <- na.omit(svm_new)
svm_new <- svm_new[,-18]
svm_new <- svm_new[,-15]
svm_new <- svm_new[,-5]
svm_new <- svm_new[,-14]
svm_new <- svm_new[,-11]
View(svm_new)


#Convert Flag to Num
FlgNum <- function(x)
{
  x <- ifelse(x == 'Y',1, 0)
  return (factor(x))
  
}
str(svm_new$Valet.Parking_PL)
svm_new$Valet.Parking_PL <- FlgNum(svm_new$Valet.Parking_PL)
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
ksvmpredLR <- predict(ksvmOutput_NPS,testData)
ksvmpredLR

actual <- testData$NPS_Type
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












