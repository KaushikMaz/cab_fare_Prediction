rm(list = ls(all=TRUE)) #clearing the setup
setwd("C:/Users/user/Downloads")# setting the environment
getwd()




#load  necessary libraries
x = c('ggplot2','corrgram','DMwR','caret','randomForest','unbalanced','C50','dummies','e1071','Information',
      'MASS', 'rpart','gbm', 'ROSE','sampling', 'DataCombine', 'inTrees')

lapply(x, require, character.only = TRUE)

y = c("dplyr","plyr","ggplot2","data.table","GGally","magrittr","lubridate","tidyr","geosphere")
lapply(y, require, character.only = TRUE)


install.packages(c("dplyr","plyr","reshape","ggplot2","data.table","corrgram","geosphere","tidyr","scales"))
install.packages(c("GGally","DataCombine"))

library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")
library('geosphere')
library('tidyr')
library('scales')
library("caret")


###Load the csv file
train=read.csv("train_cab.csv",header=T,na.strings = c(""," ","NA"))
# View(train)
test=read.csv("test.csv",header=T,na.strings = c(""," ","NA"))
# View(test)


####################################Explore the data###########################################

str(train)
str(test)


#Splitting the  pickup_date attribute into various  entities like 'month', 'year', 'date', 'dayofweek', 'hour'
prepare_datetime = function (x){
  
  return(x %>% 
           mutate(
             pickup_datetime = ymd_hms(pickup_datetime),
             month = month(pickup_datetime),
             year = year(pickup_datetime),
             day = day(pickup_datetime),
             dayOfWeek = wday(pickup_datetime),
             hour = hour(pickup_datetime)
           ))
}

#Applyting the function into train & test dataset
train =  prepare_datetime(train)
test = prepare_datetime(test)



###Fare amount can't be any value less than or equal to 0

#changing the datype of fare_amount
train$fare_amount=as.numeric(as.character(train$fare_amount)) 

#Function to remove values less than or equal to 0
filter_fare_amount = function(x){
  x = x %>% 
    filter(fare_amount>=1)
  return(x)
}

#applying the function into train dataset 
train=filter_fare_amount(train)



##Passenger Count can't be more than 6 and must not be in decimal numbers
##In given dataset, there are data with 0 passenger count thus remove it
#Function to apply the above stated
prepare_passenger_count = function(x){
  x = x %>% 
    filter(passenger_count>=1 , passenger_count <=6 ) %>%
    mutate( passenger_count = floor(passenger_count)   )
  # it removes na values as well
  return(x)
}

#function applied on train & test dataset
train = prepare_passenger_count(train) 
test = prepare_passenger_count(test)


#Checking if any longitude is < -180 or > 180
summary(train$pickup_longitude)
summary(train$dropoff_longitude)

#Checking if any latitude is < -90 or > 90
summary(train$pickup_latitude)
summary(train$dropoff_latitude)

#Function to remove values outside of prescribed values for latitute and longitutde
filter_lat_long = function(x){
  x = x %>% 
    filter(pickup_latitude<=90 , pickup_latitude>=-90)
  x = x %>% 
    filter(dropoff_latitude<=90 , dropoff_latitude>=-90)
  
  x = x %>% 
    filter(pickup_longitude<=180 , pickup_longitude>=-180)
  
  x = x %>% 
    filter(dropoff_longitude<=180 , dropoff_longitude>=-180)
  
  return(x)
}

#Function applied on train & test datset
train = filter_lat_long(train) 
test = filter_lat_long(test)



################################ Missing value analysis###############################
#To see if there are any missing values
Missing_val = data.frame(sapply(train, function(x) sum(is.na(x))))


#changing the variable name of the dataframe
Missing_val$Variables = row.names(Missing_val)
row.names(Missing_val) = NULL
names(Missing_val)[1] = "Missing_values"

#Show Percentage of missing value
Missing_val$Missing_percentage=(Missing_val$Missing_values/nrow(train))*100

train = na.omit(train)#removing the misisng  values



######################## Calculating distance from pickup and drop coordinates###########
prepare_distance = function(x){
  
  x = x %>% 
    mutate(distance = by(x, 1:nrow(x), function(row) { 
      distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude,row$dropoff_latitude))/1000}))
  #Removing the distance which is 0
  x=  x[-which(x$distance == 0),]
  
  return(x)
  
}
train = prepare_distance(train)
test = prepare_distance(test)

#Getting Summary of distance
summary(train$distance)
summary(test$distance)


#Removing the distance which are > 150 
train = train[-which (train$distance > 150 ),]

#Removing the fare_amount which is >1000
train = train[-which(train$fare_amount > 1000),]

 


##############################Exploratory Data Analysis################################

####################################UNIVARIATE ANALYSIS###############################
#Function to visualize variables

Univariate_distribtuion=function(value){
ggplot(train,aes_string(x=value))+geom_histogram(fill="DarkslateBlue", colour="Blue",bins=30)+
    theme_bw()
}
#Visualize the distribution of target variable "fare_amount"
Univariate_distribtuion(train$fare_amount)

#Visualize the distribution of  "passenger_count"
Univariate_distribtuion(train$passenger_count)

#Visualize the distribution of  "month"
Univariate_distribtuion(train$month)

#Visualize the distribution of  "year"
Univariate_distribtuion(train$year)

#Visualize the distribution of  "day"
Univariate_distribtuion(train$day)

#Visualize the distribution of  "dayofweek"
Univariate_distribtuion(train$dayOfWeek)

#Visualize the distribution of  "hour"
Univariate_distribtuion(train$hour)

#Visualize the distribution of  "distance"
Univariate_distribtuion(train$distance)


##################################Bivariate Analysis##################################


#####################Distribution of passenger_count over fare################

#PLoting the graph for passenger count and Fare
ggplot(data=train, aes(x=train$passenger_count, y=train$fare_amount)) + geom_point()+
  ggtitle("Time and Fare Plot") +
  xlab("Passenger Count ") + 
  ylab("Fare")

#From the Graph, passenger count is not affecting the fare.
# we can see that single passengers are the most frequent travellers, and the highest fare also seems to come from cabs which carry just 1 passenger.


#####################Distribution of no of trips over the years################
train %>%
  dplyr::count(year)

train %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(count = n() ) %>%
  ggplot( aes(x = year, y = count, fill = year)) +
  geom_bar(stat="identity") +
  theme(legend.position = "none") +
  labs(title = "Distribution of no of trips over the years",
       x = "passenger count",
       y = "Total Count")

#From the graph,the no of trips over the year is uniform,maximum : 2012 and minimum : 2015



#####################Distribution of fare_amount over the years##################
train %>%
  dplyr::count(year)

train %>%
  dplyr::group_by(year)%>%
  dplyr::summarise(fare_amount = mean(fare_amount) ) %>%
  ggplot( aes(x = year, y =fare_amount, fill = year)) +
  geom_bar(stat="identity") +
  theme(legend.position = "none") +
  labs(title = "Distribution of fare amount",
       x = "Years",
       y = "Fare amount")

#Avg Fare amount has beern increasing over the years.


#####################Distribution of fare_amount over the months################
train %>%
  dplyr::count(month)

train %>%
  dplyr::group_by(month)%>%
  dplyr::summarise(fare_amount = mean(fare_amount) ) %>%
  ggplot( aes(x = month, y =fare_amount, fill = month)) +
  geom_bar(stat="identity") +
  theme(legend.position = "none") +
  labs(title = "Distribution of fare_amount over the months",
       x = "Months",
       y = "fare amount")
#The fares throught the month mostly seem uniform, with the maximum fare received on the 10th



#####################Distribution of fare_amount over hours##########################

#Hist of Hours
hist(as.numeric(train$hour), xlab = "Hours", main=paste("Hist of Hours"),col = "Green")

#from the graph,low frequency of the cab is found on  morning hours

#PLoting the graph for Hours and Fare
ggplot(data=train, aes(x=train$hour, y=train$fare_amount)) + geom_point()+  
  ggtitle("Time and Fare Plot") +
  xlab("Hours ") + 
  ylab("Fare")


#From the above graph we can see that the timeing is not affecting too much. Maximin dots are below 100. 


############################Distribution of fare_amount over day of the week####################

hist(as.numeric(train$dayOfWeek), xlab = "DayOfWeek", main=paste("Hist of Dayofweek"),col = "Red")

#day of the week doesn't seem to have that much of an influence on the number of cab rides

#Plotting the graph for passenger Dayofweek and Fare

ggplot(data=train, aes(x=train$dayOfWeek, y=train$fare_amount)) + geom_point()+  
ggtitle("Day count and Fare Plot") +
xlab("Day of Week ") + 
ylab("Fare")


#The highest fares seem to be on a Sunday and Monday, and the lowest on Wednesday and Friday.

##############################Distribution of fare_amount over distance################

ggplot(data=train, aes(x=train$distance, y=train$fare_amount)) + geom_point()+geom_line()+  
  ggtitle("Distance and Fare Plot") +
  xlab("Distance ") + 
  ylab("Fare")


# From the above graph, distance is found to be  an important independent variable. 

################################## Outlier Analysis ###############################################

### ## ## BoxPlots - Distribution and Outlier Check
 numeric_index = sapply(train,is.numeric) #selecting only integer
 numeric_data = train[,numeric_index]
 cnames  = colnames(numeric_data)
 
 ##function for plotting the boxplot
 for( i in 1:length(cnames)) {
   print(i)
   assign(paste0('gn',i), ggplot( data = train,aes_string( y= cnames[i], x = 'fare_amount'),)+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="fare_amount")+
            ggtitle(paste("Box plot of cnt for",cnames[i])))
 }
# # ## Plotting plots together
 gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
 gridExtra::grid.arrange(gn3,gn4,gn5,ncol=3)
 gridExtra::grid.arrange(gn6,gn7,gn8,ncol=3)
 gridExtra::grid.arrange(gn9,gn10,gn11,ncol=3)
 gridExtra::grid.arrange(gn12,ncol=1)

 
 ##Removing the outliers
cnames = c("distance", "fare_amount")
for( i in cnames){
  print(i)
  val = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  train = train[ which(! train[,i] %in% val),]
}




##################################Feature Selection################################################
## Correlation Plot

corrgram(train[,c('pickup_longitude','pickup_latitude','dropoff_longitude','dropoff_longitude','dropoff_latitude','passenger_count','month', 'year', 'day' ,'hour' ,'dayOfWeek', 'distance')],order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")




# names(train)



## dimensionality  reduction
##removing the redundant variables fro dataset

test = subset(test,select=-c(pickup_datetime,dropoff_longitude,dropoff_latitude,pickup_latitude,pickup_longitude))
train = subset(train,select=-c(pickup_datetime,dropoff_longitude,dropoff_latitude,pickup_latitude,pickup_longitude))




##################################Feature Scaling################################################

#Normality check
#Train Data
cnames = c("distance")
for( i in cnames){
  train[,i] = (train[,i] - min(train[,i])) / ( max(train[,i]) -min( train[,i]))
}
#Test data
for( i in cnames){
  test[,i] = (test[,i] - min(test[,i])) / ( max(test[,i]) -min( test[,i]))
}




################################### Evaluation matrics#######################################
EVM = function( y_actual, y_predict) {
  print("MAPE")
  m = mean(abs( (y_actual - y_predict) / y_actual) )
  print(m  )
  difference = y_actual - y_predict
  root_mean_square = sqrt(mean(difference^2))
  print("RMSE")
  print(root_mean_square)
  print("R square value")
  rss <- sum((y_predict - y_actual) ^ 2)  ## residual sum of squares
  tss <- sum((y_actual - mean(y_actual)) ^ 2)  ## total sum of squares
  rsq <- 1 - (rss/tss)
  print(rsq)
}




###################################Model Development#######################################
#Clean the environment

rm(list=setdiff(ls(), c("train","test","EVM")))
set.seed(1234)

##Creating train and test data
train.index = createDataPartition(train$fare_amount,p= 0.8,list = F)
train_data = train[train.index,1:8]
test_data = train[c(-train.index),1:8]

################################## LINEAR REGRESSION #########################################

linerModel <- lm(fare_amount ~., data = train_data)
summary(linerModel)

linear_predict <- predict(linerModel,test_data)

EVM(test_data[,1],linear_predict)

# "MAPE" =0.18024
# "RMSE" =1.8451
# R-squared = 0.64370



################################## DECISION TREE MODEL #########################################

DT = rpart(fare_amount ~ ., data = train_data, method = 'anova')


predictions_DT = predict( DT, test_data[,-1])
print(DT)

EVM(test_data[,1], predictions_DT)

# "MAPE" = 0.197386
# "RMSE" = 1.968644
# R-squared = 0.5935116



################################## RANDOM FOREST #########################################

RF = randomForest(fare_amount ~ .,train_data , importance = TRUE, ntree = 500)

predictions_RF = predict( RF, test_data[,-1])
# plot(RF)

EVM(test_data[,1], predictions_RF)
# "MAPE" = 0.17633
# "RMSE" = 1.76341
# R-squared = 0.6744



################################## GRADIENT BOOST #########################################

model_gbm <- caret::train(fare_amount ~ .,
                          data = train_data,
                          method = "gbm",
                          distribution="gaussian",
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 5, 
                                                   repeats = 3, 
                                                   verboseIter = FALSE),
                          verbose = 0)

data_gbm = predict(model_gbm, test_data)
EVM(test_data[,1], data_gbm)

# "MAPE" = 0.1667488
# "RMSE" = 1.73542
# R-squared = 0.68719





## out of all the models, Gradient boost Model proves out to be the  finest among all
## and is applied on the test data  


test$fare_amount <- predict(model_gbm,test)

#graph between fare_amount and distance of test data
gplot <- ggplot(data=test, aes(x=distance, y=fare_amount)) + geom_point()+ geom_line()+ 
  ggtitle("Distance and Fare Plot") +
  xlab("Distance ") + 
  ylab("Fare")
gplot

##Test data file with predicted fare_amount
write.csv(test, file="cab_fare_predictionR.csv", row.names = FALSE)




