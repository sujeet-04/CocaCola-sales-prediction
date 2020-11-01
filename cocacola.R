                  ###################################################
                  #     Time Series Analysis of Cocacola sales      #
                  ###################################################

library(readxl) #Library to read excel data
                  
cocacola_sales <- read_excel("D:\\Data Science\\assignment\\CocaCola_Sales_Rawdata.xlsx",sheet = 1)
View(cocacola_sales)                  

#Library use for Forecasting
library(fpp2)
library(forecast)
library(ggplot2)

cocacola_sales_ts <- ts(cocacola_sales$Sales,frequency = 4,start = c(1986,1))
print(cocacola_sales_ts)

                       ################################
                       #     Preliminary Analysis     #
                       ################################

#Time plot
autoplot(cocacola_sales_ts,color="blue")+
  ggtitle("Time-Plot:CocaCola Sales Trend on different quarter")+
  ylab(" Sales Figure ")

#Data Has a positive trend it shows that the sales figures increasing continuously during
#different quarters of the years,to analyze further deep into the time series we need to remove trend
Diff_ts <- diff(cocacola_sales_ts)

#Time plot of differentiate data
autoplot(Diff_ts,color="blue")+
  ggtitle("Time-Plot:change in the sales figures of CocaCola sales Across The Quarters")+
  ylab("Sales Figures")

#now as we make our trend stationary we will look into our seasonality

ggseasonplot(Diff_ts)

#From the Seasonal plot we can say that there is a sudden peak in the sales of cocacola on
#2nd quarter and drop in the sales data on quarter 1 and quarter 4
#so there is a seasonal drop and peak in the sales figure of cocacola.

#Lets look into the seasonal subseries plot for more clear understanding about the pattern.

ggsubseriesplot(Diff_ts)

#From the above plot we can say that there is both trend and seasonality in our data
#set.As we had done analysis on our time series now we will build a model on it and 
#according to the accuracy we select our model.we use two Analysis techniques :-
#1.Model Based Forecasting Analysis
#2.Data driven Forecasting Analysis.

                           #########################
                           #   Data Preperation    #
                           #########################

#As we are dealing with the quarters so we need to create dummy variable for 4 quarters.

QUarter_dummy=data.frame(rep(c(1,0,0,0),11),rep(c(0,1,0,0),11),rep(c(0,0,1,0),11),
              rep(c(0,0,0,1),11))
View(QUarter_dummy)

#Rename the columns
colnames(QUarter_dummy) <- c("Q1","Q2","Q3","Q4")
View(QUarter_dummy)

#We Need 42 rows as we have to remove 2 rows from Quarter_dummy data set.
QUarter_dummy <- QUarter_dummy[1:42,]

#combine the dummy data set into the cocacola sales data set
cocacola_sales2 <- data.frame(cocacola_sales,QUarter_dummy)
head(cocacola_sales2)
View(cocacola_sales2)

#We have to build few columns into the cocacola_Sales2 for calculating different models.
cocacola_sales2["t"] <-1:42 
View(cocacola_sales2)
head(cocacola_sales2)

cocacola_sales2["t_squared"] <- cocacola_sales2["t"]*cocacola_sales2["t"]
cocacola_sales2["log_Sales"] <- log(cocacola_sales2["Sales"])
View(cocacola_sales2)
head(cocacola_sales2)

               ###################################################
               #   Data Partition into training and testing set  #
               ###################################################

#Training Set 
train_1 <- cocacola_sales2[1:36,]
View(train_1)
head(train_1)

train_2 <- cocacola_sales[1:36,] #Select data from 1986 to 1993 into training set
View(train_2)
head(train_2)

train_ts <- ts(train_2$Sales,frequency = 4,start = c(1986,1)) #transform into time series
print(train_ts)

#Testing Set
test_1 <- cocacola_sales2[37:42,]
View(test_1)
head(test_1)

test_2 <- cocacola_sales[37:42,] #Select data from 2002 into testing set
head(test_2)

test_ts <- ts(test_2$Sales,frequency = 4,start = c(1995,1))#Transform into time series
print(test_ts)

#Create a function to calculate Mean Absolute Percentage Error
mape <- function(actual,pred){
  mape <- mean(abs((actual-pred)/actual))*100
  return(mape)
}

#######################################################################################
# From the Visualization we can conclude that there is both additive and multiplicative 
# seasonality in the data set because the mean is constant till 1992 and there is a 
#increase in trend in 1993 so will try to build both multiplicative seasonality model
#and Additive seasonality model and check for accuracy.
#######################################################################################


                ##############################################
                #      Model Based Forecasting Analysis      #
                ##############################################
attach(train_1)

              ########### Multiplicative Seasonality #############

Multi_Season <- lm(log_Sales ~Q1+Q2+Q3+Q4,data = train_1)
summary(Multi_Season)


#Comput the model on test data set and predict the value 
Multi_Season_pred <- data.frame(predict(Multi_Season,newdata = test_1,interval = "predict"))
Multi_Season_pred

#As our prediction contains logrithmic function within it we need to transform it
Multi_Season_pred1 <- exp(Multi_Season_pred$fit)
Multi_Season_pred1

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Multi_Season_pred1) #MAPE=42.83


############# Multiplicative Seasonality with linear data ###########
Multi_Season_linear <- lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train_1)
summary(Multi_Season_linear)

#Comput the model on test data set and predict the value 
Multi_Season_linear_pred <- data.frame(predict(Multi_Season_linear,newdata = test_1,interval = "predict"))
Multi_Season_linear_pred

#As our prediction contains logrithmic function within it we need to transform it
Multi_Season_pred2 <- exp(Multi_Season_linear_pred$fit)
Multi_Season_pred2

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Multi_Season_pred2) #MAPE=6.83

############## Additive Seasonality ################

Add_season <- lm(train_1$Sales~Q1+Q2+Q3+Q4,data = train_1)
summary(Add_season)


#Comput the model on test data set and predict the value 
Add_season_pred <- data.frame(predict(Add_season,newdata = test_1,interval = "predict"))
Add_season_pred

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Add_season_pred$fit) #MAPE=40.98

############## Additive Seasonality with linear model ################

Add_season_linear <- lm(train_1$Sales~t+Q1+Q2+Q3+Q4,data = train_1)
summary(Add_season_linear)


#Comput the model on test data set and predict the value 
Add_season_linear_pred <- data.frame(predict(Add_season_linear,newdata = test_1,interval = "predict"))
Add_season_linear_pred

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Add_season_linear_pred$fit) #MAPE=10.92


############## Additive Seasonality with quadratic model ################

Add_season_quad <- lm(train_1$Sales~t_squared+Q1+Q2+Q3+Q4,data = train_1)
summary(Add_season_quad)


#Comput the model on test data set and predict the value 
Add_season_quad_pred <- data.frame(predict(Add_season_quad,newdata = test_1,interval = "predict"))
Add_season_quad_pred

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Add_season_quad_pred$fit) #MAPE=4.51



                  ##########################################
                  ##    Data Driven forecasting Analysis  ##
                  ##########################################

############## Naive Forecasting Method #################

naive_model <- snaive(train_ts,h=6)
naive_model
summary(naive_model)

plot(naive_model)

#For calculating the mape we need the point forecast of naive model and compare it with test data.
naive_df <- data.frame(naive_model)
naive_pred <- naive_df$Point.Forecast

mape(test_1$Sales,naive_pred)
#Mean Absolute Percentage Error=13.10


###############  Holt's Trend Method  ###################

#Build a model for alpha=0.2 and beta=0.2
holt_model <- holt(train_ts,h=6,alpha = 0.2,beta = 0.2)
summary(holt_model)

plot(holt_model)

#Transform the above model into the data frame.   
holt_df <- data.frame(holt_model)
holt_pred<- holt_df$Point.Forecast
#Checking for accuracy of the model.
mape(test_1$Sales,holt_pred) #MAPE=10.83


#Build a model for alpha=0.3 and beta=0.3
holt_model_1 <- holt(train_ts,h=6,alpha = 0.3,beta = 0.3)
summary(holt_model_1)

plot(holt_model_1)

#Transform the above model into the data frame.   
holt_df1 <- data.frame(holt_model_1)
holt_pred1<- holt_df1$Point.Forecast
#Checking for accuracy of the model.
mape(test_1$Sales,holt_pred1) #MAPE=13.15
#The mape goes up so we have to try for leeser value alpha and beta

#Build a model for alpha=0.1 and beta=0.1
holt_model_2 <- holt(train_ts,h=6,alpha = 0.1,beta = 0.1)
summary(holt_model_2)

plot(holt_model_2)

#Transform the above model into the data frame.   
holt_df2 <- data.frame(holt_model_2)
holt_pred2 <- holt_df2$Point.Forecast
#Checking for accuracy of the model.
mape(test_1$Sales,holt_pred2) #MAPE=9.21


#Build a model without a optimum value for level and trend
holt_model_3 <- holt(train_ts,h=6)
summary(holt_model_3)

plot(holt_model_3)

#Transform the above model into the data frame.   
holt_df3 <- data.frame(holt_model_3)
holt_pred3 <- holt_df3$Point.Forecast
#Checking for accuracy of the model.
mape(test_1$Sales,holt_pred3) #MAPE=9.21

###############  Holt winter exponential method  ###################

#Build a model for alpha=0.1,beta=0.1 and gamma=0.1

holt_winter_model <- HoltWinters(train_ts,alpha = 0.1,beta = 0.1,gamma = 0.1)
holt_winter_model
summary(holt_winter_model)


plot(holt_winter_model)

#Lets predict the passenger count for next 12 month and transform it into data frame.
holt_winter_pred <- predict(holt_winter_model,n.ahead = 6)
holt_winter_pred<-data.frame(holt_winter_pred)
holt_winter_pred
#Mean Absolute Percentage Error
mape(test_1$Sales,holt_winter_pred$fit)#MAPE=11.00


#Build a model for alpha=0.1,beta=0.1 and gamma=0.05

holt_winter_model2 <- HoltWinters(train_ts,alpha = 0.1,beta = 0.1,gamma = 0.05)
summary(holt_winter_model2)


plot(holt_winter_model2)

#Lets predict the passenger count for next 12 month and transform it into data frame.
holt_winter_pred2 <- predict(holt_winter_model2,n.ahead = 6)
holt_winter_pred2 <- data.frame(holt_winter_pred2)
holt_winter_pred2
#Mean Absolute Percentage Error
mape(test_1$Sales,holt_winter_pred2$fit)#MAPE=11.46

#Lets Build our model without assigning any optimum value for alpha,beta and gamma.

holt_winter_model3 <- HoltWinters(train_ts)
holt_winter_model3
summary(holt_winter_model3)


plot(holt_winter_model3)

#Lets predict the passenger count for next 12 month and transform it into data frame.
holt_winter_pred3 <- predict(holt_winter_model3,n.ahead = 6)
holt_winter_pred3 <- data.frame(holt_winter_pred3)
holt_winter_pred3
#Mean Absolute Percentage Error
mape(test_1$Sales,holt_winter_pred3$fit)#MAPE=4.36

################################################################################
#We got our least value for mape from the models we built till down.
#so this model can provide us with the least error forecasted values
#Lets apply our airlines data set into the model and forecast for next 14 quarters
################################################################################

Final_model <- HoltWinters(cocacola_sales_ts)
Final_model
summary(Final_model)

#Build a prediction model for forecasting next 14 quarters
final_pred <- predict(Final_model,n.ahead = 14)
final_pred <- data.frame(final_pred)
final_pred$fit
#Transform the fitted value into the time series
final_pred_forecast <-ts(final_pred$fit,frequency = 4,start = c(1996,3)) 

#ploting the predicted forecast with the actual data
ts.plot(cocacola_sales_ts,final_pred_forecast,log='y',lty=c(1,5))

