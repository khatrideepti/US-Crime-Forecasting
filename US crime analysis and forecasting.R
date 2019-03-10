library(fpp)
library(fpp2)
library(TTR)
library(forecast)
crime <- read.csv("C:/Users/deept/Downloads/Data_Fall_2018_Crimes.csv")
crime_ts <- ts(crime$Data, start=c(2008,1),frequency = 12)
crime_ts1=window(crime_ts,start=c(2012,1),end=c(2018,2))
crime_ts1
plot(crime_ts)
Acf(crime_ts,lag=120)

#Plot and Inference
#	Show a time series plot.
plot(crime_ts1)
Acf(crime_ts1,lag=74)

#There is seasonality in the data
#there were Seasonal peaks in Jan from year 2008 to 2014, afterward it is not visible 
#Decreasing trend, crimes are decreased over the years 


#Central Tendency
#	What are the min, max, mean, median, 1st and 3rd Quartile values of the times series? 
 
summary(crime_ts1)

#	Show the box plot. 

boxplot(crime_ts1)

#Can you summarize your observation about the time series from the summary stats and box plot? 
#On an average there happens 1072 crimes every year
#Either 619 crimes or 1510 crimes happens less probably  

#Plot the decomposition of the time series.
decompose_crimets1=decompose(crime_ts1)
plot(decompose_crimets1)
#Is the times series seasonal?
#Yes it is seasonal 

#	Is the decomposition additive or multiplicative?
#Additive seasonal
decompose_crimets1$type

#	If seasonal, what are the values of the seasonal monthly indices? 
#Yes it is seasonal and following are Seasonal Indices
decompose_crimets1$figure


#For which month is the value of time series high and for which month is it low? 
#August Highest and Feb lowest


#Can you think of the reason behind the value being high in those months and low in those months?

#Reason is the weather, in August weather is amazing, many people rome around, many tourists comes,
#so more the people higher is the crime rate 
#In febraury it is the coldest month most of the people remain inside, they come out only for urgent work and getin quickly
#so less chances of crime 
#Therefore crime rate is highest in August and lowest in Febraury


#	Show the plot for time series adjusted for seasonality. Overlay this with the line for actual time series?
#Does seasonality have big fluctuations to the value of time series? 
seasonal_adj_crime=seasadj(decompose_crimets1)
plot(crime_ts1)
lines(seasonal_adj_crime,col='red')

#Yes for seasonality has the big fluctuationis to the value of time series  

#Naive Forecast


#Output

naive_forecast<-naive(crime_ts1,12)
plot(naive_forecast)
naive_forecast

#	Perform Residual Analysis for this technique. 
#	Do a plot of residuals. What does the plot indicate?
checkresiduals(naive_forecast)
residual_analysis<-residuals(naive_forecast)
plot(residual_analysis)

#There are highly significant values, as there is fluctuations it is not close to 0

 #	Do a Histogram plot of residuals. What does the plot indicate?

hist(residual_analysis)
#It is not normal,but skewed
  #	Do a plot of fitted values vs. residuals. What does the plot indicate?

plot(naive_forecast$fitted[2-5],naive_forecast$residuals[2-5],col=c("red","blue"))
abline(0,0,col='blue')

#There is some pattern, error has some information 

  #	Do a plot of actual values vs. residuals. What does the plot indicate?
attributes(naive_forecast)
plot(naive_forecast$x[2-5],naive_forecast$residuals[2-5])
abline(0,0,col='blue')
#there are many points above and below the mean line, some information is left in the residual
#residual is significant 

#	Do an ACF plot of the residuals? What does this plot indicate?

Acf(naive_forecast$residuals)
# there are lag every 6 months, residual has some information left, this forecasting method
#did not perform well


  #	Print the 5 measures of accuracy for this forecasting technique
accuracy(naive_forecast)

#	Forecast 
#	Time series value for next year. Show table and plot
naive_forecast

#	Summarize this forecasting technique
#	How good is the accuracy?


  #	What does it predict the value of time series will be in one year?
#710 crimes in the coming year 
naive_forecast
  
  #	Other observation
#here it is showing the 719 crimes will happen next year, but this it not good result 
#as it is same as Febraury, and above as mentioned Febraury has lowest crime 
# it is not going to be same in August or other months 




#Simple Moving Averages
#	Plot the graph for time series.
plot(crime_ts1)
#	Show the Simple Moving average of order 3 on the plot above in Red
ma3=ma(crime_ts1,order=3)
lines(ma3,col='RED')
#	Show the Simple Moving average of order 6 on the plot above in Blue
ma6=ma(crime_ts1,order=6)
lines(ma6,col='BLUE')

#	Show the Simple Moving average of order 9 on the plot above in Green
ma9=ma(crime_ts1,order=9)
lines(ma9,col='GREEN')

#	(Bonus) show the forecast of next 12 months using one of the simple average order that you feel works best for time series
ma_forecast=forecast(ma9,16)
plot(ma_forecast)
ma_forecast

#	What are your observations of the plot as the moving average order goes up?
  #As the order goes up, line becomes smooth, better chances of  good forecast

#Smoothing
#	Perform a smoothing forecast for next 12 months for the time series.

ses_crime=ses(crime_ts1,12)
plot(ses_crime)
ses_crime

summary(ses_crime)
  #	What is the value of alpha?  What does that value signify? 
      #alpha = 0.879 
      #it signifies the optimal smoothing parameter for the model to get minimum erro

  #	What is the value of initial state? 
      #Initial states:
      # l = 1176.0159 
  #	What is the value of sigma?  What does the sigma signify?
      #sigma:  151.4782
      #signies the variation around the residual mean
  
  #	Perform Residual Analysis for this technique. 
checkresiduals(ses_crime)
  #	Do a plot of residuals. What does the plot indicate?
  plot(ses_crime$residuals)
  #Flutations are there, residuals significant
  
  #	Do a Histogram plot of residuals. What does the plot indicate?
  hist(ses_crime$residuals)
    #Histogram is not normal but skewed, indicates not a good forecast
  
  #	Do a plot of fitted values vs. residuals. What does the plot indicate? 
  plot(ses_crime$fitted[2-5],ses_crime$residuals[2-5])
  abline(0,0,col='blue')
  #there are many points above mean line, thus there is a pattern which shows 
  #that error component influences forecast model , there are information still left in residual 
  
  #	Do a plot of actual values vs. residuals. What does the plot indicate?
  plot(ses_crime$x[2-5],ses_crime$residuals[2-5])
  abline(0,0,col='blue')
  
  #It shows a pattern, also there is leverage (many points at one place)
  #information is still in the residual, which can be extracted with better model
  #Therefore we can say error component influences the forecast component
  
  #	Do an ACF plot of the residuals? What does this plot indicate?
  Acf(ses_crime$residuals)
  #showing the pattern still exists, there is lag every 6 months 
  
  #	Print the 5 measures of accuracy for this forecasting technique
  accuracy(ses_crime)
  
  #	Forecast 
  #	Time series value for next year. Show table and plot
  ses_crime
  plot(ses_crime)
  
  #	Summarize this forecasting technique
    #This is not efficient as mentioned above by residuals analysis, there could be better 
    #forecasting model than this  
  
  #	How good is the accuracy?
    
  #	What does it predict the value of time series will be in one year?
    # for the next number of crimes will be  710.2906
    
  #	Other observation
    #it is better than naive bayes, as accuracy is higher 
  
  #Holt-Winters 
  #.	Perform Holt-Winters forecast for next 12 months for the time series.
  ?HoltWinters
  hw_crime=HoltWinters(crime_ts1)
  hw_crime_forecast=forecast(hw_crime,h=12)
  plot(hw_crime_forecast)
  hw_crime
  #What is the value of alpha?  What does that value signify? 
  #Value of  alpha: 0.0406707, signifies level reacts to backdated observations 
  #(in case if it close to 1, we say more weights are given to recent observations but 
  #it's not the case here)
   
  #What is the value of beta? What does that value signify?
  #Value of beta : 0.08400935, signifies trend depends on previous value 
  
  #What is the value of gamma? What does that value signify?
  #Gamma is 0.05016539, signifies seasonality repeats according to cycle at regular time period
  
  #What is the value of initial states for the level, trend and seasonality? What do these values signify? 
  #a is level, b is trend, si to s12 is seasonality for 12 months respectively
  hw_crime$coefficients
  
  #What is the value of sigma?  What does the sigma signify?
  
  sd(complete.cases(hw_crime_forecast$residuals))
  # Value of sigma =0.3711156, signifies value of standard deviation
  
    #.	Perform Residual Analysis for this technique. 
  
  checkresiduals(hw_crime_forecast)
  
  #Do a plot of residuals. What does the plot indicate?
  plot(hw_crime_forecast$residuals)
  #for year 2012 it is ok, but for rest it still has values but it looks random
  summary(hw_crime_forecast$residuals)
   #Do a Histogram plot of residuals. What does the plot indicate?
  hist(hw_crime_forecast$residuals)
  #it is skewed, but as compared to other methods this is better 
  
  #Do a plot of fitted values vs. residuals. What does the plot indicate? 
  plot(hw_crime_forecast$fitted[2-5],hw_crime_forecast$residuals[2-5])
  abline(0,0,col='blue')
  #Variance is still there, it shows 5 outliers, some leverage ,
  #residual still has some significnce, possible there exists some method which can perform better
  
  #Do a plot of actual values vs. residuals. What does the plot indicate?
  plot(hw_crime_forecast$x[2-5],hw_crime_forecast$residuals[2-5])
  abline(0,0,col='blue')
  #Variance is still there, it shows 5 outliers, some leverage ,
  #residual still has some significnce, possible there exists some method which can perform better
  
  #Do an ACF plot of the residuals? What does this plot indicate?
  Acf(hw_crime_forecast$residuals,lag=74)
  #shows there is no autocorelation, which shows it is good method of forecast
  
  #Print the 5 measures of accuracy for this forecasting technique
  accuracy(hw_crime_forecast)
  
  #	Forecast 
  
  #	Time series value for next year. Show table and plot
  plot(hw_crime_forecast)
  hw_crime_forecast
  #Following is the forecast for next 1 year 
  #Point Forecast    
  #Mar 2018       702.4429
  #Apr 2018       751.9465
  #May 2018       827.3633
  #Jun 2018       920.5189
  #Jul 2018      1011.6039
  #Aug 2018      1084.9511
  #Sep 2018       902.0591
  #Oct 2018       915.7697
  #Nov 2018       772.6761
  #Dec 2018       793.0333
  #Jan 2019       640.6465
  #Feb 2019       449.2181
  
  #	Summarize this forecasting technique
  #Holts Winter Forecast: It is built on simple smoothing forecast concept, here it has been adjusted for trend and seasonality both, which is done 2 scientists Holts and Winter. It comprises of forecast equation and 3 smoothing equations.
 # Smoothing equations involves calculation of level, trend and seasonality based on respective smoothing constant, which is calculated such that SSE should be minimum.
#  Once we have level, trend and seasonality, forecast model is built using forecast equation.
 # Following are calculations
#  - Forecast equation: Y^t+p = (Lt + p*Tt)*St-s+p
 # - Level equation: Lt = @Yt/St-s + (1-@)(Lt-1 + T t-1)
  #- Trend Equation: Tt = &(Lt-Lt-1) + (1-&)Tt-1
#  - Seasonal Equation: !(Yt/Lt)+(1-!)St-s Where Lt = new smoothed Value @ = smoothing constant for level Yt = Actual forecast at time t & = Smoothing constant for trend Tt = trend estimate p = period for which to calculate forecast on Y^t+p = Forecast for p period into the future s = length of seasonality ! = Seasonality constant St = seasonality estimate.
 # It has advantage of simple smoothing and we have considered seasonality in the model, so it gives more accurate more forecast model.
  
  #	How good is the accuracy?
  
  
  
  # What does it predict the value of time series will be in one year?
  #Following is the forecast for next 1 year 
  #Point Forecast    
  #Mar 2018       702.4429
  #Apr 2018       751.9465
  #May 2018       827.3633
  #Jun 2018       920.5189
  #Jul 2018      1011.6039
  #Aug 2018      1084.9511
  #Sep 2018       902.0591
  #Oct 2018       915.7697
  #Nov 2018       772.6761
  #Dec 2018       793.0333
  #Jan 2019       640.6465
  #Feb 2019       449.2181
  
  # Other observation
  #This is the better model than Naive, Simple smoothing
  #Because of 2 reasons
  #1. Acf plot of residuals show residuals is insignificant
  #2. When we look at values of forecast for next 12 months, it shows high in August and low in feb
  #   which is matching our data
  

  #	Is Time Series data stationary? How did you verify? Please post the output from one of the test. 
  adf.test(crime_ts1,k=0)
  #p value is .01<.05
  # ADF test says differences is required if p-value is > 0.05
  #It says it is stationary, trend stationary , no difference for trend is required but other method shows difference is required becuase of seasonality 
  
  
  kpss.test(crime_ts1)
  # p value is .01 <.05
  # Kipps test says differences is required if p-value is < 0.05
  #Therefore  we can says its non-stationary and requires difference
  
  #	How many differences are needed to make it stationary?   
  nsdiffs(crime_ts1)
  ndiffs(crime_ts1)
  #1 difference for seasonality and one diff for trend, but actualy after 1 seasonal diff ts became stationary
  
  crime_ts1_after_diff=diff(crime_ts1,12)


     adf.test(crime_ts1_after_diff,k=0)
  #p value is .01<.05
  # ADF test says differences is required if p-value is > 0.05
     #stationary
  
   kpss.test(crime_ts1_after_diff)
  #p value is .1>.05
  # Kipps test says differences is required if p-value is < 0.05
  #There we can says its stationary now
  nsdiffs(crime_ts1_after_diff)
  ndiffs(crime_ts1_after_diff)
  #we don't need second difference 
  #Now after 1 seasonal difference we have stationary time series 
  
    #	Is Seasonality component needed?
      #Yes
    #	Plot the Time Series chart of the differenced series. 
  
plot(crime_ts1_after_diff)
  #	Plot the ACF and PACF plot of the differenced series. 
Acf(crime_ts1_after_diff,lag=74)
 #q = 0,1,2,3,4,5 and Q=0,1,2 and d=0
Pacf(crime_ts1_after_diff,lag=74)
 #p = 0,1,2,3,4,5 and P=0,1,2 and D=1

tsdisplay(crime_ts1_after_diff,lag.max=40)
auto.arima(crime_ts1)
auto.arima(crime_ts1,trace=TRUE, stepwise = FALSE)



  #	Based on the ACF and PACF, which are the possible ARIMA model possible? 
tsdisplay(crime_ts1_after_diff)

#p = 0,1  q=0,1,2,3
#ARIMA(0,1,0) , ARIMA(0,1,1), ARIMA(0,1,2), ARIMA(0,1,3) ,ARIMA(1,1,0) , ARIMA(1,1,1),ARIMA(1,1,2), ARIMA(1,1,3)

fit1=Arima(crime_ts1, order=c(0,0,0), seasonal=c(0,1,1))
fit2=Arima(crime_ts1, order=c(0,0,1), seasonal=c(0,1,1))
fit3=Arima(crime_ts1, order=c(1,0,2), seasonal=c(0,1,1))
fit4=Arima(crime_ts1, order=c(1,0,2), seasonal=c(1,1,1))
#ARIMA(0,0,0)(0,1,1)[12]
#AIC=772.91   AICc=773.12   BIC=777.17
#ARIMA(0,0,1)(0,1,1)[12] 
#AIC=758.98   AICc=759.39   BIC=765.36
#ARIMA(1,0,2)(0,1,1)[12] 
#AIC=737.53   AICc=738.6   BIC=748.17
#Arima(crime_ts1, order=c(1,0,2), seasonal=c(1,1,1))
#IC=739.38   AICc=740.91   BIC=752.14





    #	Show the AIC, BIC and Sigma^2 for the possible models?
#all possible model and there AIC are as follows 
auto.arima(crime_ts1)
auto.arima(crime_ts1,trace=TRUE, stepwise = FALSE)

#We have to choose between ARIMA(0,0,4)(0,1,1)[12] and ARIMA(0,0,0)(0,1,1)[12]

fit_Arima <- Arima(crime_ts1, order=c(0,0,4), seasonal=c(0,1,1), include.drift = TRUE)
fit_Arima
tsdisplay(fit_Arima$residuals)

fit1_Arima <- Arima(crime_ts1, order=c(0,0,0), seasonal=c(0,1,1), include.drift = TRUE)
fit1_Arima

fit_res <- residuals(fit_Arima)
fit1_res <- residuals(fit1_Arima)
checkresiduals(fit_Arima)
checkresiduals(fit1_Arima)
Box.test(fit_res, lag=16, fitdf=4, type="Ljung")
Box.test(fit1_res, lag=16, fitdf=4, type="Ljung")

    #	Based on the above AIC, BIC and Sigma^2 values, which model will you select?
#According to principle of Parsimony, I decided to choose simple model i.e. ARIMA(0,0,0)(0,1,1)[12] because 
# AIC are close, but when i did residual analysis and Box test it is not a good model
#So i will choose 
    
    #	What is the final formula for ARIMA with the coefficients? 
#Arima(crime_ts1, order=c(0,0,4), seasonal=c(0,1,1), include.drift = TRUE)
    #	Perform Residual Analysis for this technique.
checkresiduals(fit_Arima)

  #	Do a plot of residuals. What does the plot indicate?
plot(fit_Arima$residuals)
    #	Do a Histogram plot of residuals. What does the plot indicate?
hist(fit_Arima$residuals)
    #	Do a plot of fitted values vs. residuals. What does the plot indicate? 
plot(fit_Arima$fitted[2-5],fit_Arima$residuals[2-5])
abline(0,0,col='blue')
    #	Do a plot of actual values vs. residuals. What does the plot indicate?
plot(fit_Arima$x[2-5],fit_Arima$residuals[2-5])
abline(0,0,col='blue')
    #	Do an ACF plot of the residuals? What does this plot indicate?
Acf(fit_Arima$residuals)
    #	Print the 5 measures of accuracy for this forecasting technique.
arima_forecast=forecast(fit_Arima,12)
accuracy(arima_forecast)
  #	Forecast 
arima_forecast=forecast(fit_Arima,12)
  #	Next one year. Show table and plot
arima_forecast
plot(arima_forecast)
  #	Next two years. Show table and plot

arima_forecast_2yr=forecast(fit_Arima,24)
arima_forecast_2yr
plot(arima_forecast_2yr)
  #	Summarize this forecasting technique
  #	How good is the accuracy?
accuracy(arima_forecast)

    # What does it predict time series will be in one year and next two years?
    
accuracy(naive_forecast)
accuracy(ses_crime)
accuracy(hw_crime_forecast)
accuracy(arima_forecast)

