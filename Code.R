data<- read.csv("C:../electricity_price_data_reshaped.csv", header=FALSE)
colnames(data)<-c('Date','Price')
head(data)
tail(data)
str(data)  #The data are a data frame

## Some numbers contain commas as thousand separator, so we removed it in order to be properly converted in numeric
data[,2]<-as.numeric(gsub(",", "", data[,2]))

unique(nchar(data[,1])) # There are 4 different formats for the date
# 11 characters: "2011.01.01."
# 10 characters: "2010.07.21"
# 8 characters: "01-01-17"
head(data[,1][nchar(data[,1])==11])
head(data[,1][nchar(data[,1])==10])
head(data[,1][nchar(data[,1])==8])

## For the 11th string date: Remove the last character (".") from the string
data[,1][nchar(data[,1])==11]<-gsub('.{1}$', '', data[,1][nchar(data[,1])==11])

require(stringr)
unique(str_sub(data[,1][nchar(data[,1])==8], start= -2)) # Only the 2017 year is with this format

tail(data)  ## The dates for 2018, are in different format from the rest with 10 characters (e.g., 31.12.2018)
##### Dates before 2018
data6a<-data[str_sub(data[,1], start= -4)!='2018',]
data6b<-data[str_sub(data[,1], start= -4)=='2018',]

library(dplyr)
data64 <- data6a %>% 
mutate(Date = if_else(nchar(data6a[,1])==8, as.Date(data6a[,1], format = "%d-%m-%y"), 
                    as.Date(data6a[,1], format = "%Y.%m.%d")))

##### For 2018
data64b<-data6b %>% 
mutate(Date=as.Date(data6b[,1], format = "%d.%m.%Y"))
data<-rbind(data64,data64b)
tail(data)
month<- strftime(data[10,1],"%m")   # Month Number
str(data)
#------------------------------------------------------

any(is.na(data))
all(colSums(is.na(data)) != 0)
sum(sapply(data,is.na))
sum(sapply(data,is.null))
sum(sapply(data,is.nan))
sum(sapply(data,is.infinite))
any(duplicated.data.frame(data)==TRUE)  

which(is.na(data[,2]))
data[965,]  # So, we see that there is not value available for the "2013.03.11" date
nrow(data) # From the 3,086 obs only one is missing value
# This shouldn’t impact in the overall modelling process. 
# We could remove it, but to the fact that we have time series data we decided to replace it with the median.

# Get median Price for march 2013
library(lubridate)
med_price<-lapply(data[year(data$Date)==2013 & months(data$Date)=="March",], median, na.rm = TRUE)$Price
data[which(is.na(data[,2])),2]<-med_price
#------------------------------------------------------

## Visualising time series data (daily data)
ggplot(data, aes(x = Date, y = Price)) +
geom_line() +
scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
theme_classic() +
labs(title = "Daily Electricity Prices",
   subtitle = "From 2010 to 2018",x = "Year",y = "Price")

## This is why we decided to aggregate the data per month
summary(data$Price)
max(data[data$Price<800,2], na.rm=T)
data[data$Price>800,]  # 2010-08-16  => 1147.96

# Get median Price for August 2010
med_price2<-lapply(data[year(data$Date)==2013 & months(data$Date)=="August",], median, na.rm = TRUE)$Price
data[which(data$Price>800,),2]<-med_price2
data[data$Price<0,]
#------------------------------------------------------

# Aggregate daily data to month/year intervals
# We will aggregate the daily values in month values, so this missing value for one day will be coerced with
# the other values of that month

#### Firstly, we will use the mean price of each month

########## First way
dat654<-data
#seperate the month and year in a different column so we can aggregate.
dat654$Month <- format(as.Date(data$Date), "%Y-%m")

library(data.table)
cleaned_data<-as.data.frame(setDT(dat654)[, .(Price = mean(Price)), by = .(Month = dat654$Month)])
head(cleaned_data)

########## Second way
library(data.table)
cleaned_data43<-as.data.frame(setDT(dat654)[, .(Price = mean(Price)), by = .(Year = year(Date), Month = months(Date))])
head(cleaned_data43)

######### Third way
############### With month number!!!
cleaned_data54<-as.data.frame(setDT(dat654)[, .(Price = mean(Price)), by = .(Year = year(Date), Month = strftime(Date,"%m"))])
head(cleaned_data54)

## Write columns year, month as one (e.g., 2010-03)
## cleaned_data32<-cleaned_data
## cleaned_data32$Date <- paste(cleaned_data32$Year, cleaned_data32$Month, sep="-")
## cleaned_data32<-cleaned_data32[,c(4,3)]

sum(sapply(cleaned_data,is.na)) #0 MISSIMG VALUES
#------------------------------------------------------

#### We will also create a dataset using the max price of each month (and later see which provides better results)
library(data.table)
cleaned_data_max_price<-as.data.frame(setDT(dat654)[, .(Price = max(Price)), by = .(Month = dat654$Month)])
head(cleaned_data_max_price)

### Same, for min, 1st quantile and 3rd quantile.

#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------
#------------------------------------------------------

## So, we have created: 

##              * cleaned_data           (aggregated by mean price of each month)
##              * cleaned_data_max_price (aggregated by max price of each month)

summary(cleaned_data$Price)
data_after_2011<-cleaned_data[cleaned_data$Month>="2011-01",]
head(data_after_2011)

## Histogram
library(ggplot2)
ggplot(data_after_2011, aes(Price)) + 
geom_histogram(bins = 50, col = "lightsalmon2",fill = "lightsalmon2", alpha = 0.5) +
geom_density(alpha = 0.1) +
labs(title = "Histogram for distribution of electricity prices",
   subtitle = "From 2011 to 2018",x = "Price",y = "Frequency") +
scale_x_continuous(breaks = seq(0, 80, 10))

### Density plot
temp_data121<-data_after_2011
temp_data121$Year<-factor(str_sub(temp_data121[,1], start= 1, end=4))

ggplot(temp_data121, aes(Price, fill = Year)) +
geom_density(position = "stack") +
labs(title = "Density plot of electricity prices",
   subtitle = "From 2011 to 2018",x = "Price",y = "Density")

######################################################################
#############  Train Test Split (Approximately 70%-30%)  #############
######################################################################
# As we made the data monthly and kept only data after 2011, we updated the frequency to 12
str(data_after_2011)

### Visualising time series data

data_kept<-data_after_2011[data_after_2011$Month>="2012-01",]
price.ts <- ts(data_kept$Price, frequency = 12, start = 2012)
## price.ts <- ts(cleaned_data$Price, frequency = 12, start = c(2010, 7))
train.ts <- window(price.ts, start = c(2012,1), end = c(2016,12))
test.ts <- window(price.ts, start = c(2017,1), end = c(2018,12))

########################################################
#############  Basic Exploratory Analysis  #############
########################################################
plot(train.ts, type="o", col="blue", lty="dashed")

library(ggplot2)
library(forecast)
require(fpp2)
t1<-autoplot(train.ts) + xlab("Year") + ylab("Price") + 
ggtitle("Electricity Prices: 2012-2016")
t1
# --------------------------------------------------
# Compute logarithms of the data, and differences
lj=log(train.ts)             # compute the logartithm of the data  
t2<-autoplot(lj) + xlab("Year") + ylab("Price") + 
ggtitle("Electricity Prices: 2012-2016")

library(gridExtra)
grid.arrange(t1, t2, ncol=2)

dlj=diff(train.ts, differences=1) 
plot(dlj, type="l", col="black", main="Differenced Monthly Electricity Prices")
abline(h=0, lwd=3, col=2)
# -----------------------------------------

win.graph()
par(mfrow=c(4,2))        # set up the graphics

plot(train.ts,type="l", col='red', lwd=1,main="Time Series plot of monthly prices", ylab="Monthly prices")
hist(train.ts, nclass=15, main="Histogram of motnhly prices")

plot(lj,type="l", col='red', lwd=1,main="Log of train set", ylab="Log of monthly prices")
hist(lj, nclass=15, main="Histogram of log of monthly prices")

plot(diff(lj, differences=1),type="l", col='red', lwd=1,main="First differences of log of monthly prices")
hist(dlj, nclass=15, main="Histogram of differences of log of monthly prices")

plot(dlj,type="l", col='red', lwd=1,main="First differences of monthly prices")
hist(dlj, nclass=15, main="Histogram of differences of log of monthly prices")

# -----------------------------------------

## Augmented dickey fuller test (checkarw stationarity)
library(tseries)
adf.test(dlj)
shapiro.test(dlj)          # normality test

par(mfrow=c(1,2))         
hist(dlj, prob=TRUE, 15, main='Histogram of first differences time series')    # histogram    
lines(density(dlj),col="red")                                                  # smooth it
qqnorm(dlj,main="Normal QQplot of first differences time series")              # normal Q-Q plot  
qqline(dlj)                                                                    # add a line    
# -----------------------------------------
# ggseasonplot(train.ts, main = "Seasonal Plot: Electricity Prices", year.labels=TRUE)

ggseasonplot(train.ts, main = "Seasonal Plot: Electricity Prices", ylab="Prices") +
theme(legend.position="right", legend.box = "vertical",
    legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90", size = 0.2))


ggsubseriesplot(train.ts, main="Seasonal Subseries Plot: Electricity Prices", ylab="Prices")
# ------------------------------------------------------------
# ------------------------------------------------------------
# Create Autocorrelation and partial autocorrelation plots

pl1<-ggAcf(dlj, lag.max = 48) + labs(title = "Autocorrelation Plot (differenced price)")
pl2<-ggPacf(dlj, lag.max = 48) + labs(title = "Partial Autocorrelation Plot (differenced price)")

grid.arrange(pl1, pl2, ncol=2)

### Ljung-Box test
Box.test(dlj, lag = 48, type = "Ljung-Box")

##########################################################################
##########################################################################
##############################  Auto.arima  ##############################
##########################################################################
##########################################################################

library(forecast)
model1<-auto.arima(train.ts) 
model1

summary(model1)


checkresiduals(model1, lag=48)
# tsdisplay(residuals(model1), lag.max=48, main='Model Residuals') 

############################################ 
############  Diagnostic plots  ############  
############################################ 
# ------------------------------------------------
#####  1) Test autocorrelation of residuals  #####
# ------------------------------------------------

pl3a<-ggAcf(model1$resid, lag.max = 48) + ggtitle("ACF of residuals")
pl3b<-ggPacf(model1$resid, lag.max = 48) + ggtitle("PACF of residuals")
grid.arrange(pl3a, pl3b, ncol=2)


### Ljung-Box test
Box.test(model1$resid, lag = 48, type = "Ljung-Box")

# ---------------------------------------------------
#####  2) Test Heteroskedasticity of residuals  #####
# ---------------------------------------------------
pl4a<-ggAcf(model1$resid^2, lag.max = 48) + ggtitle("ACF of squared residuals")
pl4b<-ggPacf(model1$resid^2, lag.max = 48) + ggtitle("PACF of squared residuals")
grid.arrange(pl4a, pl4b, ncol=2)

plot(model1$resid, type="p",xlab="Year",ylab="Residuals",main="Residuals from 'auto.arima' method")

# -------------------------------------------
#####  3) Test Normality of residuals   #####
# -------------------------------------------
par(mfrow=c(1,2))
qqnorm(model1$resid, main="Normal QQplot of residuals") 
qqline(model1$resid)


hist(x = model1$resid,freq=FALSE,  main="Histogram for residuals", xlab="Residuals")
lines(x = density(x = model1$resid), col = "red",lwd=2)

#  gghistogram(model1$resid) + ggtitle("Histogram of residuals")
# ------------------------------------------------------------

###### Time Series Decomposition ###### 
## Decomposition plot
autoplot(decompose(diff(train.ts), type = "additive")) +
labs(title = 'Decomposition of Monthly Prices',x = 'Year', subtitle = 'Using first differences')

#########################################################
######################  Forecasts  ######################
#########################################################
# We will create forecasts for the next 24 months (2 years) for the 2017 and 2018 years (test dataset)
## In auto.arima the fitted values came from the training set (model1$fitted)
# So, the predictions will be in the scale of the actual electicity prices
model1<-auto.arima(train.ts) 
model1

# Forecasts 24 months ahead
forecast_auto_arima=predict(model1,24)   
forecast_auto_arima     

UL=forecast_auto_arima$pred+forecast_auto_arima$se
LL=forecast_auto_arima$pred-forecast_auto_arima$se

# plot of forecasts with 1 s.e. 
minx = min(train.ts,LL); maxx = max(train.ts,UL) 
ts.plot(train.ts, forecast_auto_arima$pred, xlim=c(2012,2018), ylim=c(minx,maxx),main="Forecasts from ARIMA(1,1,1)(1,0,0)[12]", xlab="Year", ylab="Price") 
lines(forecast_auto_arima$pred, col="red", type="o") 
lines(UL, col="blue", lty="dashed") 
lines(LL, col="blue", lty="dashed")


library(Metrics)
rmse(test.ts, forecast_auto_arima$pred)

# =====================================
######### 2nd way ########

# fit.ar <- auto.arima(train.ts)
predictions_auto_arima <- model1 %>% forecast(h=24)
predictions_auto_arima

predictions_auto_arima %>% autoplot() + 
labs(x="Year", y="Price")

## predictions_auto_arima$mean  # point forecasts

rmse(test.ts, predictions_auto_arima$mean)
# accuracy(train.ar, test.ts)

##############################################################################################
##############################################################################################
######################  Model with regressors (Time Series regression)  ######################
##############################################################################################
##############################################################################################

# =============================================================================
#################  Data Preparation for regression variables  #################
# =============================================================================

data_reg<- read.csv("C:/../Primary_Energy_Consumption_by_Source.csv", header=TRUE)

data_reg<-data_reg[,c(1,13)]
colnames(data_reg)[1]<-"Date"

data_reg$Year <- substr(data_reg$Date,1,4)
data_reg$Month<-substr(data_reg$Date,5,max(nchar(data_reg$Date)))
data_reg$Month<-trimws(data_reg$Month, which = c("both"))
data_reg$Month<-match(data_reg$Month, month.name) # convert month name to number
data_reg$Month <- as.character(data_reg$Month)
data_reg$Month[nchar(data_reg$Month)=="1"]<-paste0("0", data_reg$Month[nchar(data_reg$Month)=="1"]) # Add trailing zero
data_reg$Date_new<-paste(data_reg$Year,"-",data_reg$Month,sep="")
data_reg<-data_reg[data_reg$Year>="2012" & data_reg$Year<="2019",c(5,2)] # Keep years from 2012 to 2019
colnames(data_reg)[1]<-"Month"

full_data_reg2019<-data_reg[data_reg$Month<="2019-06",]
regressor_data_2019<-data_reg[data_reg$Month<="2019-06" & data_reg$Month>="2019-01",]
data_reg<-data_reg[data_reg$Month<="2018-12",] # Keep data until 2019-06

head(data_reg)
tail(data_reg)
regressor_data_2019


any(is.na(data_reg))
all(colSums(is.na(data_reg)) != 0)
sum(sapply(data_reg,is.na))
sum(sapply(data_reg,is.null))
sum(sapply(data_reg,is.nan))
sum(sapply(data_reg,is.infinite))
any(duplicated.data.frame(data_reg)==TRUE)  


library(plyr)
data_with_regressor<-join(data_reg, data_kept, type = "inner")
head(data_with_regressor)
tail(data_with_regressor)

summary(data_reg)

#### Correlation with Price
cor(data_with_regressor$Price,data_with_regressor$Total.Primary.Energy.Consumption)

# =============================================================================
tail(data_with_regressor)       ## Until "2018-12"!!!
regressor_data_2019  ## First 6 months of 2019!!!

library(forecast)
model_reg1<-auto.arima(train.ts, xreg = as.matrix(data_with_regressor[data_with_regressor$Month<="2016-12",2]))
model_reg1

## Same ARIMA model found by auto.arima as before!!!!

# To generate your 1-step ahead forecast we supply the future values of Total.Primary.Energy.Consumption
# for the the time period for which we generate our predictions (2017-01 until 2018-12)
test_regressor_data<-data_with_regressor[data_with_regressor$Month>="2017-01",2]
predictions_reg<-predict(model_reg1, 24, newxreg=test_regressor_data)
predictions_reg

UL_reg=predictions_reg$pred+predictions_reg$se
LL_reg=predictions_reg$pred-predictions_reg$se

# plot of forecasts with 1 s.e. 
minx_reg = min(train.ts,LL_reg); maxx_reg = max(train.ts,UL_reg) 
ts.plot(train.ts, predictions_reg$pred, xlim=c(2012,2018), ylim=c(minx_reg,maxx_reg),main="Forecasts from ARIMA(1,1,1)(1,0,0)[12] with regressor", xlab="Year", ylab="Price") 
lines(predictions_reg$pred, col="red", type="o") 
lines(UL_reg, col="blue", lty="dashed") 
lines(LL_reg, col="blue", lty="dashed")

library(Metrics)
rmse(test.ts, predictions_reg$pred)

############ Diagnostic testing ############

### Ljung-Box test
checkresiduals(model_reg1, lag=48, plots = FALSE)
checkresiduals(model_reg1, test = FALSE)  # Only the plots

# ---------------------------------------------------
#####  2) Test Heteroskedasticity of residuals  #####
# ---------------------------------------------------

## Now, that we have regressors, we can also test heteroskedasticity using this plot!!!

## Residual plots against fitted values
cbind(Fitted = model_reg1$fitted,
      Residuals=model_reg1$residuals) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point() + ggtitle("Residual plots against fitted values")


##########################################################################################
##########################################################################################
##########################  Holt-Winters Exponential Smoothing  ##########################
##########################################################################################
##########################################################################################

### We need to have data without trend (which we do as we took the differences) and non-seasonal

###### Time Series Decomposition ###### 
## Decomposition plot
autoplot(decompose(train.ts, type = "additive")) +
  labs(title = 'Decomposition of Monthly Prices',x = 'Year')

## To make forecasts, we can fit a predictive model using the HoltWinters() function. 
## For example, to fit a predictive model for the log of the monthly sales in the souvenir shop, we type:

# Estimate parameters
train.ses <- forecast(HoltWinters(train.ts, seasonal = "additive"), h=24)  # first_way
# predict(HoltWinters(train.ts),24)      # 2nd way
# train.ses <- ses(train.ts, h=24, initial="simple")
summary(train.ses)

autoplot(fc_hw) +
  autolayer(fitted(fc_hw), series="Fitted") +
  ylab("Monthly Prices") + xlab("Year") +
  theme(legend.position="bottom", legend.box = "vertical",
        legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"))

############################################ 
############  Diagnostic plots  ############  
############################################ 
# Residuals
# checkresiduals(train.ses)
# ------------------------------------------------
#####  1) Test autocorrelation of residuals  #####
# ------------------------------------------------
pl23a<-ggAcf(train.ses$residuals[!is.na(train.ses$residuals)], lag.max = 24) + ggtitle("ACF of residuals")
pl23b<-ggPacf(train.ses$residuals[!is.na(train.ses$residuals)], lag.max = 24) + ggtitle("PACF of residuals")
grid.arrange(pl23a, pl23b, ncol=2)

### Ljung-Box test
# checkresiduals(train.ses, lag=36, plot = FALSE)

par(mfrow=c(1,2))
# ---------------------------------------------------
#####  2) Test Heteroskedasticity of residuals  #####
# ---------------------------------------------------
plot(train.ses$residuals[!is.na(train.ses$residuals)], type="p",xlab="Year",ylab="Residuals",main="Residuals from 'HW' method")
# -------------------------------------------
#####  3) Test Normality of residuals   #####
# -------------------------------------------
qqnorm(train.ses$residuals[!is.na(train.ses$residuals)], main="Normal QQplot of residuals") 
qqline(train.ses$residuals[!is.na(train.ses$residuals)])
#  gghistogram(model1$resid) + ggtitle("Histogram of residuals")
# ------------------------------------------------

# Evaluate forecasts
library(Metrics)
rmse(test.ts, train.ses$mean)

###################################################################################################
###################################################################################################
#########################  Rolling Sample (Time series cross-validation)  #########################
###################################################################################################
###################################################################################################
library(forecast)
library(tseries)


## auto_arima_forecasts
auto_arima_forecasts_regressor <- function(y, h, xreg=full_data_reg2019[,2]){
  xreg32 <- xreg[1:length(y)]
  if(length(xreg) < length(y) + h)
    stop("Not enough xreg data for forecasting")
  newxreg32 <- xreg[length(y) + (1:h)]
  
  fit <- auto.arima(y, xreg=xreg32)
  return(forecast(fit, xreg = newxreg32, h = h))
}

## auto_arima_forecasts
auto_arima_forecasts <- function(y, h){
  fit <- auto.arima(y)
  return(forecast(fit, h = h))
}

## Simple Exponential Smoothing Forecasts  (ses_forecasts)
## train.ses <- ses(train.ts, h=24, initial="simple")
# ses_forecasts <- function(x,h){ses(x, h, initial="simple")}

## Holt-Winters Exponential Smoothing
ses_forecasts <- function(y, h){
  fit <- HoltWinters(y, seasonal = "additive")
  return(forecast(fit, h = h))
}

####################################################
######### Forecasts without rolling window #########
####################################################
# Fit a model to each rolling origin subset to  calculate forecast errors in a matrix for 6 step-forecast (h=6)
# Forecasts 6 months ahead

auto_arima_cv<-tsCV(price.ts, auto_arima_forecasts, h = 6)
auto_arima_reg_cv<-tsCV(price.ts, auto_arima_forecasts_regressor, h = 6)
ses_cv<-tsCV(price.ts, ses_forecasts, h = 6)

results_temp<-data.frame(Model=c("Auto Arima", "Auto Arima with regressors", "Holt-Winters Exponential Smoothing"), 
                         RMSE1=rbind(
                           sqrt(mean(auto_arima_cv^ 2, na.rm = TRUE)),
                           sqrt(mean(auto_arima_reg_cv^ 2, na.rm = TRUE)),
                           sqrt(mean(ses_cv^ 2, na.rm = TRUE))
                         ))
colnames(results_temp)[2]<-"RMSE (h=1)"
results_temp


### After some trial and error, we observed that just by excluding the first 3 months of 2012,
### the predictions become a lot a lot (RMSE smaller)
## This is because the prices were really higher during these 3 months, compares to all the other years!!
# We also saw, that regarding HolsWinter, the best results were achiever using info from January 2014.

results_final<-data.frame(Model=c("Auto Arima", "Auto Arima with regressors", "Holt-Winters Exponential Smoothing"), 
                          RMSE1=rbind(
                            rmse(ts(price.ts[4:length(price.ts)] - auto_arima_cv[3:(length(auto_arima_cv[,1])-1),1]), 
                                 ts(price.ts[4:length(price.ts)])),
                            rmse(ts(price.ts[4:length(price.ts)] - auto_arima_reg_cv[3:(length(auto_arima_reg_cv[,1])-1),1]), 
                                 ts(price.ts[4:length(price.ts)])),
                            rmse(ts(price.ts[25:length(price.ts)] - ses_cv[24:(length(ses_cv[,1])-1),1]), 
                                 ts(price.ts[25:length(price.ts)]))),
                          
                          RMSE2=rbind(
                            rmse(ts(price.ts[5:length(price.ts)] - auto_arima_cv[3:(length(auto_arima_cv[,1])-2),2]), 
                                 ts(price.ts[5:length(price.ts)])),
                            rmse(ts(price.ts[5:length(price.ts)] - auto_arima_reg_cv[3:(length(auto_arima_reg_cv[,1])-2),2]), 
                                 ts(price.ts[5:length(price.ts)])),
                            rmse(ts(price.ts[26:length(price.ts)] - ses_cv[24:(length(ses_cv[,1])-2),2]), 
                                 ts(price.ts[26:length(price.ts)]))),
                          
                          RMSE3=rbind(
                            rmse(ts(price.ts[6:length(price.ts)] - auto_arima_cv[3:(length(auto_arima_cv[,1])-3),3]), 
                                 ts(price.ts[6:length(price.ts)])),
                            rmse(ts(price.ts[6:length(price.ts)] - auto_arima_reg_cv[3:(length(auto_arima_reg_cv[,1])-3),3]), 
                                 ts(price.ts[6:length(price.ts)])),
                            rmse(ts(price.ts[27:length(price.ts)] - ses_cv[24:(length(ses_cv[,1])-3),3]), 
                                 ts(price.ts[27:length(price.ts)]))),
                          
                          RMSE4=rbind(
                            rmse(ts(price.ts[7:length(price.ts)] - auto_arima_cv[3:(length(auto_arima_cv[,1])-4),4]), 
                                 ts(price.ts[7:length(price.ts)])),
                            rmse(ts(price.ts[7:length(price.ts)] - auto_arima_reg_cv[3:(length(auto_arima_reg_cv[,1])-4),4]), 
                                 ts(price.ts[7:length(price.ts)])),
                            rmse(ts(price.ts[28:length(price.ts)] - ses_cv[24:(length(ses_cv[,1])-4),4]), 
                                 ts(price.ts[28:length(price.ts)]))),
                          
                          RMSE5=rbind(
                            rmse(ts(price.ts[8:length(price.ts)] - auto_arima_cv[3:(length(auto_arima_cv[,1])-5),5]), 
                                 ts(price.ts[8:length(price.ts)])),
                            rmse(ts(price.ts[8:length(price.ts)] - auto_arima_reg_cv[3:(length(auto_arima_reg_cv[,1])-5),5]), 
                                 ts(price.ts[8:length(price.ts)])),
                            rmse(ts(price.ts[29:length(price.ts)] - ses_cv[24:(length(ses_cv[,1])-5),5]), 
                                 ts(price.ts[29:length(price.ts)]))),
                          
                          RMSE6=rbind(
                            rmse(ts(price.ts[9:length(price.ts)] - auto_arima_cv[3:(length(auto_arima_cv[,1])-6),6]), 
                                 ts(price.ts[9:length(price.ts)])),
                            rmse(ts(price.ts[9:length(price.ts)] - auto_arima_reg_cv[3:(length(auto_arima_reg_cv[,1])-6),6]), 
                                 ts(price.ts[9:length(price.ts)])),
                            rmse(ts(price.ts[30:length(price.ts)] - ses_cv[24:(length(ses_cv[,1])-6),6]), 
                                 ts(price.ts[30:length(price.ts)])))
)

colnames(results_final)[2:7]<-c("RMSE (h=1)","RMSE (h=2)", "RMSE (h=3)", "RMSE (h=4)", "RMSE (h=5)", "RMSE (h=6)")
results_final

# Plot the RMSE values against the forecast horizon
plot_data<- data.frame(h = 1:6, 
                       RMSE_arima = as.vector(t(results_final[1,2:7])),
                       RMSE_arima_reg = as.vector(t(results_final[2,2:7])),
                       RMSE_SES = as.vector(t(results_final[3,2:7])))

win.graph()
gra1<-ggplot(plot_data, aes(x = h, y = RMSE_arima)) + geom_point() + geom_line() +
  labs(title = "RMSE for auto arima",x = "Forecast horizon (h)",y = "RMSE") +
  scale_x_continuous(breaks = seq(1, 6))
gra2<-ggplot(plot_data, aes(x = h, y = RMSE_arima_reg)) + geom_point() + geom_line() +
  labs(title = "RMSE for auto arima with regressors",x = "Forecast horizon (h)",y = "RMSE") +
  scale_x_continuous(breaks = seq(1, 6))
gra3<-ggplot(plot_data, aes(x = h, y = RMSE_SES)) + geom_point() + geom_line() +
  labs(title = "RMSE for HW",x = "Forecast horizon (h)",y = "RMSE") +
  scale_x_continuous(breaks = seq(1, 6))

grid.arrange(gra1, gra2, gra3, ncol=3)


#################################################################################################################
#################################################################################################################
#################  Predictions with the dataset converted from daily to monthly data using the: #################
#############      max, min, median, 1st and 3rd quantile price of each month (instead of mean)     #############
#################################################################################################################
#################################################################################################################

## For Max price of each month
cleaned_data_max_price<-as.data.frame(setDT(dat654)[, .(Price = max(Price)), by = .(Month = dat654$Month)])

## For Min price of each month
cleaned_data_max_price<-as.data.frame(setDT(dat654)[, .(Price = min(Price)), by = .(Month = dat654$Month)])

## For price in the 3rd quantile of each month
cleaned_data_max_price<-as.data.frame(setDT(dat654)[, .(Price = unname(quantile(Price, probs = 0.75))), by = .(Month = dat654$Month)])

## For price in the 1st quantile of each month
cleaned_data_max_price<-as.data.frame(setDT(dat654)[, .(Price = unname(quantile(Price, probs = 0.25))), by = .(Month = dat654$Month)])

## For Median price of each month
cleaned_data_max_price<-as.data.frame(setDT(dat654)[, .(Price = median(Price)), by = .(Month = dat654$Month)])



data_after_2011_max<-cleaned_data_max_price[cleaned_data_max_price$Month>="2011-01",]
data_kept_max<-data_after_2011_max[data_after_2011_max$Month>="2012-01",]
price.ts_max <- ts(data_kept_max$Price, frequency = 12, start = 2012)

####################################################
######### Forecasts without rolling window #########
####################################################

auto_arima_cv_max<-tsCV(price.ts_max, auto_arima_forecasts, h = 6)
auto_arima_reg_cv_max<-tsCV(price.ts_max, auto_arima_forecasts_regressor, h = 6)
ses_cv_max<-tsCV(price.ts_max, ses_forecasts, h = 6)


results_final_max<-data.frame(Model=c("Auto Arima", "Auto Arima with regressors", "Holt-Winters Exponential Smoothing"), 
                              RMSE1=rbind(
                                rmse(ts(price.ts_max[4:length(price.ts_max)] - auto_arima_cv_max[3:(length(auto_arima_cv_max[,1])-1),1]), 
                                     ts(price.ts_max[4:length(price.ts_max)])),
                                rmse(ts(price.ts_max[4:length(price.ts_max)] - auto_arima_reg_cv_max[3:(length(auto_arima_reg_cv_max[,1])-1),1]), 
                                     ts(price.ts_max[4:length(price.ts_max)])),
                                rmse(ts(price.ts[25:length(price.ts)] - ses_cv_max[24:(length(ses_cv_max[,1])-1),1]), 
                                     ts(price.ts[25:length(price.ts)]))),
                              
                              RMSE2=rbind(
                                rmse(ts(price.ts_max[5:length(price.ts_max)] - auto_arima_cv_max[3:(length(auto_arima_cv_max[,1])-2),2]), 
                                     ts(price.ts_max[5:length(price.ts_max)])),
                                rmse(ts(price.ts_max[5:length(price.ts_max)] - auto_arima_reg_cv_max[3:(length(auto_arima_reg_cv_max[,1])-2),2]), 
                                     ts(price.ts_max[5:length(price.ts_max)])),
                                rmse(ts(price.ts[26:length(price.ts)] - ses_cv_max[24:(length(ses_cv_max[,1])-2),2]), 
                                     ts(price.ts[26:length(price.ts)]))),
                              
                              RMSE3=rbind(
                                rmse(ts(price.ts_max[6:length(price.ts_max)] - auto_arima_cv_max[3:(length(auto_arima_cv_max[,1])-3),3]), 
                                     ts(price.ts_max[6:length(price.ts_max)])),
                                rmse(ts(price.ts_max[6:length(price.ts_max)] - auto_arima_reg_cv_max[3:(length(auto_arima_reg_cv_max[,1])-3),3]), 
                                     ts(price.ts_max[6:length(price.ts_max)])),
                                rmse(ts(price.ts[27:length(price.ts)] - ses_cv_max[24:(length(ses_cv_max[,1])-3),3]), 
                                     ts(price.ts[27:length(price.ts)]))),
                              
                              RMSE4=rbind(
                                rmse(ts(price.ts_max[7:length(price.ts_max)] - auto_arima_cv_max[3:(length(auto_arima_cv_max[,1])-4),4]), 
                                     ts(price.ts_max[7:length(price.ts_max)])),
                                rmse(ts(price.ts_max[7:length(price.ts_max)] - auto_arima_reg_cv_max[3:(length(auto_arima_reg_cv_max[,1])-4),4]), 
                                     ts(price.ts_max[7:length(price.ts_max)])),
                                rmse(ts(price.ts[28:length(price.ts)] - ses_cv_max[24:(length(ses_cv_max[,1])-4),4]), 
                                     ts(price.ts[28:length(price.ts)]))),
                              
                              RMSE5=rbind(
                                rmse(ts(price.ts_max[8:length(price.ts_max)] - auto_arima_cv_max[3:(length(auto_arima_cv_max[,1])-5),5]), 
                                     ts(price.ts_max[8:length(price.ts_max)])),
                                rmse(ts(price.ts_max[8:length(price.ts_max)] - auto_arima_reg_cv_max[3:(length(auto_arima_reg_cv_max[,1])-5),5]), 
                                     ts(price.ts_max[8:length(price.ts_max)])),
                                rmse(ts(price.ts[29:length(price.ts)] - ses_cv_max[24:(length(ses_cv_max[,1])-5),5]), 
                                     ts(price.ts[29:length(price.ts)]))),
                              
                              RMSE6=rbind(
                                rmse(ts(price.ts_max[9:length(price.ts_max)] - auto_arima_cv_max[3:(length(auto_arima_cv_max[,1])-6),6]), 
                                     ts(price.ts_max[9:length(price.ts_max)])),
                                rmse(ts(price.ts_max[9:length(price.ts_max)] - auto_arima_reg_cv_max[3:(length(auto_arima_reg_cv_max[,1])-6),6]), 
                                     ts(price.ts_max[9:length(price.ts_max)])),
                                rmse(ts(price.ts[30:length(price.ts)] - ses_cv_max[24:(length(ses_cv_max[,1])-6),6]), 
                                     ts(price.ts[30:length(price.ts)])))
)


colnames(results_final_max)[2:7]<-c("RMSE (h=1)","RMSE (h=2)", "RMSE (h=3)", "RMSE (h=4)", "RMSE (h=5)", "RMSE (h=6)")
results_final_max

###############################################################################################
###############################################################################################
###############################  Forecasts for 1st half of 2019  ##############################
###############################################################################################
###############################################################################################

results_final
results_final_max

### Keep data after 2012-03!!!
regressor_data_kept<-data_with_regressor[4:length(data_with_regressor[,2]),]
head(regressor_data_kept)


###########  Auto.arima  ###########
model_auto_arima_2019<-auto.arima(price.ts[4:length(price.ts)]) 
model_auto_arima_2019


# Forecasts 6 months ahead
fc_auto_arima_2019=predict(model_auto_arima_2019,6)   
fc_auto_arima_2019    

fc_auto_arima_2019$pred

########### Model me regressors (Time Series regression)  ###########
model_reg_2019<-auto.arima(price.ts[4:length(price.ts)], xreg = as.matrix(regressor_data_kept[,2]))
model_reg_2019

predictions_reg_2019<-predict(model_reg_2019, 6, newxreg=regressor_data_2019[,2])
predictions_reg_2019

predictions_reg_2019$pred


###########  Simple Exponential Smoothing (SES)  ###########
fc_ses_2019 <- ses_forecasts(window(price.ts, start = 2014), h=6)
fc_ses_2019

HoltWinters(window(price.ts, start = 2014), seasonal = "additive")
# -------------------------------------------------------------------------

######## Predictions for first 3 months of 2019 (using the model from SES)
pre4<-fc_ses_2019$mean[c(3:5)]

######## Prediction for 4th till 6th month of 2019 (using the model from Auto.arima)
pre5<-fc_auto_arima_2019$pred[c(1,2,6)]

######## Prediction for first 6th month of 2019 (using the model from Auto.arima with regressor)
# pre6<-predictions_reg_2019$pred[6]


month_names<-factor(1:6, levels=1:6, labels=c("January","February","March", "April","May","June"),ordered=TRUE)
Final_predictions<-data.frame(Month=month_names, Forecast=c(pre5[1:2], pre4, pre5[3]))
Final_predictions


ggplot(Final_predictions, aes(x = Month, y = Forecast, group = 1)) + geom_point() + geom_line() +
  labs(title = "Forecasts for 2019",x = "Months",y = "Price")
