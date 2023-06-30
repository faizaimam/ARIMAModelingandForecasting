#Import the time series covid data
covid <-read.csv("owid-covid-data.csv")
head(covid)
covid[["date"]] <- as.Date(covid$date)

#Separate the data country-wise
covid_bangladesh <- covid[grep("Bangladesh", covid$location), ]
covid_india <- covid[grep("India", covid$location), ]
covid_pakistan <- covid[grep("Pakistan", covid$location), ]

#Keep only date and total_cases, new_cases
library(dplyr)
covid_bd <- select(covid_bangladesh, date, new_cases)
covid_ind <- select(covid_india, date, new_cases)
covid_pak <- select(covid_pakistan, date, new_cases)

#Replace NA by zero
covid_bd[is.na(covid_bd)] <- 0
covid_ind[is.na(covid_ind)] <- 0
covid_pak[is.na(covid_pak)] <- 0


#Transform the data into time series
library(zoo)

bangladesh <- zoo(covid_bd[-1], seq(from=as.Date("2020-03-08"), to = as.Date("2022-06-29"), by=1))
india <- zoo(covid_ind[-1], seq(from=as.Date("2020-01-30"), to = as.Date("2022-06-29"), by=1))
pakistan <- zoo(covid_pak[-1], seq(from=as.Date("2020-02-25"), to = as.Date("2022-06-29"), by=1))

#Plotting data country-wise
library(ggplot2)

#Bangladesh
bd_plot <- ggplot(covid_bd, aes(x=date, y= new_cases)) +
  geom_line( color="steelblue") + 
  geom_point(size = .25, color = "grey") +
  xlab("Time") +
  ylab("No. of New Cases (Bangladesh)")+
  scale_x_date(date_labels = "%b %Y")
bd_plot

#India
options(scipen=999)
format(summary(covid_ind), big.mark = ",")
ind_plot <- ggplot(covid_ind, aes(x=date, y= new_cases)) +
  geom_line( color="red") + 
  geom_point(size = .25, color = "grey") +
  xlab("Time") +
  ylab("No. of New Cases (India)")+
  scale_x_date(date_labels = "%b %Y")
ind_plot
max(india$new_cases)

#Pakistan
pak_plot <- ggplot(covid_pak, aes(x=date, y= new_cases)) +
  geom_line( color="green") + 
  geom_point(size = .25, color = "grey") +
  xlab("Time") +
  ylab("No. of New Cases (Pakistan)") +
  scale_x_date(date_labels = "%b %Y")
pak_plot

#Comparison Plot
library(ggpubr)
ggarrange(bd_plot, ind_plot, pak_plot, ncol = 1 , nrow = 3)

#packages needed for analysis
library(forecast)
library(tseries)


#Modeling
#Bangladesh
#Model Selection
plot(bangladesh)
adf.test(bangladesh$new_cases)
acf(bangladesh$new_cases)

newCases_bd <- diff(bangladesh, 1)
newCasesbd1
plot(newCases_bd)
adf.test(newCases_bd)
acf(newCases_bd)
#2, 5, 7, 10 (ma)
pacf(newCases_bd)
#2, 5, 7 (ar)

#all possible models upto lag 10
bd1 <- arima(bangladesh, order=c(2,1,2))
bd2 <- arima(bangladesh, order=c(5,1,2))
bd3 <- arima(bangladesh, order=c(7,1,2))
bd4 <- arima(bangladesh, order=c(2,1,5))
bd5 <- arima(bangladesh, order=c(5,1,5))
bd6 <- arima(bangladesh, order=c(7,1,5)) #final model
bd7 <- arima(bangladesh, order=c(2,1,7))
bd8 <- arima(bangladesh, order=c(5,1,7))
bd9 <- arima(bangladesh, order=c(7,1,7)) 

#bd10 <- arima(bangladesh, order=c(2,1,10))
#bd11 <- arima(bangladesh, order=c(5,1,10))
#bd12 <- arima(bangladesh, order=c(7,1,10))


#checking AIC
aic_bd <- c(bd1$aic, bd2$aic, bd3$aic, bd4$aic, bd5$aic, bd6$aic, bd7$aic, bd8$aic, bd9$aic) # bd10$aic, bd11$aic, bd12$aic)
min(aic_bd)
min(abs(ll_bd))
ll_bd <- c(bd1$loglik, bd2$loglik, bd3$loglik, bd4$loglik, bd5$loglik, bd6$loglik, bd7$loglik, bd8$loglik, bd9$loglik)

#diagnostic test
tsdiag(bd6)


#India
#Model Selection
plot(india)
adf.test(india)
acf(india)
newCases_ind <- diff(india, 1)
adf.test(newCases_ind)
acf(newCases_ind)
#1,3,6,7,8 (ma) 4
pacf(newCases_ind)
#1, 3, 6,7,9(ar) 10
ind1 <- arima(india, order = c(1, 1, 1))
ind2 <- arima(india, order = c(3, 1, 1))
ind3 <- arima(india, order = c(6, 1, 1))
ind4 <- arima(india, order = c(7, 1, 1))
ind5 <- arima(india, order = c(9, 1, 1))

ind6 <- arima(india, order = c(1, 1, 3))
ind7 <- arima(india, order = c(3, 1, 3))
ind8 <- arima(india, order = c(6, 1, 3))
ind9 <- arima(india, order = c(7, 1, 3))
ind10 <- arima(india, order = c(9, 1, 3))

ind11 <- arima(india, order = c(1, 1, 6))
ind12 <- arima(india, order = c(3, 1, 6))
ind13 <- arima(india, order = c(6, 1, 6))
ind14 <- arima(india, order = c(7, 1, 6))
ind15 <- arima(india, order = c(9, 1, 6))

ind16 <- arima(india, order = c(1, 1, 7))
ind17 <- arima(india, order = c(3, 1, 7))
ind18 <- arima(india, order = c(6, 1, 7))
ind19 <- arima(india, order = c(7, 1, 7))
ind20 <- arima(india, order = c(9, 1, 7))

ind21 <- arima(india, order = c(1, 1, 8))
ind22 <- arima(india, order = c(3, 1, 8))
ind23 <- arima(india, order = c(6, 1, 8))
ind24 <- arima(india, order = c(7, 1, 8)) #final model
ind25 <- arima(india, order = c(9, 1, 8))

aic_ind<- c(ind1$aic, ind2$aic, ind3$aic, ind4$aic, ind5$aic, ind6$aic, ind7$aic, ind8$aic, ind9$aic, ind10$aic, ind11$aic, ind12$aic, ind13$aic, ind14$aic, ind15$aic, ind16$aic, ind17$aic, ind18$aic, ind19$aic, ind20$aic, ind21$aic, ind22$aic, ind23$aic, ind24$aic, ind25$aic)
ll_ind<- c(ind1$loglik, ind2$loglik, ind3$loglik, ind4$loglik, ind5$loglik, ind6$loglik, ind7$loglik, ind8$loglik, ind9$loglik, ind10$loglik, ind11$loglik, ind12$loglik, ind13$loglik, ind14$loglik, ind15$loglik, ind16$loglik, ind17$loglik, ind18$loglik, ind19$loglik, ind20$loglik, ind21$loglik, ind22$loglik, ind23$loglik, ind24$loglik, ind25$loglik)
length(ll_ind)
length(aic_ind)
min(aic_ind)
tsdiag(ind15)


#Pakistan
#Model Selection
adf.test(pakistan)
acf(pakistan)
newCases_pak <- diff(pakistan, 1)
adf.test(newCases_pak)
acf(newCases_pak)
#1,2, 3 (ma)
pacf(newCases_pak)
#1, 2, 3,4,8, 10 (ar)

pak1 <- arima(pakistan, order = c(1, 1,1))
pak2 <- arima(pakistan, order = c(2, 1,1))
pak3 <- arima(pakistan, order = c(3, 1,1))
pak4 <- arima(pakistan, order = c(4, 1,1))
pak5 <- arima(pakistan, order = c(8, 1,1))

pak6 <- arima(pakistan, order = c(1, 1,2))
pak7 <- arima(pakistan, order = c(2, 1,2))
pak8 <- arima(pakistan, order = c(3, 1,2))
pak9 <- arima(pakistan, order = c(4, 1,2))
pak10 <- arima(pakistan, order = c(8, 1,2))

pak11 <- arima(pakistan, order = c(1, 1,3))
pak12 <- arima(pakistan, order = c(2, 1,3))
pak13 <- arima(pakistan, order = c(3, 1,3)) #final model
pak14 <- arima(pakistan, order = c(4, 1,3))
pak15 <- arima(pakistan, order = c(8, 1,3))

#checking aic
aic_pak <- c(pak1$aic, pak2$aic, pak3$aic, pak4$aic, pak5$aic, pak6$aic, pak7$aic, pak8$aic, pak9$aic, pak10$aic, pak11$aic, pak12$aic, pak13$aic, pak14$aic, pak15$aic)
length(aic_pak)

ll_pak <- c(pak1$loglik, pak2$loglik, pak3$loglik, pak4$loglik, pak5$loglik, pak6$loglik, pak7$loglik, pak8$loglik, pak9$loglik, pak10$loglik, pak11$loglik, pak12$loglik, pak13$loglik, pak14$loglik, pak15$loglik) 
min(aic_pak)
tsdiag(pak13)


#Forecasting: 
bangladesh_forecast <- forecast(bd6, 365)
dates <- seq(as.Date("2022-06-30"), as.Date("2023-06-29"), by="days")
bd <- cbind(dates,as.data.frame(bangladesh_forecast))
plot(bangladesh_forecast)

india_forecast <- forecast(ind24, 365)
ind <- cbind(dates,as.data.frame(india_forecast))
plot(india_forecast)

pakistan_forecast <- forecast(pak13, 365)
pak <- cbind(dates,as.data.frame(pakistan_forecast))
plot(pakistan_forecast)


