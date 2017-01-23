# Exchange-Rate-Fluctuations

Analysis of the seasonality and trends of the US Dollar, Euro, and Turkish Lira over the last 3 years, and evaluation of the ideal time to invest the Dollar in the European and Turkish markets to achieve maximum ROI


###Data
Loading data & exploration
The first thing that we do is analyse the time series data by reading it into R, and plotting the data over time.  
```{r}
exch <- read.csv("C:/Users/Ushnik/Desktop/exchange.csv", header=TRUE, stringsAsFactors=FALSE)
View(exch)
names(exch)
str(exch)
head(exch)
summary(exch)
Date<-as.Date(exch$Date, "%m/%d/%Y")
exch$Date<-Date
str(exch)
View(exch)
```

The data set has 37 monthly observations of Exchange Rates of the Eur to the Dollar andd the Lira to the Dollar.

### Time Series Objects
We then store both the exchage rates in two time series objects in R. To store the data in a time series object, we use the `ts()` function in R.  

```{r}
ts.eur.usd<-ts(exch$EUR.to.USD.Rate,  start=c(2013,11), frequency = 12) 
ts.eur.usd
ts.tur.usd<-ts(exch$TUR.to.USD.Rate, start=c(2013,11), frequency = 12) 
ts.tur.usd
```

### Time Series Plot
We then plot the exchange prices of Euro and Lira from 2013 to 2016.

```{r}
plot.ts(ts.eur.usd, ylab= "Exchange Rates of Eur to USD")
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/1.PNG)

```{r}
plot.ts(ts.tur.usd, ylab= "Exchange Rates of Tur to USD")
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/2.PNG)

### Consolidated Graphical representation of Euro to USD and Lira to USD 

We compare the graphical trends of both the cuurrencies againt the Dollar in a consolidted graph.

```{r}

par(mar=c(2,8,4,8)+0.1)
plot(ts.eur.usd, col="blue", ylim=c(1,1.5), axes=F, ylab="")
box()
axis(2, col="blue")
mtext("Eur to USD", side=2, line=3)
par(new=T)
plot(ts.tur.usd, col="red", ylim=c(0.2,0.5), axes=F, ylab="")
axis(4, col="red")
mtext("Turkish Lira to USD", side=4, line=3)
axis(1, xlim=c(2013, 2016))
mtext("Eur to USD vs Turkish Lira to USD",  line=3)

```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/3.PNG)

###Decomposing, Holt Smoothening and forcasting for Euro

To estimate the trend, seasonal and irregular components of Euro over the given period, we decompose the time series obtained.We then go onto forecast rates for 2017 `(h=12)` which returns values within confidence levels of 80% and 95% respectively for the exchange rate forecasts of the Euro to the Dollar.

```{r}
library(forecast)
ts.eur.usd.d <- decompose(ts.eur.usd)
plot(ts.eur.usd.d)
```

![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/4.PNG)

```{r}
ts.eur.holt <- HoltWinters(ts.eur.usd, gamma=FALSE)
plot(ts.eur.holt, ylab="Exchange rates of Eur to USD", xlab="Year")
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/5.PNG)

```{r}
ts.eur.forecasts <- forecast.HoltWinters(ts.eur.holt, h=12)  
plot.forecast(ts.eur.forecasts, ylab="Exchange rates of Eur to USD", xlab="Year")
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/6.PNG)

```{r}
ts.eur.holt <- HoltWinters(ts.eur.usd, gamma=TRUE)
ts.eur.forecasts <- forecast.HoltWinters(ts.eur.holt, h=12)
ts.eur.forecasts
plot.forecast(ts.eur.forecasts, ylab="Exchange rates of Eur to USD", xlab="Year")
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/7.PNG)

###Decomposing, Holt Smoothening and forcasting for Lira

To estimate the trend, seasonal and irregular components of Lira over the given period, we decompose the time series obtained. We then go onto forecast rates for 2017 `(h=12)` which returns values within confidence levels of 80% and 95% respectively for the exchange rate forecasts of the Lira to the Dollar.

```{r}

ts.tur.usd.d <- decompose(ts.tur.usd)
plot(ts.tur.usd.d)
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/8.PNG)

```{r}
ts.tur.holt <- HoltWinters(ts.tur.usd, gamma=FALSE)
plot(ts.tur.holt, ylab="Exchange rates of TurKish Lira to USD", xlab="Year")
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/9.PNG)

```{r}
ts.tur.forecasts <- forecast.HoltWinters(ts.tur.holt, h=12)  
plot.forecast(ts.tur.forecasts, ylab="Exchange rates of TurKish Lira to USD", xlab="Year")
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/10.PNG)

```{r}
ts.tur.holt <- HoltWinters(ts.tur.usd, gamma=TRUE)
ts.tur.forecasts <- forecast.HoltWinters(ts.tur.holt, h=12)
ts.tur.forecasts
plot.forecast(ts.tur.forecasts, ylab="Exchange rates of TurKish Lira to USD", xlab="Year")
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/11.PNG)

###Forecast for Eur to USD exchange rates using Arima Model

We use the Arima model to further consolidate our Forecast Models achieved as above. We use the `auto.arima` function to predict the (p,d,q) variables and use the returned values for an optimum forecast model.

```{r}
auto.arima(ts.eur.usd)
eur.arima<-arima(ts.eur.usd, c(0,1,0))   
eur.arima.forecasts <- forecast.Arima(eur.arima, h=12)
eur.arima.forecasts
plot(eur.arima.forecasts)
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/12.PNG)

The ?forecast errors? are calculated as the observed values minus predicted values, for each time point. We can only calculate the forecast errors for the time period covered by our original time series, which is 2013 to 2016 for the exchange rate data. As mentioned above, one measure of the accuracy of the predictive model is the sum-of-squared-errors (SSE) for the in-sample forecast errors.

The in-sample forecast errors are stored in the named element ?residuals? of the list variable returned by `forecast.HoltWinters()`. If the predictive model cannot be improved upon, there should be no correlations between forecast errors for successive predictions. In other words, if there are correlations between forecast errors for successive predictions, it is likely that the simple exponential smoothing forecasts could be improved upon by another forecasting technique.

To figure out whether this is the case, we can obtain a correlogram of the in-sample forecast errors for lags 1-20. We can calculate a correlogram of the forecast errors using the `acf()` function in R. To specify the maximum lag that we want to look at, we use the `lag.max` parameter in `acf()`.

For example, to calculate a correlogram of the forecast errors for the exchange rate data for lags 1-20, we type:

```{r}
acf(eur.arima.forecasts$residuals, lag.max=20)
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/13.PNG)

We can see from the correlogram that the autocorrelation at lag 13 is just touching the significance bounds. To test whether there is significant evidence for non-zero correlations at lags 1-20, we can carry out a Ljung-Box test. This can be done in R using the `Box.test()` function. The maximum lag that we want to look at is specified using the ?lag? parameter in the `Box.test()` function. For example, to test whether there are non-zero autocorrelations at lags 1-20, for the in-sample forecast errors for exchange rate data, we type:

```{r}
Box.test(eur.arima.forecasts$residuals, lag=20, type="Ljung-Box")
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/14.PNG)

Here the Ljung-Box test statistic is 21.9, and the p-value is 0.3, so there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.

To be sure that the predictive model cannot be improved upon, it is also a good idea to check whether the forecast errors are normally distributed with mean zero and constant variance. To check whether the forecast errors have constant variance, we can make a time plot of the in-sample forecast errors:

```{r}
plot.ts(eur.arima.forecasts$residuals)            
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/15.PNG)

The plot shows that the in-sample forecast errors do not seem to have a constant variance over time, although the size of the fluctuations in the start of the time series may be slightly less than that at later dates.

We therefore difference the given forecast once to obtain constant variance over time. This is also supported by the `auto.arima` function predicting a integrated variable "d" as 1.

```{r}
eur.usd.diff <- diff(eur.arima.forecasts$residuals, differences = 1)
plot.ts(eur.usd.diff)
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/16.PNG)

To check whether the forecast errors are normally distributed with mean zero, we can plot a histogram of the forecast errors, with an overlaid normal curve that has mean zero and the same standard deviation as the distribution of forecast errors. To do this, we can define an R function ?plotForecastErrors()?, below:

```{r}

PlotForecastErrors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors)-mysd*5
  mymax <- max(forecasterrors)+mysd*5
  mynorm <- rnorm(10000,mean=0,sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2<mymin)
  {
    mymin<-mymin2
  }
  if (mymax2>mymax)
  {
    mymax<-mymax2
  }
  mybins <- seq(mymin,mymax,mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

PlotForecastErrors(eur.arima.forecasts$residuals)
mean(eur.arima.forecasts$residuals)
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/17.PNG)

The plot shows that the distribution of forecast errors is roughly centred on zero (supported by the `mean` function), and is more or less normally distributed and so it is plausible that the forecast errors are normally distributed with mean zero.

The Ljung-Box test showed that there is little evidence of non-zero autocorrelations in the in-sample forecast errors, and the distribution of forecast errors seems to be normally distributed with mean zero. This suggests that the simple exponential smoothing method provides an adequate predictive model for the exchange rates, which probably cannot be improved upon. Furthermore, the assumptions that the 80% and 95% predictions intervals were based upon (that there are no autocorrelations in the forecast errors, and the forecast errors are normally distributed with mean zero and constant variance) are probably valid.

###Forecast for Turkish Lira to USD exchange rates using Arima Model

We carry out the same procedure to achieve forecast values using the Arima model and check for forecast errors.

```{r}
auto.arima(ts.tur.usd)
tur.arima<-arima(ts.tur.usd, c(0,1,0))   
tur.arima.forecasts <- forecast.Arima(tur.arima, h=12)
tur.arima.forecasts
plot(tur.arima.forecasts)
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/18.PNG)

```{r}
acf(tur.arima.forecasts$residuals, lag.max=20)
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/19.PNG)

```{r}
Box.test(tur.arima.forecasts$residuals, lag=20, type="Ljung-Box")

plot.ts(tur.arima.forecasts$residuals)            
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/20.PNG)

```{r}
tur.usd.diff <- diff(tur.arima.forecasts$residuals, differences = 1)
plot.ts(tur.usd.diff)
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/21.PNG)

```{r}
PlotForecastErrors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors)-mysd*5
  mymax <- max(forecasterrors)+mysd*5
  mynorm <- rnorm(10000,mean=0,sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2<mymin)
  {
    mymin<-mymin2
  }
  if (mymax2>mymax)
  {
    mymax<-mymax2
  }
  mybins <- seq(mymin,mymax,mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

PlotForecastErrors(tur.arima.forecasts$residuals)
```
![](https://github.com/ushnik/Exchange-Rate-Fluctuations/blob/master/Exchange%20Rate_files/22.PNG)

```{r}
mean(tur.arima.forecasts$residuals)
```

### Conclusion:

1. Seasonality and trends
Euro to USD:
In the Decomposition of time series graph of Euros to USD, we observe that there has been a decreasing trend from mid 2014 to mid 2015 and has been roughly constant thereafter.
There is also a seasonal effect on the currency exchanges: We see that at the beginning of the year, the exchange rates are low, they increase during March and are highest in June. Again, we see  a drop in the rates towards the year end.

LIRA to USD:
In the Decomposition of time series graph of Lira to USD, we observe that there has been a decreasing trend from mid 2014 and has been gradually decreasing since then. 
We can also see a seasonal effect represented by exchange rate highs in June, a sudden drop in September-October and a gradual increase towards the end of the year.

2. The exchange rates are not the same but follow a similar trend. They have been decreasing from 2014. The seasonal effect for both the Lira and Euro show its highest value in June with a decrease towards the end of the year and an increase during the beginning the next year.

3.The company should invest in other markets when the exchange rates are it the lowest.  
As per the summary of our forecasts, we should invest in European markets in the month of March, 2017 when, at an 80% confidence level (range of values from 0.8845 to 1.111 Euros to the Dollar), point forecast value is projected at 0.9978 Euros towards the Dollar. 
We should invest in Turkey during October as, at a confidence level of 80%, (range of values from 0.1757 to 0.2902 Lira to Dollar) the point forecast value is projected at 0.2329 Lira towards the Dollar.
