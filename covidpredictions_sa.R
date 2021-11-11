library(tidyverse)
library(zoo)
library(ggthemes)
library(lubridate)
library(xts)
library(forecast)
library(rtweet)

accessed_date<-format(as.POSIXlt(Sys.time(), "EST5EDT" ),"%b %d")

ontcases<- read.csv("Full COVID-19 Summary Data for Ontario.csv") %>% rename("cases" = "Change.in.cases") %>% select(Date, cases)
ontcases$Date <- mdy(ontcases$Date)
onttrain <-filter(ontcases, Date <="2021-11-01") %>% select(Date, cases) 
ontts <-xts(onttrain$cases, order.by=onttrain$Date)
  attr(ontts, 'frequency') <-7
  stl_ontts <-stl(ontts,"periodic")
  plot(stl_ontts, main = "Ontario Covid Cases Time Series Decomposition")
  ontts_sa<-xts((onttrain$cases -stl_ontts$time.series[,1]), order.by=onttrain$Date)
onteval<-filter(ontcases, Date >"2021-11-01") %>% select(Date, cases) 

arimamodel<-Arima(ontts, order=c(14,1,0), seasonal=c(0,0,0), lambda="auto")
arimamodel2<-Arima(ontts, order=c(14,1,0), seasonal=c(2,1,0), lambda="auto")

fc<-forecast(arimamodel,h=14)
fc2<-forecast(arimamodel2,h=14)

plotdata<-data.frame(fc,fc2, Date=seq.Date(max(onttrain$Date) + 1, by="1 day", length.out=14))

ggplot() +
 geom_line(data=filter(onttrain, Date>"2021-08-01"),aes(x=Date, y=cases), size=1.2, alpha=0.7) +
 geom_ribbon(data=plotdata, aes(x=Date,ymin=Lo.95.1, ymax=Hi.95.1), fill="mediumorchid1", alpha=0.2) +
 geom_line(data=plotdata,aes(x=Date, y=Point.Forecast.1), color="mediumorchid1", size=1.2) +
  geom_point(data=onteval,aes(x=Date, y=cases), color="black") +
 scale_x_date(breaks = scales::pretty_breaks(n = 12)) + 
 expand_limits(y=0) +
 theme_fivethirtyeight()+
 ylab("Ontario Covid Cases") + 
 ggtitle("COVID-19 Cases Per Day with ARIMA(14,1,0)x(2,1,0) Forecast")  

ggsave("data/covidarima_sa.png", plot=last_plot(),width=8,height=4.5,dpi=200)

# Tweet the Graph 
#covidplots_token <- rtweet::create_token(
#  app = "covidplots",
#  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
#  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
#  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
#  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET"),
#)
#post_tweet(paste("Ontario cases forecast from ARIMA(14,1,0) model. The model is estimated from all available COVID data up to Nov 1, and forecased out 2 weeks.  Black is actual data, purple is forecast with 95% forecast interval. Updated with actual values up to",accessed_date),
#           media="data/covidarima.png",
#           token=covidplots_token)
