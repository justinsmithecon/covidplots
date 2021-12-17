library(tidyverse)
library(zoo)
library(ggthemes)
library(lubridate)
library(xts)
library(forecast)
library(rtweet)
library(Metrics)

accessed_date<-format(as.POSIXlt(Sys.time(), "EST5EDT" ),"%b %d")

ontcases<- read.csv("Full COVID-19 Summary Data for Ontario.csv") %>% rename("cases" = "Change.in.cases") %>% select(Date, cases)
ontcases$Date <- mdy(ontcases$Date)
onttrain <-filter(ontcases, Date <="2021-12-15") %>% select(Date, cases) 
ontts <-xts(onttrain$cases, order.by=onttrain$Date)
  attr(ontts, 'frequency') <-7
onteval<-filter(ontcases, Date >"2021-12-15") %>% select(Date, cases) 


tb<-tbats(as.ts(ontts))
fc<-forecast(tb,h=14)

plotdata<-data.frame(fc, Date=seq.Date(max(onttrain$Date) + 1, by="1 day", length.out=14))

ggplot() +
  geom_line(data=filter(onttrain, Date>"2021-08-01"),aes(x=Date, y=cases), size=1.2, alpha=0.7) +
  geom_ribbon(data=plotdata, aes(x=Date,ymin=Lo.95, ymax=Hi.95), fill="mediumorchid1", alpha=0.2) +
  geom_line(data=plotdata,aes(x=Date, y=Point.Forecast), color="mediumorchid1", size=1.2) +
  geom_line(data=onteval,aes(x=Date, y=cases), color="black", size=1.2, alpha=0.7) +
  scale_x_date(breaks = scales::pretty_breaks(n = 12)) + 
  expand_limits(y=0) +
  theme_fivethirtyeight()+
  ylab("Ontario Covid Cases") + 
  ggtitle("COVID-19 Cases Per Day with TBATS Forecast")  

ggsave("data/covidarima.png", plot=last_plot(),width=8,height=4.5,dpi=200)


rmsecalc<-left_join(onteval,plotdata,by="Date") %>% mutate(sqerr = (cases-Point.Forecast)^2)
rmse<-rmse(rmsecalc$cases, rmsecalc$Point.Forecast) %>% round(digits=2)


#Tweet the Graph 
covidplots_token <- rtweet::create_token(
  app = "covidplots",
  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET"),
)

if(is.nan(rmse)=="TRUE"){
post_tweet(paste("Ontario cases forecast from ARIMA(4,1,3)x(1,0,1) model. Model is estimated from Ontario data up to Nov 18, then forecased out 2 weeks.  Black is actual data, purple is forecast with 95% forecast interval. Updated with actual values up to ",accessed_date, sep=""),
             media="data/covidarima.png",
             token=covidplots_token) 
} else {
post_tweet(paste("Ontario cases forecast from ARIMA(4,1,3)x(1,0,1) model. Model is estimated from Ontario data up to Nov 18, then forecased out 2 weeks.  Black is actual data, purple is forecast with 95% forecast interval. Updated with actual values up to ",accessed_date,". Rolling RMSE is ", rmse, sep=""),
             media="data/covidarima.png",
             token=covidplots_token) 
}
