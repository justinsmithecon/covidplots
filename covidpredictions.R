library(tidyverse)
library(zoo)
library(ggthemes)
library(lubridate)
library(xts)
library(forecast)
library(rtweet)



phucases<- read.csv("All case trends data.csv") %>% rename("phu" = "Public.Health.Unit", "cases" = "Cases.by.reported.date") %>% select(Date, phu,cases)
phucases$Date <- mdy(phucases$Date)
onttrain <-filter(phucases,phu == "Ontario", Date <="2021-11-01") %>% select(Date, cases) 
ontts <-xts(onttrain$cases, onttrain$Date)
onteval<-filter(phucases,phu == "Ontario", Date >"2021-11-01") %>% select(Date, cases) 

arimamodel<-Arima(ontts, order=c(14,1,0))
fc<-forecast(arimamodel,h=14)
plotdata<-data.frame(fc, Date=seq.Date(max(onttrain$Date) + 1, by="1 day", length.out=14))

ggplot() +
 geom_line(data=filter(onttrain, Date>"2021-08-01"),aes(x=Date, y=cases), size=1.2, alpha=0.7) +
  geom_ribbon(data=plotdata, aes(x=Date,ymin=Lo.95, ymax=Hi.95), fill="mediumorchid1", alpha=0.2) +
 geom_line(data=plotdata,aes(x=Date, y=Point.Forecast), color="mediumorchid1", size=1.2) +
 geom_point(data=onteval,aes(x=Date, y=cases), color="black") +
 scale_x_date(breaks = scales::pretty_breaks(n = 12)) + 
 theme_fivethirtyeight()+
 ylab("Ontario Covid Cases") + 
 ggtitle("COVID-19 Cases Per Day with ARIMA(14,1,0) Forecast")  

ggsave("data/covidarima.png", plot=last_plot(),width=8,height=4.5,dpi=200)

# Tweet the Graph 
covidplots_token <- rtweet::create_token(
  app = "covidplots",
  consumer_key =    Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET"),
)
post_tweet("Ontario Cases Forecast from ARIMA(14,1,0) Model",
           media="data/covidarima.png",
           token=covidplots_token)
