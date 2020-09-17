rm(list=ls(all=TRUE))


library("RSocrata")
library("zoo")
library(ggplot2)
library(dplyr)
library(TSstudio)
library(forecast)

#they have pretty good intrustions on the socrata or seattle data web sites 

df <- read.socrata(
  "https://data.seattle.gov/resource/7mre-hcut.json",
  app_token = "Your_app_token_here",
  email     = "Your_email",
  password  = "Your_password")

df <- na.omit(df)
df$fremont_bridge_nb <- as.integer(df$fremont_bridge_nb)
df$fremont_bridge_sb <- as.integer(df$fremont_bridge_sb)
typeof(df$fremont_bridge_nb)

hourly_riders_nb <- zoo(
  x         = df[["fremont_bridge_nb"]],
  order.by  = df[["date"]],
  frequency = 24
)

autoplot(hourly_riders_nb) #well there was a clearupward trend....more people are biking!



##################What up covid######################################

covid_riders_nb <- window(hourly_riders_nb, start="2020-01-01")
autoplot(covid_riders_nb)


##### I do not like doing this but here it is making a ts object to make a pretty graph #######

ts <- ts(covid_riders_nb, frequency = 24)
ggseasonplot(ts, year.labels = TRUE) +
  ylab("Crossers_Per_Hour") +
  ggtitle("People Crossing a Bridge on Bikes (Every day of 2020)") #intresting plot



#####I think going back here and doing some more analysis on ridership per day would be intrestsing 

################ Next step is to collapse the data into days.      ############################

df_day <- df %>% mutate(as.Date(date))
df_day <- df_day %>% rename(
    `date_day` =`as.Date(date)`)

aggdata <-aggregate(df_day, by=list(df_day$date_day),
                    FUN=mean, na.rm=TRUE)

################    Might as well just show sums per day                 ######################

aggdata$fremont_bridge_nb <- aggdata$fremont_bridge_nb*24
aggdata$fremont_bridge_sb <- aggdata$fremont_bridge_sb*24
aggdata$fremont_bridge_nb <- as.integer(aggdata$fremont_bridge_nb)
aggdata$fremont_bridge_sb <- as.integer(aggdata$fremont_bridge_sb)
 

################ Um, sidenote here, I wonder how different north/south are   #################

aggdata$diff <- aggdata$fremont_bridge_nb - aggdata$fremont_bridge_sb #so + is more north - is more south

daily_riders_diff <- zoo(
  x         = aggdata[["diff"]],
  order.by  = aggdata[["date_day"]],
  frequency = 7)


daily_riders_diff <- window(daily_riders_diff, start="2020-02-19")#first case in washington was feb, 19th
autoplot(daily_riders_diff)

################   Intresting, looks like more people are riding north   ######################
################   Wonder if there is an easier way back or someting?    ######################

################   Now lets look at total crossings                      ######################
################   How covid/lockdown has affected traffic               ######################


aggdata$total_riders <- aggdata$fremont_bridge_nb + aggdata$fremont_bridge_sb #so + is more north - is more south


daily_riders_total <- zoo(
  x         = aggdata[["total_riders"]],
  order.by  = aggdata[["date_day"]],
  frequency = 7)


daily_riders_total_2020 <- window(daily_riders_total,start="2020-01-01", end = "2020-09-01" ) #only have data till 9-01
daily_riders_total_2019 <- window(daily_riders_total, start = "2019-01-01", end = "2019-09-01" ) #only have data till 9-01


autoplot(daily_riders_total_2020) ####okay everything lines up



#############                         Final Plot             #############################



daily_riders_total_2020_ts <- ts(daily_riders_total_2020, frequency = 7)
daily_riders_total_2019_ts <- ts(daily_riders_total_2019, frequency = 7)

comb_ts <- cbind(daily_riders_total_2020_ts, daily_riders_total_2019_ts)

decomp <- (decompose(comb_ts))
trend <- (decomp$trend)
comb_ts_f <- cbind(comb_ts,trend)


colnames(comb_ts_f) <- c("2020_Actual", "2019_Actual", "2020_Trend", "2019_Trend")

pallete = c('steelblue3', 'tomato3', 'yellow2', 'navy blue') 
#are they ugly colors, yes, im not a designer, change them if you want#
autoplot(comb_ts_f,
         series=NULL,
         facet = FALSE,
         lwd = 1,
         xlab = "Weeks",
         ylab = "Riders per Day")+ scale_colour_manual(values=pallete) #2nd good chart


################## So way less riders than before, but still no real increse right? ##########

##################      Well lets zoom in on the most recent months of 2020            ##########


daily_riders_total_forcast <- window(daily_riders_total, start = "2020-05-01", end = "2020-09-01" ) #only have data till 9-01
print(daily_riders_total_forcast)

daily_riders_total_forcast_ts <- ts(daily_riders_total_forcast, frequency = 7)

print(daily_riders_total_forcast_ts)
decomp <- (decompose(daily_riders_total_forcast_ts))
plot(decomp)

linearMod <- tslm(daily_riders_total_forcast_ts ~ trend + season)  # build linear regression model

library(texreg)
screenreg(linearMod)

###         trend         5.92 ***
###                       (1.35) 
###         So we got this postive trend goign on, about 6 new riders a day 

autoplot(forecast(linearMod)) #is it a good forcast, no, is it kinda fun to look at, yeah

######################################################################################

#Looks like ridership is starting to increse, but at a pretty slow pace
#Still not a ton of commuters though

######################################################################################
