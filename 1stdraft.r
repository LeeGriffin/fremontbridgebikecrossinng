#rm(list=ls(all=TRUE))

library(RSocrata, quietly = TRUE)
library(zoo, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(TSstudio, quietly = TRUE)
library(forecast, quietly = TRUE)
library(texreg, quietly = TRUE)


#they have pretty good instructions on the socrata or Seattle data web sites 
#but you will need to add your own app_token, email, and password :) 
#in case it was not clear, your will also need to have those packages installed (probably going to be asked to install broom as well)

df <- read.socrata(
  "https://data.seattle.gov/resource/7mre-hcut.json",
  app_token = "7mzZjSmMCKGYC9cVREJWHD5ZY",
  email     = "leegriffin54@gmail.com",
  password  = "Lakeview21!")

df <- na.omit(df)
df$fremont_bridge_nb <- as.integer(df$fremont_bridge_nb)
df$fremont_bridge_sb <- as.integer(df$fremont_bridge_sb)
typeof(df$fremont_bridge_nb)

hourly_riders_nb <- zoo(
  x         = df[["fremont_bridge_nb"]],
  order.by  = df[["date"]],
  frequency = 24
)

autoplot(hourly_riders_nb)#well there was a clearupward trend.... 



################   Northbound riders by hour           ##########

lockdown_2020 <- window(hourly_riders_nb, start="2020-02-19", end="2021-02-19") #First case in WA was feb 19th
normal_2019 <- window(hourly_riders_nb, start="2019-02-19", end="2020-02-18")
lockdown_riders_hour <- cbind(lockdown_2020,normal_2019)

autoplot(lockdown_riders_hour,
         series=NULL,
         facet = FALSE)

#Pretty clear drop

################   I do not like doing this but here it is making a ts object to make a pretty graph   #######

ts_2020 <- ts(lockdown_2020, frequency = 24)
ggseasonplot(ts_2020, year.labels = TRUE) +
  ylab("Crossers_Per_Hour") +
  ggtitle("People Crossing a Bridge on Bikes (02-19-2020 to 08-31-2020)") #intresting plot

ts_2019 <- ts (normal_2019, frequency = 24)
ggseasonplot(ts_2019, year.labels = TRUE) +
  ylab("Crossers_Per_Hour") +
  ggtitle("People Crossing a Bridge on Bikes ((02-19-2019 to 08-31-2019))") #intresting plot

#Looks like that drop has alot do to do with commuters... shocker

################   next step is to collapse the data into days         ####################

df_day <- df %>% mutate(as.Date(date))
df_day <- df_day %>% rename(
    `date_day` = `as.Date(date)`)

aggdata <-aggregate(df_day, by=list(df_day$date_day),
                    FUN=mean, na.rm=TRUE)

################   might as well just show sums per day                 #################

aggdata$fremont_bridge_nb <- aggdata$fremont_bridge_nb*24
aggdata$fremont_bridge_sb <- aggdata$fremont_bridge_sb*24
aggdata$fremont_bridge_nb <- as.integer(aggdata$fremont_bridge_nb)
aggdata$fremont_bridge_sb <- as.integer(aggdata$fremont_bridge_sb)
 

################   Um, sidenote here, I wonder how different north/south are    #########

aggdata$diff <- aggdata$fremont_bridge_nb - aggdata$fremont_bridge_sb #so + is more north - is more south

daily_riders_diff <- zoo(
  x         = aggdata[["diff"]],
  order.by  = aggdata[["date_day"]],
  frequency = 7)


daily_riders_diff <- window(daily_riders_diff, start="2020-02-19")#first case in washington was feb, 19th
autoplot(daily_riders_diff)

################   Intresting, looks like more people are riding north   ######################
################   Wonder if there is an easier way back or someting?    ######################


aggdata$total_riders <- aggdata$fremont_bridge_nb + aggdata$fremont_bridge_sb #so + is more north - is more south


daily_riders_total <- zoo(
  x         = aggdata[["total_riders"]],
  order.by  = aggdata[["date_day"]],
  frequency = 7)

autoplot(daily_riders_total_2020) ####okay everything lines up

daily_riders_total_2020 <- window(daily_riders_total,start="2020-01-01", end = "2020-08-31" ) 
#only have data till 8-31
daily_riders_total_2019 <- window(daily_riders_total, start = "2019-01-01", end = "2019-08-31" )

autoplot(daily_riders_total_2020) ####okay everything lines up



################   Final Plot             #############################



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


################   So way less riders than before, but still no real increase right? ##########

##################      Well lets zoom in on the most recent months of 2020            ##########


daily_riders_total_forcast <- window(daily_riders_total, start = "2020-05-01", end = "2020-08-31" )
#only have data till 08-31
print(daily_riders_total_forcast)

daily_riders_total_forcast_ts <- ts(daily_riders_total_forcast, frequency = 7)

print(daily_riders_total_forcast_ts)
decomp <- (decompose(daily_riders_total_forcast_ts))
plot(decomp)

linearMod <- tslm(daily_riders_total_forcast_ts ~ trend + season)  # build linear regression model

screenreg(linearMod)

###         trend         5.92 ***
###                       (1.35) 
###         So we got this postive trend going on, about 6 new riders a day, heads up that comment is from aug 2020

autoplot(forecast(linearMod)) #is it a good forecast? no, is it kinda fun to look at, yeah


######################################################################################

#Looks like ridership is starting to increase, but at a pretty slow pace
#Still not a ton of commuters though

######################################################################################
