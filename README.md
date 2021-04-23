---
title: "R Notebook"
output: html_notebook
---

# Packages
```{r}
library(lubridate)
library(tidyverse)
library(data.table)
library(factoextra)
library(caret)
library(rpart)
library(rpart.plot)
library(formattable)
```


# Reading in the Data
```{r}
weather_df <- fread("dataexport_20210418T182703.csv", na.strings = "NA")
taxi_df <- fread("yellow_tripdata_2019_05.csv", na.strings = "NA")
taxi_zone <- fread("taxi_zone_lookup.csv")
```


# Data Quality & Wrangling    

## Taxi Dataframe
```{r}
n0 <- nrow(taxi_df) # 7565261

# Filtering
taxi_df <- taxi_df[taxi_df$payment_type == 1, ] # only credit cards tracked (5456057), -2109204
# is.na(taxi_df$payment_type) %>% sum()

# designated yellow pick up zones
taxi_zone <- taxi_zone %>%
  filter(Borough == "Manhattan", service_zone == "Yellow Zone")
taxi_df <- taxi_df %>%
  filter(PULocationID %in% taxi_zone$LocationID) # rm -571762 out of bounds pickup

# zones by name
taxi_df <- taxi_df %>% full_join(., taxi_zone[, c("LocationID", "Zone")],
                      by = c("PULocationID" = "LocationID"))
colnames(taxi_df)[19] <- "PU_zone"
taxi_df$PU_zone <- ifelse(is.na(taxi_df$PU_zone), "N/A", taxi_df$PU_zone)
taxi_df <- taxi_df %>% full_join(., taxi_zone[, c("LocationID", "Zone")],
                      by = c("DOLocationID" = "LocationID"))
colnames(taxi_df)[20] <- "DO_zone"
taxi_df$DO_zone <- ifelse(is.na(taxi_df$DO_zone), "N/A", taxi_df$DO_zone)

# trip type
# table(taxi_df$tip_amount == 0, taxi_df$RatecodeID) # from data dictionary
taxi_df <- taxi_df %>% filter(RatecodeID != 5 & RatecodeID != 6 & RatecodeID != 99) #rm Negotiated, Group, Unknown (-4184,-1,-7)
# taxi_df$rateCode.f <- taxi_df$RatecodeID %>%
#   factor(., levels = c(1, 2, 3, 4),
#          labels = c("Standard", ))

# trip distance
taxi_df <- taxi_df %>%
  filter(trip_distance >= 0.02) # trip < Lex/Madison Sth, smallest block 75ft -12101

# time reformat
taxi_df$tpep_pickup_datetime <- as.POSIXct(strptime(taxi_df$tpep_pickup_datetime,
                                                    "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))
taxi_df$tpep_dropoff_datetime <- as.POSIXct(strptime(taxi_df$tpep_dropoff_datetime,
                                                     "%Y-%m-%d %H:%M:%S", tz = "America/New_York"))

# dates not in May
taxi_df$date <- date(taxi_df$tpep_pickup_datetime) # date
taxi_df <- taxi_df %>% filter(date >= "2019-05-01" & date <= "2019-05-31") # rm -231

# day, hour
taxi_df$day <- wday(taxi_df$tpep_pickup_datetime) %>%
  factor(., levels = c(1, 2, 3, 4, 5, 6, 7),
         labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
taxi_df$hour <- hour(taxi_df$tpep_pickup_datetime)

# rounding the Pickup time to nearest Hour b/c weather data
positxct_pu_time <- as.POSIXct(taxi_df$tpep_pickup_datetime)
taxi_df$rounded_pu_time <- round_date(positxct_pu_time, unit = "hour")

```


## Weather
```{r}
weather_df <- weather_df[,-(6)] # rm wind direction
weather_df <- weather_df[-c(1:8),] #rm metadata
col_names <- c("timestamp", "temp", "percip", "humid", "windspd")
colnames(weather_df) <- col_names
weather_df <- weather_df[-c(1),] # rm colname duplicate
weather_df$temp <- as.double(unlist(weather_df$temp)) # char to numeric
weather_df$humid <- as.double(unlist(weather_df$humid))
weather_df$percip <- as.double(unlist(weather_df$percip))
weather_df$windspd <- as.double(unlist(weather_df$windspd))

# Set tz
numeric_time <- strptime(weather_df$timestamp, "%Y%m%d T%H%M", tz = "America/New_York")
weather_df$rounded_pu_time <- as.POSIXct(numeric_time, origin="1970-01-01")
```


## Merge
```{r}
# Merging the Datasets
dat <- taxi_df %>% full_join(weather_df) # n = 4866764
# names(dat)
dat <- dat[, -c(1, 7:10, 16, 21, 24:25)] # rm columns with invariable info
```


### Write Out (For processing speed)
```{r}
fwrite(dat, "yellowtaxi.csv")
```



# Read in Clean-er Data
```{r}
dat <- fread("yellowtaxi.csv", na.strings = "NA")
```

## Additional Weather QC
```{r}
# sapply(dat, function(x) sum(is.na(x)))
dat <- dat %>% filter(!is.na(temp)) # no weather data 31/05/2019, -4128

# weather EDA
# sapply(weather_df[, 2:5], summary)
# sum(dat$percip > 0)/nrow(dat) * 100 # days that rained 14.43%
```


## Additional Taxi QC
```{r}
dat$RatecodeID <- factor(dat$RatecodeID, levels = c(1, 2, 3, 4),
                         labels = c("Standard", "JFK", "Newark", "Nassau/Westchester"))

dat$day <- factor(dat$day,
                  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# regroup zones
nyc_zones <- fread("taxi_zone_limited.csv")
nyc_zones <- nyc_zones[, -c(1, 4:5)]

dat <- dat %>% left_join(., nyc_zones,
                      by = c("DO_zone" = "Zone"))
dat <- dat %>% left_join(., nyc_zones,
                      by = c("PU_zone" = "Zone"))
dat <- dat[, -c("PU_zone", "DO_zone")]
colnames(dat)[19] <- "DO_zone"
colnames(dat)[20] <- "PU_zone"
dat$DO_zone <- ifelse(is.na(dat$DO_zone), "N/A", dat$DO_zone)
dat$DO_zone <- factor(dat$DO_zone)
dat$PU_zone <- factor(dat$PU_zone)
```


# Added Features

## Trip Time
```{r}
# trip in Seconds
dat$trip_time <- (as.numeric(dat$tpep_dropoff_datetime) - as.numeric(dat$tpep_pickup_datetime))

# rm outlier trip times
# tail(sort(dat$trip_time))
trip_time_upr <- median(dat$trip_time) + 2*sd(dat$trip_time)
dat <- dat %>%
  filter(trip_time >= 30 &
           trip_time <= trip_time_upr) # trip < 30s (-13793) or > 2sd+mean (-2)

hist(log(dat$trip_time)) # log normal
hist(log(dat$trip_distance))
```


## Speed
```{r}
# Avg Speed
dat$av_mph <- dat$trip_distance / (dat$trip_time / 3600)
dat <- dat %>% filter(av_mph >= 0.5 & av_mph <= 70) # rm speeds over ticketable limit, -322

log(dat$av_mph+1) %>% hist() # log normal + 1, b/c of speed < 1
```


## Fare
```{r}
# fare adjustments
dat <- dat %>%
  filter(total_amount > 0)   # rm refunds or NA fare, -25

# Tip percentage per Total Fare + Fees (tax incl in fare, surcharges etc)
dat$tip_perc <- dat$tip_amount / dat$total_amount * 100

summary(dat$tip_perc)
hist(dat$tip_perc)
```


### Tip Percentile Breaks
```{r}
breaks = c(0, 5, 10, 15, 18, 20, 25, 100)
labels = c("0-5%","5-10%", "10-15%", "15-18%", "18-20%", "20-25%", ">25%")
dat$tip_size <- cut(dat$tip_perc, breaks=breaks, labels=labels) # as factor
table(dat$tip_size)

# Table of Tip Size by Day
z <- table(dat$day, dat$tip_size) %>% as.data.frame.matrix()
customGreen = "#DeF7E9"
customGreen0 = "#46965a"

formattable(z, align =c("l","c","c","c","c","c","c","r"),
            list(`Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  `0-5%`= color_tile(customGreen, customGreen0),
  `5-10%`= color_tile(customGreen, customGreen0),
  `10-15%`= color_tile(customGreen, customGreen0),
  `15-18%`= color_tile(customGreen, customGreen0),
  `18-20%`= color_tile(customGreen, customGreen0),
  `20-25%`= color_tile(customGreen, customGreen0),
  `>25%`= color_tile(customGreen, customGreen0)
))

# Table of Tip Size by Hour
z2 <- table(dat$hour, dat$tip_size) %>% as.data.frame.matrix()
blueDark <- "#2e86c1"
Orangey <- "#ffc300"

formattable(z2, align =c("l","c","c","c","c","c","r"),
            list(`Indicator Name` = formatter("span",
                                    style = ~ style(color = "grey",
                                    font.weight = "bold",
                                    area(col = 2:8) ~ function(x) percent(x / 100, digits = 0)),
  `12am`= color_tile(Orangey, blueDark),
  `1am`= color_tile(Orangey, blueDark),
  `2am`= color_tile(Orangey, blueDark),
  `3am`= color_tile(Orangey, blueDark),
  `4am`= color_tile(Orangey, blueDark),
  `5am`= color_tile(Orangey, blueDark),
  `6am`= color_tile(Orangey, blueDark),
  `7am`= color_tile(Orangey, blueDark),
  `8am`= color_tile(Orangey, blueDark),
  `9am`= color_tile(Orangey, blueDark),
  `10am`= color_tile(Orangey, blueDark),
  `11am`= color_tile(Orangey, blueDark),
  `Noon`= color_tile(Orangey, blueDark),
  `1pm`= color_tile(Orangey, blueDark),
  `2pm`= color_tile(Orangey, blueDark),
  `3pm`= color_tile(Orangey, blueDark),
  `4pm`= color_tile(Orangey, blueDark),
  `5pm`= color_tile(Orangey, blueDark),
  `6pm`= color_tile(Orangey, blueDark),
  `7pm`= color_tile(Orangey, blueDark),
  `8pm`= color_tile(Orangey, blueDark),
  `9pm`= color_tile(Orangey, blueDark),
  `10pm`= color_tile(Orangey, blueDark),
  `11pm`= color_tile(Orangey, blueDark)
))



# tip_by_day <-
 
  dat %>%
  group_by(day, tip_size) %>%
  summarise(n = n())
 
table(dat$day, dat$tip_size)

tip_by_day_mean <- as.vector(tip_by_day$a_mean)

barplot(tip_by_day_mean, names.arg = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), ylab = "Day of the Week", xlab = "Average Tip", main = "Average Tip Per Day", las = 2)


tip_by_hour <- dat %>%
  group_by(hour) %>%
  summarise(a_mean = mean(tip_perc))

tip_by_hour_mean <- as.vector(tip_by_hour$a_mean)
barplot(tip_by_hour_mean, names.arg = c(0:23), ylab = "Hour of the Day", xlab = "Average Tip", main = "Average Tip Per Hour", las = 2)
```




# EDA Tip percentage
```{r eval=FALSE, include=FALSE}
# pal_n <- 12
# hex <- hue_pal()(pal_n)
# show_col(hex)
# wes_palette("FantasticFox1", 3, "discrete")[3]

hist(dat$tip_perc)
boxplot((dat$tip_perc))
summary(dat$tip_perc)



# all case, all agency together
p1 <- ggplot(three11, aes(case_cost.adj)) +  
  geom_histogram(color = 1, fill = "#46ACC8") +
  theme_bw() +
  labs(title = "Cost Distribution of 311 Calls", x = "", y = "Frequency") +
  xlim(-4, 4) +
  scale_y_continuous(breaks = c(10000, 20000, 30000,
                                40000, 50000, 60000)) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.y.left = element_text(margin = margin(l=2,r=8, "pt")),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_blank(), axis.line.x = element_blank(),
        plot.margin = margin(t=0,6,0,6, "pt"))
## Warning in unit(c(t, r, b, l), unit): NAs introduced by coercion
# all cases, by agency
p2 <- ggplot(three11, aes(case_cost.adj, y = agency, fill = agency)) +
  geom_density_ridges_gradient() +
  theme_bw() +
  xlim(-4, 4) +
  labs(x = bquote("Std."~log[10]~"(Cost per case hour + 1)"), y = "  ") +
  theme(legend.position = "none", plot.margin = margin(t=-2,6,0,6, "pt"))

# mushed together
cowplot::plot_grid(p1, p2, ncol= 1, rel_heights = c(0.4,1))
 

```


# Elastic net
```{r}
# https://bradleyboehmke.github.io/HOML/regularized-regression.html#regularized-regression

# 6.2.3 Elastic nets
# A generalization of the ridge and lasso penalties, called the elastic net (Zou and Hastie 2005), combines the two penalties:
#
# Although lasso models perform feature selection, when two strongly correlated features are pushed towards zero, one may be pushed fully to zero while the other remains in the model. Furthermore, the process of one being in and one being out is not very systematic. In contrast, the ridge regression penalty is a little more effective in systematically handling correlated features together. Consequently, the advantage of the elastic net penalty is that it enables effective regularization via the ridge penalty with the feature selection characteristics of the lasso penalty.

library(glmnet)



mod_df <- dat[, -c("tpep_pickup_datetime", "tpep_dropoff_datetime", "date")] %>% as.matrix.data.frame()
Matrix::sparse.model.matrix(tip_perc ~ ., mod_df, standardize = T)[,1]

mod_df %>% class()
sapply(mod_df, class)
```


# Recursive Partioning & Regression Tree
```{r}
tip_dt <- rpart(tip_perc ~ ., data = dat)
tip_dt #
```




# Additional Features
```{r}
library(LaF)
library(ffbase)
.dat <- laf_open_csv("yellowtaxi.csv",
                     column_types = c("integer", "double", "integer",
                                      "integer", "double", "double", "double",
                                      "double", "double", "double", "double",
                                      "double", "categorical", "categorical", "categorical",
                                      "integer", "double", "double", "double", "double"),
                     column_names = c("passenger_count", "trip_distance", "PULocationID",
                                      "DOLocationID", "fare", "extra", "mta_tax",
                                      "tip", "tolls", "improvement_surcharge", "total",
                                      "congestion_surcharge", "rateCode.f", "date", "day",
                                      "hour", "temp", "percip", "humid", "windspd"))

dat.ffdf <- ffbase::laf_to_ffdf(laf = .dat)
summary(dat.ffdf)


# trip in Seconds
dat$total_trip_time <- (as.numeric(dat$tpep_dropoff_datetime) - as.numeric(dat$tpep_pickup_datetime))

# rm outlier trip times
# tail(sort(dat$total_trip_time))
trip_time_upr <- mean(dat$total_trip_time) + 2*sd(dat$total_trip_time)
dat <- dat %>%
  filter(total_trip_time >= 30 &
           total_trip_time <= trip_time_upr) # trip < 30s (-13795) | > 2sd+mean (-2)
# hist(log(dat$total_trip_time)) # log normal

# Avg Speed
dat$av_mph <- dat$trip_distance / (dat$total_trip_time / 3600)



dat.ff <- ffbase::laf_to_ffdf(laf = data.table::copy(dat))


k_max <- 10
outs <- matrix(NA, ncol=2, nrow=k_max)
for(k_guess in seq_len(k_max)){
  km_out <- kmeans(dat$av_mph, centers=k_guess)
  outs[k_guess, 1] <- km_out$betweenss
  outs[k_guess, 2] <- km_out$tot.withinss
}
plot(dat[,"total_trip_time"], dat[, "trip_distance"], col=km_out$cluster)
tail(time_dist)


spd.km <- fviz_nbclust(time_dist, kmeans, k.max=7, method = "wss")+
  labs(subtitle = "Elbow Method")
hc_cos.wss # no distinct elbow


km_out <- bigkmeans(time_dist, centers=4, iter.max = 10, nstart = 1, dist = "euclid")


head(sort(dat$av_mph))
total_trip_time

NbClust(time_dist[,3], distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

time_dist %>% big.matrix(ncol=3)


time_dist <- dat %>% select(total_trip_time, trip_distance, av_mph) %>% unlist() %>% as.matrix()
head(time_dist)

km_out <- kmeans(time_dist$av_mph, centers = 4)
plot(time_dist[,1], time_dist[,2], col = cols[km_out$cluster], pch = 16)

table(km_out$cluster, dat$av_mph)
length(km_out$cluster)
dim(time_dist)

# # filter out outlier speed
# spd_lm <- lm(avg_mph ~ total_trip_time + trip_distance, data = taxi_df)
# plot(spd_lm$residuals)
#
# tail(sort(taxi_df$total_trip_time))
# quantile(taxi_df$avg_mph)
