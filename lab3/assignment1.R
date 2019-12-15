#install.packages("lubridate")
library(geosphere)
library(lubridate)
require(stats)

set.seed(1234567890)
stations <- read.csv("stations.csv", fileEncoding = "latin1") 
temps <- read.csv("temps50k.csv") 
st <- merge(stations,temps,by="station_number")

target_point = c(18.0574, 59.3420)
h_distance <- 100000    # These three values are up to the students 
h_date <- 12
h_time <- 6

pred_date <- "2011-08-04" # The date to predict (up to the students) 
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
temp <- vector(length=length(times))
# Students’ code here
df_before <- subset(st, as.Date(date) < as.Date(pred_date))


get_date_diff <- function(day1, day2, h_date) {
  diff <- abs(yday(day1) - yday(day2))
  if (diff > 182) {
    return (abs(diff - 365)/h_date)
  }
  return (diff/h_date)
}

get_time_diff <- function( time2, time1, h_time) {
  time1.numeric <- as.numeric(substring(time1,1,2))
  diff <- abs(time1.numeric - time2)
  if (diff > 12) {
    return (abs(diff - 24)/h_time)
  }
  return (diff/h_time)
}

get_xy_diff <- function(target, x, h_distance) {
  x_lat <- as.numeric(x[4])
  x_long <- as.numeric(x[5])
  dist <- distHaversine(c(x_long, x_lat), target)
  return (dist[1]/h_distance)
}

euc_norm <- function(x) return (sqrt(sum(x^2)))


date_kernel <- function(X, target, h_date) {
  u <- abs(sapply(X$date, function(x) get_date_diff(target, x, h_date)))
  #u <- (as.numeric(as.Date(X$date) - as.Date(target), unit="days"))/h_date
  k <- exp(-u^2)
  return (k)
}


time_kernel <- function(X, target, h_time) {
  u <- abs(sapply(X$time, function(x) get_time_diff(target, x, h_time)))
  k <- exp(-u^2)
  return (k)
}

distance_kernel <- function(X, target, h_distance) {
  u <- abs(apply(X, 1, function(x) get_xy_diff(target, x, h_distance)))
  k <- exp(-u^2)
  return (k)
}


#date_distances <- date_kernel(df_before, pred_date, h_date)
#plot(date_distances)
xy_distances <- distance_kernel(df_before, target_point, h_distance)
#plot(xy_distances)
time_distances <- time_kernel(df_before, 12, h_time)


denom = c()
nomi = c()


pred_dates <- c("2011-01-04", "2011-02-04", "2011-03-04", "2011-04-04", "2011-05-04", "2011-06-04", "2011-07-04", "2011-08-04", "2011-09-04", "2011-10-04","2011-11-04", "2011-12-04")

pred = c()
for (x in 1:length(pred_dates))
{
  temp_date_distances <- date_kernel(df_before, pred_dates[x], h_date)
  for (i in 1:length(df_before$date)) {
    denom[i] <- (temp_date_distances[i] * time_distances[i] * xy_distances[[i]]) 
    nomi[i] <- (denom[i] * df_before$air_temperature[i])
  }
  pred[x] <- sum(nomi) / sum(denom)
  
}

plot(pred, type="o", ylab="Celcius")



'pred = c()
for (x in 1:length(times))
{
  temp_time <- as.numeric(substring(times[x], 1, 2))
  temp_time_distances <- time_kernel(df_before, temp_time, h_time)
  for (i in 1:length(df_before$date)) {
    denom[i] <- (date_distances[i] * temp_time_distances[i] * xy_distances[[i]]) 
    nomi[i] <- (denom[i] * df_before$air_temperature[i])
  }
  pred[x] <- sum(nomi) / sum(denom)
  
}'
  
#plot(pred, type="o", ylab="Celcius")
#plot(temp, type="o")




