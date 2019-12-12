#install.packages("lubridate")
library(geosphere)
library(lubridate)
require(stats)

set.seed(1234567890)
stations <- read.csv("stations.csv", fileEncoding = "latin1") 
temps <- read.csv("temps50k.csv") 
st <- merge(stations,temps,by="station_number")

target_point = c(18.0686, 59.3293)
h_distance <- 100000    # These three values are up to the students 
h_date <- 7
h_time <- 6
date <- "2010-11-04" # The date to predict (up to the students) 
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
temp <- vector(length=length(times))
# Students’ code here


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
  return (dist/h_distance)
}

euc_norm <- function(x) return (sqrt(sum(x^2)))


date_kernel <- function(X, target, h_date) {
  u <- sapply(X$date, function(x) get_date_diff(target, x, h_date))
  #u <- (as.numeric(as.Date(X$date) - as.Date(target), unit="days"))/h_date
  k <- exp(-u^2)
  return (k)
}


time_kernel <- function(X, target, h_time) {
  u <- sapply(X$time, function(x) get_time_diff(target, x, h_time))
  k <- exp(-u^2)
  return (k)
}

distance_kernel <- function(X, target, h_distance) {
  u <- apply(X, 1, function(x) get_xy_diff(target, x, h_distance))
  k <- exp(-u^2)
  return (k)
}


date_distances <- date_kernel(st, date, h_date)
plot(date_distances)
time_distances <- time_kernel(st, 12, h_date)
plot(time_distances)
xy_distances <- distance_kernel(st, target_point, h_distance)
plot(xy_distances)
#plot(temp, type="o")




