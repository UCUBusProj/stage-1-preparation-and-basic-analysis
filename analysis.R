## Lybraries
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(lubridate)
library(stplanr)
library(sp)
library(geosphere)

## Load dataset
data <- read.csv("data-tidy/tidy_busdata_220717_1230.csv", encoding="UTF-8", stringsAsFactors = F)
data$datetime <- as.POSIXct(strptime(data$datetime, "%Y-%m-%d %H:%M:%OS", tz="Europe/Kiev"))
df_routes <- read.csv("data-tidy/routes.csv", encoding="UTF-8")
df_stops <- read.csv("data-tidy/bus_stops.csv", encoding="UTF-8")

## Dimensions
dim_desc(data)

## Statistics
summary(data)

## Head and tail
head(data)
tail(data)

## Analyze routes

### Route stops count
df_route_stops <- read.csv("data-tidy/df_route_stops.csv", encoding="UTF-8")
df_route_stops %>%
  group_by(routeid) %>%
  summarise (n = n()) -> df_route_stops_stats

df_route_stops_stats %>% arrange(desc(n)) -> df_route_stops_stats

df_route_stops_stats$routeid <- factor(as.character(df_route_stops_stats$routeid), levels = df_route_stops_stats$routeid)

ggplot(data=df_route_stops_stats, aes(x=routeid, y=n, fill=routeid)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette="Set1") +
  coord_flip()

### Route lenght
df_route_path <- read.csv("data-tidy/df_route_path.csv", encoding="UTF-8")

dim(df_route_path)

total_dist <- function(x) {
  x_m <- cbind(x[1:nrow(x)-1,c('lon','lat')], x[2:nrow(x),c('lon','lat')])
  
  pointsGeoDist <- function(x) {
    distm(c(x[1], x[2]), c(x[3], x[4]), fun=distHaversine)
  }
  
  x_m$dist<-apply(x_m, 1, pointsGeoDist)
  
  sum(x_m$dist)
}

df_route_path %>% 
  group_by(routeid) %>%
  do(data.frame(total_dist=total_dist(.))) -> df_route_length

df_route_length %>% arrange(desc(df_route_length$total_dist)) -> df_route_length

df_route_length$routeid <- factor(as.character(df_route_length$routeid), levels = df_route_length$routeid)

ggplot(data=df_route_length, aes(x=routeid, y=total_dist, fill=routeid)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette="Set1") +
  coord_flip()

### Route vehicle records count
data %>%
  group_by(routeid) %>%
  summarise (rec_n = as.double(n())) -> data_route_cnt

df_vehicles <- read.csv("data-tidy/vehicles.csv", encoding="UTF-8")

df_vehicles %>%
  group_by(routeid) %>%
  summarise (veh_n = as.double(n())) -> vehicles_route_cnt

data_route_vehicle_cnt <- inner_join(data_route_cnt, vehicles_route_cnt)
data_route_vehicle_cnt$rec_veh_n <-data_route_vehicle_cnt$rec_n/data_route_vehicle_cnt$veh_n

data_route_vehicle_cnt %>% arrange(desc(rec_veh_n)) -> data_route_vehicle_cnt

data_route_vehicle_cnt$routeid <- factor(as.character(data_route_vehicle_cnt$routeid), levels = data_route_vehicle_cnt$routeid)

ggplot(data=data_route_vehicle_cnt, aes(x=routeid, y=rec_veh_n, fill=routeid)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette="Set1") +
  coord_flip()

### Route record delay stats

calc_delay <- function(x) {
  arrange(x, datetime) -> x
  x_m <- cbind(x[1:nrow(x)-1,c('datetime')], x[2:nrow(x),c('datetime')])
  colnames(x_m) <- c('dt1', 'dt2')
  x_m$dt1 <- strptime(x_m$dt1, "%Y-%m-%d %H:%M:%OS", tz="Europe/Kiev")
  x_m$dt2 <- strptime(x_m$dt2, "%Y-%m-%d %H:%M:%OS", tz="Europe/Kiev")
  x_m$diff<-as.numeric(x_m$dt2-x_m$dt1)
  x_m$diff
}




data %>%
  group_by(routeid, vehicleid) %>%
  do(data.frame(delay=calc_delay(.))) %>% 
  summarise (median = as.double(median(delay)))-> data_route_vehicle_delay



### Route vehicle count

### Route record delay stats by vehicles


