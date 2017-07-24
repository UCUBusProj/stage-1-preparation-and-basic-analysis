library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(geosphere)
library(fossil)

## Load data
data <- read.csv("data-tidy/tidy_busdata_240717_1430.csv", encoding="UTF-8", stringsAsFactors = F)
data$datetime <- as.POSIXct(strptime(data$datetime, "%Y-%m-%d %H:%M:%OS", tz="Europe/Kiev"))
df_route_path <- read.csv("data-tidy/all_paths.csv", encoding="UTF-8")

data %>% 
  filter(state == 1) %>% 
  filter(routeid == 712991) %>% 
  group_by(routeid, vehicleid) %>% 
  summarise() -> route_vehicle_g

route_vehicle_g <- data.frame(route_vehicle_g)

dataset_r_v_x_list <- list()
group_by_gps_records <- function(rid, vid) {
  data %>% filter(state == 1 & routeid == rid & vehicleid == vid) -> dataset_r_v
  dataset_r_v <- as.data.frame(dataset_r_v)
  
  nrows <- nrow(dataset_r_v)
  #print(nrows)
  cols <- c('datetime', 'lon','lat')
  dataset_r_v_x <- cbind(dataset_r_v[1:nrows-1,c('routeid', 'vehicleid')], dataset_r_v[1:nrows-1,cols], dataset_r_v[2:nrows,cols])
  colnames(dataset_r_v_x) <- c('routeid', 'vehicleid', 't1', 'lon1', 'lat1', 't2', 'lon2', 'lat2')
  dataset_r_v_x
  
  dataset_r_v_x_list[[length(dataset_r_v_x_list)+1]] <<- dataset_r_v_x
}


mapply(group_by_gps_records, route_vehicle_g$routeid, route_vehicle_g$vehicleid)
dataset_r_v_x_df <- do.call(rbind, dataset_r_v_x_list)

dataset_r_v_x_df$bear <- mapply(earth.bear, dataset_r_v_x_df$lon1,dataset_r_v_x_df$lat1,dataset_r_v_x_df$lon2,dataset_r_v_x_df$lat2)
dataset_r_v_x_df$tdiff <- difftime(dataset_r_v_x_df$t2,dataset_r_v_x_df$t1, tz="Europe/Kiev", units = "secs")

dataset_r_v_x_df <- dataset_r_v_x_df[dataset_r_v_x_df$tdiff <= 120 & dataset_r_v_x_df$tdiff >=0 ,]

write.csv(dataset_r_v_x_df, "data-calc/dataset_r_v_x.csv", fileEncoding="UTF-8", row.names = F)



res_list <- list()
mapply(find_path_indexes, route_vehicle_g$routeid, route_vehicle_g$vehicleid)
res_df = do.call(rbind, res_list)

#res <- find_path_indexes(dataset_r_v_x_df_difff, 712991, 64360)

colnames(res_df) <- c("p_index1", "p_index2", "bear", "p_lon1", "p_lat1", "p_lon2", "p_lat2", "dist")

count(res_df[res_df$p_index1 == -1, ])

r_v_x_df_difff_res <- cbind(dataset_r_v_x_df, res_df)
r_v_x_df_difff_res$tdiff <- as.numeric(r_v_x_df_difff_res$tdiff)
r_v_x_df_difff_res$speed <- r_v_x_df_difff_res$dist/(r_v_x_df_difff_res$tdiff/(60*60))

r_v_x_df_difff_res <- r_v_x_df_difff_res[r_v_x_df_difff_res$p_index1 > 0,]

write.csv(r_v_x_df_difff_res, "data-calc/r_v_x_df_difff_res.csv", fileEncoding="UTF-8", row.names = F)

hist(r_v_x_df_difff_res$speed)

dim(r_v_x_df_difff_res)
summary(r_v_x_df_difff_res)

sample <- r_v_x_df_difff_res[29,]

sample$nr <- row.names(sample)

sq_map <- get_map(location = c(24.04865, 49.79876), source = "google", zoom = 15, maptype = 'hybrid', api_key = google_api_key)
ggmap(sq_map) + 
  geom_point(data = sample, mapping = aes(x = lon1, y = lat1), color = "red", alpha = 0.75) +
  geom_point(data = sample, mapping = aes(x = lon2, y = lat2), color = "orange", alpha = 0.75) +
  geom_point(data = sample, mapping = aes(x = p_lon1, y = p_lat1), color = "green", alpha = 0.75) +
  geom_point(data = sample, mapping = aes(x = p_lon2, y = p_lat2), color = "blue", alpha = 0.75) +
  facet_wrap("nr")


#rid <- route_vehicle_g[1,]$routeid
#vid <- route_vehicle_g[1,]$vehicleid

find_path_indexes <- function(rid, vid)
{
  dataset_r_v_x_df %>% filter(routeid == rid & vehicleid == vid) -> dataset
  dataset <- as.data.frame(dataset)
  
  print(paste('routeid', rid, 'vehicleid', vid, Sys.time(), "start", "nrows:", nrow(dataset)))
  res <- mapply(get_path_indx, dataset$routeid, dataset$lon1, dataset$lat1, dataset$lon2, dataset$lat2, dataset$bear)
  print(paste('routeid', rid, 'vehicleid', vid, Sys.time(), "finish"))
  print(typeof(res))
  print(dim(res))
  res_dataset <- data.frame(unlist(res[1,]), unlist(res[2,]),unlist(res[3,]),unlist(res[4,]), unlist(res[5,]),unlist(res[6,]),unlist(res[7,]),unlist(res[8,]))
  res_list[[length(res_list)+1]] <<- res_dataset
}

data %>% filter(routeid == 712991 & vehicleid == 64360) -> data_712991_64364
data_712991_64364 <- data.frame(data_712991_64364)
nrows <- nrow(data_712991_64364)
cols <- c('datetime','lon','lat')
data_712991_64364_x <- cbind(data_712991_64364[1:nrows-1,c('routeid')], data_712991_64364[1:nrows-1,cols], data_712991_64364[2:nrows,cols])
colnames(data_712991_64364_x) <- c('routeid', 't1', 'lon1', 'lat1', 't2', 'lon2', 'lat2')
x <- dataset_r_v_x_df_difff[11,]

routeid <- x$routeid
lon1 <- x$lon1
lat1 <- x$lat1
lon2 <- x$lon2
lat2 <- x$lat2
bear <- x$bear

##Function to find closest path coord
get_path_indx <- function(routeid, lon1, lat1, lon2, lat2, bear) {
  df_path <- df_route_path[df_route_path$route_id == routeid, ]
  p_len <- nrow(df_path)
  rownames(df_path) <- NULL
  
  vdm1 <- unlist(distm(c(lon1, lat1), cbind(df_path$lng_start, df_path$lat_start)))
  vdm2 <- unlist(distm(c(lon2, lat2), cbind(df_path$lng_finish, df_path$lat_finish)))

  search_index <- 1
  p_index1 <- -1
  p_index2 <- -1
  p_bear <- -1
  while (search_index < p_len)
  {
    p_index1 <-
      which(vdm1 == sort(vdm1, partial = search_index)[1])[1]
    
    #print(paste("1p", p_index1, vdm1[p_index1], sep=" "))
    
    if (p_index1 < p_len)
    {
    p_index2 <-
      p_index1 + which(vdm2[p_index1+1:p_len] == sort(vdm2[(p_index1+1):p_len], partial =
                                                 1)[1])[1]
    
    }
    else
    {
      p_index2 <- p_index1
    }
    #print(paste("2p", p_index2, vdm2[p_index2], sep=" "))
    
    p_bear <- earth.bear(df_path[p_index1,"lng_start"], df_path[p_index1,"lat_start"], 
                         df_path[p_index2,"lng_finish"], df_path[p_index2,"lat_finish"])
    
    ##If coordinates are in correct order
    if (p_index1 != -1 & p_index1 <= p_index2 & abs(bear-p_bear) < 45 & (p_index2 - p_index1) < 20)
    {
      break
    }
    p_index1 <- -1
    p_index2 <- -1
    p_bear <- -1
    search_index<- search_index+1
    #print(paste("p_index1 > p_index2, search_index+1 = ", search_index, sep=" "))
  }
  
  p_lon1 <- -1
  p_lat1 <- -1
  p_lon2 <- -1
  p_lat2 <- -1
  
  if(p_index1 > -1 & p_index1 > -1)
  {
    p_lon1 <- df_path[p_index1, "lng_start"]
    p_lat1 <- df_path[p_index1, "lat_start"]
    p_lon2 <- df_path[p_index2, "lng_finish"]
    p_lat2 <- df_path[p_index2, "lat_finish"]
  }
  
  p_dist <- sum(df_path[p_index1:p_index2,]$dist)
  c(p_index1, p_index2, p_bear, p_lon1, p_lat1, p_lon2, p_lat2, p_dist)
}


cols <- c("lon", "lat")
df_path <- df_route_path[df_route_path$routeid == 712991, ]
rownames(df_path) <- NULL
df_path_x <- cbind(df_path[1:nrow(df_path)-1,c('routeid')], df_path[1:nrow(df_path)-1,cols], df_path[2:nrow(df_path),cols])
colnames(df_path_x) <- c('routeid', 'lon1', 'lat1', 'lon2', 'lat2')
df_path_x$dist <- mapply(earth.dist.cust, df_path_x$lon1, df_path_x$lat1, df_path_x$lon2, df_path_x$lat2)*1000
sum(df_path_x[1:45,]$dist)


earth.dist.cust <- function (long1, lat1, long2, lat2) 
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}