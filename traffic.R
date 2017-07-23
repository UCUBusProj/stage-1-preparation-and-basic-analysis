library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(geosphere)


## Load data
data <- read.csv("data-tidy/tidy_busdata_230717_1400.csv", encoding="UTF-8", stringsAsFactors = F)
data$datetime <- as.POSIXct(strptime(data$datetime, "%Y-%m-%d %H:%M:%OS", tz="Europe/Kiev"))
df_route_path <- read.csv("data-tidy/df_route_path.csv", encoding="UTF-8")

summary(data$angle)

data %>% 
  filter(routeid == 712991 & vehicleid == 64360) %>% 
  group_by(routeid, vehicleid) %>% 
  summarise() -> route_vehicle_g

route_vehicle_g <- data.frame(route_vehicle_g)
res_list <- list()
mapply(get_path_indx_r_v, route_vehicle_g$routeid, route_vehicle_g$vehicleid)
res_df = do.call(rbind, res_list)

write.csv(res_df, "data-calc/data_with_path_indexes.csv", fileEncoding="UTF-8", row.names = F)

get_path_indx_r_v <- function(rid, vid)
{
  print(paste('routeid', rid, 'vehicleid',vid, Sys.time(), "start"))
  data %>% filter(routeid == rid & vehicleid == vid) %>% 
    group_by_gps_records() -> gr
  print(dim(gr))
 res<-find_path_indexes(gr, rid, vid)
 print(typeof(res))
 gr$p1 <- res[[1]]
 gr$p2 <- res[[2]]
  res_list[[length(res_list)+1]] <<- gr
}


group_by_gps_records <- function(dataset) {
  nrows <- nrow(dataset)
  #print(nrows)
  cols <- c('datetime','lon','lat')
  dataset_x <- cbind(dataset[1:nrows-1,c('routeid', 'vehicleid')], dataset[1:nrows-1,cols], dataset[2:nrows,cols])
  colnames(dataset_x) <- c('routeid', 'vehicleid', 't1', 'lon1', 'lat1', 't2', 'lon2', 'lat2')
  dataset_x
}

find_path_indexes <- function(dataset, routeid, vehicleid)
{
  print(paste('routeid', routeid, 'vehicleid', vehicleid, Sys.time(), "start", "nrows:", nrow(dataset)))
  res <<- mapply(get_path_indx, dataset$routeid, dataset$lon1, dataset$lat1, dataset$lon2, dataset$lat2)
  print(paste('routeid', routeid, 'vehicleid', vehicleid, Sys.time(), "finish"))
  print(typeof(res))
  print(dim(res))
  data.frame(unlist(res[1,]), unlist(res[2,]))
}

data %>% filter(routeid == 712991 & vehicleid == 64360) -> data_712991_64364
data_712991_64364 <- data.frame(data_712991_64364)
nrows <- nrow(data_712991_64364)
cols <- c('datetime','lon','lat')
data_712991_64364_x <- cbind(data_712991_64364[1:nrows-1,c('routeid')], data_712991_64364[1:nrows-1,cols], data_712991_64364[2:nrows,cols])
colnames(data_712991_64364_x) <- c('routeid', 't1', 'lon1', 'lat1', 't2', 'lon2', 'lat2')
x <- data_712991_64364_x[11,]

routeid <- x$routeid
lon1 <- x$lon1
lat1 <- x$lat1
lon2 <- x$lon2
lat2 <- x$lat2

##Function to find closest path coord
#get_path_indx <- function(routeid, lon1, lat1, lon2, lat2) {
  df_path <- df_route_path[df_route_path$routeid == routeid, ]
  p_len <- nrow(df_path)
  rownames(df_path) <- NULL
  
  dm <- distm(cbind(c(lon1, lon2),c(lat1, lat2)), cbind(df_path$lon, df_path$lat))
  
  vdm <- unlist(dm)
  p_index1.1 <- -1
  p_index1.2 <- -1
  p_index2.1 <- -1
  p_index2.2 <- -1
  search_index <- 1
  while (search_index < p_len)
  {
    p_index1.1 <-
      which(vdm[1, ] == sort(vdm[1, ], partial = search_index)[1])[1]
    
    #print(paste("1p", p_index1.1, vdm[1, p_index1.1], sep=" "))
    
    p_index1.2 <-
      p_index1.1+ which(vdm[2, (p_index1.1+1):p_len] == sort(vdm[2, (p_index1.1+1):p_len], partial =
                                                 1)[1])[1]
    #print(paste("2p", p_index1.2, vdm[2, p_index1.2], sep=" "))
    
    ##If coordinates are in correct order
    if (p_index1.1 < p_index1.2)
    {
      break
    }
    p_index1.1 <- -1
    p_index1.2 <- -1
    search_index<- search_index+1
    #print(paste("p_index1.1 > p_index1.2, search_index+1 = ", search_index, sep=" "))
  }
  
  search_index <- 1
  while (search_index < p_len)
  {
    p_index2.1 <-
      which(rev(vdm[1, ]) == sort(vdm[1, ], partial = search_index)[1])[1]
    
    #print(paste("1p", p_index2.1, vdm[1, p_index2.1], sep=" "))
    
    p_index2.2 <-
      p_index2.1 + which(vdm[2, (p_index2.1+1):p_len] == sort(vdm[2, (p_index2.1+1):p_len], partial =
                                                               1)[1])[1]
    #print(paste("2p", p_index2.2, vdm[2, p_index2.2], sep=" "))
    
    ##If coordinates are in correct order
    if (p_index2.1 < p_index2.2)
    {
      break
    }
    p_index2.1 <- -1
    p_index2.2 <- -1
    search_index<- search_index+1
    #print(paste("p_index2.1 > p_index2.2, search_index+1 = ", search_index, sep=" "))
  }
  
  data.frame(c(p_index1.1),c(p_index1.2))
#}