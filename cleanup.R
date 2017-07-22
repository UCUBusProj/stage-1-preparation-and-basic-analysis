## Load libraries
library(dplyr)
library(jsonlite)
library(stringr)
library(lubridate)

## Fix for Ukrainian characters
Sys.setlocale('LC_ALL', 'Ukrainian')

## Load all Lviv bus stops
df_bus_stops <- fromJSON("data-init/bus_stops.json")

## Take a look at it's variance
summary(df_bus_stops)

## Take a look at top 3 rows
head(df_bus_stops, 3)

## Remove ComplexEditor column (all NA)
df_bus_stops <- subset(df_bus_stops, select=-c(ComplexEditor))

## Trim Name column values
df_bus_stops$Name <-  gsub("\"", "", df_bus_stops$Name)

## Trim Name column values
df_bus_stops$Name <- str_trim(df_bus_stops$Name)

## Rename columns
colnames(df_bus_stops) <- c("lon", "lat", "code", "name", "id")

## Take a look at it's variance
summary(df_bus_stops)

## Take a look at top 3 rows
head(df_bus_stops, 3)

## save it to 
write.csv(df_bus_stops, "data-tidy/bus_stops.csv", fileEncoding="UTF-8", row.names = F)


## Load all Lviv bus stops
df_routes <- fromJSON("data-init/routes.json")

## Take a look at it's variance
summary(df_routes)

## Take a look at top 3 rows
head(df_routes, 3)

## Remove ComplexEditor column (all NA)
df_routes <- subset(df_routes, select=-c(ComplexEditor))

## Trim Name column values
df_routes$Name <-  gsub("\"", "", df_routes$Name)

## Trim Name column values
df_routes$Name <- str_trim(df_routes$Name)

## Rename columns
colnames(df_routes) <- c("code", "name", "id")

## Take a look at it's variance
summary(df_routes)

## Take a look at top 3 rows
head(df_routes, 3)

## save it to 
write.csv(df_routes, "data-tidy/routes.csv", fileEncoding="UTF-8", row.names = F)


## Read routes stops
fn_route_stops <- list.files(path = "data-init/", pattern = "[0-9]+_stops\\.json$", full.names = T)

for(i in 1:length(fn_route_stops)){
  
  fn <- fn_route_stops[i]
  print(fn)
  
  ## Load all Lviv bus stops
  df_route_stops <- fromJSON(fn)
  
  ## Remove ComplexEditor column (all NA)
  df_route_stops <- subset(df_route_stops, select=-c(ComplexEditor))
  
  ## Trim Name column values
  df_route_stops$Name <-  gsub("\"", "", df_route_stops$Name)
  
  ## Trim Name column values
  df_route_stops$Name <- str_trim(df_route_stops$Name)
  
  ## Rename columns
  colnames(df_route_stops) <- c("lon", "lat", "code", "name", "id")
  
  fn_csv <- paste("data-tidy/", tools::file_path_sans_ext(basename(fn)), ".csv", sep = "")
  
  ## save it to 
  write.csv(df_route_stops, fn_csv, fileEncoding="UTF-8", row.names = F)
}


## Read routes path
fn_route_path <- list.files(path = "data-init/", pattern = "[0-9]+_path\\.json$", full.names = T)

for(i in 1:length(fn_route_path)){
  fn <- fn_route_path[i]
  print(fn)
  
  ## Load all Lviv bus stops
  df_route_path <- fromJSON(fn)
  
  ## Remove ComplexEditor column (all NA)
  df_route_path <- df_route_path[, c("X", "Y")]
  
  ## Rename columns
  colnames(df_route_path) <- c("lon", "lat")
  
  fn_csv <- paste("data-tidy/", tools::file_path_sans_ext(basename(fn)), ".csv", sep = "")
  
  ## save it to 
  write.csv(df_route_path, fn_csv, fileEncoding="UTF-8", row.names = F)
}

## Cleanup vehicles GPS data

data <- read.csv("data-init/busdata_220717_1230.csv", encoding="UTF-8")

## Take a look at it's variance
summary(data)

## Take a look at top 3 rows
head(data, 3)

## Convert datatime from string to POSIXct
data$datetime <- as.character(data$datetime)
data$datetime <- as.POSIXct(strptime(data$datetime, "%Y-%m-%d %H:%M:%OS", tz="GMT"))

## Convert timezone to Europe/Kiev
data$datetime <- with_tz(data$datetime, tzone = "Europe/Kiev")


## Remove columns with no data
data <- subset(data, select = -c(endpoint, iterationend, iterationstart, routecode, startpoint))

## Remove columns with useless data
data <- subset(data, select = -c(routename))


## Amalyze rows with NA values
na_data <- data[rowSums(is.na(data)) > 0, ]

summary(na_data)

# Seems like there is no useful data there, so we can remove theese rows
data <- data[rowSums(is.na(data)) == 0, ]


## Exctract data about vehicles
df_vehicles <- unique(subset(data, select = c(vehicleid, vehiclename, lowfloor)))
df_vehicles$lowfloor <- as.character(df_vehicles$lowfloor)
df_vehicles$lowfloor <- ifelse(df_vehicles$lowfloor == "t", 1, 0)
write.csv(df_vehicles, "data-tidy/vehicles.csv", fileEncoding="UTF-8", row.names = F)

## Remove columns we have already exctracted
data <- subset(data, select = -c(vehiclename, lowfloor))

## Update column names
colnames(data) <- c("datetime", "angle", "routeid", "state", "timetopoint", "vehicleid", "lon", "lat")

## Save tidy dataset
write.csv(data, "data-tidy/tidy_busdata_220717_1230.csv", fileEncoding="UTF-8", row.names = F)
