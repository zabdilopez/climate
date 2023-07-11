# Load libraries
library(tidyverse, glue, lubridate)

#global weather stations####
#first step is to obtain the invetory of weather stations from NOAA #

inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url,
           col_names = c("station","lat", "lon","variable","start","end"))

#local weather station####
#Second step is to select the area where you want to find the weather stations
my_lat <- 14.595875057696952 *2 *pi/360 # you need to convert the coordinates                                            in radians for it to make sense
my_lon <- -90.58743121166637 *2 *pi/360

#Third step is to search in the inventory the closest weather station to your position
my_station <- inventory %>%
  mutate(lat_r = lat *2 *pi/360,
         lon_r = lon *2 *pi/360,
         d = 1.609344 * 3936 * acos((sin(lat_r) * sin(my_lat)) + cos(lat_r) * cos(my_lat) * cos(my_lon - lon_r))
         )%>%
  filter(start < 1960 & end > 2020) %>%
  top_n(n = -1, d) %>%
  distinct(station)%>%
  pull(station) 

#download data from the weather station####
#with the weather station close to your position now you can download the data from it
station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz")

#this will help to check which data does the weather station gather
table1 <- read_csv(station_daily)
unique(table1$TAVG)

#arranging the information####
local_weather <- read_csv(station_daily,
         col_names = c("station", "date", "variable", "value", "a", "b", "c","d"))%>% #put corresponding names to each column
  select(date, variable, value) %>% #most relevant data is on this columns
  pivot_wider(names_from = "variable", values_from = "value")%>% #pivot to have all the info in columns
              #values_fill = 0)%>% #convert the NAs to 0. coment that out because the NAs are not necessarily 0 values
  select(date, TAVG, TMAX, TMIN, PRCP) %>% #select from the columns the info that is needed
  mutate(date = ymd(date)) #convert the dates to normal format.

#cleaning up the information
#Tmax and Tmin from tenths to normal data

local_weather$TAVG <- local_weather$TAVG /10
local_weather$TMAX <- local_weather$TMAX /10
local_weather$TMIN <- local_weather$TMIN /10

#PRCP from tenths to normal data

local_weather$PRCP <- local_weather$PRCP /10
